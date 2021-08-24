rm(list=ls())
library("tidyverse")
library("tidytext")
library("stringr")
library("jsonlite")
library("readxl")
library("rvest")
library("httr")
library("xml2")
library("RCurl")
library("magrittr")
library("furrr")
library("conflicted")
conflict_prefer("filter", "dplyr")
conflict_prefer("fromJSON","jsonlite")
conflict_prefer("flatten","jsonlite")

# Today's date
dagensdatum <- Sys.Date()


# Ställ in arbetskatalog
# Detta kommando definierar arbetskatalogen till den katalog där detta skript är sparat
# Alla skript och datafiler bör därför placeras i samma katalog
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Spara denna katalog för referens längre ned
arbetskatalog <- getwd()

# Skapar katalogen "temp tidy" i arbetskatalogen
# Denna nya katalog används nedan
dir.create("temp tidy")




################################################################################
# Kör filen "0 preamble...", som importerar vissa data och förbereder funktioner 
source("0 preamble 210819.R", local=FALSE, encoding="UTF-8")
################################################################################

################################################################################
### Kör dessa steg endast en gång
  # Importera och städa data. 
  # Dela upp i lista och behandla varje listelement som egen fil
  # Sammanfoga alla rader för respektive år. 
  # Därefter sammanfogas alla rader för alla år
################################################################################
the_years <- 2006:2020

the_years %>% map(~{
    setwd(arbetskatalog)
    thisyear <- .x
    print(thisyear)

    # Importera, redigera och dela upp i lista
    this_year_listdata <- 
      # Importera data
      my_import_raw_data_function( thisyear ) %>%
      # Filtrera för söktermer
      my_big_tidy_function() %>% 
      # Förbered för att spara
      group_by(listkolumn) %>% 
      group_split() 
  
    # Dela upp detta års data i lista
    # Tokensiera och spara data
    setwd(file.path(arbetskatalog ,"temp tidy"))
    this_year_listnumbers <- 1:length(this_year_listdata)
    # loop over list
    this_year_listnumbers %>% 
      map(~{
        this_year_listdata[[.x]] %>% 
          my_unnest_function()  %>%        # unnest and filter
          write_rds(file=paste0("temp_unnested_",thisyear,"_",.x,".rds"))
        
        return(NULL)
        }) # Detta år klart
    
    # Sammanfoga rader för detta år och spara temporär fil
    this_year_listnumbers %>% 
      map_dfr(~read_rds(file=paste0("temp_unnested_",thisyear,"_",.x,".rds"))) %>% 
      write_rds(file=paste0("temp_unnested_",thisyear,".rds"))
    
    return(NULL)
    }) # Alla år klara
  
# Sammanfoga data för alla år
setwd(file.path(arbetskatalog, "temp tidy"))
all_data_unnested <- the_years %>% 
  map_dfr(~{
    read_rds(file=paste0("temp_unnested_",.x,".rds")) %>% 
      mutate(
        employer.organization_number=as.character(employer.organization_number),
        employer.workplace = as.character(employer.workplace)
        )
    })

all_data_unnested$year %>% unique

###############################################################################
### Exportera till fil. Denna fil används för analys.
###############################################################################
setwd(arbetskatalog)
all_data_unnested %>% 
  write_rds(file=paste0("all_data_unnested_",dagensdatum,".rds"), compress='gz')



################################################################################
### Summera totalt antal annonser och vakanser per år
# resultat = en tabell som exporteras som rds-fil
################################################################################
setwd(arbetskatalog)
the_years %>% map_dfr(~{
  the_year <- .x
  print(the_year)
  
  fromJSON( paste0( the_year,".json" ) ) %>% 
    flatten() %>% 
    tibble() %>% 
    # Räkna antal
    add_tally()  %>% 
    # Välj variabler
    select(number_of_vacancies, 
           # variabel n skapas ovan
           total_n_ads_peryear = n) %>% 
    # beräkna antal vakanser
    mutate(number_of_vacancies= as.numeric(number_of_vacancies)) %>% 
    mutate(
      year= the_year,
      total_n_vacancies_peryear = sum(number_of_vacancies, na.rm=TRUE)
          ) %>% 
    relocate(year, total_n_ads_peryear, total_n_vacancies_peryear) %>% 
    distinct(year, total_n_ads_peryear, total_n_vacancies_peryear) %>% 
    
    return()
    }) %>% 
  # Exportera tabell med alla år till fil
  write_rds("total_n_ads_vacancies.rds")



################################################################################
### Beräkna antal annonser och vakanser per kommun och år
################################################################################
setwd(arbetskatalog)
the_years %>% map_dfr(~{
  the_year <- .x
  print(the_year)
  fromJSON( paste0( the_year,".json" ) ) %>% 
    flatten() %>% 
    tibble() %>% 
    
    # per municipality_code
    mutate(municipality_code = as.numeric(workplace_address.municipality_code)) %>% 
    drop_na(municipality_code) %>% 
    group_by(municipality_code) %>% 
    
    # räkna
    add_tally()  %>% 
    select(number_of_vacancies, 
           sum_n_ads_permunicipality_peryear = n) %>% 
    
    # antal vakanser
    mutate(number_of_vacancies= as.numeric(number_of_vacancies)) %>% 
    mutate(sum_n_vacancies_permunicipality_peryear = sum(number_of_vacancies, na.rm=TRUE),
           year= the_year
          ) %>% 
    relocate(year, municipality_code) %>% 
    arrange(year, municipality_code) %>% 
    distinct(year, municipality_code, 
             sum_n_ads_permunicipality_peryear, 
             sum_n_vacancies_permunicipality_peryear) %>% 
    
    return()
}) %>% 
  
  # Lägg till kommun & länskod fr SCB
  left_join(kommun_lanskoder, by = "municipality_code") %>% 

  write_rds("sum_n_ads_vacancies_permunicipality.rds")



################################################################################
### Summera antal annonser och vakanser per ssyk
# resultat = tabell med en observation per ssyk, exporteras till fil 
################################################################################
setwd(arbetskatalog)
the_years %>% map_dfr(~{
  the_year <- .x
  print(the_year)
  
  fromJSON( paste0( the_year,".json" ) ) %>% 
    flatten() %>% 
    tibble() %>%
  
  ## sammanfoga med SSYK. 
  # SSYK-tabell matchas mot AF yrkesindex
  rename(af_yrkes_id = occupation.legacy_ams_taxonomy_id) %>% 
  left_join(ssyk_af_key) %>% 
    
    # radera observationer där värden saknas för ssyk_2012 och id
    drop_na(ssyk_2012, id) %>% 
    
    # n anonnser per ssyk
    group_by(ssyk_2012) %>% 
    mutate( total_ads_perssyk_peryear = n_distinct(id, na.rm=TRUE ) ) %>% 
    
    # n vakanser per ssyk
    mutate(number_of_vacancies= as.numeric(number_of_vacancies)) %>% 
    mutate(total_vac_perssyk_peryear = sum(number_of_vacancies, na.rm=TRUE) ) %>%
    distinct(ssyk_2012, total_ads_perssyk_peryear, total_vac_perssyk_peryear) %>% 
    
    arrange(ssyk_2012) %>% mutate(year= the_year) %>% 
    
    return()
  }) %>% 
  
  # Exportera tabell till fil
  write_rds("totalsum_ads_vac_perssyk_peryear.rds")

