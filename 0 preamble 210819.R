# rm(list = ls())

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
library("rgdal")
library("maptools")
library("rgeos")
library("widyr")
library("igraph")
library("ggraph")
library("sf")
library("conflicted")
conflict_prefer("filter", "dplyr")
conflict_prefer("fromJSON","jsonlite")
conflict_prefer("flatten","jsonlite")

# Spara dagens datum i objekt
dagensdatum <- Sys.Date()

# Ställ in arbetskatalog
# Detta kommando definierar arbetskatalogen till den katalog där detta skript är sparat
# Alla skript och datafiler bör därför placeras i samma katalog
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

################################################################################
### Importera ordlistor och söktermer
################################################################################
### Söktermer. Kolumner:
  # word = matchas mot själva datan, inklusive felstavningar och annat.
  # word_rensat = den term som sökordet syftar på
ordlista_data <- read_xlsx("ordlistor textanalys.xlsx",
                           sheet="söktermer") %>% 
  select(-forklaring, -`...5`, -`...6`)

# Stoppord
stoppord_data      <- read_xlsx("ordlistor textanalys.xlsx", 
                                sheet = "stoppord") 

# Lista för att justera sammansatta ord
bytord_data        <- read_xlsx("ordlistor textanalys.xlsx", 
                                sheet="sammansatta_ord")

# Ordlista för utbildningsnivå    
ordlista_edu <- read_excel("ordlistor textanalys.xlsx", 
                           sheet="utbildning")

# Mönster som ska raderas 
remove_pattern_data <- str_c(read_xlsx("ordlistor textanalys.xlsx", 
                                       sheet="remove_pattern")$pattern, collapse = "|")

# Sentimentlista för sociala kompetenser
sentimentwords <- read_xlsx("ordlistor textanalys.xlsx", 
                            sheet="sentiments")
  
# Sammanfoga sentimentlistan med ordlistorna
ordlista_data <- ordlista_data %>% full_join(sentimentwords %>% 
                                                 mutate(omrade="sentiments", word_rensat=word))

# Radera ord i sentimentlistan från stopporden 
stoppord_data <- stoppord_data %>% anti_join(sentimentwords) 


c("basics", "programming", "mjukvara", "sentiments") %>% 
  map(~{ 
    ordlista_data$word_rensat[ordlista_data$omrade==.x] %>% unique() %>%  toString() %>% return()
  })


# Gör att vanliga företagsnamn stavas konsekvent
change_name_patterns_data <- c("sverige ab", "sweden ab", "-stockholm", "[\\s]+it", 
                               "[\\s]+ab", "pöyry","skills") %>% 
  str_c(collapse = "|")

# Arbetsgivare
arbetsgivare <- read_excel("ordlistor textanalys.xlsx", 
                           sheet="arbetsgivare") %>%
  mutate(employer = tolower(employer),
         employer = str_trim(employer, side = "both"))

### SSYK och yrkes-ID
# nyckel mellan Arbetsförmedlingens yrkes-ID och SSYK
ssyk_af_key <- 
  fromJSON(txt = "occId_2_ssyk2012.json", flatten=T) %>% flatten_df() %>% 
  pivot_longer(cols=everything()) %>% 
  mutate(af_yrkes_id=as.character(name), 
         ssyk_2012 = as.numeric(value)) %>% select(-name, -value)

# ssyk_2012 och arbetstitlar  
ssyk_key_worklabels <- 
  fromJSON(txt="occId_2_name.json", flatten=T) %>% flatten_df() %>%  pivot_longer(everything()) %>% 
  mutate(ssyk =as.numeric(name)) %>% 
  select(-name) %>% relocate(ssyk)

# Läs in SSYK, ensiffriga koder
ssyk_koder <- read_excel("ssyk-2012-koder.xlsx", 
                         sheet = "1-siffer") %>%
  rename(ssyk_2012_1 = "SSYK 2012 kod",
         yrkestitel_ssyk = "Benämning")


# Importera lista med kommun och länskoder 
kommun_lanskoder <- read_xlsx("ordlistor textanalys.xlsx", sheet="Län och kommuner 2021") %>% 
  rename(municipality_code = KnKod,
         lan_kod = LnKod) %>%
  mutate(
    municipality_code = as.numeric(municipality_code),
    municipality_code_2= as.numeric(municipality_code) %/% 100 )


#### SNI-nyckel
sni_key_df <- read_rds("sni_key_df.rds")
sni_key_df <- sni_key_df %>% rename(id=annons_id)

# Läs in SNI, tvåsiffriga koder
sni_koder <- read_excel("sni2007.xlsx",
                        sheet = "Huvudgrupp (Tvåsiffer)",
                        skip = 2) %>%
  rename(sni_2 = "Huvudgrupp",
         benamning_sni = "Benämning")

    
##########################################################################
### Importera data för att rita kartor över Sveriges kommuner
##########################################################################
download_map <- FALSE
# Ladda ej ned filerna om dessa redan ligger i katalogen eller om 'download_map' = FALSE
if (!file.exists("map_kom/Kommun_Sweref99TM_region.shp") & download_map) {
  
  # Laddar ned kartfiler från SCB
  temp <- tempfile()
  download.file("https://www.scb.se/contentassets/3443fea3fa6640f7a57ea15d9a372d33/shape_svenska.zip",temp)
  
  # Packar upp filerna med kommunkartan i working directory
  unzip(temp, files = "KommunSweref99TM.zip", exdir = paste0(getwd(),"/map_kom"))
  unzip(zipfile = paste0(getwd(), "/map_kom/KommunSweref99TM.zip"),
        exdir = paste0(getwd(), "/map_kom"))
  
  # Tar bort tempfile
  unlink(temp)
  
  # Tar bort zip-arkivet med kommunkartan
  file.remove("map_kom/KommunSweref99TM.zip")
}

# Spara geografisk information från shp-filerna i nytt R-objekt
sveriges_kommuner_karta <- st_read("map_kom/Kommun_Sweref99TM_region.shp") %>% 
  mutate_if(is.factor, as.character) %>% 
  rename(municipality_code = KnKod, 
         kommun_namn = KnNamn) %>% 
  mutate(municipality_code = as.numeric(municipality_code))



##########################################################################
# begrepp som består av flera ord ("Visual studio") slås samman till ett ord
# genom att byta ut blankslag till understreck. 
##########################################################################
fsammansatta_ord <-   function(dftxt, nyaord) 
{
  for (i in 1:nrow(nyaord)) {
    dftxt$annonstext <- str_replace_all(dftxt$annonstext,
        # Ersätt detta ord...
        regex(nyaord$orginal_ord[i],ignore_case = TRUE),
        # ...med det här ordet
        as.character(nyaord$nytt_ord[i])
      )
  }
  return(dftxt)
}

################################################################################
### Funktion för att importera data
################################################################################
my_import_raw_data_function <- function(the_year) 
  {
    # the_year <- 2006
    this_raw_data <- fromJSON( paste0( the_year,".json" ) ) %>% 
      flatten() %>% 
      tibble() %>% 
      mutate(year= the_year, 
             # annons_id
             id=as.character(id) 
             )  %>% 
    # Välj ut slumpvis rader för att testa funktionen
    slice_sample(n=10000)
      

    # Generera unika id-nummer om detta inte redan finns för samtliga annonser i tabellen
    n_obs <- this_raw_data %>% count() 
    n_id <- this_raw_data$id %>% unique() %>% length()
    
    # create unique id if missing
    if(n_obs>n_id){
      this_raw_data <- this_raw_data %>% 
                        mutate(id=paste0("y_", the_year,"_",row_number()))
                  }
    
    # Skapa några variabler
    this_raw_data %>%
    mutate(
          # Add variable on annual number of ads
          n_ads_peryear = n_distinct(id),
          # Number of vacancies
          number_of_vacancies = as.numeric(number_of_vacancies),
          # Kommunkod
          municipality_code   = as.numeric(workplace_address.municipality_code)
          ) %>% 
  
    # Spara de variabler vi ska använda
    select(
          # unikt annons-id, när det finns
          id, year, 
          # AF:s yrkesindex. Matchas mot SSYK
          af_yrkes_id = occupation.legacy_ams_taxonomy_id,
          # kommun- och länskoder
          municipality_code,
          # n annonser & vakanser
          n_ads_peryear, number_of_vacancies , 
          # annat
          description.text, 
          employer.name, employer.organization_number, employer.workplace
          ) %>% 

    ######################################################
    # Sammanfoga med SSYK & SNI: ssyk_af_key, sni_key_df
    # SNI matchas mot annons_id. Denna tabell från Simon Benjamminsson @ AF JobTechDev
    # OBS: Vissa annonser har 2+ SNI --> flera rader i tabellen, samma annons
    left_join(sni_key_df) %>% 
    
    # SSYK-tabell matchas mot AF yrkesindex
    left_join(ssyk_af_key) %>% 
    
    # Kommun och länskod läggs på från SCB
    left_join(kommun_lanskoder, by = "municipality_code") %>% 
    
    # Liten städning
    mutate(municipality_code_2 = case_when(municipality_code==1917 ~ 19,
                                           TRUE ~ municipality_code_2),
           LnNamn = case_when(municipality_code== 1917 ~ "Västmanlands län",
                              TRUE ~ LnNamn),
           KnNamn = case_when(municipality_code== 1917 ~ "Heby kommun",
                              TRUE ~ KnNamn)) %>% 
    
    # Dela upp tabellen med heltalsdivision    
    mutate(listkolumn = row_number() %/% 50000) %>% 
    
    return()
}




################################################################################
### Funktion för att redigera tabellen och förbereda data för analys
# take_this_table = tabell med rådatan, en annons per rad
################################################################################
my_big_tidy_function <- function(take_this_table) {     
  take_this_table %>% 
    mutate(
      description.text = str_replace_all(
        string = description.text,
        # radbryt (return), newline, tab
        pattern = "[\\r\\n\\t]{1,}",    
        replacement = " "           )
      ,
      description.text = str_replace_all(
        string = description.text,
        
        # mer än 1 mellanslag
        pattern = "[\\s]{2,}",
        replacement = " " )
    ) %>% 
    
    rename(annonstext= description.text) %>% 
    mutate(
      annonstext= str_replace_all(annonstext,
                                  regex("(?<=visual)\\s(?=\\w*[\\+\\#]*)", ignore_case = TRUE), "_"  ) ,
                                  # ord med '++'
      annonstext= str_replace_all(annonstext,
                                  regex("\\+\\+", ignore_case = TRUE), "_plusplus") ,
                                  # C# = C sharp
      annonstext= str_replace_all(annonstext,
                                  regex("#", ignore_case = TRUE),      "sharp") ,
      annonstext= str_replace_all(annonstext,
                                  regex("\\.net", ignore_case = TRUE),  "dotNet") ,
      annonstext= str_replace_all(annonstext,
                                  regex("R&D", ignore_case = TRUE),     "RnD") ,
      annonstext= str_replace_all(annonstext,
                                  # D++ 
                                  regex("d\\+\\+", ignore_case = TRUE), "d_plusplus") ,
      annonstext= str_replace_all(annonstext,
                                  # F++ 
                                  regex("f\\+\\+", ignore_case = TRUE), "f_plusplus") ,
      annonstext= str_replace_all(annonstext,
                                  # 'html 5' --> 'html5'
                                  regex("html\\s5", ignore_case = TRUE), "html5") ,
      annonstext= str_replace_all(annonstext,
                                  # apple.carplay --> apple_carplay , standard för koppling av iOS-enhet mot bil
                                  regex("apple\\.carplay", ignore_case = TRUE), "apple_carplay") ,
      annonstext= str_replace_all(annonstext, 
                                  # ' - '  eller  '- '
                                  regex(" - |- ", ignore_case = TRUE), " ") ,
      annonstext= str_replace_all(annonstext, 
                                  regex("\\\xad", ignore_case = TRUE), " ") 
      
    ) %>%  # end of mutate
    
    # Mer städning
    mutate(
      # ta bort telefonnummer
      annonstext = str_replace_all(annonstext, 
                                   regex("\\+46[0-9\\s-\\(\\)_]+|07[0-9\\s-_]+", ignore_case = TRUE),   "") ,
      # ta bort webbadresser
      annonstext = str_replace_all(annonstext,
                                   regex("www\\.\\w+\\.\\w+", ignore_case = TRUE), "") ,
      # ta bort kvarstående .html så att kompetenskrav på html kan mätas
      annonstext = str_replace_all(annonstext,
                                   regex("\\.html", ignore_case = TRUE), "") ,
      # ersätta bindestreck med två understreck
      annonstext = str_replace_all(annonstext, 
                                   regex("-", ignore_case = TRUE), "__") 
    ) %>%    # end of mutate 
    
    
    # remove_pattern_data = se ovan
    mutate(annonstext = str_remove_all(annonstext, 
                                       remove_pattern_data),
           annonstext = str_replace_all(annonstext, "c _plusplus", "c_plusplus")) %>% 
    
  # Ersätt sammansatta ord med 1 ord, utifrån ordlista fr fil
  fsammansatta_ord(nyaord = bytord_data) %>% 
  
  # Gemener
  mutate(annonstext = tolower( annonstext))  %>% 
  
  return()
}



################################################################################
## Funktion för att tokenisera data och fitrerar för söktermer
# Spara termerna för "område" för säkerhets skull
omraden_of_interest <-   c("basics", "programming", "mjukvara", "sentiments")
################################################################################

my_unnest_function <- function(this_tidy_list_file)
{
  temp_unnest_data <- this_tidy_list_file %>% 
  # tokenisera: analysenhet = ett ord
  unnest_tokens(output = word,          # result variable
                input = annonstext,     # unnest this variable
                token='words') %>%      # analysis unit
  # stoppord
  anti_join(stoppord_data, by = "word") %>%
  
  # sammanfoga tabeller: ord och område 
  left_join(ordlista_data,  by = "word")
  
  # skapa tabell med utbildningsnivåer
  temp_edu_table <-   temp_unnest_data %>%
    filter(word %in% ordlista_edu$word) %>%
      left_join(ordlista_edu) %>%
    select(id, edu_rank, edu_level) %>%
    group_by(id) %>%
    filter(edu_rank == max(edu_rank)) %>% # Endast högsta nivå per annons kvarstår.
    ungroup() %>%
    distinct()
  
  # Filtrera den tokeniserade tabellen  
  # Spara annonser som nämner minst en sökterm
  temp_unnest_data %>%
    filter(omrade %in% omraden_of_interest) %>% 
  
  # Sammanfoga tokeniserade tabellen och tabell med utbildningsnivå
    left_join(temp_edu_table) %>%
    distinct() %>% 
    
  # Bestäm om arbetsgivare är kommun, region, stat eller annat
    mutate(employer.name = tolower(employer.name)) %>%
    separate(employer.name, 
             sep = ",",
             into = c("employer", 
                      "employer_specification")) %>%
    left_join(arbetsgivare) %>%
    
  # Ändra "landsting" till "region"    
  mutate(employer_own_specification = ifelse(str_detect(employer, "landsting"), 
                                             "region", 
                                             employer_own_specification),
         employer_own = ifelse(str_detect(employer, "landsting"), 
                               "region", 
                               employer_own),
           
         # Använd företagsnamn för privat sektor
         employer_own = ifelse(is.na(employer_own), 
                               employer, 
                               employer_own)) %>% 
    
  # Ändra stavning för några vanligt förekommande arbetsgivare 
  mutate(employer_own = str_remove_all(employer_own, change_name_patterns_data),
         employer_own = str_trim(employer_own),
         employer_own = ifelse(employer_own == "manpower professional", 
                               "manpower", 
                               employer_own)) %>% 
  return()
}





################################################################################
### Funktion för att analysera utbildningsnivå 
# this_unnested_file = tokeniserade tabellen som skapas ovan
# inquiry = de utbildningsnivåer som efterfrågas i annonsen
# resultat: 3 tabeller 
################################################################################
education_level_function <-  function(this_unnested_file)
{
  this_unnested_file <- this_unnested_file %>%
  # Slå samman omrade
  mutate(omrade=if_else(omrade=="mjukvara","programming",omrade)) %>% 
    
  # 0. Transformera till faktorvärden, importera SSYK-data
  mutate(edu_level = factor(edu_level, levels = c("grundskola", "gymnasie",
                                            "yrkeshögskola", "högskola",
                                            "doktor"))) %>%
    
  # SSYK-titlar
  mutate(ssyk_2012_1 = str_extract(ssyk_2012, "[\\d]"),
         ssyk_2012_1 = as.numeric(ssyk_2012_1)) %>%
  left_join(ssyk_koder)

  ### 1. Räkna antal som eftefrågar en utbildningsnivå per område och år
  # a. Räkna antal poster per område och år
  annual_omrade <- this_unnested_file %>%
    select(id, year, total_n_ads_peryear, omrade) %>%
    distinct() %>%
    count(omrade, year, total_n_ads_peryear) %>%
    rename(n_omrade = n)
  
  # b. Räkna procentuell andel av annonserna som nämner utbildningsnivå, per område och år
  annual_edu <- this_unnested_file %>%
    select(edu_level, omrade, id, year, edu_rank) %>%
    distinct() %>%
    count(year, omrade, edu_level, .drop = TRUE) %>%
    rename(n_level = n) %>%
    full_join(annual_omrade) %>%
    mutate(n_level = n_level / n_omrade,
           edu_level = factor(edu_level, levels = c("grundskola", "gymnasie",
                                            "yrkeshögskola", "högskola",
                                            "doktor"))) %>%
    arrange(edu_level) %>%
    pivot_wider(names_from = edu_level, values_from = n_level) %>%
    select(-n_omrade, n_omrade, # Flytta variabler till sista kolumn.
           -total_n_ads_peryear, total_n_ads_peryear)
  
  ### 2. Utbildningsnivå enligt SSYK:s klassifikation
  # a. Extrahera första siffran från SSYK_2012
  this_unnested_file <- this_unnested_file %>%
    mutate(ssyk_2012_1 = str_extract(ssyk_2012, "[\\d]"),
           ssyk_2012_1 = as.numeric(ssyk_2012_1)) %>%
    left_join(ssyk_koder)
  
  # b. Andel poster med SSYK-titlar som saknar uttalade krav på utbildningsnivå
  ssyk_na <- this_unnested_file %>%
    filter(is.na(edu_level)) %>%
    count(yrkestitel_ssyk, ssyk_2012_1) %>%
    arrange(ssyk_2012_1) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-ssyk_2012_1) %>%
    rename(n_na = n,
           proportion_na = proportion)
  
  # c. Andel annonser med SSYK-titel som har uttryckligt krav på utbildningsnå
  ssyk_edu <- this_unnested_file %>%
    filter(!is.na(edu_level)) %>%
    count(yrkestitel_ssyk, ssyk_2012_1) %>%
    arrange(ssyk_2012_1) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-ssyk_2012_1) %>%
    left_join(ssyk_na)
  
  ### 3. Andel eftefrågad utbildningsnivå i relation till SSYK-klassificering. Ignorera NA.
  ssyk_edu_levels <- this_unnested_file %>%
    count(yrkestitel_ssyk, ssyk_2012_1, edu_level) %>%
    mutate(n = n / sum(n)) %>%
    pivot_wider(names_from = edu_level, values_from = n) %>%
    arrange(ssyk_2012_1) %>%
    select(-ssyk_2012_1)
  
  return(list(annual_edu, ssyk_edu, ssyk_edu_levels))
}


  
  
  
################################################################################
### Function för att analysera arbetsgivare, 
# this_unnested_file = tokeniserade tabellen som skapas ovan
# inquiry: fördelning av privata och offentliga arbetsgivare
# resultat: 5 tabeller 
################################################################################
owners_function <-  function(this_unnested_file)
{
  this_unnested_file <- this_unnested_file %>% filter(omrade %in% c("programming", "mjukvara"))
  
  ### 1. List topp tio arbetsgivare, 2006-2020
  top_ten_total <- this_unnested_file %>%
    select(id, year, employer_own, omrade) %>%
    distinct() %>%
    count(employer_own, sort = TRUE) %>%
    top_n(10)
  
  
  ### 2. Räkna annonser per år för de arbetsgivare som är bland största annonsör minst ett år.
  # a. Lista arbetsgivare som varit topp 5 minst ett år
  top_five <- this_unnested_file %>%
    select(id, year, employer_own, omrade) %>%
    distinct() %>%
    count(employer_own, year) %>%
    group_by(year) %>%
    top_n(5) %>%
    ungroup()
  
  # b. Antal annonser per år för topp-5-arbetsgivarna
  top_five_year <- this_unnested_file %>%
    filter(employer_own %in% top_five$employer_own) %>%
    select(id, year, employer_own, omrade) %>%
    distinct() %>%
    count(employer_own, year, sort = TRUE) %>%
    group_by(year) %>%
    top_n(5) %>%
    arrange(year) %>%
    ungroup() %>%
    pivot_wider(names_from = year, values_from = n)
  
  ### 3. Topp 10 arbetsgivarna per område 2006-2020
  top_ten_area <- this_unnested_file %>%
    select(id, year, employer_own, omrade) %>%
    distinct() %>%
    count(employer_own, omrade, sort = TRUE) %>%
    group_by(omrade) %>%
    top_n(n = 10) %>%
    ungroup() %>%
    arrange(omrade)
  
  
  ### 4. Anlysera fördlening av annonser enligt SNI
  # a. Sammanfoga data med SNI-koder 
  df_own_sni  <- this_unnested_file %>%
    mutate(sni_2 = str_extract(sni, "[\\d]{2}")) %>%
    left_join(sni_koder) %>%
    filter(!is.na(sni))
  
  # b. Lista vanligaste SNI-koderna
  top_ten_sni <- df_own_sni %>%
    select(id,  employer_own, benamning_sni) %>%
    distinct() %>%
    count(benamning_sni, sort = TRUE) %>%
    mutate(percent = n / sum(n)) %>%
    top_n(10)
  
  
  ### 5. Lista vanligaste digitala kompetenserna som efterfrågas av offentlig sektor
  top_ten_public <- this_unnested_file %>%
    filter(employer_own %in% c("kommun", "region", "stat")) %>%
    select(id, year, word_rensat, employer_own) %>%
    distinct() %>%
    count(employer_own, word_rensat, sort = TRUE) %>%
    group_by(employer_own) %>%
    top_n(10) %>%
    ungroup()
  
  return(list(top_ten_total, top_five_year, top_ten_area, top_ten_sni, top_ten_public))
}



