rm(list=ls())
library("tidyverse")
library("patchwork")
library("ggrepel")
library("conflicted")
library("writexl")
conflict_prefer("filter", "dplyr")
conflict_prefer("flatten", "jsonlite")
conflict_prefer("lag", "dplyr")
options(scipen=999)

### Digitaliseringsprojektets grafiska profil
tvv_orange <- "#E87200"
tvv_orangegreen <- "#748654"
tvv_green <- "#0099A8"
tvv_blue <- "#3A8DDE"
tvv_purple <- "#9164CC"

# Ställ in arbetskatalog
# Detta kommando definierar arbetskatalogen till den katalog där detta skript är sparat
# Alla skript och datafiler bör därför placeras i samma katalog
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##########################################################################
### Import prepped lists & functions
source("0 preamble 210819.R", local=FALSE, encoding="UTF-8")

### Import ssyk labels
ssyk_labels <- read_xlsx("ssyk-2012-koder.xlsx", sheet="4-siffer") %>% 
  rename(ssyk_2012 = `SSYK 2012 kod`,
         ssyk_label = Benämning)

#### Count total n ads
read_rds("total_n_ads_vacancies.rds") %>% 
  summarise(sum(total_n_ads_peryear ), 
            sum(total_n_vacancies_peryear))

##########################################################################
### Import main dataset
all_data_unnested <-
        # get rds file with unnested data
        read_rds("all_data_unnested.rds") %>%
        
        # import and merge with count of total number of ads per year
        left_join(read_rds("total_n_ads_vacancies.rds")  ) %>% 
  
        ## get a sni-key for sni bokstav
        mutate(sni_5siff = sni,
               sni_2siff = str_sub(sni_5siff,1,2)) %>%
        left_join(
                read_xlsx("sni2007.xlsx", 
                          sheet="Huvudgrupp (Tvåsiffer)",
                                      range="a3:d91") %>%
                        select(sni_2siff=Huvudgrupp, 
                               sni_bkstv = SNI2007Avdelning,
                               sni_label = Aktivitetsart
                               )
                ) %>% 
  # join some missing stuff for omrade=mjukvara
  left_join(
            read_xlsx("ordlistor textanalys.xlsx", sheet="söktermer") %>% 
            filter(omrade=="mjukvara") %>% 
            select(word, wr_temp = word_rensat) , 
      by="word") %>% 
  mutate(word_rensat = 
           if_else(omrade=="mjukvara", 
                   wr_temp, 
                   word_rensat)) %>% 
  select(-wr_temp) 

all_data_unnested <- all_data_unnested %>% 
  mutate(word_rensat=if_else(word_rensat=="MATHLAB", "MATLAB", word_rensat))
 
#### list all search terms
"all_data_unnested$omrade %>% unique %>% map(
  ~{
    dff <- all_data_unnested %>%
      filter(omrade==.x) %>% 
      distinct(word_rensat) %>% 
      arrange(word_rensat)
    
    dff$word_rensat %>% 
      unique %>% 
      toString() %>% 
      return()
  })"





### list search terms for educ
edu_words <- read_excel("ordlistor textanalys.xlsx", sheet="utbildning") %>% 
  select(word) %>% unique  
edu_words$word %>% toString()

#### sample data w only the social skills
data_unnested_social <- 
  all_data_unnested %>% filter(omrade=="sentiments")

# resave main dataset w/o social skills
all_data_unnested <- all_data_unnested %>% 
  filter(omrade!="sentiments")

# sort out which ads to keep for the social skills
# filter for ads that mention digital skills
  the_ad_ids <- all_data_unnested$id
  
  data_unnested_social <- data_unnested_social %>% 
    filter(id %in% the_ad_ids) 

  
################################################################################
### check specified municipality for large employers  
all_data_unnested %>% filter(municipality_code==1280,
                             year>=2018) %>% 
    distinct(id, employer) %>%
    count(employer) %>% arrange(desc(n))
  
  
  
  
  
  
  
################################################################################
### GRAPH RESULTS
################################################################################
  
  
###############################################################################
### Maps of Sweden
# by municipality (kommun) = municipality_code 
###############################################################################
sveriges_kommuner_karta

our_threshold <- .08
the_map_years <- 2018
number_ads_thres <- 100

gfsl <- geom_sf_label(aes(geometry=geometry , 
                          label=if_else(prc_ds_ads_permunicipality> our_threshold, 
                                        kommun_namn, NULL)), 
                      size=2, hjust="inward", vjust="outward") 

###############################################################################
sum_ads_per_municipality <- 
  read_rds("sum_n_ads_vacancies_permunicipality.rds")  %>% 
  filter(year >= the_map_years) %>% 
  # sum per municipality, time period
  group_by(municipality_code) %>% 
  summarise(total_ads_permunicipality = sum(sum_n_ads_permunicipality_peryear, na.rm=TRUE)) 

ads_perkommun_formap <- all_data_unnested %>% 
  filter(
      # only selected years
      year >= the_map_years,
      # only digital skills
      omrade != "sentiments", 
      omrade != "basics",
      # filter kommun
      !is.na(municipality_code) 
      ) %>% 
  # count DS-ads per county
  distinct(municipality_code, id) %>% 
  group_by(municipality_code) %>% 
  summarise(n_ds_ads_permunicipality = n_distinct(id, na.rm=TRUE)) %>% 
  # join with sums of ads & vac per municipality
  left_join(sum_ads_per_municipality) %>% 
  # calc percent DS / total n ads
  mutate(prc_ds_ads_permunicipality = n_ds_ads_permunicipality / total_ads_permunicipality)

### export results table
ads_perkommun_formap %>% 
  full_join(sveriges_kommuner_karta) %>% 
  select(municipality_code: kommun_namn) %>% write.csv2("ads_perkommun.csv")

### Graph the map of Sweden
g_sweden <- ads_perkommun_formap %>% 
  full_join(sveriges_kommuner_karta) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry,  
              fill= prc_ds_ads_permunicipality)) +
  theme_void() + 
  theme(legend.position='none') +
  scale_fill_gradient(high=tvv_orange, low='white', 
                      name="Procent", 
                      breaks = c(0,.05,.1,.15,.2,.25),
                      labels = scales::percent_format()) 

g_sweden
####################################################################
### big city counties
####################################################################
sum_ads_per_municipality <- 
  read_rds("sum_n_ads_vacancies_permunicipality.rds") %>% 
  filter(year >= the_map_years) %>% 
  # sum per municipality
  group_by(municipality_code) %>% 
  summarise(total_ads_permunicipality = sum(sum_n_ads_permunicipality_peryear, na.rm=TRUE))  

ads_perkommun_formap <- all_data_unnested %>% 
  filter(
    # only selected years
    year >= the_map_years,
    # only digital skills
    omrade != "sentiments",
    omrade != "basics",
    # filter kommun
    !is.na(municipality_code) 
    ) %>% 
  # count DS-ads per municipality
    distinct(municipality_code, id) %>% 
    group_by(municipality_code) %>% 
    summarise(n_ds_ads_permunicipality = n_distinct(id, na.rm=TRUE)) %>% 
  # add total sum of ads
    left_join(sum_ads_per_municipality) %>% 
  # calc prc DS ads
    mutate(prc_ds_ads_permunicipality = n_ds_ads_permunicipality / total_ads_permunicipality) 


ads_perkommun_formap %>% 
# export to excel
  write_xlsx("data figur 3.xlsx")


## loop storstadslänen
big_cities <- c(12,14,1) %>% map(~{ 
  ads_perkommun_formap %>% 
    mutate(county_code = municipality_code %/% 100) %>% 
    full_join(sveriges_kommuner_karta) %>% 
    filter(county_code == .x) %>% 
    # graph this län
    ggplot() +
    geom_sf(aes(geometry = geometry, 
                fill= prc_ds_ads_permunicipality), show.legend = FALSE) +
    theme_void() + 
    scale_fill_gradient(high=tvv_orange, low='white',
                        name="Procent", 
                        breaks = c(0,.05,.1,.15,.2,.25),
                        labels = scales::percent_format())
})

## map on sthlm
sthlm <- ads_perkommun_formap %>% 
  mutate(county_code = municipality_code %/% 100) %>% 
  full_join(sveriges_kommuner_karta) %>% 
  # filter for sthlm
  filter(county_code == 1) %>% 
  # the graph map
  ggplot() + geom_sf(aes(geometry = geometry, 
                         fill= prc_ds_ads_permunicipality)) + 
  theme_void() + 
  theme(legend.position = 'left') +
  scale_fill_gradient(high=tvv_orange, low='white',
                      name="Procent", 
                      breaks = c(0,.05,.1,.15,.2,.25),
                      labels = scales::percent_format()) 

## "Andel annonser för digital kompetens \nper kommun senaste åren"
g_bigcities_1 <- 
  ( sthlm + ggtitle("Stockholm") ) + 
  ( 
    (big_cities[[2]] + ggtitle("Västa Götaland") ) / 
      (big_cities[[1]] + ggtitle("Skåne") )
  ) 


g_bigcities_2 <- 
  ( sthlm + ggtitle("Stockholm") + gfsl ) + 
  ( 
    (big_cities[[2]] + ggtitle("Västa Götaland") + gfsl ) / 
      (big_cities[[1]] + ggtitle("Skåne") + gfsl )
  ) 

## grid layout for patchwork
layout <- c(
  area(t=1,l=1,b=10,r=2), 
  area(t=2,l=3,b=9,r=6)
)

g_mymaps_1 <- g_sweden + 
  labs(subtitle="Procent av samtliga annonser per kommun 2018-2020") +
  g_bigcities_1 + plot_layout(design=layout)

g_mymaps_2 <- g_sweden + 
  geom_sf_label(aes(geometry = geometry , 
                    label = if_else(prc_ds_ads_permunicipality > our_threshold &
                                      n_ds_ads_permunicipality > number_ads_thres, 
                                    kommun_namn, NULL)),
                size=2, 
                hjust="inward", 
                vjust="outward") +
  labs(subtitle="Procent av samtliga annonser per kommun 2018-2020") +
  
  g_bigcities_2 + 
  plot_layout(design=layout) +
  plot_annotation(caption=paste0("Kommuner markerade med namn: >",our_threshold*100," % annonser som nämner ett eller flera programspråk, 
                                 motsvarande minst ", number_ads_thres," annonser. Grå kommuner = data saknas."))


g_mymaps_2
g_mymaps_1 %>% ggsave(file="g_mymaps_1.png", width=8, height=5)
g_mymaps_2 %>% ggsave(file="g_mymaps_2.png", width=9, height=7)





###############################################################################
### MAPS OF SWEDEN
# by county (län) = municipality_code_2
###############################################################################
sum_ads_per_county <- 
  read_rds("sum_n_ads_vacancies_permunicipality.rds") %>% 
  # sum per county, year
  group_by(year, municipality_code_2) %>% 
  # ignore municipality
  summarise(total_ads_percounty = sum(sum_n_ads_permunicipality_peryear, na.rm=TRUE)) %>% 
  # rename for join-commands below
  rename(county_code = municipality_code_2)

ads_perlan_formap <- all_data_unnested %>% 
  filter(!is.na(municipality_code_2)) %>%
  # simplify
  mutate(county_code = as.character(municipality_code_2)) %>% 
  
  # count DS-ads per county
  distinct(year, county_code, id) %>% 
  group_by(year, county_code) %>% 
  summarise(n_ads_perlan = n_distinct(id, na.rm=TRUE)) %>% 
  
  # join with sums of ads & vac per municipality
  mutate(county_code = as.numeric(county_code)) %>% 
  left_join(sum_ads_per_county) %>% 
  
  # calc percent DS / total n ads
  mutate(prc_ds_ads_percounty = n_ads_perlan / total_ads_percounty)
###############################################################################


















################################################################################
  # Some graph settings
  li <- geom_line(color=tvv_purple, size=1.2)
  th <- theme(
    axis.text.x = element_text(angle=90), 
    text = element_text(size=8)
  ) 
  #axis.title.y = element_text(angle=90),
  ga1 <- geom_area(position='stack') 
  ga2 <- geom_area(position='stack', color='black', show.legend = FALSE)
  scx <- scale_x_continuous(breaks=2006:2020, name=NULL)
  scy <- scale_y_continuous(labels = function(x) paste0(x*100, " %")) # Multiply by 100 & add % 
################################################################################



##########################################################################
### N ads, per omrade
##########################################################################
n_DS_ads <- all_data_unnested %>% 
  # fix omrade
  mutate(omrade = if_else(omrade=="mjukvara","1_avancerad",omrade) ) %>% 
  mutate(omrade = if_else(omrade=="programming", "1_avancerad", omrade)) %>% 
  mutate(omrade = if_else(omrade=="basics", "3_basics", omrade)) %>% 
    
  distinct(year, id) %>% 
  group_by(year) %>% 
  mutate(n_DS_ads = n_distinct(id)) %>% distinct(year, n_DS_ads) %>% 
  # import and merge with count of total number of ads per year
  left_join(read_rds("total_n_ads_vacancies.rds")  )  
  
################################################# 
# count basic / programming / both  
  
n_ads_peromrade <- all_data_unnested %>%
  # fix omrade
  mutate(omrade = if_else(omrade=="mjukvara","1_avancerad",omrade) ) %>% 
  mutate(omrade = if_else(omrade=="programming", "1_avancerad", omrade)) %>% 
  mutate(omrade = if_else(omrade=="basics", "3_basics", omrade)) %>% 
      
  # spara distinkta obs
  distinct(year, id, omrade) %>%
  # räkna annons-id
    group_by(year) %>% 
    add_count(id) %>% 
  # koda som "both"
    mutate(omrade = if_else(n>1, "2_both", omrade)) %>% 
  # spara distinkta obs
  distinct(year, id, omrade) %>% 
  
  # gruppera per år och område
  group_by(year, omrade) %>% 
  mutate(n_peromrade = n_distinct(id)) %>%
    
  distinct(year, omrade, n_peromrade) %>% 
  # import and merge with count of total number of ads per year
  left_join(read_rds( "total_n_ads_vacancies.rds")  )  


g1 <- ggplot(n_ads_peromrade, 
             aes(x=year, y=n_peromrade / 1000, 
             fill=omrade, 
             linetype=omrade)) +
    geom_area(color="black") +
    th + scx +
    labs(y="Antal annonser, 1000-tal") +
    theme(legend.position = 'none') +
    scale_fill_manual(breaks=c("1_avancerad","2_both", "3_basics"),
                      values=c(tvv_orange,tvv_orangegreen, tvv_green))

g1
g_ads_prc <- ggplot(n_ads_peromrade ,
                    aes(x=year, y=n_peromrade / total_n_ads_peryear, 
                        linetype=omrade, fill=omrade)) +
  geom_area(color='black') +
  th + scx + scy +
  labs(y="Procent av alla annonser") +
  scale_linetype(name=NULL, labels=NULL, breaks=NULL) +
  scale_fill_manual(name="Kompetens \nsom nämns", 
                          breaks=c("1_avancerad","2_both", "3_basics"),
                          labels=c("Fördjupad","Grundläggande &\nfördjupad", "Grundläggande"), 
                      values=c(tvv_orange, tvv_orangegreen, tvv_green)    )

g_nads_peromrade <-  (g1 + g_ads_prc)
g_nads_peromrade
# g_nads_peromrade %>% ggsave(file="g_nads_peromrade.png", width=7, height=4)




################################################################################
### N vacancies
################################################################################
n_vac <-   all_data_unnested %>%
  # fix omrade
  mutate(omrade = if_else(omrade=="mjukvara","1_avancerad",omrade) ) %>% 
  mutate(omrade = if_else(omrade=="programming", "1_avancerad", omrade)) %>% 
  mutate(omrade = if_else(omrade=="basics", "3_basics", omrade)) %>% 
  
  distinct(year, id, number_of_vacancies) %>% 
  group_by(year) %>% 
  mutate(n_vac = sum(number_of_vacancies, na.rm = TRUE)) %>% 
  # import and merge with count of total number of ads per year
  left_join(read_rds("total_n_ads_vacancies.rds") ) %>% 
  distinct(year, n_vac, total_n_vacancies_peryear) 

n_vac_peromrade <- all_data_unnested %>% 
  # fix omrade
  mutate(omrade = if_else(omrade=="mjukvara","1_avancerad",omrade) ) %>% 
  mutate(omrade = if_else(omrade=="programming", "1_avancerad", omrade)) %>% 
  mutate(omrade = if_else(omrade=="basics", "3_basics", omrade)) %>% 
  
  # spara distinkta obs
  distinct(year, omrade, id, number_of_vacancies) %>% 
  
  # identifiera annonser som har omrade=both
  # räkna annons-id
  group_by(year) %>% 
  add_count(id) %>% 
  # koda som "both"
  mutate(omrade = if_else(n>1, "2_both", omrade)) %>% 
  # spara distinkta obs
  distinct(year, id, omrade, number_of_vacancies) %>% 
  
  # n vacancies per id
  #group_by(id) %>% 
  #mutate(nv_perid = number_of_vacancies / n() , 
  #       nv_perid = if_else(is.na(nv_perid), 0, nv_perid)) %>% 
  # sum per id, year, omrade
  group_by(year, omrade) %>% 
  mutate(n_v_peromrade = sum(number_of_vacancies, na.rm = TRUE)) %>% 
  
  # import and merge with count of total number of ads per year
  left_join(read_rds("total_n_ads_vacancies.rds") ) %>% 
  distinct(year, omrade, n_v_peromrade, total_n_vacancies_peryear)
  

g_vac_n <-  ggplot(n_vac_peromrade, 
                   aes(x=year, 
                    y=n_v_peromrade, 
                    fill=omrade, 
                    linetype=omrade)) +
  geom_area(color='black') +
  th + scx +
  labs(y="Antal vakanser, 1000-tal") +
  theme(legend.position = 'none') +
  scale_linetype(name=NULL, labels=NULL, breaks=NULL) +
  scale_fill_manual(name="Kompetens", 
                    breaks=c("1_avancerad","2_both", "3_basics"),
                    labels=c("Fördjupad","Grundläggande &\nfördjupad", "Grundläggande"), 
                    values=c(tvv_orange, tvv_orangegreen, tvv_green)    )


g_vac_n
g_vac_prc <- 
  ggplot(n_vac_peromrade, 
         aes(x=year, 
             y=n_v_peromrade / total_n_vacancies_peryear, 
             fill=omrade, 
             linetype=omrade)) +
  geom_area(color='black') +
  th + scx + scy+
  labs(y="Procent av alla vakanser") +
  scale_linetype(name=NULL, labels=NULL, breaks=NULL) +
  scale_fill_manual(name="Kompetens", 
                    breaks=c("1_avancerad","2_both", "3_basics"),
                    labels=c("Fördjupad","Grundläggande &\nfördjupad", "Grundläggande"), 
                    values=c(tvv_orange, tvv_orangegreen, tvv_green)    )



(g_vac_n + g_vac_prc)
# (g_vac_n + g_vac_prc) %>% ggsave(file="g_vac_peromrade.png", width=7, height=3)


############################################################
# export graph to file: prc vacancies & ads
g_vac_ads_prc <- 
(g_vac_prc + labs(title="Vakanser") +
  theme(legend.position = 'none')) +  
  (g_ads_prc + labs(title="Annonser") ) 

g_vac_ads_prc
g_vac_ads_prc %>% 
  ggsave(file="g_vac_ads_prc.png", width=7, height=4)
  



################################################################################
### count per word
################################################################################
n_words_allyears <- all_data_unnested %>% 
  count(omrade, word_rensat) %>% 
  drop_na(omrade, word_rensat) %>% 
  distinct(omrade, word_rensat, n) %>%
  group_by(omrade) %>% 
  arrange(n) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  mutate(word_rensat = reorder_within(word_rensat, n, omrade)) %>% 
  
  # the graph
  ggplot(aes(y=word_rensat,
             x=n / 1000, 
             fill=omrade )
         ) + 
  scale_fill_manual(values= c(tvv_green, tvv_orange, tvv_blue)) + 
  scale_y_reordered() +
  geom_col(show.legend=FALSE) + 
  facet_wrap(~omrade, scales='free') +
  labs(x="Antal omnämnande, 1000-tal", y=NULL)
  
n_words_allyears
# n_words_allyears %>% ggsave(file="n_words_allyears.png", width=7, height=3)


################################################################################
# n words per year | programming
################################################################################
temp_df <- all_data_unnested %>% 
  filter(omrade=="programming") %>%
  group_by(year) %>% 
  count(word_rensat) %>% 
  distinct(year, word_rensat, n) %>%
  ungroup()  


  # the graph
g_proglang_year <- 
temp_df %>% 
  group_by(year) %>% top_n(10) %>% ungroup() %>% 
  # add ads count per year
  left_join(
    all_data_unnested %>% distinct(year, total_n_ads_peryear )
  ) %>% 
  #filter(word_rensat=="python")
  ggplot(aes(x=year,
             y=n/ total_n_ads_peryear, 
             color=fct_reorder(.f = word_rensat, 
                               .x = n, 
                               .fun = tail, n = 1, .desc = TRUE) 
             )) + 
  geom_line() + scx +
  scale_color_discrete(name="programming", 
                       guide=guide_legend(ncol=2)) +
  labs(x=NULL,
       y="Andel av alla annonser",
       caption="Inkl. 10 vanligaste resp. år.") +
  theme(axis.text.x = element_text(angle=90)) 
  
g_proglang_year
# g_proglang_year %>% ggsave(file="g_proglang_year.png", width=7, height=3)


################################################################################
### n vacancies per skill  
# barplot w pedagogic arrows > library("ggpubr")
library("ggpubr")
library("zoo")
################################################################################
temp_df_proglang <- all_data_unnested %>% 
  filter(omrade=="programming") %>%
  group_by(year) %>% 
  # count word_rensat = summarize to n
  count(word_rensat) %>% 
  # calc moving average
  arrange(word_rensat, year) %>% 
  group_by(word_rensat) %>% 
  mutate(n_movmean = rollmean(n, k=3, fill=NA, align='right')  ) %>% 
  # tidy, trim
  distinct(year, word_rensat, n, n_movmean) %>%
  ungroup() %>% 
  # calc change in moving average
  mutate(diff_prog= n_movmean - lag(n_movmean, 3)) %>% 
  #mutate(diff_prog = if_else(year==2020, lag(diff_prog), diff_prog)) %>% 
  mutate(increasing = if_else(diff_prog >0, TRUE, FALSE)) %>% 
  filter(year==2020) %>% 
  arrange(desc(n)) %>%
  ungroup() %>% 
  slice_max(n, n=15)   

# exportera till excel 
temp_df_proglang %>% 
  write_xlsx("data figur 2.xlsx")

# the graph
g_pedagogic <- temp_df_proglang %>% 
  ggplot(aes(y=reorder(word_rensat, n) , 
             x=n)) + 
  geom_col(fill=tvv_orange) +
  geom_point(temp_df_proglang %>% filter(increasing==TRUE),
             mapping=aes(y=reorder(word_rensat, n) , 
                         x=n),
             shape=24, size=4, fill=tvv_green, color=tvv_green) +
  geom_point(temp_df_proglang %>% filter(increasing==FALSE),
             mapping=aes(y=reorder(word_rensat, n) , 
                         x=n),
             shape=25, size=4, fill='red', color='red') + 
  labs(x="Antal annonser", 
       title="Annonser programspråk",
       subtitle="Antal 2020 (staplar) och förändring 2017-2020 (pilar)",
       y=NULL, 
       caption="Förändring beräknat på medelvärden för tre senaste åren: 2018-2020 minus 2015-2017.") +
  theme(text=element_text(size=10))

g_pedagogic
g_pedagogic %>%  ggsave(file="g_pedagogic.png", width=5, height=4)












################################################################################
### n vacancies per ssyk
################################################################################
## count per ssyk 1
temp_ssyk_sums_df <- all_data_unnested %>% 
  filter(omrade=="basics") %>% 
  # n per ssyk level 1-siffra
  mutate(ssyk_1siff = ssyk_2012 %/% 1000) %>% 
  drop_na(ssyk_1siff) %>% 
  # distinct id
  distinct(year, id, ssyk_1siff, number_of_vacancies) %>% 
  # n vacancies per id
  group_by(id) %>% 
  mutate(n_vc_id = number_of_vacancies / n(), 
         n_vc_id = if_else(is.na(n_vc_id), 0, n_vc_id)
        ) %>% 
  # n vac per ssyk_1siff
    group_by(year, ssyk_1siff) %>% 
    mutate( nv_perssyk = sum(n_vc_id, na.rm=TRUE ) ) %>%
    distinct(year, ssyk_1siff, nv_perssyk) %>% 
    ungroup %>% 
  
  # labels for graph
  mutate(ssyk_label=case_when(
    ssyk_1siff==1 ~ "Chefsyrken",
    ssyk_1siff==2 ~ "Fördjupad högskolekompetens",
    ssyk_1siff==3 ~ "Högskolekompetens el. motsv.",
    ssyk_1siff==4 ~ "Administration och kundtjänst",
    ssyk_1siff==5 ~ "Service, omsorg, försäljning",
    ssyk_1siff==6 ~ "Lantbruk, skogsbruk, trädgård",
    ssyk_1siff==7 ~ "Bygg",
    ssyk_1siff==8 ~ "Transport, maskinell tillverkning",
    ssyk_1siff==9 ~ "Kortare utbildning",
    ssyk_1siff==0 ~ "Militärt arbete"
  ))

# exportera till excel 
temp_ssyk_sums_df %>% 
  arrange(nv_perssyk) %>% 
  write_xlsx("data figur 7.xlsx")


### graph ssyk 1: n ads per ssyk
g_ssyk_n <- temp_ssyk_sums_df %>% 
  ggplot(aes(x=year, 
             y=nv_perssyk / 1000, 
             color=fct_reorder(.f=ssyk_label, 
                               .x=nv_perssyk, 
                               .fun=tail, n = 1, .desc = TRUE), 
             linetype=fct_reorder(.f=ssyk_label, 
                                  .x=nv_perssyk, 
                                  .fun=tail, n = 1, .desc = TRUE)) ) + 
  geom_line() +
  scx + th +
  theme(legend.position = 'none') +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
  labs(color="Yrkesområde", linetype="Yrkesområde", 
       y= "Antal vakanser, 1000-tal", 
       subtitle="Antal vakanser")

g_ssyk_n


### graph ssyk 2: % ads per ssyk
g_ssyk_prc <- temp_ssyk_sums_df %>% 
  # shares
  group_by(year) %>%  
  mutate(prc_nv_perssyk= nv_perssyk/ sum(nv_perssyk, na.rm=TRUE)) %>% 
  # graph
  filter(year!=2006) %>% 
  ggplot(aes(x=year, 
             y=prc_nv_perssyk, 
             color=fct_reorder(.f=ssyk_label, .x=nv_perssyk, 
                               .fun=tail, n = 1, .desc = TRUE), 
             linetype=fct_reorder(.f=ssyk_label, .x=nv_perssyk, 
                                  .fun=tail, n = 1, .desc = TRUE) )
         ) + 
  geom_line() +
  scx + th + scy +
  labs(color="Yrkesområde", linetype="Yrkesområde", 
       y= "Procent", 
       subtitle="Fördelning av vakanser som eftefrågar \ndigital kompetens")

# Exportera de 2 diagrammen i en fil
(g_ssyk_n + g_ssyk_prc )  + 
  theme(legend.position="right")

(g_ssyk_n + g_ssyk_prc) %>% 
  ggsave(file="g_ssyk.png", width=7, height=4)


g_ssyk_prc
#g_ssyk_prc %>% ggsave(file="g_ssyk_prc.png", width=6, height=3)


################################################################################
### ssyk 5-siff
selected_year <- 2020
################################################################################
temp_df_ssyk5 <- all_data_unnested %>% 
  filter(year==selected_year) %>% 
  mutate(omrade=if_else(omrade=="mjukvara","programming",omrade)) %>% 
  
  # n per ssyk 
  drop_na(ssyk_2012) %>% 
  # distinct id
  distinct(id, omrade, 
           ssyk_2012, number_of_vacancies) %>% 
  # n vacancies per id
  group_by(id) %>% 
  mutate(n_vc_id = number_of_vacancies / n()) %>% 
  # n vac per ssyk
  group_by(omrade, ssyk_2012) %>% 
  mutate( nv_perssyk = sum(n_vc_id, na.rm=TRUE ) ) %>%
  distinct(ssyk_2012, nv_perssyk) %>% 
  
  # add ssyk labels
  left_join(ssyk_labels) %>% 
  select(nv_perssyk, ssyk_label)

# Exportera till excel
temp_df_ssyk5 %>% 
  write_xlsx("data figur 4.xlsx")


g_ssyk5 <- temp_df_ssyk5 %>%
  group_by(omrade) %>% 
  arrange(omrade, desc(nv_perssyk)) %>% 
  top_n(nv_perssyk , n=10) %>% 
  ungroup %>% 
  mutate(
  # fix title labels for graph
  omrade= case_when(omrade=="basics" ~ "Grundläggande \nkompetens",
                           omrade=="programming" ~ "Fördjupad \nkompetens") ,
  # fix facets for graph
  facet= factor(omrade, levels=c("Grundläggande \nkompetens",
                                   "Fördjupad \nkompetens")) ,
  # fix axis labels for graph
  ssyk_label = str_wrap(ssyk_label, width=25)
  ) %>% 
  # graph top ssyk per omrade
  ggplot(aes(x=nv_perssyk/1000, 
             y=reorder_within(ssyk_label, nv_perssyk, omrade))) + 
  scale_y_reordered() +
  geom_col(fill=tvv_orange) +
  facet_wrap(~facet, scales='free') +
  labs(x="Antal vakanser, 1000-tal", 
       y=NULL, 
       caption=paste0("År ", selected_year,".")) +
  theme(text=element_text(size=8)) +
  ggtitle("Yrken där någon av söktermerna nämns")

g_ssyk5
g_ssyk5 %>% ggsave(file="g_ssyk5.png", width=6, height=4)





################################################################################
### prc of vacancies per ssyk 5
################################################################################
read_rds("totalsum_ads_vac_perssyk_peryear.rds") %>% 
  group_by(year) %>% 
  summarize(sum= sum(total_ads_perssyk_peryear , na.rm=TRUE)) %>% distinct
################################################################################
# check some
all_data_unnested %>% 
  filter(ssyk_2012==2173) %>% 
  select(word)

temp_prc_ssyk <- 
  all_data_unnested %>% 
  filter(year==selected_year) %>%
  mutate(omrade=if_else(omrade=="mjukvara","programming",omrade)) %>% 
  
  # n per ssyk 
  drop_na(ssyk_2012) %>% 
  # distinct id
  distinct(id, omrade, 
           ssyk_2012, number_of_vacancies) %>% 
  # n vacancies per id
    group_by(omrade, id) %>% 
    mutate(n_vc_id = number_of_vacancies / n()) %>% 
    select(-id, -number_of_vacancies) %>% 
  # n vac per ssyk 
    group_by(omrade, ssyk_2012) %>% 
    mutate( nv_perssyk = sum(n_vc_id, na.rm=TRUE ) ) %>%
  
  # filter for >100 vacancies
    filter(nv_perssyk >= 100) %>% 
  
  ### n DS-vac per ssyk / total vac per ssyk
  # join table w total sums per ssyk
  left_join(read_rds("totalsum_ads_vac_perssyk_peryear.rds") %>% filter(year==selected_year) ) %>% 
  
  # prc
  mutate(prc_nv_perssyk = nv_perssyk / total_vac_perssyk_peryear) %>% 
  distinct(year, ssyk_2012, omrade, prc_nv_perssyk) %>% 
  
  # add ssyk labels
  left_join(ssyk_labels) %>% 
  arrange(desc(prc_nv_perssyk)) 

# exportera till excel
temp_prc_ssyk %>% 
  write_xlsx("data figur 5.xlsx")


# graph prc per ssyk
g_prc_ssyk <- temp_prc_ssyk  %>%
  group_by(omrade) %>% 
  arrange(omrade, desc(prc_nv_perssyk)) %>% 
  top_n(prc_nv_perssyk , n=10) %>% 
  ungroup %>% 
  
  mutate(
  # fix title labels for graph
    omrade= case_when(omrade=="basics" ~ "Grundläggande \nkompetens",
                           omrade=="programming" ~ "Fördjupad \nkompetens"), 
  # fix facets for graph
    facet= factor(omrade, levels=c("Grundläggande \nkompetens",
                                     "Fördjupad \nkompetens")) ,
  # fix axis label for graph       
    ssyk_label = str_wrap(ssyk_label, width=25)
  ) %>% 
  # graph top ssyk per omrade
  ggplot(aes(x= prc_nv_perssyk, 
             y=reorder_within(ssyk_label, prc_nv_perssyk, omrade))) + 
  scale_y_reordered() +
  scale_x_continuous(labels = scales::percent) +
  geom_col(fill=tvv_orange) +
  facet_wrap(~facet, scales='free') +
  labs(x="Procent av samtliga vakanser per yrke", 
       y=NULL, 
       caption=paste0("År ", selected_year, ". Yrken med < 100 vakanser visas ej.")) +
  theme(text=element_text(size=8)) +
  ggtitle("Yrken där någon av söktermerna nämns")


g_prc_ssyk
g_prc_ssyk %>% ggsave(file="g_prc_ssyk.png", width=6, height=4)



################################################################################
### n ssyk per term
################################################################################
g_ssyk_persearchterm <- all_data_unnested %>% 
  filter(year==2019, 
         word_rensat %in% c("java","python","GIS")) %>% 
  # n per ssyk 
  drop_na(ssyk_2012) %>% 
  # distinct id
  distinct(id, ssyk_2012, number_of_vacancies, word_rensat) %>% 
  # n vacancies per id
  group_by(id) %>% 
  mutate(n_vc_id = number_of_vacancies / n()) %>% 
  # n vac per ssyk
  group_by(ssyk_2012, word_rensat) %>% 
  mutate( nv_perssyk = sum(n_vc_id, na.rm=TRUE ) ) %>%
  distinct(ssyk_2012, nv_perssyk, word_rensat) %>% 
  
  # add ssyk labels
  left_join(ssyk_labels) %>% 
  select(nv_perssyk, ssyk_label, word_rensat) %>% 
  
  arrange(desc(nv_perssyk)) %>% 
  ungroup() %>% 
  # graph ssyk per term
  group_by(word_rensat) %>% 
  slice_max(nv_perssyk , n=10) %>% 
  ggplot(aes(x=nv_perssyk / 100, 
             y=reorder(ssyk_label, nv_perssyk))) + 
  facet_wrap(~word_rensat, scales="free_x") +
  geom_col(fill=tvv_purple) +
  labs(x="Antal vakanser, 100-tal", 
       y=NULL, 
       subtitle="Yrken där söktermerna nämns") 


g_ssyk_persearchterm
# g_ssyk_persearchterm %>% ggsave(file="g_ssyk_persearchterm.png", width=6, height=3)


###############################################################################
### TOP SKILLS PER SSYK
###############################################################################
valt_yrke1 <- 2512 # Mjukvaru- och systemutvecklare m.fl.
valt_yrke2 <- 5321 # Grundutbildade sjuksköterskor

temp_dfg_programskills <- all_data_unnested %>%
  # FILTRERA EN FELTRÄFF
  filter(word_rensat!="Oracle Siebel CRM servers") %>% 
  # filter year
  filter(year>2018) %>%
  
  # add ssyk labels
  left_join(ssyk_labels) %>% 
  filter(ssyk_2012 %in% c(valt_yrke1, valt_yrke2)) %>% 
  drop_na(ssyk_2012) %>% 
  distinct(ssyk_2012, ssyk_label, id, word_rensat) %>% 
  # n word mentions per ssyk
  group_by(ssyk_2012) %>% 
  add_count(word_rensat, sort=TRUE) %>% 
  distinct(ssyk_label, word_rensat, n) %>%
  # select number of words in graph
  top_n(5) %>% 
  # fix axis labels for graph
  mutate(
    ssyk_label = str_wrap(ssyk_label, width=25)
    )  
 
# exportera till excel 
temp_dfg_programskills %>% 
  write_xlsx("data figur 6.xlsx")
  
g_programskills <- temp_dfg_programskills %>%   
  # graph it
  ggplot(aes(y=reorder(n, word_rensat), 
             x=n/1000, 
             fill=n)) + 
  geom_col(position='dodge') +
  geom_text(aes(x=.5 , label=word_rensat), 
          size=3, hjust='left') +
  scale_fill_gradient(low='sienna4', high='sienna1', 
                             name=NULL,
                      breaks=NULL, labels=NULL) +
  scale_y_reordered() +
  labs(y=NULL, x="Antal annonser, 1000-tal") +
  theme(plot.title = element_text(hjust = 0.5, size=12), 
            text=element_text(size=8), 
            legend.position='none') +
  facet_wrap(~ssyk_label, scales='free')

g_programskills
g_programskills %>% ggsave(file="g_programskills.png", width=6, height=4)


################################################################################
### WHO ARE THE EMPLOYERS
# owners_function() > ready made function from preamble
# 1. top 10 employers 2006-2020
# 2. list employers that been among the top at least one year
# 3. Top ten employers per area, 2006-2021
# 4. SNI stuff / ignore
# 5. most common demanded skills for public sector
##############################################################################

employers_result <- owners_function(all_data_unnested)
employers_result[[4]]

employers_result[[1]]
employers_result[[2]] %>% select(employer_own, `2019`, `2020`)

employers_result[[3]]

employers_result[[5]]


################################################################################






################################################################################
### Results per sni
  # only data for 2018-2019
################################################################################
all_data_unnested %>% colnames

### Simple check: where do we have SNI-data?  
# most hits in SNI N, M, J
ads_per_sni <- all_data_unnested %>% 
    drop_na(sni_label, id) %>% 
    mutate(sni_label = paste(sni_bkstv,sni_label)) %>% 
    group_by(year, sni_label) %>% 
    distinct(year, sni_label, id) %>% 
    summarise(ads_per_sni = n_distinct(id)) %>% 
  ggplot(aes(x=ads_per_sni, y=reorder(sni_label, ads_per_sni))) + 
  geom_col(position='dodge') +
  facet_wrap(~year)

ads_per_sni

### Filter for 2018 (most SNI here)
all_data_unnested %>% 
  drop_na(sni_label, id) %>% 
  filter(year==2018) %>% 
  # Get SNI labels 
  mutate(sni_label = paste(sni_bkstv,sni_label)) %>% 
  # Count some
  group_by(sni_label) %>% 
  distinct(sni_label, id) %>% 
  summarise(ads_per_sni = n_distinct(id)) %>% 
  ggplot(aes(x=ads_per_sni, 
             y=reorder(sni_label, ads_per_sni))) +
    geom_col(fill=tvv_orange)



################################################################################
# lets check some SNI
this_sni <- "N"
chosen_snis <- c("N","Q","M")
################################################################################

temp_sni_df <- 
  all_data_unnested %>% 
    # keep only this SNI
    filter(sni_bkstv == this_sni) %>% 
    
    # count words for this SNI
    count(omrade, word_rensat) %>% 
    distinct(omrade, word_rensat, n) %>%
    group_by(omrade) %>% 
    top_n(20) %>% 
    ungroup() %>% 
    mutate(word_rensat = reorder_within(word_rensat, n, omrade)) 
    

### SSYK per SNI 
all_data_unnested %>% 
  filter(year==2018, 
         sni_bkstv %in% chosen_snis) %>% 
  # n per ssyk 
  drop_na(ssyk_2012) %>% 
  # distinct id
  distinct(id, ssyk_2012, number_of_vacancies, sni_bkstv) %>% 
  # n vacancies per id
  group_by(id, sni_bkstv) %>% 
  mutate(n_vc_id = number_of_vacancies / n()) %>% 
  # n vac per ssyk
  group_by(ssyk_2012, sni_bkstv) %>% 
  mutate( nv_perssyk = sum(n_vc_id, na.rm=TRUE ) ) %>%
  distinct(ssyk_2012, nv_perssyk, sni_bkstv) %>% 
  
  # add ssyk labels
  left_join(ssyk_labels) %>% 
  select(nv_perssyk, ssyk_label, sni_bkstv) %>% 

  # sort by group
  group_by(sni_bkstv) %>% 
  arrange(desc(nv_perssyk)) %>% 
  slice(1:7) %>% 
  
  # graph
  ggplot(aes(x=nv_perssyk, 
             y=reorder_within(ssyk_label, nv_perssyk, sni_bkstv, )
             )) + 
  geom_col(fill=tvv_orange) + 
  scale_y_reordered() +
  # scale_y_discrete(label = function(x) stringr::str_trunc(x, 20)) +
  facet_wrap(~sni_bkstv, scales='free_y', ncol = 1) 



# the graph
temp_sni_df %>% 
    ggplot(aes(y=word_rensat,
               x=n / 1000, 
               fill=omrade)
    ) + 
    scale_y_reordered() +
    geom_col(show.legend=FALSE) + 
    facet_wrap(~omrade, scales='free') +
    labs(x="Antal omnämnande, 1000-tal", 
         y=NULL,
         title=paste("Resultat för SNI =", this_sni))
  



################################################################################
### level of education demanded, outspoken in the ad
# edu_rank = highest education level mentioned in ads text

# prepped function generates 3 tables with results 
edu_results <- education_level_function(all_data_unnested)
# 1: grundskola/gymnasie/... , per omrade per year
# 2: ssyk_1
# 3: ssyk_1 comb wih grundskola/gymnasie/...  
################################################################################

##############################
### 3 just for checking
ssyk_edu <- edu_results[[3]]

ssyk_edu %>% colnames

ssyk_edu %>% select(-`NA`) %>% filter(!is.na(yrkestitel_ssyk)) %>% 
  ggplot(aes(x=yrkestitel_ssyk, 
             y=yrkeshögskola)) + 
  geom_col() +
  theme(axis.text.x=element_text(angle=90),
        text=element_text(size=9))

###############################
### 1 education as % of DS-ads
educ_levels <- edu_results[[1]]
educ_levels

educ_levels %>% 
  filter(omrade=="programming") %>%
  pivot_longer(cols=grundskola:doktor) %>% 
  ggplot(aes(x=year, y=value, color=name)) +  geom_line()


educ_levels %>% 
  ggplot(aes(x=year, y=n_omrade, fill=omrade)) + geom_area()
  

# exportera till excel
educ_levels %>% 
  write_xlsx("data figur 8.xlsx")

### graph share of DS ads
g_dsprc_educ <- educ_levels %>% 
  # filter(omrade=="basics") %>% 
  mutate(omrade=case_when(
    omrade=="basics" ~"Grundläggande \ndigital kompetens", 
    omrade %in% c("programming","mjukvara") ~"Fördjupad \ndigital kompetens"
  )) %>% 
  pivot_longer(grundskola:doktor) %>% 
  mutate(
    facet= factor(omrade, levels=c("Grundläggande \ndigital kompetens",
                                   "Fördjupad \ndigital kompetens")),
    order=case_when(
      name=='doktor'  ~1,
      name=='högskola'  ~2,
      name=='yrkeshögskola'  ~3,
      name=='gymnasie' ~4, 
      name=='grundskola'  ~5)
    ) %>% 
  ggplot(aes(x=year, y=value, fill=reorder(name, order))) + 
  geom_area() +
  scale_fill_manual(values=c(tvv_green, tvv_orange, tvv_blue, tvv_purple, "#004A98"), 
                    name="Utbildningsnivå", 
                    labels=c("Forskarnivå",
                             "Högskola", 
                             "Yrkeshögskola",
                             "Gymnasium",
                             "Grundskola")) +
  scale_y_continuous(breaks=seq(0,.5,.1), 
                     labels = scales::percent_format(accuracy=1)) +
  labs(x=NULL, y="Procent av annonserna") +
  scale_x_continuous(breaks=seq(2006,2020,4)) +
  theme(axis.text.x =element_text(angle=90)) +
  facet_wrap(~facet)

g_dsprc_educ <- g_dsprc_educ + labs(caption="Resterande procent för respektive kompetensområde \nsaknar uppgift om utbildningskrav.")
g_dsprc_educ
g_dsprc_educ %>% ggsave(file="g_dsprc_educ.png", width=7, height=4)


  
################################################################################
### network graphs on words
set.seed(300)
################################################################################
## network graph: 
n_threshold = 200
start_year=2018

gen_my_network_graph <- function( n_threshold , 
                                  correlation_threshold , 
                                  designtype , 
                                  start_year)
{
temp_nc_top_correlations <- 
  all_data_unnested %>%
    
    ### some filter
    filter(year >= start_year) %>% 
    add_count(word_rensat) %>% 
    filter(n>= n_threshold) %>% 
  
    # correlation
    pairwise_cor(word_rensat, 
                 id, 
                 sort = TRUE) %>%
    # Keep only correlations above threshold
    filter(correlation > correlation_threshold) 
  
### Add verticies with
# word count for point size
# omrade for fill colors
temp_vert <- temp_nc_top_correlations %>% 
      left_join(
        all_data_unnested %>% add_count(word_rensat) %>% 
        select(word_rensat, n) %>% distinct(word_rensat, n) ,  
        by=c("item1"="word_rensat")) %>% 
      select(word_rensat=item1, n_perword= n) %>% 
      distinct(word_rensat, n_perword) %>% 
    # Add omrade
    left_join(all_data_unnested %>% 
              distinct(word_rensat, omrade) ) %>% 
      distinct(word_rensat, .keep_all=TRUE ) %>% 
      rename(item1=word_rensat)
  
# själva diagrammet
temp_nc_top_correlations %>% 
    graph_from_data_frame(vertices = temp_vert) %>%
    ggraph(layout = designtype) +
    # linjen mellan punkterna 
    geom_edge_link0(aes(edge_alpha = correlation), 
                   show.legend = FALSE, 
                   edge_color = "black") +
    # punkterna
    geom_node_point(aes(size = n_perword / 1000, 
                        fill=omrade, 
                        color=omrade)) +
    # texterna intill punkterna
    geom_node_text(aes(label = name), 
                   size=3,
                   repel = TRUE, 
                   max.overlaps=20, 
                   hjust="inward") +
  
    scale_fill_manual(name=NULL, breaks=NULL,
      values=c(tvv_blue, tvv_green, tvv_orange, tvv_purple)) +
    scale_size(range = c(3,15), 
                 name="Antal annonser, \n1000-tal") +
  
    theme_void() +
    theme(text = element_text(size=8)) +
    scale_alpha(range = c(.1,.5)) +
    scale_color_manual(name="Kompetens",
            breaks=c("basics",
                     "mjukvara",
                     "programming",
                     "sentiments"), 
            labels=c("Grundläggande",
                     "Mjukvara",
                     "Programspråk", 
                     "Sociala"), 
            values=c(tvv_blue, tvv_green, tvv_orange, tvv_purple)
            ) +
    labs(caption=paste0("Minst ", n_threshold," annonser. \nMinst ", correlation_threshold, " korrelationsgrad (mellan 0-1).")) %>% 
    
    return()
}
##################################################
### Create the graphs
### 2 small Grid networks
g_network_grid <- 
(
  gen_my_network_graph(10000, .05, "grid" , 2018) +
    theme(text = element_text(size=8), legend.position = 'none', 
          panel.background = element_rect("white","white")) +
  gen_my_network_graph(500, .3, "grid" , 2018) +
    theme(text = element_text(size=8)) +
  plot_layout(design=c(area(1,2,9,9),
                       area(1,11,9,20))) 
) +
  plot_annotation(subtitle = "Alla annonser 2018-2020")

# exportera till excel 
g_network_grid$data %>% 
  write_xlsx("data figur 10.xlsx")

g_network_grid
g_network_grid %>% 
   ggsave(file="g_network_grid.png", width=7, height=4)


#################################################################  
pl <-  plot_annotation(subtitle = "Alla annonser 2018-2020")
g_big_network1 <- gen_my_network_graph(500, .1, "stress", 2018)  
g_big_network2 <- gen_my_network_graph(250, .1, "kk", 2018)  
g_big_network3 <- gen_my_network_graph(100, .2, "grid", 2018)  

### Export one big network: the IT universe
# g_big_network2 <- g_big_network2 + pl

g_big_network3 <- g_big_network3 + pl

# exportera till excel
g_big_network3$data %>% 
  write_xlsx("data figur 9.xlsx")

g_big_network3  %>% 
  ggsave(file="g_network_allDSwords.png", width=7, height=4)





################################################################# 
### Social skills, sentiments: check some network graphs
################################################################# 
# connect social skills to main dataset again
all_data_unnested <- 
  all_data_unnested %>% 
  # this sample created at top of script
  bind_rows(data_unnested_social)

all_data_unnested$omrade %>% unique

g_social_digital_network <- 
  gen_my_network_graph(5000, .1, "grid", 2018) + 
  plot_annotation(subtitle = "Alla annonser 2018-2020")

# exportera till excel
g_social_digital_network$data %>% 
  write_xlsx("data figur 11.xlsx")

g_social_digital_network
g_social_digital_network %>% 
  ggsave(file="g_social_digital_network.png", 
         width=6, height=4)
################################################################# 
