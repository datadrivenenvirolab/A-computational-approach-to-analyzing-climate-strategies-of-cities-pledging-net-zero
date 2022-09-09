# Angel Hsu
# Cities Net-Zero NLP Paper
# Fixes to Cities meta file
# April 2022

# libraries
library(ClimActor)
library(tidyverse)

setwd("~/analysis/data")

cities_meta <- read_csv("net_zero_NLP_metadata_master_w_climate.csv") %>%
               dplyr::select(name, iso, entity_type, climate_class_present) %>%
               distinct()

cities_meta_old <- read_csv("net_zero_NLP_metadata_master.csv")
cities_meta_coord <- read_csv("net_zero_NLP_metadata_master_lat_lngfix.csv")

# merge correct lat/lng from ClimActor and previous manual fix file 
cities_meta_old <- cities_meta_old %>% mutate(name = case_when(name == "Toronto" ~ "Toronto, ON", 
                                                               name == "Jaszbereny Varosi Onkormanyzat" ~ "Jászberény Városi Önkormányzat",
                                                               TRUE ~ as.character(name))) %>%
                    left_join(contextuals %>% dplyr::select(name, iso, entity_type, lat, lng), by=c("name", "iso", "entity_type")) %>%
  mutate_at(vars(matches("lat", "lng")), funs(replace(., . == "", NA))) %>% 
  mutate(lat = coalesce(lat.y, lat.x),
         lng = coalesce(lng.y, lng.x)) %>%
  dplyr::select(-contains(".")) 

# find missing lat/lng cities
sel <- which(is.na(cities_meta_old$lat))

cities_meta_coord <- cities_meta_coord %>% filter(name %in% name[sel]) %>% distinct(name, iso, entity_type, lat, lng)

cities_meta_old <- cities_meta_old %>% 
                   left_join(cities_meta_coord %>% dplyr::select(name, iso, entity_type, lat, lng),  by=c("name", "iso", "entity_type")) %>%
                   mutate_at(vars(matches("lat", "lng")), funs(replace(., . == "", NA))) %>% 
                   mutate_at(vars(contains("lat"), contains("lng")), ~ as.numeric(.)) %>%
                   mutate(lat = coalesce(lat.y, lat.x),
                   lng = coalesce(lng.y, lng.x)) %>%
                   dplyr::select(-contains(".")) 

# merge in climate zone information
cities_meta_old <- cities_meta_old %>% left_join(cities_meta, by=c("name", "iso", "entity_type")) %>%
                   write_csv("net_zero_NLP_metadata_master_w_climate.csv")


