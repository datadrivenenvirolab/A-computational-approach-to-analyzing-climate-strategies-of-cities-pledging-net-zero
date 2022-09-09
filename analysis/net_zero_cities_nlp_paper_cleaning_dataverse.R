# Angel Hsu
# Prep net-zero dataset for Dataverse
# Jan 2022

setwd("~/analysis")

# libraries
library(tidyverse)

# read in data
y <- read_csv("data/net_zero_text.csv") %>% dplyr::select(-coordinator_name, -support_type)

y_new <- y %>% distinct(name, iso,target_year, .keep_all=TRUE) %>%
         dplyr::select(name, iso, entity_type, country, region, lat, lng, population, population_year,baseline_emissions, baseline_year, raw_net_zero_commitment,
                       ghg_reduction_target, target_year, percent_reduction, initiatives_committed:text) %>%
         write_csv("data/Sidd/net_zero_cities_full_dataset.csv")
