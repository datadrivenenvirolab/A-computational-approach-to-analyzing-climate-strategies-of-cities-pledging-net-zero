# Angel Hsu
# Net-Zero NLP Analysis
# Subnationals data extraction for Siddharth

# setwd
setwd("~/Documents/GitHub/net_zero")

# load libraries
library(tidyverse)

# Read in data
x <- read.csv("data/netzero_subnationals_merged_cleaned_17Sep.csv", stringsAsFactors = FALSE)
y <- read.csv("data/cities_standardized_final.csv", stringsAsFactors = FALSE)

# replace UK and USA
x <- x%>%
     mutate(country = str_replace(country, "USA", "United States of America"),
            country = str_replace(country, "UK", "United Kingdom"))

# extract relevant columns for Siddharth to run regression
new <- y %>% left_join(dplyr::select(x, name, iso, entity_type, country, population, area, state, admin_1, initiatives_committed, lat, lng, region, baseline_emissions, baseline_year, net_zero_target_status, percent_reduction, target_year)) %>%
       filter(percent_reduction >= 80) %>%
       distinct(name, iso, entity_type, .keep_all=TRUE) %>%
       write_csv("data/Sidd/net_zero_plan_cities_targets_Siddharth.csv")

# without target information (slightly more cities)
new <- y %>% left_join(dplyr::select(x, name, iso, entity_type, country, population, area, state, admin_1, initiatives_committed, lat, lng, region, baseline_emissions, baseline_year)) %>%
          distinct(name, iso, entity_type, .keep_all=TRUE) %>%
          write_csv("data/Sidd/net_zero_plan_cities_Siddharth.csv")
