# Angel Hsu
# DDL 
# June 2021
# analysis for net-zero NLP paper

# setwd
setwd("~/analysis")

# libraries
library(ClimActor)
library(tidyverse)
library(stargazer)
library(ggplot2)

# read in data
nz <- read.csv("data/net_zero_NLP_metadata_master.csv") # masterlist of the actors
net_zero_text.csv
# summary statistics table
nz %>% filter(entity_type == "Region") %>% distinct(name, iso, entity_type, .keep_all=TRUE) %>% dplyr::select(entity_type, area, population, percent_reduction, target_year, baseline_year, emis_per_capita, pop_density) %>% 
        stargazer(., type = "text", title="Table 1: Summary Statistics", out="tables/table1summary.html") #327 cities and 5 regions (Aberdeenshire, Region Zuid-W)
  


