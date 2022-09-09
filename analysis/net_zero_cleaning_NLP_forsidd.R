# Angel Hsu
# DDL 
# November 17, 2020
# data merging/cleaning for Sidd/net-zero action plan analysis

# setwd
setwd("~/analysis")

# libraries
library(ClimActor)
library(tidyverse)
library(googlesheets4)
library(Gmisc)

# read in data
x <- read_csv("data/netzero_subnationals_merged_cleaned_1Oct.csv")
eu <- read_sheet("https://docs.google.com/spreadsheets/d/1fB_rf5Dk5mkK2iQWTsMUXGkPBZVCOtFaDoWOGW2itig/edit#gid=2140733479") %>%
      dplyr::select(name, iso, net_zero_target_status)
eucom <- read.csv("~/Google Drive/NAZCA/NAZCA Developers/EU Covenant of Mayors Jul 2020/EUCOM_coordinators_clean.csv", stringsAsFactors = F)

# clean eu names
eu$entity_type <- "City"

# add country
eu$country <- country_dict$right[match(eu$iso, country_dict$iso)]

# clean names
eu <- eu %>% mutate(name=str_trim(name, side = c("right")))
                    
precleaned_names <- eu$name
eu_clean <- clean_name(eu, key_dict, clean_enc = F)

sum(!precleaned_names %in% eu_clean$name) # check to see how many need to be manually cleaned, not in key dictionary

# merge with eucom to identify which in secretariat
eu_clean <- eu_clean %>%
      left_join(dplyr::select(eucom, name, iso, coordinator_name, support_type), by=c("name", "iso")) %>%
      mutate(net_zero_target_status=as.character(net_zero_target_status)) %>%
      write_csv("data/Sidd/eu_cities_netzerostatus_coordinators.csv")

## additional data cleaning - putting together a master datafile for all data used in net-zero NLP analysis
# From Sidd's email: useful metadata - 1. Region; 2. Net zero Target status; 3. Coordination network membership (if there's anything more than what you already sent for EU cities.
# I also did some analysis using the following variables that didn't yield anything worthwhile before, but are worth redoing and documenting given Dr. Hsu's feedback today. 
# 1. Area ; 2. Population; 3. GDP (we did not include); 4. Emissions (also should be normalized and also the scopes made consistent)

full_list <- read.csv("data/Sidd/final_full_citylist_clean.csv", stringsAsFactors = F)

# add in country, region information
full_list <- full_list %>% left_join(dplyr::select(country_dict, iso, right, region), by=c("iso")) %>%
             distinct() %>%
             rename(country="right") # 332 entries

# add in net-zero target status information
full_list_comb <- full_list %>%
                    left_join(dplyr::select(x, name, iso, area, lat, lng, population, population_year, baseline_year, baseline_emissions, raw_net_zero_commitment, ghg_reduction_target, target_year, percent_reduction, initiatives_committed, net_zero_target_status), by=c("name", "iso")) %>%
                    group_by(name, iso) %>%
                    distinct(percent_reduction, target_year, .keep_all=TRUE) %>%
                    mutate_at(vars(percent_reduction, area, population, baseline_emissions), funs(as.numeric)) %>%
                    filter(percent_reduction >= 80) %>%
                    mutate_at(vars(ghg_reduction_target, raw_net_zero_commitment, net_zero_target_status), na_if, "NA") %>%
                   # mutate(across(c(ghg_reduction_target, raw_net_zero_commitment, net_zero_target_status)), na_if, "NA") %>% ### filters too aggressive
                   # filter_at(vars(ghg_reduction_target, raw_net_zero_commitment, net_zero_target_status), all_vars(!is.na(.))) %>%
                    mutate(pop_density = population/area, emis_per_capita = baseline_emissions/population) %>%
                    left_join(dplyr::select(eu_clean, name, iso, coordinator_name, support_type), by=c("name", "iso")) %>%
                    # write_csv("data/Sidd/net_zero_NLP_metadata_master.csv")

# new version that produces a column "econ_wide_net_zero" column
  
full_list2 <-   full_list %>%
  left_join(dplyr::select(x, name, iso, area, lat, lng, population, population_year, baseline_year, baseline_emissions, raw_net_zero_commitment, ghg_reduction_target, target_year, percent_reduction, initiatives_committed, net_zero_target_status), by=c("name", "iso")) 

full_list_comb2 <- full_list2 %>%
                   group_by(name, iso) %>%
                  distinct(percent_reduction, target_year, .keep_all=TRUE) %>%
                  filter(str_detect(raw_net_zero_commitment, regex("carbon neutral|net zero|Race to Zero", ignore_case=T))) %>%
                  bind_rows(filter(full_list2, percent_reduction >= 80 | is.na(percent_reduction))) %>%
                  distinct() %>%
                  filter_at(vars(percent_reduction, raw_net_zero_commitment), all_vars(!is.na(.))) %>%
                  distinct(percent_reduction, target_year, .keep_all=TRUE) %>%
                  mutate(econ_wide_net_zero = 1) %>%
                  bind_rows(anti_join(full_list2, ., by=c("name", "iso", "entity_type"))) %>%
                  mutate_at(vars(percent_reduction, area, population, baseline_emissions), funs(as.numeric)) %>%
                  mutate(pop_density = population/area, emis_per_capita = baseline_emissions/population) %>%
                  left_join(dplyr::select(eu_clean, name, iso, coordinator_name, support_type), by=c("name", "iso")) %>%
                  write_csv("data/Sidd/net_zero_NLP_metadata_master.csv")
  
# descriptive stats table
all <- full_list_comb2 %>%
  filter(entity_type == "City") %>%
  distinct(name, iso, .keep_all=TRUE) %>%
  dplyr::group_by(N = n(), add = TRUE) %>% 
  mutate(target_year=str_replace(target_year, "long-term", "Long term"), target_year=str_replace(target_year, "Long term", "2050"), target_year=as.numeric(target_year)) %>%
  dplyr::select(population, baseline_year, target_year, percent_reduction, emis_per_capita) %>%
  dplyr::summarise_all(funs(mean, median, min, max, sd), na.rm = TRUE) %>%
  mutate(across(where(is.numeric), round, 2))

sum_table <- all %>% 
  dplyr::select(contains("mean")) %>%
  pivot_longer(cols=contains("mean"), names_to="variable", values_to="mean") %>%
  mutate(variable=str_replace_all(variable, "_mean","")) %>%
  bind_cols(all %>% dplyr::select(contains("median")) %>% pivot_longer(cols=contains("median"), names_to="delete", values_to="median")) %>%
  bind_cols(all %>% dplyr::select(contains("sd")) %>% pivot_longer(cols=contains("sd"), names_to="delete", values_to="sd")) %>%
  bind_cols(all %>% dplyr::select(contains("min")) %>% pivot_longer(cols=contains("min"), names_to="delete", values_to="min")) %>%
  bind_cols(all %>% dplyr::select(contains("max")) %>% pivot_longer(cols=contains("max"), names_to="delete", values_to="max")) %>%
  dplyr::select(-contains("delete")) %>%
  mutate(variable=str_replace_all(variable, "_", " "), variable=str_replace_all(variable, "emis per capita", "Emissions per capita"))
  

sum_table_ex <- htmlTable(sum_table,
          align = "rrrr",
          rgroupCSSseparator = "",
          rowlabel = "",
          rnames=" ",
          caption = "Table 1. Summary Statistics",
          ctable = TRUE
)

# using stargazer
library(stargazer)
full_list_comb2 %>% filter(entity_type=="City") %>%  distinct(name, iso, .keep_all=TRUE) %>%
  dplyr::select(population, baseline_year, target_year, percent_reduction, emis_per_capita) %>%
 
stargazer( 
  type = "html", 
  title = "Summary Statistics", 
  summary = TRUE, 
  out = "sid/tables/summary_stats.html")

# number of cities by region
full_list_comb2 %>% filter(entity_type=="City") %>%  distinct(name, iso, .keep_all=TRUE) %>%
  group_by(region) %>%
  summarise(n=n())
