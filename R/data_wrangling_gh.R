library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
source("R/helper_fcts.R")

today <- gsub("-", "_", Sys.Date())

###Global health###
gh_data <- read.csv("https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv")
#Save original data
write.csv(gh_data, paste("data/raw/gh_data_", today, ".csv"), row.names=FALSE)

#Rename variables and pivot data
gh_data <- gh_data %>% 
  count(Case_status, Location_Admin0, Date_report_source_I) %>% 
  filter(Case_status != "omit_error") %>%
  pivot_wider(names_from = Case_status, values_from = n) %>% 
  rename(new_confirmed_cases_orig = confirmed,
         new_suspected_cases_orig = suspected,
         location = Location_Admin0,
         date = Date_report_source_I)

gh_data <- gh_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  #Fill out missing days
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  #Calculate CUM from NEW cases
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_orig, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>% 
  # mutate(
  #   cum_confirmed_cases_new = cumsum(new_confirmed_cases_calc),
  # ) %>% 
  #Convert 0 back to NA for smooth calculations
  mutate(
    new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
    new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  ) %>% 
  #Calculate smooth variables
  mutate(
    all_new_confirmed_cases = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
    all_new_confirmed_cases = ifelse(is.na(all_new_confirmed_cases), 0, all_new_confirmed_cases),
    all_cum_confirmed_cases = cumsum(all_new_confirmed_cases),
    all_new_suspected_cases = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
    all_new_suspected_cases = ifelse(is.na(all_new_suspected_cases ), 0, all_new_suspected_cases ),
    all_cum_suspected_cases = cumsum(all_new_suspected_cases)
  ) %>% 
  #Convert NA back to 0 for reporting
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  ) %>% 
  #Remove original columns
  select(-ends_with("_orig"))

#Add datasource  suffix
gh_data <- gh_data %>% 
  rename_with(~ paste0(., "_gh"), -c(location, date))
#Rename countries
gh_data <- gh_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Republic of the Congo", "Congo", location),
         location = gsub("Democratic Congo", "Democratic Republic of the Congo", location),
         location = gsub( "United Kingdom", "United Kingdom of Great Britain and Northern Ireland", location))
#Save data
write_excel_csv(gh_data, "data/reported/mpox_data_gh.csv")
