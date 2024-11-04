library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
source("R/helper_fcts.R")

###WHO World data###
who_data <- read.csv("data/output/who_data.csv")

#Rename variables
who_data <- who_data  %>% 
  rename(location = country,
         cum_confirmed_cases_orig = total_confirmed_cases,
         cum_suspected_cases_orig = total_probable_cases,
         new_suspected_cases_orig = new_probable_cases,
         new_confirmed_cases_orig = new_confirmed_cases
  ) %>% 
  mutate(date = as.Date(format(as.Date(date, format = "%d/%m/%Y"), "%Y/%m/%d"))) %>% 
  select(-c(iso3, who_region, total_deaths, new_deaths))

who_data <- who_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>%
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_orig, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>%
  mutate(
    cum_confirmed_cases_barplot = cumsum(new_confirmed_cases_calc),
  ) %>% 
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
    all_new_suspected_cases = ifelse(is.na(all_new_suspected_cases), 0, all_new_suspected_cases),
    all_cum_suspected_cases = cumsum(all_new_suspected_cases)
  ) %>%
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  )

#Add datasource  suffix
who_data <- who_data %>% 
  rename_with(~ paste0(., "_who"), -c(location, date))
#Rename countries
who_data <- who_data %>% 
  mutate(location = gsub("CÃ´te dâ€™Ivoire", "Côte d’Ivoire", location),
         location = gsub( "CuraÃ§ao", "Curaçao", location),
         location = gsub( "Netherlands", "Netherlands (Kingdom of the)", location),
         location = gsub( "Saint Martin", "Saint Martin (French Part)", location),
         location = gsub( "TÃ¼rkiye", "Türkiye", location),
         location = gsub( "The United Kingdom", "United Kingdom of Great Britain and Northern Ireland", location))

write_excel_csv(who_data, "data/reported/mpox_data_who.csv")
