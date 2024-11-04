library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
source("R/helper_fcts.R")

today <- gsub("-", "_", Sys.Date())

###Our world in data###
owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")
write.csv(owd_data, paste("data/raw/owd_data_", today, ".csv"), row.names=FALSE)

#Rename variables
owd_data <- owd_data %>% 
  select(location, date, iso_code, total_cases, new_cases, suspected_cases_cumulative) %>% 
  rename(cum_confirmed_cases_orig = total_cases,
         new_confirmed_cases_orig = new_cases,
         cum_suspected_cases_orig = suspected_cases_cumulative
  )

#Get the min date with suspected cases (2024)
owd_data_min_date_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  filter(date >= as.Date("2024-01-01")) %>%
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_suspected_cases_orig) %>% 
  rename(cum_suspected_cases_orig_min_plhd = cum_suspected_cases_orig)
#Get the max date with suspected cases (up to end 2023)
owd_data_max_date_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  filter(date < as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == max(date)) %>%
  select(location, date, cum_suspected_cases_orig) %>% 
  rename(cum_suspected_cases_orig_max_plhd = cum_suspected_cases_orig) %>% 
  select(-c(date))
#Sustract max cum up to end 2023 from min cum 2024  
owd_data_date_suspected <- owd_data_min_date_suspected %>% 
  left_join(owd_data_max_date_suspected, by=c("location")) %>% 
  mutate(cum_suspected_cases_orig_max_plhd = replace_na(cum_suspected_cases_orig_max_plhd, 0)) %>% 
  mutate(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig_min_plhd - cum_suspected_cases_orig_max_plhd) %>% 
  select(-c(cum_suspected_cases_orig_min_plhd, cum_suspected_cases_orig_max_plhd))

#Calculate new suspected cases based on cumulative data
owd_data_new_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = NA) %>% 
  left_join(owd_data_date_suspected, by=join_by(location, date)) %>% 
  # filter(date >= as.Date("2024-01-01")) %>% 
  mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_suspected_cases_calc)

#Get the min date with confirmed cases
# owd_data_min_date <- owd_data %>%
#   group_by(location) %>%
#   arrange(location, date) %>%
#   filter(date == min(date)) %>%
#   mutate(date=as.Date(date)) %>%
#   select(location, date, cum_confirmed_cases_orig) %>%
#   rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig)

owd_data <- owd_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  #Get min date data
  # left_join(owd_data_min_date, by=join_by(location, date)) %>% 
  # mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_orig)) %>% 
  left_join(owd_data_new_suspected, by=join_by(location, date)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  #Calculate smooth variables
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
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
owd_data <- owd_data %>% 
  rename_with(~ paste0(., "_owd"), -c(location, date))
#Rename countries
owd_data <- owd_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Democratic Republic of Congo", "Democratic Republic of the Congo", location),
         location = gsub( "Bolivia", "Bolivia (Plurinational State of)", location),
         location = gsub( "Curacao", "Curaçao", location),
         location = gsub( "Iran", "Iran (Islamic Republic of)", location),
         location = gsub( "Laos", "Lao People's Democratic Republic", location),
         location = gsub( "Moldova", "Republic of Moldova", location),
         location = gsub( "Netherlands", "Netherlands (Kingdom of the)", location),
         location = gsub( "Russia", "Russian Federation", location),
         location = gsub( "Saint Martin \\(French part\\)", "Saint Martin (French Part)", location),
         location = gsub( "South Korea", "Republic of Korea", location),
         location = gsub( "Turkey", "Türkiye", location),
         location = gsub( "United Kingdom", "United Kingdom of Great Britain and Northern Ireland", location),
         location = gsub( "United States", "United States of America", location),
         location = gsub( "Venezuela", "Venezuela (Bolivarian Republic of)", location),
         location = gsub( "Vietnam", "Viet Nam", location))

write_excel_csv(owd_data, "data/reported/mpox_data_owd.csv")
