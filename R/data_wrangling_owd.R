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
#Save original data
write.csv(owd_data, paste("data/raw/owd_data_", today, ".csv"), row.names=FALSE)

#Rename variables
owd_data <- owd_data %>% 
  select(location, date, total_cases, new_cases, suspected_cases_cumulative) %>% 
  rename(cum_confirmed_cases_orig = total_cases,
         new_confirmed_cases_orig = new_cases,
         cum_suspected_cases_orig = suspected_cases_cumulative
  )

#CALCULATE NEW SUSPECTED CASES FROM CUM (AS NEW DOES NOT EXIST IN THE DATA)
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
#Subtract max cum up to end 2023 from min cum 2024  
owd_data_date_suspected <- owd_data_min_date_suspected %>% 
  left_join(owd_data_max_date_suspected, by=c("location")) %>% 
  mutate(cum_suspected_cases_orig_max_plhd = replace_na(cum_suspected_cases_orig_max_plhd, 0)) %>% 
  mutate(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig_min_plhd - cum_suspected_cases_orig_max_plhd) %>% 
  select(-c(cum_suspected_cases_orig_min_plhd, cum_suspected_cases_orig_max_plhd))
#Calculate NEW suspected cases based on CUM data
owd_data_new_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = NA) %>% 
  left_join(owd_data_date_suspected, by=join_by(location, date)) %>% 
  mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_suspected_cases_calc)

#SUBTRACT <2024 CONFIRMED CASES FOR CUM-NEW COMPARISONS
#Get the max date with CUM confirmed cases (up to end 2023)
# owd_data_max_date_confirmed <- owd_data %>%
#   filter(!is.na(cum_confirmed_cases_orig)) %>%
#   filter(date < as.Date("2024-01-01")) %>%
#   group_by(location) %>%
#   arrange(location, date) %>%
#   filter(date == max(date)) %>%
#   select(location, date, cum_confirmed_cases_orig) %>%
#   rename(cum_confirmed_cases_orig_max_plhd = cum_confirmed_cases_orig) %>%
#   select(-c(date))
#Subtract CUM <2024 confirmed cases
# owd_data_confirmed <- owd_data %>%
#   mutate(date = as.Date(date)) %>% 
#   filter(date >= as.Date("2024-01-01")) %>% 
#   left_join(owd_data_max_date_confirmed, by=join_by(location)) %>%
#   mutate(cum_confirmed_cases_cum = cum_confirmed_cases_orig - cum_confirmed_cases_orig_max_plhd)  %>%
#   select(location, date, cum_confirmed_cases_cum) %>% 
#   filter(!is.na(cum_confirmed_cases_cum))

owd_data <- owd_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  #Fill out missing days
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  #Get NEW suspected cases
  left_join(owd_data_new_suspected, by=join_by(location, date)) %>%
  #Get subtracted CUM confirmed 
  # left_join(owd_data_confirmed, by=join_by(location, date)) %>% 
  # mutate(cum_confirmed_cases_cum = ifelse((is.na(cum_confirmed_cases_cum) & !is.na(cum_confirmed_cases_orig)), cum_confirmed_cases_orig, cum_confirmed_cases_cum)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  # fill(cum_confirmed_cases_cum, .direction = "down") %>%
  #Calculate CUM from NEW cases
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
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
    all_new_suspected_cases = ifelse(is.na(all_new_suspected_cases), 0, all_new_suspected_cases),
    all_cum_suspected_cases = cumsum(all_new_suspected_cases)
  ) %>% 
  #Convert NA back to 0 for reporting
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  ) %>% 
  #Remove original columns
  select(-ends_with("_orig"))
# %>% 
#   mutate(
#     cum_confirmed_cases_new = replace_na(cum_confirmed_cases_new, 0),
#     cum_confirmed_cases_cum = replace_na(cum_confirmed_cases_cum, 0)
#Compare CUM from NEW vs CUM from CUM subtracted
#     cum_comparison = ifelse(cum_confirmed_cases_new==cum_confirmed_cases_cum, "OK", "Not equal")
#     )

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
#Save data
write_excel_csv(owd_data, "data/reported/mpox_data_owd.csv")
