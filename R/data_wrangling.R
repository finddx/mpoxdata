library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
source("R/helper_fcts.R")


today <- gsub("-", "_", Sys.Date())
#Global health
gh_data <- read.csv("https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv")
write.csv(gh_data, paste("data/raw/gh_data_", today, ".csv"), row.names=FALSE)

gh_data <- gh_data %>% 
  count(Case_status, Location_Admin0, Date_report_source_I) %>% 
  filter(Case_status != "omit_error") %>%
  pivot_wider(names_from = Case_status, values_from = n) %>% 
  rename(new_cases = confirmed,
         suspected_cases = suspected,
         location = Location_Admin0,
         date = Date_report_source_I)

gh_data <- gh_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(
    new_cases = replace_na(new_cases, 0),
    cum_new_cases = cumsum(new_cases),
    suspected_cases = replace_na(suspected_cases, 0),
    cum_suspected_cases = cumsum(suspected_cases)
  ) %>% 
  mutate(
    new_cases = ifelse(new_cases==0, NA, new_cases),
    suspected_cases = ifelse(suspected_cases==0, NA, suspected_cases)
  ) %>% 
  mutate(
    smooth_new_cases = smooth_new_tests(new_cases, cum_new_cases),
    smooth_new_cases = ifelse(is.na(smooth_new_cases), 0, smooth_new_cases),
    smooth_cum_new_cases = cumsum(smooth_new_cases),
    smooth_suspected_cases = smooth_new_tests(suspected_cases, cum_suspected_cases),
    smooth_suspected_cases = ifelse(is.na(smooth_suspected_cases), 0, smooth_suspected_cases),
    smooth_cum_suspected_cases = cumsum(smooth_suspected_cases)
  ) %>% 
  mutate(
    new_cases = replace_na(new_cases, 0),
    suspected_cases = replace_na(suspected_cases, 0)
  )

gh_data <- gh_data %>%
  mutate(year = year(date)) %>%
  group_by(location, date = if_else(year < 2024, as.character(year), as.character(date))) %>%
  summarise(across(c(new_cases, smooth_new_cases, suspected_cases,  smooth_suspected_cases), ~ sum(.x, na.rm = TRUE)),
            across(c(cum_new_cases, smooth_cum_new_cases, cum_suspected_cases, smooth_cum_suspected_cases), ~ last(.x))) %>% 
  ungroup()

#Our world in data
owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")
write.csv(owd_data, paste("data/raw/owd_data_", today, ".csv"), row.names=FALSE)

owd_data <- owd_data %>% 
  select(location, date, iso_code, total_cases, new_cases, suspected_cases_cumulative) %>% 
  rename(total_suspected_cases = suspected_cases_cumulative)

owd_data_min_date_suspected <- owd_data %>% 
  filter(!is.na(total_suspected_cases)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  # mutate(date=as.Date(date)) %>% 
  select(location, date, total_suspected_cases) %>% 
  rename(total_suspected_cases_plhd = total_suspected_cases)

owd_data_new_suspected <- owd_data %>% 
  filter(!is.na(total_suspected_cases)) %>% 
  mutate(suspected_cases = NA) %>% 
  left_join(owd_data_min_date_suspected, by=join_by(location, date)) %>% 
  mutate(suspected_cases=ifelse(!is.na(total_suspected_cases_plhd), total_suspected_cases_plhd, suspected_cases)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  # mutate(suspected_cases2 = total_suspected_cases - lag(total_suspected_cases)) %>%  
  mutate(suspected_cases = calc_new_t(total_suspected_cases, suspected_cases)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, suspected_cases)
#new_cases_smoothed, new_cases_per_million, total_cases_per_million, new_cases_smoothed_per_million

#Get the min date with data
owd_data_min_date <- owd_data %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  mutate(date=as.Date(date)) %>% 
  select(location, date, total_cases) %>% 
  rename(total_cases_plhd = total_cases)

owd_data <- owd_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2022-05-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  left_join(owd_data_min_date, by=join_by(location, date)) %>% 
  mutate(new_cases=ifelse(!is.na(total_cases_plhd), total_cases_plhd, new_cases)) %>% 
  left_join(owd_data_new_suspected, by=join_by(location, date)) %>% 
  # mutate(total_suspected_cases = replace_na(total_suspected_cases, 0),
  #        suspected_cases = total_suspected_cases - lag(total_suspected_cases),
  #        ) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(
    new_cases = replace_na(new_cases, 0),
    cum_new_cases = cumsum(new_cases),
    suspected_cases = replace_na(suspected_cases, 0),
    cum_suspected_cases = cumsum(suspected_cases)
  ) %>% 
  mutate(
    new_cases = ifelse(new_cases==0, NA, new_cases),
    suspected_cases = ifelse(suspected_cases==0, NA, suspected_cases)
  ) %>% 
  mutate(
    smooth_new_cases = smooth_new_tests(new_cases, cum_new_cases),
    smooth_new_cases = ifelse(is.na(smooth_new_cases), 0, smooth_new_cases),
    smooth_cum_new_cases = cumsum(smooth_new_cases),
    smooth_suspected_cases = smooth_new_tests(suspected_cases, cum_suspected_cases),
    smooth_suspected_cases = ifelse(is.na(smooth_suspected_cases), 0, smooth_suspected_cases),
    smooth_cum_suspected_cases = cumsum(smooth_suspected_cases)
  ) %>% 
  mutate(
    new_cases = replace_na(new_cases, 0),
    suspected_cases = replace_na(suspected_cases, 0)
  ) %>%
  select(-c(total_cases_plhd, total_cases, total_suspected_cases, iso_code))

owd_data <- owd_data %>%
  mutate(year = year(date)) %>%
  group_by(location, date = if_else(year < 2024, as.character(year), as.character(date))) %>%
  summarise(across(c(new_cases, smooth_new_cases, suspected_cases,  smooth_suspected_cases), ~ sum(.x, na.rm = TRUE)),
            across(c(cum_new_cases, smooth_cum_new_cases, cum_suspected_cases, smooth_cum_suspected_cases), ~ last(.x))) %>% 
  ungroup()

# Status of a case. Cases which are discarded were previously suspected but have now been confirmed negative, and should be excluded from case counts.

gh_data <- gh_data %>% 
  rename_with(~ paste0("gh_", .), -c(location, date))
gh_data <- gh_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Republic of the Congo", "Congo", location),
         location = gsub("Democratic Congo", "Democratic Republic of the Congo", location),)

owd_data <- owd_data %>% 
  rename_with(~ paste0("owd_", .), -c(location, date))
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


#Country data
data <- read.csv("data/raw/country_info.csv")

data_dates <- data %>%
  mutate(date = NA) %>%
  group_by(UN_country) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  complete(date = seq.Date(as.Date("2022-01-01"), as.Date("2023-12-31"), by = "year")) %>%
  mutate(date = if_else(year(date) < 2024, as.character(year(date)), as.character(date))) %>% 
  select(UN_country, date)

data <- data %>%
  full_join(data_dates, by=join_by(UN_country))
data <- data %>% 
  full_join(gh_data, by=join_by(UN_country == location, date==date))
data <- data %>% 
  full_join(owd_data, by=join_by(UN_country == location, date==date)) 

data <- data %>% 
  mutate(set=ifelse(!is.na(ISO.alpha3.Code), "Country", ifelse(UN_country=="World", "World", "Region"))) %>% 
  rename(iso3 = ISO.alpha3.Code) %>% 
  relocate(set, iso3, everything())

write.csv(data, "data/output/mpox_data.csv", row.names=FALSE)