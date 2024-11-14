library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
source("R/helper_fcts.R")

#Get datasource data
who_data <- read.csv("data/reported/mpox_data_who.csv")
who_data <- who_data %>% 
  mutate(date = as.Date(date))
acdc_data <- read.csv("data/reported/mpox_data_acdc.csv")
acdc_data <- acdc_data %>% 
  mutate(date = as.Date(date))
gh_data <- read.csv("data/reported/mpox_data_gh.csv")
gh_data <- gh_data %>% 
  mutate(date = as.Date(date))
owd_data <- read.csv("data/reported/mpox_data_owd.csv")
owd_data <- owd_data %>% 
  mutate(date = as.Date(date))

#Get country data
data <- read.csv("data/raw/country_info.csv")
pop_data <- read.csv("data/raw/un_pop_2024.csv")
pop_data <- pop_data %>% 
  rename(
    country = Region..subregion..country.or.area..,
    unit = ISO3.Alpha.code,
    pop = Population) %>%
  select(unit, pop) %>% 
  filter(unit!="") %>% 
  mutate(pop = as.numeric(gsub(" ", "", pop)))

#Fill out 2024 days
data_dates <- data %>%
  mutate(date = NA) %>%
  group_by(UN_country) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  # mutate(date = if_else(year(date) < 2024, as.character(year(date)), as.character(date))) %>% 
  select(UN_country, date) %>% 
  filter(!is.na(date))

#Filter countries with no info data and rename variables
data <- data %>%
  full_join(data_dates, by=join_by(UN_country)) %>% 
  filter(!is.na(ISO.alpha3.Code)) %>% 
  filter(UN_country != "Antarctica") %>% 
  filter(nchar(trimws(WHO_region)) > 0) %>% 
  # filter(nchar(trimws(Income.group)) > 0) %>% 
  rename(unit = ISO.alpha3.Code,
         income=Income.group,
         who_region=WHO_region,
         continent=UN_region,
         country=UN_country) %>% 
  mutate(income = case_when(
    income=="High income" ~ "High",
    income=="Upper middle income" ~ "Upper middle",
    income=="Lower middle income" ~ "Lower middle",
    income=="Low income" ~ "Low"))

#Join pop data
data <- data %>% 
  left_join(pop_data, by="unit")

#Join datasets
data <- data %>% 
  left_join(who_data, by=join_by(country == location, date==date)) 
data <- data %>% 
  left_join(acdc_data, by=join_by(country == location, date==date)) 
# data <- data %>% 
#   full_join(acdc_pdf_data, by=join_by(country == location, date==date)) 
data <- data %>% 
  left_join(gh_data, by=join_by(country == location, date==date))
data <- data %>% 
  left_join(owd_data, by=join_by(country == location, date==date)) 

data <- data %>% 
  filter(!is.na(unit)) %>% 
  mutate(set="country") %>% 
  select(-c(WHO_country, UN_subregion, latitude, longitude)) #, contains("_calc_")

data <- data %>%
  mutate(pop_100k = pop / 100000) %>% 
  mutate(pop = pop / 1000) %>% 
  #Calculate DXGap based on smooth vars
  mutate(
    dxgap_who = dxGap(all_cum_suspected_cases_who, all_cum_confirmed_cases_who),
    dxgap_acdc = dxGap(all_cum_suspected_cases_acdc, all_cum_confirmed_cases_acdc),
    dxgap_gh = dxGap(all_cum_suspected_cases_gh, all_cum_confirmed_cases_gh),
    dxgap_owd = dxGap(all_cum_suspected_cases_owd, all_cum_confirmed_cases_owd)
    ) %>%
  mutate(across(where(is.numeric), ~ ifelse(. %in% c(-Inf, Inf, NaN), NA, .))) %>%
  #Calculate Per capita based on smooth vars
  mutate(
    across(
      c(all_new_confirmed_cases_who, all_new_confirmed_cases_acdc,all_new_confirmed_cases_gh, all_new_confirmed_cases_owd, 
        all_cum_confirmed_cases_who, all_cum_confirmed_cases_acdc, all_cum_confirmed_cases_gh, all_cum_confirmed_cases_owd,
        all_new_suspected_cases_who, all_new_suspected_cases_acdc, all_new_suspected_cases_gh, all_new_suspected_cases_owd,
        all_cum_suspected_cases_who, all_cum_suspected_cases_acdc, all_cum_suspected_cases_gh, all_cum_suspected_cases_owd),
      ~ .x / pop,
      .names = "cap_{col}"
    )) %>%
  mutate(
    across(
      c(all_new_confirmed_cases_who, all_new_confirmed_cases_acdc,all_new_confirmed_cases_gh, all_new_confirmed_cases_owd, 
        all_cum_confirmed_cases_who, all_cum_confirmed_cases_acdc, all_cum_confirmed_cases_gh, all_cum_confirmed_cases_owd,
        all_new_suspected_cases_who, all_new_suspected_cases_acdc, all_new_suspected_cases_gh, all_new_suspected_cases_owd,
        all_cum_suspected_cases_who, all_cum_suspected_cases_acdc, all_cum_suspected_cases_gh, all_cum_suspected_cases_owd),
      ~ .x / pop_100k,
      .names = "cap100k_{col}"
    ))

#Calculate values by groups
remove_vars <- c("set","unit", "country", "continent", "who_region", "income")
data_un_region <- summariseSet(dataset=data, group_var="continent", remove_vars=remove_vars, operation=sum)
data_un_region <- data_un_region %>% 
  mutate(set="continent")
data_who_region <- summariseSet(dataset=data, group_var="who_region", remove_vars=remove_vars, operation=sum)
data_who_region <- data_who_region %>% 
  mutate(set="who_region")
data_income_region <- summariseSet(dataset=data, group_var="income", remove_vars=remove_vars, operation=sum)
data_income_region <- data_income_region %>% 
  mutate(set="income")

data <- bind_rows(data, data_un_region, data_who_region, data_income_region)

#NEED TO REMOVE STRINGS
# Moroco ACDC 3 (2022) 2 (2024), 

#Rename countries for map
data <- data %>% 
  mutate(
    country = ifelse(country=="Congo", "Republic of the Congo", country),
    # country = gsub("", "French Southern and Antarctic Lands", country),
    country = ifelse(country=="Bahamas", "The Bahamas", country),
    country = ifelse(country=="Bolivia (Plurinational State of)", "Bolivia", country),
    country = ifelse(country=="Brunei Darussalam", "Brunei", country),
    country = ifelse(country=="Côte d’Ivoire", "Ivory Coast", country),
    # country = gsub("", "Northern Cyprus", country),
    country = ifelse(country=="Czechia", "Czech Republic", country),
    # country = gsub("", "Falkland Islands", country),
    country = ifelse(country=="United Kingdom of Great Britain and Northern Ireland", "United Kingdom", country),
    country = ifelse(country=="Guinea-Bissau", "Guinea Bissau", country),
    # country = gsub("", "French Guiana", country),
    country = ifelse(country=="Iran (Islamic Republic of)", "Iran", country),
    country = ifelse(country=="Republic of Korea", "South Korea", country),
    # country = gsub("", "Kosovo", country),
    country = ifelse(country=="Lao People's Democratic Republic", "Laos", country),
    country = ifelse(country=="Republic of Moldova", "Moldova", country),
    country = ifelse(country=="North Macedonia", "Macedonia", country),
    country = ifelse(country=="Netherlands (Kingdom of the)", "Netherlands", country),
    country = ifelse(country=="Democratic People's Republic of Korea", "North Korea", country),
    country = ifelse(country=="Russian Federation", "Russia", country),
    # country = gsub("", "Western Sahara", country),
    # country = gsub("", "Somaliland", country),
    country = ifelse(country=="Serbia", "Republic of Serbia", country),
    country = ifelse(country=="Eswatini", "Swaziland", country),
    country = ifelse(country=="Syrian Arab Republic", "Syria", country),
    country = ifelse(country=="Timor-Leste", "East Timor", country),
    country = ifelse(country=="Türkiye", "Turkey", country),
    # country = gsub("", "Taiwan", country),
    country =ifelse(country=="Venezuela (Bolivarian Republic of)", "Venezuela", country),
    country = ifelse(country=="Viet Nam", "Vietnam", country),
    country = ifelse(country=="State of Palestine", "West Bank", country)
  ) %>%
  rename(time=date) %>%
  mutate(name = case_when(
    set=="country" ~ country,
    set=="income" ~ income,
    set=="continent" ~ continent,
    set=="who_region" ~ who_region
  )) 

data <- data %>% 
  mutate(across(starts_with("all_"), ~ replace_na(.x, 0))) %>% 
  relocate(set, name, country, continent, who_region, income, pop, pop_100k, time, everything())
#Save data
write_excel_csv(data, "data/reported/mpox_data.csv")
