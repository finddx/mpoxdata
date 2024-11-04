library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)
library(stringr)
source("R/helper_fcts.R")

###Africa CDC data###
acdc_data <- read.csv("data/output/acdc_data.csv")

#Initial cleaning
acdc_data <- acdc_data %>% 
  mutate(Issue.Date = ifelse(Issue.Date=="", NA, Issue.Date)) %>%
  fill(Issue.Date, .direction = "down") %>% 
  #Pivot data on selected columns
  pivot_longer(cols = -c(Issue.Date, X),
               names_to = "date",
               values_to = "value") %>% 
  filter(X=="Cumulative Cases" | X=="Cumulative Confirmed Cases" | X== "Cumulative Suspected Cases" | X=="New Cases" | X=="New Confirmed Cases" | X=="Calculated Testing Rate" | X=="Calculated Positivity Rate (%)") %>% 
  #Clean dates
  mutate(date = gsub("X", "", date),
         date = gsub("(\\.23\\.|\\.24\\.).*$", "\\1", date),
         date = sub("\\.$", "", date)) %>% 
  mutate(value = trimws(gsub("\\(.*?\\)", "", value))) %>%
  mutate(value = ifelse(value=="N/A", NA, value)) %>% 
  mutate(across(everything(), ~ na_if(., "")),
         across(everything(), ~ if_else(str_detect(., "No info"), NA, .))) %>% 
  #THERE ARE STILL SOME DISTINCT VALUES, BUT DUPLICATES IN OTHER COLS, TO CLARIFY WITH CONSULTANT
  distinct(Issue.Date, date, X, .keep_all = TRUE) %>% #value,
  pivot_wider(names_from = X,
              values_from = value) 

acdc_data <- acdc_data %>% 
  # mutate(`Cumulative Cases` = ifelse(is.na(`Cumulative Cases`), `Cumulative Suspected Cases`, `Cumulative Cases`)) %>% 
  #Rename variables
  rename(location = Issue.Date,
         cum_confirmed_cases_orig = "Cumulative Confirmed Cases",
         cum_suspected_cases_orig = "Cumulative Cases",
         new_suspected_cases_orig = "New Cases",
         new_confirmed_cases_orig = "New Confirmed Cases",
         testing_rate_orig = "Calculated Testing Rate",#Not sure if need to use calculated or reported
         positivity_rate_orig = "Calculated Positivity Rate (%)"#Not sure if need to use calculated or reported
  ) %>% 
  mutate(
    date = gsub("\\.", "-", date),
    date = format(as.Date(date, format = "%d-%b-%y"), "%Y-%m-%d")) %>% 
  select(location, date, new_confirmed_cases_orig, new_suspected_cases_orig, cum_confirmed_cases_orig, cum_suspected_cases_orig, testing_rate_orig, positivity_rate_orig) %>% 
  filter(rowSums(!is.na(select(., -location, -date))) > 0) %>% 
  mutate_at(c("cum_confirmed_cases_orig", "cum_suspected_cases_orig", "new_suspected_cases_orig", "new_confirmed_cases_orig"), as.numeric)


#Get min cum values
# acdc_data_min_date_suspected <- acdc_data %>% 
#   filter(!is.na(cum_suspected_cases_orig)) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>%  
#   filter(date == min(date)) %>%
#   select(location, date, cum_suspected_cases_orig) %>% 
#   rename(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig) %>% 
#   filter(date >= as.Date("2024-01-01"))

# acdc_data_new_suspected <- acdc_data %>% 
#   filter(!is.na(cum_suspected_cases_orig)) %>% 
#   mutate(new_suspected_cases_calc = NA) %>% 
#   left_join(acdc_data_min_date_suspected, by=join_by(location, date)) %>% 
#   filter(date >= as.Date("2024-01-01")) %>% 
#   mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
#   group_by(location) %>%
#   mutate(n_row=n()) %>% 
#   filter(n_row >1) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>%  
#   mutate(cum_suspected_cases_orig = as.numeric(cum_suspected_cases_orig)) %>% 
#   mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
#   mutate(date = as.Date(date)) %>% 
#   select(location, date, new_suspected_cases_calc) 

# acdc_data_min_date <- acdc_data %>% 
#   filter(!is.na(cum_confirmed_cases_orig)) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>%  
#   filter(date == min(date)) %>%
#   select(location, date, cum_confirmed_cases_orig) %>% 
#   rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig) %>% 
#   filter(date >= as.Date("2024-01-01"))

# acdc_data_new <- acdc_data %>% 
#   filter(!is.na(cum_confirmed_cases_orig)) %>% 
#   mutate(new_confirmed_cases_calc = NA) %>% 
#   left_join(acdc_data_min_date, by=join_by(location, date)) %>% 
#   filter(date >= as.Date("2024-01-01")) %>% 
#   mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_calc)) %>%
#   group_by(location) %>%
#   mutate(n_row=n()) %>% 
#   filter(n_row >1) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>%  
#   mutate(cum_confirmed_cases_orig = as.numeric(cum_confirmed_cases_orig)) %>% 
#   mutate(new_confirmed_cases_calc = calc_new_t(cum_confirmed_cases_orig, new_confirmed_cases_calc)) %>% 
#   mutate(date=as.Date(date)) %>% 
#   select(location, date, new_confirmed_cases_calc) 

acdc_data <- acdc_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  # left_join(acdc_data_new, by=join_by(location, date)) %>% 
  # left_join(acdc_data_new_suspected, by=join_by(location, date)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  # mutate(new_confirmed_cases_calc = as.numeric(new_confirmed_cases_calc),
         # new_suspected_cases_calc = as.numeric(new_suspected_cases_calc)) %>% 
  mutate(new_confirmed_cases_calc = as.numeric(new_confirmed_cases_orig),
         new_suspected_cases_calc = as.numeric(new_suspected_cases_orig)) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
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
acdc_data <- acdc_data %>% 
  rename_with(~ paste0(., "_acdc"), -c(location, date))
#Rename countries
acdc_data <- acdc_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Congo Republic", "Congo", location))

write_excel_csv(acdc_data, "data/reported/mpox_data_acdc.csv")
