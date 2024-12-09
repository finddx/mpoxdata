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
  filter(X=="Cumulative Cases" | X=="Cumulative Confirmed Cases" | X== "Cumulative Suspected Cases" | X=="New Cases" | X=="New Confirmed Cases" | X=="Calculated Testing Rate" |  X=="Reported Testing Rate (%)" | X=="Calculated Positivity Rate (%)" | X=="Reported Positivity Rate (%)" ) %>% 
  #Clean dates
  mutate(date = gsub("X", "", date),
         date = gsub("(\\.23\\.|\\.24\\.).*$", "\\1", date),
         date = sub("\\.$", "", date)) %>% 
  mutate(value = trimws(gsub("\\(.*?\\)", "", value))) %>%
  mutate(value = ifelse(value=="N/A", NA, value)) %>% 
  mutate(across(everything(), ~ na_if(., "")),
         across(everything(), ~ if_else(str_detect(., "No info"), NA, .))) %>% 
  #THERE ARE STILL SOME DISTINCT VALUES, BUT DUPLICATES IN OTHER COLS, TO CLARIFY WITH CONSULTANT
  # distinct(Issue.Date, date, X, .keep_all = TRUE) %>% #value,
  pivot_wider(names_from = X,
              values_from = value) 

acdc_data <- acdc_data %>% 
  #Rename variables
  rename(location = Issue.Date,
         cum_confirmed_cases_orig = "Cumulative Confirmed Cases",
         cum_suspected_cases_orig = "Cumulative Cases",
         new_suspected_cases_orig = "New Cases",
         new_confirmed_cases_orig = "New Confirmed Cases",
         testing_rate_orig = "Reported Testing Rate (%)",
         positivity_rate_orig = "Reported Positivity Rate (%)",
         testing_rate_calc = "Calculated Testing Rate",
         positivity_rate_calc = "Calculated Positivity Rate (%)"
  ) %>% 
  mutate(
    date = gsub("\\.", "-", date),
    date = format(as.Date(date, format = "%d-%b-%y"), "%Y-%m-%d")) %>% 
  select(location, date, new_confirmed_cases_orig, new_suspected_cases_orig, cum_confirmed_cases_orig, cum_suspected_cases_orig, testing_rate_orig, positivity_rate_orig, testing_rate_calc, positivity_rate_calc) %>% 
  filter(rowSums(!is.na(select(., -location, -date))) > 0) %>% 
  mutate_at(c("cum_confirmed_cases_orig", "cum_suspected_cases_orig", "new_suspected_cases_orig", "new_confirmed_cases_orig", "testing_rate_orig", "positivity_rate_orig", "testing_rate_calc", "positivity_rate_calc"), as.numeric)


acdc_data <- acdc_data %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2024-01-01")) %>% 
  group_by(location) %>%
  #Fill out missing days
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  #Get orig CUM confirmed as no CUM data from <2024
  # mutate(cum_confirmed_cases_cum = cum_confirmed_cases_orig) %>% 
  # fill(cum_confirmed_cases_cum, .direction = "down") %>%
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
    all_new_suspected_cases = ifelse(is.na(all_new_suspected_cases), 0, all_new_suspected_cases),
    all_cum_suspected_cases = cumsum(all_new_suspected_cases)
  ) %>% 
  #Convert NA back to 0 for reporting
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  ) %>% 
  #Remove original columns
  select(-ends_with("cases_orig"))
# %>% 
#   mutate(
#     cum_confirmed_cases_new = replace_na(cum_confirmed_cases_new, 0),
#     cum_confirmed_cases_cum = replace_na(cum_confirmed_cases_cum, 0),
#     cum_comparison = ifelse(cum_confirmed_cases_new==cum_confirmed_cases_cum, "OK", "Not equal")
#     )

#Calculate N of test
acdc_data <- acdc_data %>% 
  mutate(
    num_tests_pos_orig = (cum_confirmed_cases_calc / positivity_rate_orig) * 100,
    num_tests_pos_calc = (cum_confirmed_cases_calc / positivity_rate_calc) * 100,
    # confirm_pos = (cum_confirmed_cases_calc / num_tests_pos_calc) * 100,
    num_tests_test_orig = (testing_rate_orig * cum_suspected_cases_calc) / 100,
    num_tests_test_calc = (testing_rate_calc * cum_suspected_cases_calc) / 100,
    # confirm_test = (num_tests_test_calc / cum_suspected_cases_calc) * 100
  )

#Add datasource  suffix
acdc_data <- acdc_data %>% 
  rename_with(~ paste0(., "_acdc"), -c(location, date))
#Rename countries
acdc_data <- acdc_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Congo Republic", "Congo", location))
#Save data
write_excel_csv(acdc_data, "data/reported/mpox_data_acdc.csv")
