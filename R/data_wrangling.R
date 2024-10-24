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
write.csv(gh_data, paste("data/raw/gh_data_", today, ".csv"), row.names=FALSE)

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
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(
    new_confirmed_cases_orig = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_orig),
    new_suspected_cases_orig = replace_na(new_suspected_cases_orig, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_orig)
  ) %>% 
  mutate(
    new_confirmed_cases_orig = ifelse(new_confirmed_cases_orig==0, NA, new_confirmed_cases_orig),
    new_suspected_cases_orig = ifelse(new_suspected_cases_orig==0, NA, new_suspected_cases_orig)
  ) %>% 
  mutate(
    smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_orig, cum_confirmed_cases_calc),
    smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
    smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
    smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_orig, cum_suspected_cases_calc),
    smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
    smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_orig = replace_na(new_confirmed_cases_orig, 0),
    new_suspected_cases_orig = replace_na(new_suspected_cases_orig, 0)
  )

# gh_data <- gh_data %>%
#   mutate(year = year(date)) %>%
#   group_by(location, date = if_else(year < 2024, as.character(year), as.character(date))) %>%
#   summarise(across(c(new_confirmed_cases_orig, smooth_new_confirmed_cases_calc, new_suspected_cases_orig,  smooth_new_suspected_cases_calc), ~ sum(.x, na.rm = TRUE)),
#             across(c(cum_confirmed_cases_calc, smooth_cum_suspected_cases_calc, cum_suspected_cases_calc, smooth_cum_suspected_cases_calc), ~ last(.x))) %>% 
#   ungroup()

###Our world in data###
owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")
write.csv(owd_data, paste("data/raw/owd_data_", today, ".csv"), row.names=FALSE)

owd_data <- owd_data %>% 
  select(location, date, iso_code, total_cases, new_cases, suspected_cases_cumulative) %>% 
  rename(cum_confirmed_cases_orig = total_cases,
         new_confirmed_cases_orig = new_cases,
         cum_suspected_cases_orig = suspected_cases_cumulative
         )

owd_data_min_date_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_suspected_cases_orig) %>% 
  rename(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig)

owd_data_new_suspected <- owd_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = NA) %>% 
  left_join(owd_data_min_date_suspected, by=join_by(location, date)) %>% 
  mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  # mutate(suspected_cases2 = total_suspected_cases - lag(total_suspected_cases)) %>%  
  mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_suspected_cases_calc)
#new_cases_smoothed, new_cases_per_million, total_cases_per_million, new_cases_smoothed_per_million

#Get the min date with data
owd_data_min_date <- owd_data %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  mutate(date=as.Date(date)) %>% 
  select(location, date, cum_confirmed_cases_orig) %>% 
  rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig)

owd_data <- owd_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2022-05-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  left_join(owd_data_min_date, by=join_by(location, date)) %>% 
  mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_orig)) %>% 
  left_join(owd_data_new_suspected, by=join_by(location, date)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
    new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  ) %>% 
  mutate(
    smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
    smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
    smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
    smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
    smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
    smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  ) 
# %>%
#   select(-c(total_cases_plhd, total_cases, total_suspected_cases, iso_code))

# owd_data <- owd_data %>%
#   mutate(year = year(date)) %>%
#   group_by(location, date = if_else(year < 2024, as.character(year), as.character(date))) %>%
#   summarise(across(c(new_confirmed_cases_calc, smooth_new_confirmed_cases_calc, new_suspected_cases_calc, smooth_new_suspected_cases_calc), ~ sum(.x, na.rm = TRUE)),
#             across(c(cum_confirmed_cases_calc, smooth_cum_confirmed_cases_calc, cum_suspected_cases_calc, smooth_cum_suspected_cases_calc), ~ last(.x))) %>% 
#   ungroup()

# Status of a case. Cases which are discarded were previously suspected but have now been confirmed negative, and should be excluded from case counts.


###Africa CDC PDFs###
acdc_pdf_data <- read.csv("data/Output/mpox_data_ACDC_reports.csv")
write.csv(gh_data, paste("data/raw/acdc_pdf_data_", today, ".csv"), row.names=FALSE)

acdc_pdf_data  <- acdc_pdf_data  %>% 
  select(Country, Date.of.Issue, Confirmed, Confirmed_New, Suspected, Suspected_New) %>% 
  rename(cum_confirmed_cases_orig = Confirmed,
         new_confirmed_cases_orig = Confirmed_New,
         cum_suspected_cases_orig = Suspected,
         new_suspected_cases_orig = Suspected_New,
         location = Country, 
         date = Date.of.Issue
  ) %>% 
  mutate(date = gsub("\\.", "-", date),
         date = format(as.Date(date, format = "%d-%m-%Y"), "%Y-%m-%d"))

#Get min cum values
acdc_pdf_data_min_date_suspected <- acdc_pdf_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_suspected_cases_orig) %>% 
  rename(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig)

acdc_pdf_data_new_suspected <- acdc_pdf_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = NA) %>% 
  left_join(acdc_pdf_data_min_date_suspected, by=join_by(location, date)) %>% 
  mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_suspected_cases_calc)


acdc_pdf_data_min_date <- acdc_pdf_data %>% 
  filter(!is.na(cum_confirmed_cases_orig)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_confirmed_cases_orig) %>% 
  rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig)

acdc_pdf_data_new <- acdc_pdf_data %>% 
  filter(!is.na(cum_confirmed_cases_orig)) %>% 
  mutate(new_confirmed_cases_calc = NA) %>% 
  left_join(acdc_pdf_data_min_date, by=join_by(location, date)) %>% 
  mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(new_confirmed_cases_calc = calc_new_t(cum_confirmed_cases_orig, new_confirmed_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_confirmed_cases_calc)

#Calculate smooth vars
acdc_pdf_data <- acdc_pdf_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  fill(cum_confirmed_cases_orig, .direction = "down") %>% 
  fill(cum_suspected_cases_orig, .direction = "down") %>% 
  left_join(acdc_pdf_data_new, by=join_by(location, date)) %>% 
  left_join(acdc_pdf_data_new_suspected, by=join_by(location, date)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
    new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  ) %>% 
  mutate(
    smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
    smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
    smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
    smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
    smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
    smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  )



###Africa CDC data###
acdc_data <- read.csv("data/raw/OPDRI_data_16_10_2024.csv")
# write.csv(gh_data, paste("data/raw/acdc_data_", today, ".csv"), row.names=FALSE)

acdc_data <- acdc_data %>% 
  pivot_longer(cols = -c(Issue.Date, X),
               names_to = "date",
               values_to = "value") %>% 
  filter(X=="Cumulative Cases" | X=="Cumulative Confirmed Cases" | X== "Cumulative Suspected Cases" | X=="New Cases" | X=="New Confirmed Cases" | X=="Testing Rate (%)" | X=="Positivity Rate (%)") %>% 
  pivot_wider(names_from = X,
              values_from = value) 

acdc_data <- acdc_data %>% 
  mutate(`Cumulative Cases` = ifelse(is.na(`Cumulative Cases`), `Cumulative Suspected Cases`, `Cumulative Cases`)) %>% 
  rename(location = Issue.Date,
         cum_confirmed_cases_orig = "Cumulative Confirmed Cases",
         cum_suspected_cases_orig = "Cumulative Cases",
         new_suspected_cases_orig = "New Cases",
         new_confirmed_cases_orig = "New Confirmed Cases",
         testing_rate_orig = "Testing Rate (%)",
         positivity_rate_orig = "Positivity Rate (%)"
  ) %>% 
  mutate(across(everything(), ~ na_if(., "")),
         across(everything(), ~ if_else(str_detect(., "No info"), NA, .))) %>% 
  mutate(date = gsub("X", "", date),
         date = gsub("\\.", "-", date),
         date = format(as.Date(date, format = "%d-%b-%y"), "%Y-%m-%d")) %>% 
  select(location, date, new_confirmed_cases_orig, new_suspected_cases_orig, cum_confirmed_cases_orig, cum_suspected_cases_orig, testing_rate_orig, positivity_rate_orig) %>% 
  filter(rowSums(!is.na(select(., -location, -date))) > 0) 

#Get min cum values
acdc_data_min_date_suspected <- acdc_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_suspected_cases_orig) %>% 
  rename(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig)

acdc_data_new_suspected <- acdc_data %>% 
  filter(!is.na(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = NA) %>% 
  left_join(acdc_data_min_date_suspected, by=join_by(location, date)) %>% 
  mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(cum_suspected_cases_orig = as.numeric(cum_suspected_cases_orig)) %>% 
  mutate(new_suspected_cases_calc = calc_new_t(cum_suspected_cases_orig, new_suspected_cases_calc)) %>% 
  mutate(date = as.Date(date)) %>% 
  select(location, date, new_suspected_cases_calc)


acdc_data_min_date <- acdc_data %>% 
  filter(!is.na(cum_confirmed_cases_orig)) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  select(location, date, cum_confirmed_cases_orig) %>% 
  rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig)

acdc_data_new <- acdc_data %>% 
  filter(!is.na(cum_confirmed_cases_orig)) %>% 
  mutate(new_confirmed_cases_calc = NA) %>% 
  left_join(acdc_data_min_date, by=join_by(location, date)) %>% 
  mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_calc)) %>%
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  mutate(cum_confirmed_cases_orig = as.numeric(cum_confirmed_cases_orig)) %>% 
  mutate(new_confirmed_cases_calc = calc_new_t(cum_confirmed_cases_orig, new_confirmed_cases_calc)) %>% 
  mutate(date=as.Date(date)) %>% 
  select(location, date, new_confirmed_cases_calc)



acdc_data <- acdc_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  fill(cum_confirmed_cases_orig, .direction = "down") %>% 
  fill(cum_suspected_cases_orig, .direction = "down") %>% 
  left_join(acdc_data_new, by=join_by(location, date)) %>% 
  left_join(acdc_data_new_suspected, by=join_by(location, date)) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(new_confirmed_cases_calc = as.numeric(new_confirmed_cases_calc),
         new_suspected_cases_calc = as.numeric(new_suspected_cases_calc)) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
    new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  ) %>% 
  mutate(
    smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
    smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
    smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
    smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
    smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
    smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  ) %>% 
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  )
  
  # #Get the min date with data
  # acdc_data_min_date <- acdc_data %>% 
  #   group_by(location) %>%
  #   arrange(location, date) %>%  
  #   filter(date == min(date)) %>%
  #   mutate(date = as.Date(date)) %>% 
  #   select(location, date, cum_confirmed_cases_orig) %>% 
#   rename(cum_confirmed_cases_orig_plhd = cum_confirmed_cases_orig)
# 
# acdc_data_min_date_suspected <- acdc_data %>% 
#   filter(!is.na(cum_suspected_cases_orig)) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>%  
#   filter(date == min(date)) %>%
#   select(location, date, cum_suspected_cases_orig) %>% 
#   rename(cum_suspected_cases_orig_plhd = cum_suspected_cases_orig)

# acdc_data <- acdc_data %>% 
#   mutate(date = as.Date(date)) %>% 
#   group_by(location) %>%
#   complete(date = seq.Date(as.Date("2024-01-01"), Sys.Date(), by = "day")) %>%
#   fill(location) %>% 
#   left_join(acdc_data_min_date, by=join_by(location, date)) %>% 
#   mutate(new_confirmed_cases_calc = ifelse(!is.na(cum_confirmed_cases_orig_plhd), cum_confirmed_cases_orig_plhd, new_confirmed_cases_orig)) %>% 
#   left_join(acdc_data_min_date_suspected, by=join_by(location, date)) %>% 
#   mutate(new_suspected_cases_calc = ifelse(!is.na(cum_suspected_cases_orig_plhd), cum_suspected_cases_orig_plhd, new_suspected_cases_orig)) %>% 
#   group_by(location) %>%
#   arrange(location, date) %>% 
#   fill(cum_confirmed_cases_orig, .direction = "down") %>% 
#   fill(cum_suspected_cases_orig, .direction = "down") %>% 
  # mutate(
  #   new_confirmed_cases_calc = as.numeric(new_confirmed_cases_calc),
  #   new_suspected_cases_calc = as.numeric(new_suspected_cases_calc)
  # ) %>%
  # mutate(
  #   new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
  #   cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
  #   new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0),
  #   cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  # ) %>% 
  # mutate(
  #   new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
  #   new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  # ) %>% 
  # mutate(
  #   smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
  #   smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
  #   smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
  #   smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
  #   smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
  #   smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  # ) %>% 
  # mutate(
  #   new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
  #   new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
  #   )

###WHO World data###
who_data <- read.csv("data/raw/world_data_08_2024.csv")
# write.csv(gh_data, paste("data/raw/who_data_", today, ".csv"), row.names=FALSE)

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
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2022-01-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  fill(cum_confirmed_cases_orig, .direction = "down") %>% 
  fill(cum_suspected_cases_orig, .direction = "down") %>% 
  group_by(location) %>%
  arrange(location, date) %>%
  fill(cum_confirmed_cases_orig, .direction = "down") %>%
  fill(cum_suspected_cases_orig, .direction = "down") %>%
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_orig, 0),
    cum_confirmed_cases_calc = cumsum(new_confirmed_cases_calc),
    new_suspected_cases_calc = replace_na(new_suspected_cases_orig, 0),
    cum_suspected_cases_calc = cumsum(new_suspected_cases_calc)
  ) %>%
  mutate(
    new_confirmed_cases_calc = ifelse(new_confirmed_cases_calc==0, NA, new_confirmed_cases_calc),
    new_suspected_cases_calc = ifelse(new_suspected_cases_calc==0, NA, new_suspected_cases_calc)
  ) %>%
  mutate(
    smooth_new_confirmed_cases_calc = smooth_new_tests(new_confirmed_cases_calc, cum_confirmed_cases_calc),
    smooth_new_confirmed_cases_calc = ifelse(is.na(smooth_new_confirmed_cases_calc), 0, smooth_new_confirmed_cases_calc),
    smooth_cum_confirmed_cases_calc = cumsum(smooth_new_confirmed_cases_calc),
    smooth_new_suspected_cases_calc = smooth_new_tests(new_suspected_cases_calc, cum_suspected_cases_calc),
    smooth_new_suspected_cases_calc = ifelse(is.na(smooth_new_suspected_cases_calc), 0, smooth_new_suspected_cases_calc),
    smooth_cum_suspected_cases_calc = cumsum(smooth_new_suspected_cases_calc)
  ) %>%
  mutate(
    new_confirmed_cases_calc = replace_na(new_confirmed_cases_calc, 0),
    new_suspected_cases_calc = replace_na(new_suspected_cases_calc, 0)
    )
  
####Country data###
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

acdc_pdf_data <- acdc_pdf_data %>% 
  rename_with(~ paste0("acdc_pdf_", .), -c(location, date))
acdc_pdf_data <- acdc_pdf_data %>% 
  mutate(location = gsub("Côte d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Congo Republic", "Congo", location))

acdc_data <- acdc_data %>% 
  rename_with(~ paste0("acdc_", .), -c(location, date))
acdc_data <- acdc_data %>% 
  mutate(location = gsub("Cote d'Ivoire", "Côte d’Ivoire", location),
         location = gsub("Congo Republic", "Congo", location))

who_data <- who_data %>% 
  rename_with(~ paste0("who_", .), -c(location, date))
who_data <- who_data %>% 
  mutate(location = gsub("CÃ´te dâ€™Ivoire", "Côte d’Ivoire", location),
         location = gsub( "CuraÃ§ao", "Curaçao", location),
         location = gsub( "Netherlands", "Netherlands (Kingdom of the)", location),
         location = gsub( "Saint Martin", "Saint Martin (French Part)", location),
         location = gsub( "TÃ¼rkiye", "Türkiye", location),
         location = gsub( "The United Kingdom", "United Kingdom of Great Britain and Northern Ireland", location))
  

# weekly _africacdc_06_10_2024

data <- read.csv("data/raw/country_info.csv")

data_dates <- data %>%
  mutate(date = NA) %>%
  group_by(UN_country) %>%
  complete(date = seq.Date(as.Date("2022-01-01"), Sys.Date(), by = "day")) %>%
  # mutate(date = if_else(year(date) < 2024, as.character(year(date)), as.character(date))) %>% 
  select(UN_country, date)

data <- data %>%
  full_join(data_dates, by=join_by(UN_country)) %>% 
  filter(!is.na(ISO.alpha3.Code)) %>% 
  filter(UN_country != "Antarctica") %>% 
  filter(nchar(trimws(WHO_region)) > 0) %>% 
  filter(nchar(trimws(Income.group)) > 0)

data <- data %>% 
  full_join(who_data, by=join_by(UN_country == location, date==date)) 
data <- data %>% 
  full_join(acdc_data, by=join_by(UN_country == location, date==date)) 
data <- data %>% 
  full_join(acdc_pdf_data, by=join_by(UN_country == location, date==date)) 
data <- data %>% 
  full_join(gh_data, by=join_by(UN_country == location, date==date))
data <- data %>% 
  full_join(owd_data, by=join_by(UN_country == location, date==date)) 

data <- data %>%
  filter(date >= "2024-01-01") 
  

data <- data %>% 
  mutate(set="Country") %>% 
  rename(iso3 = ISO.alpha3.Code) %>% 
  filter(!is.na(iso3)) %>% 
  # relocate(set, iso3, everything()) %>% 
  select(-c(owd_iso_code, ends_with("_plhd"))) 

data <- data %>% 
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
# %>% 
#   relocate(iso3, UN_country, UN_region, UN_subregion, WHO_region, Income.group, latitude, longitude, date,
#            who_new_confirmed_cases_orig, acdc_new_confirmed_cases_orig, acdc_pdf_new_confirmed_cases_orig,
#            gh_new_confirmed_cases_orig, owd_new_confirmed_cases_orig,
#            who_cum_confirmed_cases_orig, acdc_cum_confirmed_cases_orig, acdc_pdf_cum_confirmed_cases_orig,
#            gh_cum_confirmed_cases_calc, owd_cum_confirmed_cases_orig,
#            who_new_suspected_cases_orig, acdc_new_suspected_cases_orig, acdc_pdf_new_suspected_cases_orig,
#            gh_new_suspected_cases_orig, owd_new_suspected_cases_calc,
#            who_cum_suspected_cases_orig, acdc_cum_suspected_cases_orig, acdc_pdf_cum_suspected_cases_orig,
#            gh_cum_suspected_cases_calc, owd_cum_suspected_cases_orig,
#            acdc_testing_rate_orig, acdc_positivity_rate_orig)

  
# acdc_pdf_cum_confirmed_cases_calc, acdc_pdf_cum_suspected_cases_calc
# acdc_pdf_smooth_new_confirmed_cases_calc, acdc_pdf_smooth_cum_suspected_cases_calc, acdc_pdf_smooth_new_suspected_cases_calc
# gh_smooth_new_confirmed_cases_calc, gh_smooth_cum_suspected_cases_calc, gh_smooth_new_suspected_cases_calc      
# owd_cum_confirmed_cases_calc, owd_new_confirmed_cases_calc, owd_cum_suspected_cases_calc
# owd_smooth_new_confirmed_cases_calc, owd_smooth_cum_confirmed_cases_calc, owd_smooth_new_suspected_cases_calc, owd_smooth_cum_suspected_cases_calc                          
           
remove_vars <- c("set","iso3", "UN_country", "UN_region", "UN_subregion", "WHO_country", "WHO_region", "Income.group", "latitude", "longitude", "acdc_testing_rate_orig", "acdc_positivity_rate_orig", "acdc_new_confirmed_cases_orig", "acdc_new_suspected_cases_orig", "acdc_cum_confirmed_cases_orig", "acdc_cum_suspected_cases_orig")
 
data_un_region <- summariseSet(dataset=data, group_var="UN_region", remove_vars=remove_vars, operation=sum)
data_un_region <- data_un_region %>% 
  mutate(set="UN_region")
data_who_region <- summariseSet(dataset=data, group_var="WHO_region", remove_vars=remove_vars, operation=sum)
data_who_region <- data_who_region %>% 
  mutate(set="WHO_region")
data_income_region <- summariseSet(dataset=data, group_var="Income.group", remove_vars=remove_vars, operation=sum)
data_income_region <- data_income_region %>% 
  mutate(set="Income.group")

data <- bind_rows(data, data_un_region, data_who_region, data_income_region)

data <- data %>% 
  mutate(
    dxgap_who_orig = dxGap(who_cum_suspected_cases_orig, who_cum_confirmed_cases_orig),
    # dxgap_acdc_orig = dxGap(acdc_cum_suspected_cases_orig, acdc_cum_confirmed_cases_orig),
    dxgap_acdc_calc = dxGap(acdc_cum_suspected_cases_calc, acdc_cum_confirmed_cases_calc),
    dxgap_acdc_pdf_orig = dxGap(acdc_pdf_cum_suspected_cases_orig, acdc_pdf_cum_confirmed_cases_orig),
    dxgap_acdc_pdf_calc = dxGap(acdc_pdf_cum_suspected_cases_calc, acdc_pdf_cum_confirmed_cases_calc),
    dxgap_gh_calc = dxGap(gh_cum_suspected_cases_calc, gh_cum_confirmed_cases_calc),
    dxgap_owd_orig = dxGap(owd_cum_suspected_cases_orig, owd_cum_confirmed_cases_orig),
    dxgap_owd_calc = dxGap(owd_cum_suspected_cases_calc, owd_cum_confirmed_cases_calc)
    ) %>% 
  mutate(across(where(is.numeric), ~ ifelse(. %in% c(-Inf, Inf, NaN), NA, .))) %>% 
  relocate(set, everything())


# suspected <- 89 #cum
# confirm <- 6 #cum
# pos_rate_orig <- 8.2
# test_rate_orig <- 82
#   
# test <-  confirm / pos_rate_orig * 100
# pos_rate <- confirm / test * 100 
# 
# test2 <-  test_rate_orig * suspected / 100
# test_rate <- test2 / suspected * 100

# Moroco ACDC 3 (2022) 2 (2024), #ASK Jamie to only keep numbers
# pos_rate <- confirm / test2 *1000


#IF date=="2024/01/01" cum = cum - lag()


write.csv(data, "data/output/mpox_data.csv", row.names=FALSE, fileEncoding="UTF-8")
