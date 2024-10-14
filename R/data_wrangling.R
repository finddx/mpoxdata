library(readr)
library(dplyr)
library(tidyr)



#####SAVE RAW SOURCES####

#Global health
gh_data <- read.csv("https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv")
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

# mutate(new_cases = total_cases - lag(total_cases)) 

calc_new_t <- function(cumulative_t, new_t) {
  for (i in 2:length(cumulative_t)) {
    if (is.na(new_t[i]) & !is.na(cumulative_t[i])) {
      new_t[i] = cumulative_t[i] - cumulative_t[i-1]
    }
  }
  new_t
}
#Our world in data
owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")
owd_data <- owd_data %>% 
  select(location, date, iso_code, total_cases, new_cases, suspected_cases_cumulative) %>% 
  rename(total_suspected_cases = suspected_cases_cumulative)

owd_datax <- owd_data %>% 
  filter(!is.na(total_suspected_cases)) %>% 
  mutate(suspected_cases = NA) %>% 
  group_by(location) %>%
  mutate(n_row=n()) %>% 
  filter(n_row >1) %>% 
  arrange(location, date) %>%  
  
  mutate(suspected_cases = calc_new_t(total_suspected_cases, suspected_cases))
#new_cases_smoothed, new_cases_per_million, total_cases_per_million, new_cases_smoothed_per_million
#Get the min date with data
owd_data_min_date <- owd_data %>% 
  group_by(location) %>%
  arrange(location, date) %>%  
  filter(date == min(date)) %>%
  mutate(date=as.Date(date)) %>% 
  select(location, date, total_cases) %>% 
  rename(total_cases_plhd = total_cases)

owd_datax <- owd_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2022-05-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  left_join(owd_data_min_date, by=join_by(location, date)) %>% 
  mutate(new_cases=ifelse(!is.na(total_cases_plhd), total_cases_plhd, new_cases)) %>% 
  mutate(total_suspected_cases = replace_na(total_suspected_cases, 0),
         suspected_cases = total_suspected_cases - lag(total_suspected_cases),
         ) %>% 
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




# Status of a case. Cases which are discarded were previously suspected but have now been confirmed negative, and should be excluded from case counts.


data <- read.csv("data/raw/country_info.csv")

data <- data %>% 
  full_join(gh_data, by=join_by(UN_country == Location_Admin0))
data <- data %>% 
  full_join(owd_data, by=join_by(ISO.alpha3.Code == iso_code)) 


write.csv(data, "data/output/mpox_data.csv")
