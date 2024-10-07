library(readr)
library(dplyr)
library(tidyr)

#Global health
gh_data <- read.csv("https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv")
gh_data <- gh_data %>% 
  count(Case_status, Location_Admin0, Date_report_source_I) %>% 
  filter(Case_status != "omit_error") %>%
  pivot_wider(names_from = Case_status, values_from = n) %>% 
  rename(new_cases = confirmed,
         location = Location_Admin0,
         date = Date_report_source_I)

gh_datad <- gh_data %>% 
  mutate(date = as.Date(date)) %>% 
  group_by(location) %>%
  complete(date = seq.Date(as.Date("2022-05-01"), Sys.Date(), by = "day")) %>%
  fill(location) %>% 
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  mutate(new_cases = ifelse(new_cases==0, NA, new_cases)) %>% 
  mutate(smooth_new_cases = smooth_new_tests(new_cases, cum_cases),
         smooth_new_cases = ifelse(is.na(smooth_new_cases), 0, smooth_new_cases),
         smooth_cum_cases = cumsum(smooth_new_cases))
# mutate(new_cases = total_cases - lag(total_cases)) 

#Our world in data
owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")
owd_data <- owd_data %>% 
  select(location, date, iso_code, total_cases, new_cases, new_cases_smoothed, new_cases_per_million, total_cases_per_million, new_cases_smoothed_per_million,suspected_cases_cumulative)
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
  group_by(location) %>%
  arrange(location, date) %>% 
  mutate(new_cases = replace_na(new_cases, 0),
         cum_cases = cumsum(new_cases)) %>% 
  mutate(new_cases = ifelse(new_cases==0, NA, new_cases)) %>%
  mutate(smooth_new_cases = smooth_new_tests(new_cases, cum_cases),
         smooth_new_cases = ifelse(is.na(smooth_new_cases), 0, smooth_new_cases),
         smooth_cum_cases = cumsum(smooth_new_cases))




# Status of a case. Cases which are discarded were previously suspected but have now been confirmed negative, and should be excluded from case counts. Cases which are omit_error were incorrectly added and should be dismissed from any data interpretation. Must be one of: ["confirmed", "probable", "suspected", "discarded", "omit_error"].



# gh_datax <- gh_datax %>% 
  # complete(date = seq.Date(min(date), max(date), by = "day")) %>%
  # fill(location) 
library(imputeTS)

gh_datax2 <- gh_datax %>%  
  mutate(new_cases = ifelse(new_cases==0,NA,new_cases)) %>% 
  mutate(
    smooth_mean = na_mean(new_cases, option = "mean"), 
  # smooth_interpolation =  na_interpolation(new_cases, option = "linear") %>% 
  # smooth_kalman =  na_kalman(new_cases, model = "StructTS", smooth = TRUE) %>% 
  # smooth_seadec =  na_seadec(new_cases, algorithm = "interpolation", find_frequency = FALSE) %>% 
  # smooth_seasplit =  na_seasplit(new_cases, algorithm = "interpolation", find_frequency = FALSE) %>% 
  # smooth_ma = na_ma(new_cases, k = 4, weighting = "linear") %>% 
  # smooth_aprox = approx(date, new_cases, date, method = "linear")$y
    smooth = smooth_new_tests(new_cases, cum_cases)) %>% 
  mutate(robust_cases = round(robust_rollmean(new_cases)),
         cum_cases2 = calc_cumulative_t(cum_cases, new_cases),
         new_cases2 = calc_new_t(cum_cases2, new_cases)
         )

  
 



 
  
  
  



  

data <- read.csv("raw/country_info.csv")

data <- data %>% 
  full_join(gh_data, by=join_by(UN_country == Location_Admin0))
data <- data %>% 
  full_join(owd_data, by=join_by(ISO.alpha3.Code == iso_code)) 


write.csv(data, "output/mpox_data.csv")
