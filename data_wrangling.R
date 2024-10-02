library(readr)
library(dplyr)
library(tidyr)


gh_data <- read.csv("https://mpox-2024.s3.eu-central-1.amazonaws.com/latest.csv")

owd_data <- read.csv("https://catalog.ourworldindata.org/explorers/who/latest/monkeypox/monkeypox.csv")

# Age, Gender, Source_I,Date_entry, Date_last_modified

# Status of a case. Cases which are discarded were previously suspected but have now been confirmed negative, and should be excluded from case counts. Cases which are omit_error were incorrectly added and should be dismissed from any data interpretation. Must be one of: ["confirmed", "probable", "suspected", "discarded", "omit_error"].

gh_data <- gh_data %>% 
  count(Case_status, Outcome, Location_Admin0, Date_report_source_I) %>% 
  filter(Case_status != "omit_error") %>%
  pivot_wider(names_from = Case_status, values_from = n)


data <- read.csv("raw/country_info.csv")

data <- data %>% 
  full_join(gh_data, by=join_by(UN_country == Location_Admin0))
data <- data %>% 
  full_join(owd_data, by=join_by(ISO.alpha3.Code == iso_code)) 


write.csv(data, "output/mpox_data.csv")
