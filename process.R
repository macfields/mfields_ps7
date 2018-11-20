
library(tidyverse)
library(readr)
library(fs)
library(janitor)
library(scales)

results <- read.csv("mt_2_results.csv")

#Explore data 
glimpse(results) 
summary(results)

#Downloading Upshot Data. "wb" makes sure that the download works whether or not you are on a windows of a mac computer. 
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

#Creates list of filenames. 

my_list <- dir_ls("2018-live-poll-results-master/data/")

#map_dfr takes a list of filenames, grabs all the files with those names, map says: do something 
#to each item in this list. .id creates a column that tells you the source of the file.

x <- map_dfr(my_list, read_csv, .id = "name") %>% 
  
  #Extract state abbreviation from "name" variable and convert to upper case. 
  mutate(state = toupper(str_sub(name, 51, 52))) %>% 
  
  #Extract wave number from the "name" variable. 
  mutate(wave = str_extract(name, pattern = "[\\d].csv$")) %>%
  
  #Parse wave as an integer instead of a character. 
  mutate(wave = parse_integer(str_sub(wave, 1, 1))) %>% 
  
  #Create office variable using str_detect to detect the presense of an office description in "names". 
  mutate(office = case_when(
    str_detect(name, pattern = "sen") ~ "SEN", 
    str_detect(name, pattern = "gov") ~ "GOV", 
    TRUE ~ "HSE")) %>% 
  
  #Extract district number from "names" variable with str_extract. Parse district as a number. 
  mutate(district = str_extract(name, pattern = "[\\d]{2}-[\\d].csv$")) %>% 
  mutate(district = parse_integer(str_sub(district, 1, 2))) %>% 
  
  #Format district into (ST-00) form. Set District to NA if the poll is for a Sen/Gov race. The
  #Office variable provides a descriptive name for the election race.
  
  mutate(District = paste(state, district, sep = "-")) %>% 
  mutate(District = ifelse(is.na(district), NA, District)) %>% 
  mutate(Office = ifelse(is.na(district), paste(state, office, sep = "-"), District)) %>% 
  
  # Select the variables I need. 
  select(name, response, age_combined, likely, state, wave, office, district, District, Office, final_weight ) %>% 
  
  #Filter out Senator and Governor Races
  filter(! office %in% c("SEN", "GOV"))

#This code is from the midterm 2 solutions. Make a tibble of only the races that were polled more than once. 
two_waves <- x %>% 
  group_by(Office) %>% 
  summarize(waves = n_distinct(wave), 
            first_wave = min(wave)) %>% 
  filter(waves >1)

#dataframe with last poll from each Upshot Race. 
forecasts <- anti_join(x, two_waves, 
                       by = c("Office" = "Office", "wave" = "first_wave")) %>% 
  mutate(rep = ifelse(response == "Rep", final_weight, 0)) %>% 
  mutate(dem = ifelse(response == "Dem", final_weight, 0)) %>% 
  group_by(Office) %>% 
    summarize(interviews = n(),  
              rep_adv = (sum(rep) - sum(dem)) / n(), 
              percent_18to29 = 100 * sum(age_combined == "18 to 29")/interviews)

#get actual election results
results <- results %>% 
  mutate(district_number = parse_integer(district, na = c("AL", "sen", "gov"))) %>% 
  mutate(district_office = case_when(str_detect(district, "sen") ~ "SEN",
                                     str_detect(district, "gov") ~ "GOV",
                                     TRUE ~ NA_character_)) %>% 
  mutate(Office = ifelse(is.na(district_number), 
                         paste(state, district_office, sep = "-"),
                         paste(state, district_number, sep = "-")))

shiny_data <- left_join(forecasts, results, by = "Office") %>% 
  mutate(rep_votes = as.numeric(rep_votes), 
         dem_votes = as.numeric(dem_votes),
         other_votes = as.numeric(other_votes)) %>% 
  mutate(result = (rep_votes - dem_votes) / (rep_votes + dem_votes + other_votes)) %>% 
  rename(forecast = rep_adv) %>%
  select(Office, state, forecast, result, percent_18to29,win_name, win_party)

write_rds(shiny_data, "ps_7/shiny_data.rds")




