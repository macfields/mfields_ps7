#
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
  mutate(district = str_extract(names, pattern = "[\\d]{2}-[\\d].csv$")) %>% 
  mutate(district = parse_integer(str_sub(district, 1, 2))) %>% 
  
  #Format district into (ST-00) form. Set District to NA if the poll is for a Sen/Gov race. The
  #Office variable provides a descriptive name for the election race.
  
  mutate(District = paste(state, district, sep = "-")) %>% 
  mutate(District = ifelse(is.na(district), NA, District)) %>% 
  mutate(Office = ifelse(is.na(district), paste(state, office, sep = "-"), District))





