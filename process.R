
library(tidyverse)
library(readr)
library(fs)
library(janitor)
library(scales)
library(reshape2)


#Downloading Upshot Data. "wb" makes sure that the download works whether on Mac or PC. 
download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")

unzip("master.zip")

#Creates list of filenames in the Upshot Data. 

my_list <- dir_ls("2018-live-poll-results-master/data/")

#map_dfr takes the list of filenames and grabs all the files in the Upshot Data with those names, then reads all of the csv files.
#.id creates a column that contains the name of the source of the file.

x <- map_dfr(my_list, read_csv, .id = "name") %>% 
  
  #The code to create the extra variables in "x" is from the midterm solutions. 
  #Extract state abbreviation from "name" variable and convert to upper case use str_sub and toupper. 
  mutate(state = toupper(str_sub(name, 51, 52))) %>% 
  
  #Extract wave number from the "name" variable. Use str_extract instead of str_sub because str_extract extracts
  #matching patterns from a string whereas str_sub extracts based on character position. 
  mutate(wave = str_extract(name, pattern = "[\\d].csv$")) %>%
  
  #Parse wave as an integer instead of a character. 
  mutate(wave = parse_integer(str_sub(wave, 1, 1))) %>% 
  
  #Create office variable using str_detect to detect the presense of an office description in "names". 
  mutate(office = case_when(
    str_detect(name, pattern = "sen") ~ "SEN", 
    str_detect(name, pattern = "gov") ~ "GOV", 
    TRUE ~ "HSE")) %>% 
  
  #Extract district number from "names" variable with str_extract. Parse district as a number instead of a character. 
  mutate(district = str_extract(name, pattern = "[\\d]{2}-[\\d].csv$")) %>% 
  mutate(district = parse_integer(str_sub(district, 1, 2))) %>% 
  
  #Format district into (ST-00) form. Set District to NA if the poll is for a Sen/Gov race. The
  #Office variable provides a descriptive name for the election race. If district is NA because it is a
  #governor or senator race, the Office variable will paste together state and office, if District is not NA, 
  #Office and District will be the same. 
  
  mutate(District = paste(state, district, sep = "-")) %>% 
  mutate(District = ifelse(is.na(district), NA, District)) %>% 
  mutate(Office = ifelse(is.na(district), paste(state, office, sep = "-"), District)) %>% 
  
  # Select the variables I need. I keep age_combined, race_eth, gender, educ, and likely because I want 
  # to look at how key demographics affect the predicted Republican Advantage. 
  select(name, response, age_combined, race_eth, gender, educ, likely, state, wave, office, district, District, Office, final_weight ) %>% 
  
  #Filter out Senator and Governor Races. I am only interested in House races. 
  filter(! office %in% c("SEN", "GOV"))

#Note: The code to create two_waves is from the midterm 2 solutions. This makes a tibble of only the races that were polled more than once.
# I need to do this because in my final analysis, I only want to include the last poll for races that were polled more than once. 
# I need a list of the districts for which I need to pull out the polls in wave 3, or Wave 2 in the case of PA-1. 
two_waves <- x %>% 
  group_by(Office) %>% 
  summarize(waves = n_distinct(wave), 
            first_wave = min(wave)) %>% 
  filter(waves >1)

#Note: the first portion of this code is from the midterm 2 solutions. 
#Here, I create a dataframe with last poll from each Upshot Race.
# I want to use anti_join because I want to get rid of polls in 
# x that have a match in two waves. I join by "Office" and "wave" in x, and 
#"Office" and "first_wave" in two_waves. This effectively gets rid of the first wave of 
# polls in districts that were polled twice. It only keeps the second poll in districts that were
#polled twice. It also includes the polls for districts that were polled once. This is exactly what I want. 

# I create the rep and dem variables so that I can calculate the republican advantage. 
# I do so with ifelse. In the case of "rep", it is equal to 0 if "response" is not equal to "REP". 
# If response is equal to "REP", the value of "rep" is the final_weight of the respondent. 
# I do this because it weights rep and dem by the final weight. 

#I follow a simular process to create the youth, black, hispanic, female, and collegegrad variables because I want the number
# of respondents in these categories to be weighted by final_weight. 

# I then group by Office and use the summarize command to calculate the republican advantage. 

forecasts <- anti_join(x, two_waves, 
                       by = c("Office" = "Office", "wave" = "first_wave")) %>% 
  mutate(rep = ifelse(response == "Rep", final_weight, 0)) %>% 
  mutate(dem = ifelse(response == "Dem", final_weight, 0)) %>% 
  mutate(youth = ifelse(age_combined == "18 to 29", final_weight, 0)) %>% 
  mutate(black = ifelse(race_eth == "Black", final_weight, 0)) %>% 
  mutate(hispanic = ifelse(race_eth == "Hispanic", final_weight, 0)) %>% 
  mutate(female = ifelse(gender == "Female", final_weight, 0)) %>% 
  mutate(collegegrad = ifelse(educ %in% c("Graduate or Professional Degreee", "Bachelors' degree"), final_weight, 0)) %>% 
  group_by(Office) %>% 
    summarize(interviews = n(),  
              rep_adv = (sum(rep) - sum(dem)) / n(), 
              per_youth = (sum(youth)/n()) *100, 
              per_black = 100 * sum(black)/n(), 
              per_hispanic = 100 * sum(hispanic)/n(), 
              per_female = 100 * sum(female)/n(), 
              per_collegegrad = 100 * sum(collegegrad)/n())

#get actual election results from Mr. Schroeder's Data

results <- read.csv("mt_2_results.csv")

#Note: this code is from the Midterm 2 solutions. 
#First converts district from a factor to an integer and creates a new variable called district_number. There are a few districts that are listed 
#as "AL", "sen", or "gov" in the results file. For these observations, we want to set the value of district_number equal to NA.

#Creates a new variable, district_office using case_when. If district is equal to "sen", the value of district_office
# is "SEN". If district is equal to gov, then the value of district_office is "GOV". All other values of district are equal to NA in the 
# new district_office variable. 

#Next, I create an "Office" variable. This alights with the "Office" variable in the x dataframe. 
# I want it to be formatted just like that variable, ST-District. I do so using if.else. 
# If district_number is na, meaning that district is equal to "AL", "sen" or "gov", it pastes the state abbreviation and the district office ("SEN, "GOV", or "NA"). 
#If the district number is not NA, it pastes the state abbreviation and district number together. 
results <- results %>% 
  mutate(district_number = parse_integer(district, na = c("AL", "sen", "gov"))) %>% 
  mutate(district_office = case_when(str_detect(district, "sen") ~ "SEN",
                                     str_detect(district, "gov") ~ "GOV",
                                     TRUE ~ NA_character_)) %>% 
  mutate(Office = ifelse(is.na(district_number), 
                         paste(state, district_office, sep = "-"),
                         paste(state, district_number, sep = "-")))

# Here I create the shiny_data to use in my shiny_app. Note: a portion of this code comes from the midterm 2 solutions. 
# I use a left join so that I keep all the polls in forecasts and add the corresponding results from the results dataframe. 
# I join by "Office" (ST-##). I then convert rep_votes,dem_votes, and other_votes from a factor to a number. 
# I create a variable called results, which is the actual republican advantage. 
# I rename the rep_adv from forecasts to "forecast". 
# I then select only the columns that I need. 

# Finally, I use gather to make it easier to plot the different demographics vs. the forecasted/actual republican advantage in my shiny app. 

shiny_data <- left_join(forecasts, results, by = "Office") %>% 
  mutate(rep_votes = as.numeric(rep_votes), 
         dem_votes = as.numeric(dem_votes),
         other_votes = as.numeric(other_votes)) %>% 
  mutate(result = (rep_votes - dem_votes) / (rep_votes + dem_votes + other_votes)) %>% 
  rename(forecast = rep_adv) %>%
  select(Office, state, forecast, result, per_youth, per_black, per_hispanic, per_female, per_collegegrad, win_name, win_party) %>%
  rename("Percent Young Respondents" = per_youth, 
         "Percent Black Respondents" = per_black, 
         "Percent Hispanic Respondents" = per_hispanic, 
         "Percent Female Respondents" = per_female,
         "Percent College Graduate Respondents" = per_collegegrad
         ) %>% 
  gather("demographic", "value", 5:9)

# I use write_rds to create a R object with my shiny data in my shiny app subdirectory. 
write_rds(shiny_data, "ps_7/shiny_data.rds")



  







