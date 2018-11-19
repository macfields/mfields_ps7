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
