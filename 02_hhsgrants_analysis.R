library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)

#load data from step 00 to use below for pulling certain columns from merged ppe for use here too
joined_ppe <- readRDS("data/joined_ppe.rds")


# HHS COVID-19 AWARD GRANT DATA ####
# source here: https://taggs.hhs.gov/coronavirus

#import the latest version of the HHS "TAGGS" data after downloading as csv
taggs_latest_raw <- read_csv("data/taggs_export_latest.csv", col_types = cols(.default = "c"))

#clean names and format columns
# **note: parse_number doesn't handle negative currency well, so because there's at least 
# one negative dollar amount in the data we'll strip out the dollar sign ($) first
hhsgrants_latest <- taggs_latest_raw %>% 
  clean_names() %>% 
  mutate(
    award_amount = str_remove(award_amount, "\\$"), 
    award_amount = parse_number(award_amount),
    action_date = mdy(action_date)
  ) 

names(taggs_latest)

#let's use the joined table for ppe above to pull out the already good-to-go state case counts and pops
tempdf <- joined_ppe %>% 
  select(
    name, state_or_local, casecount, censuspop2010, cases_per_100k
  )

#since the HHS data has only state abbreviations, not full names, we'll need to add that
#we'll use tidycensus' built in tables to pull together
statelist <- tidycensus::fips_codes %>% 
  select(state, state_name) %>% 
  distinct() %>% 
  mutate(state_name = str_to_lower(state_name))

tempdf <- left_join(tempdf, statelist, by = c("name" = "state_name")) 

tempdf

#ok now we're ready to join the temporary table back to our HHS grant data




