library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)
library(sf)
library(tigris)
library(tmap)
library(tmaptools)

# 30B in initial CARES ACT Hospital fund breakdowns -- released state-by-state by Republican cmte members:
# https://republicans-waysandmeansforms.house.gov/uploadedfiles/first_distribution_summary_by_state.pdf

# converted from pdf to csv, import the state-by-state tallies here:
cares30B_data <- read_csv("data/first_distribution_summary_by_state.csv")

#set column names to match existing scripts
colnames(cares30B_data) <- c("state", "num_facilities", "total_dollars")

cares30B_data

#limit to just US states and DC, no territories
head(fips_codes) #built into tidycensus
us <- unique(fips_codes$state)[1:51]

cares30B_data <- cares30B_data %>% 
  filter(state %in% us)


#let's pull more state populations from **2018 census** ACS 1 year estimates ####
census_statepops2018 <- tidycensus::get_acs(geography = "state",
                                variables = c(totalpop_2018 = "B01003_001"),
                                survey = "acs1")

#clean names, remove PR and state names to lowercase
census_statepops2018 <- census_statepops2018 %>% 
  clean_names() %>% 
  mutate(name = str_trim(str_to_lower(name))) %>% 
  filter(name != "puerto rico") %>% 
  select(geoid, 
         name, 
         censuspop2018 = estimate)

census_statepops2018

#since the HHS data has only state abbreviations, not full names, we'll need to add that
#we'll use tidycensus' built in fips table once again 
statelist <- tidycensus::fips_codes %>% 
  select(state, state_name) %>% 
  distinct() %>% 
  mutate(state_name = str_to_lower(state_name))

cares30B_data <- inner_join(statelist, cares30B_data, by = "state")


#ok now we're ready to join back to our CARES state level data
joined_cares_by_state <- inner_join(cares30B_data, census_statepops2018, by = c("state_name" = "name")) %>% 
  select(geoid, everything())


#with population done, let's now bring in the case counts 
#we'll use the file saved in step 00 of coronavirus state-level case counts from terminal ####
term_cases <- readRDS("data/term_cases.rds")

#join it to the main table
joined_cares_by_state <- inner_join(joined_cares_by_state, term_cases, by = c("state_name" = "name"))

#now we'll calculate cases per 100k based on population
joined_cares_by_state <- joined_cares_by_state %>% 
  rename(casecount = term_case_counts) %>% 
  mutate(
    cases_per_100k = round_half_up(casecount / censuspop2018 * 100000)
  ) 



### CALCULATING THE RATIOS ####

#add calculated fields and ranks
joined_cares_by_state <- joined_cares_by_state %>% 
  mutate(
    #dollars per person / per capita
    dollars_percapita = total_dollars / censuspop2018,
    dollars_percapita_rank = min_rank(desc(dollars_percapita)),
    #dollars per case
    dollars_percase = total_dollars / casecount,
    dollars_percase_rank = min_rank(desc(dollars_percase)),
  ) 

#add a ranking for population and straight case count
joined_cares_by_state <- joined_cares_by_state %>% 
  mutate(
    # cases_per_100k_rank = min_rank(desc(cases_per_100k)),  #this measure was used earlier, here removed
    casecount_rank = min_rank(desc(casecount)),
    pop_rank = min_rank(desc(censuspop2018))
  )

names(joined_cares_by_state)


#now, let's create a calculated column that shows the spread between the rankings, so we can spot the largest differences
joined_cares_by_state <- joined_cares_by_state %>% 
  mutate(
    rankspread_population_dollarsperpop = abs(pop_rank - dollars_percapita_rank),
    rankspread_casecount_dollarspercase = abs(casecount_rank - dollars_percase_rank),
  )


names(joined_cares_by_state)


#reorder columns to make it easier to read
joined_cares_by_state <- joined_cares_by_state %>%
  select(geoid,
      state_name,
      total_dollars,
      casecount,
      dollars_percase,
      censuspop2018,
      dollars_percapita,
      casecount_rank,
      dollars_percase_rank,
      rankspread_casecount_dollarspercase,
      pop_rank,
      dollars_percapita_rank,
      rankspread_population_dollarsperpop,
)


#save results for sharing
write_xlsx(joined_cares_by_state, "output/joined_CARES30B_by_state.xlsx")





### MAPPING #### --------------------------------------------------------

#pull down state boundaries - run once then can use saved file below
# states_geo <- states(class = "sf", cb = TRUE)
# saveRDS(states_geo, "data/states_geo.rds")

#load saved state boundary file
states_geo <- readRDS("data/states_geo.rds")

#lower 48 only + DC
states_geo_lower48 <- states_geo %>% 
  filter(STUSPS %in% us,
         STUSPS != "AK",
         STUSPS != "HI"
         )

#join to taggs_by_state
joined_cares_by_state_geo_lower48 <- geo_join(states_geo_lower48, joined_cares_by_state, "STATEFP", "geoid")

#mapping out the states based on dollars-per-case
map_dollarspercase_lower48 <- tm_shape(joined_cares_by_state_geo_lower48) +
    tm_polygons("dollars_percase") 

map_dollarspercase_lower48

#export to pdf
tmap_save(map_dollarspercase_lower48, "output/map_CARES30B_dollarspercase_lower48.pdf")
