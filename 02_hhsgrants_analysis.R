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

# HHS COVID-19 AWARD GRANT DATA ####
# source here: https://taggs.hhs.gov/coronavirus

#import the latest version of the HHS "TAGGS" data after downloading as csv
taggs_latest_raw <- read_csv("data/taggs_export_latest.csv", col_types = cols(.default = "c"))

#clean names and format columns
# **note: parse_number doesn't handle negative currency well, so because there's at least 
# one negative dollar amount in the data we'll strip out the dollar sign ($) first
taggs_latest <- taggs_latest_raw %>% 
  clean_names() %>% 
  mutate(
    award_amount = str_remove(award_amount, "\\$"), 
    award_amount = parse_number(award_amount),
    action_date = mdy(action_date)
  ) 

glimpse(taggs_latest)


# quick counts to see what we've got here ####

#how many agencies?
taggs_latest %>% 
  group_by(opdiv) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

#how many programs?
taggs_latest %>% 
  group_by(opdiv, cfda_program_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many award titles?
taggs_latest %>% 
  group_by(opdiv, award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many recipients?
taggs_latest %>% 
  group_by(recipient_name) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many approp codes?
taggs_latest %>% 
  group_by(approp_code) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many states? (there are territories in here too it looks like)
taggs_latest %>% 
  group_by(state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 



### FILTERING #### 

#first we'll limit to just US states and DC, no territories
head(fips_codes) #built into tidycensus
us <- unique(fips_codes$state)[1:51]

taggs_filtered <- taggs_latest %>% 
  filter(state %in% us)

taggs_filtered %>% 
  count(state)

#per guidance from BB health team expert, we'll filter out all NIH grants
taggs_filtered <- taggs_filtered %>%
  filter(opdiv != "NIH")

#confirm it's gone
taggs_filtered %>%
  count(opdiv)

# **may be additional categories of grants that also need to be removed... tbd

#let's take a look at the particulars of certainly categories of grants

#NIH
taggs_filtered %>% 
  filter(opdiv == "NIH") %>% 
  group_by(cfda_program_title, award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))


taggs_filtered %>% 
  filter(cfda_program_title == "Allergy and Infectious Diseases Research") %>% 
  group_by(cfda_program_title, award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

taggs_filtered

#let's look at breakdowns of all non-HRSA programs
all_nonHRSA_sums <- taggs_filtered %>% 
  filter(opdiv != "HRSA") %>% 
  group_by(cfda_program_title, opdiv, award_title, recipient_name) %>% 
  summarise(total_dollars = sum(award_amount)) %>% 
  arrange(cfda_program_title, desc(total_dollars)) 

all_nonHRSA_sums

#export
# write_csv(all_nonHRSA_sums, "output/all_nonHRSA_sums.csv")

#What about anything else with "national" in the name anywhere...
taggs_filtered %>%
  filter(str_detect(str_to_upper(recipient_name), "NATIONAL")) %>% 
  select(award_title, recipient_name, city)

taggs_filtered %>%
  filter(str_detect(str_to_upper(award_title), "NATIONAL")) %>% 
  select(award_title, recipient_name, city)


# **also does DC itself need to be removed? Are there grants tagged as DC that have nothing to do with funding *for* DC residents?
# Let's see what's up with DC
taggs_filtered %>% 
  filter(state == "DC") 

#there appear to be a couple of national Native American-centered orgs in DC
taggs_filtered %>% 
  filter(state == "DC",
    str_detect(recipient_name, "INDIAN")) %>% 
  select(recipient_name, award_amount)

#let's see what those groups' make up as percentage of money tied to DC
taggs_filtered %>% 
  filter(state == "DC") %>% 
  mutate(
    natlnativeamer = if_else(str_detect(recipient_name, "INDIAN"), "Y", "N")
      ) %>% 
  group_by(natlnativeamer) %>% 
  summarise(sumtotal = sum(award_amount)) %>% 
  mutate(pct = sumtotal / sum(sumtotal) * 100)
         
#So it makes up nearly 40 percent of DC's money. This probably needs to be dealt with to avoid throwing off analysis.

#Per the guidance as of 5p on Mon, we'releaving DC as a jurisdiction but taking OUT the two national native organizations 
#from DC's pot of money, so they are not skewing things. We'll explain why this was done in the piece.
taggs_filtered <- taggs_filtered %>% 
  filter(recipient_name != "NATIONAL INDIAN HEALTH BOARD, INC")

taggs_filtered <- taggs_filtered %>% 
  filter(recipient_name != "NATIONAL COUNCIL OF URBAN INDIAN HEALTH")


#we'll take out the Ebola Training Ctr in Atlanta for similar reasons
taggs_filtered <- taggs_filtered %>% 
  filter(award_title != "National Ebola Training and Education Center") 




#how much money per approp code now in the filtered data?
taggs_filtered %>% 
  group_by(approp_code) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many award titles?
taggs_filtered %>% 
  group_by(opdiv, award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

taggs_filtered %>% 
  group_by(award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))



#### ALASKA ANALYSIS #### --------------------------------------------------------------------

#let's explore what's up with Alaska's money, since as we'll see below it has a very high proportion of funding
#despite its low number of cases and small population
alaskaonly <- taggs_filtered %>% 
  filter(state == "AK")


#let's see which programs generating the most for alaska
alaskaonly %>% 
  group_by(cfda_program_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  mutate(pct = total_dollars / sum(total_dollars) * 100) %>% 
  arrange(desc(total_dollars)) 

#now by award title
alaskaonly %>% 
  group_by(award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  mutate(pct = total_dollars / sum(total_dollars) * 100) %>% 
  arrange(desc(total_dollars)) 

#by recipient
alaskaonly %>% 
  group_by(recipient_name) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  mutate(pct = total_dollars / sum(total_dollars) * 100) %>% 
  arrange(desc(total_dollars)) 


### now let's look at the overall native american-oriented funding mechanisms and how much Alaska gets 

# programs
taggs_filtered %>% 
  group_by(cfda_program_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

# there appear to be two programs exclusively aimed at native communities
# let's isolate them here to examine
nativeprogramsonly <- taggs_filtered %>% 
  filter(cfda_program_title %in% c("Tribal Public Health Capacity Building and Quality Improvement Umbrella Cooperative Agreement",
                                   "Special Programs for the Aging, Title VI, Part A, Grants to Indian Tribes, Part B, Grants to Native Hawaiians"))


# what states are getting the most from these programs combined
nativeprogramsonly %>% 
  group_by(state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  mutate(pct = total_dollars / sum(total_dollars) * 100) %>% 
  arrange(desc(total_dollars)) 
#Alaska #2, Okla. #1 - virtually tied for first. Twice as much as the next states down, CA, NM, WI, MT

#What about separately?
nativeprogramsonly %>% 
  group_by(cfda_program_title, state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))
#Looks like Alaska #1` for Special Programs for Aging Tribal Grants, #2 for Tribal Public Health Policy grants


#award titles
alaskaonly %>% 
  group_by(cfda_program_title, award_title) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))




### AGGREGATING BY STATE #### ----------------------------------------------------------------


#now let's aggregate spending by state to work with
taggs_bystate <- taggs_filtered %>% 
  group_by(state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

taggs_bystate

# unlike the PPE which was tied to 2010 populations b/c of its role in decisionmaking,
# this analysis on HHS grants shouldn't be restricted to that?
# so let's pull more recent state populations from **2018 census** ACS 1 year estimates ####
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

taggs_bystate <- inner_join(statelist, taggs_bystate, by = "state")

#ok now we're ready to join back to our HHS grant data
joined_taggs_bystate <- inner_join(taggs_bystate, census_statepops2018, by = c("state_name" = "name")) %>% 
  select(geoid, everything())



#with population done, let's now bring in the case counts 
#we'll use the file saved in step 00 of coronavirus state-level case counts from terminal ####
term_cases <- readRDS("data/term_cases.rds")

#join it to the main table
joined_taggs_bystate <- inner_join(joined_taggs_bystate, term_cases, by = "state_name")

#now we'll calculate cases per 100k based on population
joined_taggs_bystate <- joined_taggs_bystate %>% 
  rename(casecount = term_case_counts) %>% 
  mutate(
    cases_per_100k = round_half_up(casecount / censuspop2018 * 100000)
  ) 



### CALCULATING THE RATIOS ####

#add calculated fields and ranks
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    #dollars per person / per capita
    dollars_percapita = total_dollars / censuspop2018,
    dollars_percapita_rank = min_rank(desc(dollars_percapita)),
    #dollars per case
    dollars_percase = total_dollars / casecount,
    dollars_percase_rank = min_rank(desc(dollars_percase)),
  ) 

#add a ranking for population and straight case count
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    # cases_per_100k_rank = min_rank(desc(cases_per_100k)),  #this measure was used earlier, here removed
    casecount_rank = min_rank(desc(casecount)),
    pop_rank = min_rank(desc(censuspop2018))
  )

names(joined_taggs_bystate)


#now, let's create a calculated column that shows the spread between the rankings, so we can spot the largest differences
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    rankspread_population_dollarsperpop = abs(pop_rank - dollars_percapita_rank),
    rankspread_casecount_dollarspercase = abs(casecount_rank - dollars_percase_rank),
  )


names(joined_taggs_bystate)


#reorder columns to make it easier to read
joined_taggs_bystate <- joined_taggs_bystate %>%
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
write_xlsx(joined_taggs_bystate, "output/joined_taggs_bystate.xlsx")





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
joined_taggs_bystate_geo_lower48 <- geo_join(states_geo_lower48, joined_taggs_bystate, "STATEFP", "geoid")

#mapping out the states based on dollars-per-case
map_dollarspercase_lower48 <- tm_shape(joined_taggs_bystate_geo_lower48) +
    tm_polygons("dollars_percase") 

map_dollarspercase_lower48

#export to pdf
tmap_save(map_dollarspercase_lower48, "output/map_dollarspercase_lower48.pdf")
