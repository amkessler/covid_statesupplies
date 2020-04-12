library(tidyverse)
library(janitor)
library(lubridate)
library(tidycensus)
library(readxl)
library(writexl)

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
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))

#how many programs?
taggs_latest %>% 
  group_by(opdiv, cfda_program_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many award titles?
taggs_latest %>% 
  group_by(opdiv, award_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many recipients?
taggs_latest %>% 
  group_by(recipient_name) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many approp codes?
taggs_latest %>% 
  group_by(approp_code) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

#how many states? (there are territories in here too it looks like)
taggs_latest %>% 
  group_by(state) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 



### FILTERING #### 

#first we'll limit to just US states and DC, no territories
head(fips_codes) #built into tidycensus
us <- unique(fips_codes$state)[1:51]

taggs_filtered <- taggs_latest %>% 
  filter(state %in% us)

taggs_filtered %>% 
  count(state)

# #per guidance from BB health team expert, we'll filter out all NIH grants
# taggs_filtered <- taggs_filtered %>% 
#   filter(opdiv != "NIH")
# 
# #confirm it's gone
# taggs_filtered %>% 
#   count(opdiv)

# **may be additional categories of grants that also need to be removed... tbd

#let's take a look at the particulars of certainly categories of grants

#NIH
taggs_filtered %>% 
  filter(opdiv == "NIH") %>% 
  group_by(cfda_program_title, award_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars))


taggs_filtered %>% 
  filter(cfda_program_title == "Allergy and Infectious Diseases Research") %>% 
  group_by(cfda_program_title, award_title) %>% 
  summarise(n = n(), total_dollars = sum(award_amount)) %>% 
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
write_xlsx(all_nonHRSA_sums, "output/all_nonHRSA_sums.xlsx")


# **also does DC itself need to be removed? Are there grants tagged as DC that have nothing to do with funding *for* DC residents?
# Let's see what's up with DC
taggs_filtered %>% 
  filter(state == "DC") %>% 
  View()

#there appear to be a couple of national Native American-centered orgs in DC
taggs_filtered %>% 
  filter(state == "DC",
    str_detect(recipient_name, "INDIAN"))

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


#What about anything with "national" in the name anywhere...
taggs_filtered %>%
  filter(str_detect(str_to_upper(recipient_name), "NATIONAL")) %>% 
  select(recipient_name, city)




### AGGREGATING BY STATE ####

#now let's aggregate spending by state to work with
taggs_bystate <- taggs_filtered %>% 
  group_by(state) %>% 
  summarise(num_records = n(), total_dollars = sum(award_amount)) %>% 
  arrange(desc(total_dollars)) 

taggs_bystate

# unlike the PPE which was tied to 2010 populations b/c of its role in decisionmaking,
# this analysis on HHS grants shouldn't be restricted to that?
# so let's pull more recent state populations from **2018 census** ACS 1 year estimates
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
#we'll use the file saved in step 00 of coronavirus state-level case counts from terminal
term_cases <- readRDS("data/term_cases.rds")

#join it to the main table
joined_taggs_bystate <- inner_join(joined_taggs_bystate, term_cases, by = c("state_name" = "name"))

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
    #by population per capita
    dollars_percapita = total_dollars / censuspop2018,
    dollars_percapita_rank = min_rank(desc(dollars_percapita)),
    dollars_percase = total_dollars / casecount,
    dollars_percase_rank = min_rank(desc(dollars_percase)),
    dollars_per_casesper100kppl = total_dollars / cases_per_100k,
    dollars_per_casesper100kppl_rank = min_rank(desc(dollars_per_casesper100kppl))
  ) 

#add a ranking too for the cases/per100k itself 
joined_taggs_bystate <- joined_taggs_bystate %>% 
  mutate(
    cases_per_100k_rank = min_rank(desc(cases_per_100k))
  )

names(joined_taggs_bystate)


#save results for sharing
write_xlsx(joined_taggs_bystate, "output/joined_taggs_bystate.xlsx")


