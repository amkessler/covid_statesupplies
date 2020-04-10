library(tidyverse)
library(janitor)
library(readxl)
library(writexl)

#import ppe data
ppe <- read_excel("data/SNS_PPE_REPORT.xlsx")

ppe <- ppe %>% 
  clean_names() %>% 
  mutate(name = str_trim(str_to_lower(name)))

names(ppe)


#import terminal case counts and pare down columns
terminal_casecounts_apr10 <- read_excel("data/terminal_casecounts_apr10.xlsx", 
                                        sheet = "cases")

terminal_casecounts_apr10 <- terminal_casecounts_apr10 %>% 
  clean_names() %>% 
  mutate(region_2 = str_trim(str_to_lower(region_2)))

names(terminal_casecounts_apr10)

term_cases <- terminal_casecounts_apr10 %>% 
  select(
    name = region_2,
    term_case_count_apr10 = latest
  )


##join 
joined <- left_join(ppe, term_cases)

#see what didn't join
joined %>% 
  filter(is.na(term_case_count_apr10))

#the local jurisdictions hand-gathered by staff need to be incorporated
#first we'll take out the cherokee nation since no cases available in the current dataset
joined <- joined %>% 
  filter(name != "cherokee nation of oklahoma")

#then we'll create a new column to merge term number for states, hand-gathered for local







