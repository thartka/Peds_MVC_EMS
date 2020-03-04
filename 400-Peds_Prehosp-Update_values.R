####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script reads replaceds "Not applicable"/"Not recorded" and 
#   adds written descriptions other cateegorical values.  An hour variable is 
#   also extracted from the date and a time of day category is created.
#
####################################################################################

library(tidyverse)
library(lubridate)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables
peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-clean.csv")

################################################################################
# Convert Not applicable and Not recorded values -------------------------------
#

peds_MVCs[peds_MVCs=="7701001"] <- "Not applicable"
peds_MVCs[peds_MVCs=="7701003"] <- "Not recorded"

################################################################################
# Set disposition flag   -------------------------------
#

# load look up table
eDisposition_12_lookup <- read_csv("./Data/Lookup_tables/eDisposition_12.csv")

# add descriptions to data
peds_MVCs <- inner_join(peds_MVCs, eDisposition_12_lookup)

# disposition codes
codes_death <- c(4212013, 4212015, 4212017, 4212019)
codes_nontrans <- c(4212021, 4212025, 4212027, 4212029)
codes_trans <- c(4212031, 4212023, 4212033, 4212035, 4212037)
codes_no_call <- c(4212001, 4212003, 4212005, 4212007, 4212009, 4212011, 4212039, 4212041, 4212043)

# flag codes
peds_MVCs <- peds_MVCs %>% mutate(death = if_else(eDisposition_12 %in% codes_death, 1, 0))
peds_MVCs <- peds_MVCs %>% mutate(nontrans = if_else(eDisposition_12 %in% codes_nontrans, 1, 0))
peds_MVCs <- peds_MVCs %>% mutate(trans = if_else(eDisposition_12 %in% codes_trans, 1, 0))
peds_MVCs <- peds_MVCs %>% mutate(no_call = if_else(eDisposition_12 %in% codes_no_call, 1, 0))

# combine in one variable
peds_MVCs <- peds_MVCs %>% 
  mutate(dispo_MVC =
           case_when(
             death == 1 ~ "Death",
             trans == 1 ~ "Transported",
             nontrans == 1 ~ "Non-transported",
             no_call == 1 ~ "Not active"
           )
  )

################################################################################
# Convert categorical values   -------------------------------
#

# convert race
peds_MVCs <- peds_MVCs %>% 
  mutate(race =
           case_when(
             race == 2514001 ~ "American Indian or Alaska Native",
             race == 2514003 ~ "Asian",
             race == 2514005 ~ "Black or African American",
             race == 2514007 ~ "Hispanic or Latino",
             race == 2514009 ~ "Native Hawaiian or Other Pacific Islander",
             race == 2514011 ~ "White",
             race == 2514010 ~ "Multiple races",
             TRUE ~ "Unknown"
           )
  )

table(peds_MVCs$race)

# convert sex
peds_MVCs <- peds_MVCs %>% 
  mutate(sex =
           case_when(
             sex == 9906001 ~ "Female",
             sex == 9906003 ~ "Male",
             TRUE ~ "Unknown"
           )
  )

table(peds_MVCs$lowest_gcs)

# convert gcs
peds_MVCs <- peds_MVCs %>% 
  mutate(lowest_gcs =
           case_when(
             lowest_gcs>=3 & lowest_gcs<=15 ~ lowest_gcs,
             TRUE ~ -9
           )
  )

# convert insurance
peds_MVCs <- peds_MVCs %>% 
  mutate(insurance =
           case_when(
             insurance == 2601001 ~ "Insurance",
             insurance == 2601003 ~ "Medicaid",
             insurance == 2601005 ~ "Medicare",
             insurance == 2601007 ~ "Not Billed (for any reason)",
             insurance == 2601009 ~ "Other Government",
             insurance == 2601011 ~ "Self Pay",
             insurance == 2601013 ~ "Workers Compensation",
             insurance == 2601015 ~ "Payment by Facility",
             insurance == 2601017 ~ "Contracted Payment",
             insurance == 2601019 ~ "Community Network",
             insurance == 2601021 ~ "No Insurance Identified",
             insurance == 2601023 ~ "Other Payment Option",
             TRUE ~ "Unknown"
           )
  )

table(peds_MVCs$insurance)

# get hour
peds_MVCs <- peds_MVCs %>% 
  mutate(arrival_time = ymd_hms(arrival_time)) %>%
  mutate(hour = hour(arrival_time)) %>% 
  select(-arrival_time)

# make time of day variable
peds_MVCs <- peds_MVCs %>% 
  mutate(time_of_day =
           case_when(
             hour<6 ~ "00:00-05:59",
             hour>=6 & hour<12 ~ "06:00-11:59",
             hour>=12 & hour<18 ~ "12:00-17:59",
             hour>=18 ~ "18:00-23:59",
             TRUE ~ "Unknown"
           )
  )      

# convert level of care
peds_MVCs <- peds_MVCs %>% 
  mutate(level_of_care =
           case_when(
             level_of_care %in% c(2215001,2215003,2215005,2215023) ~ "BLS",
             level_of_care %in% c(2215009,2215011,2215013,2215015,2215017,2215019) ~ "ALS",
             level_of_care %in% c(2215021) ~ "Critical care",
             TRUE ~ "Unknown"
           )
  )  

table(peds_MVCs$acuity)

# convert acuity
peds_MVCs <- peds_MVCs %>% 
  mutate(acuity =
           case_when(
             acuity == 4219001 ~ "Critical (Red)",
             acuity == 4219003 ~ "Emergent (Yellow)",
             acuity == 4219005 ~ "Lower Acuity (Green)",
             acuity == 4219007 ~ "Dead without Resuscitation Efforts (Black)",
             TRUE ~ "Unknown"
           )
  )  


# convert response type
peds_MVCs <- peds_MVCs %>% 
  mutate(response_type =
            case_when(
              response_type == 2205001 ~ "Scene response",
              response_type %in% c(2205003,2205005,2205007,2205009,2205011,2205013) ~ "Other response",
              TRUE ~ "Unknown"
            )
   )  

table(peds_MVCs$age)

# make age categories
peds_MVCs <- peds_MVCs %>% 
  mutate(age_cat =
           case_when(
             age <1 ~ "<1 year",
             age <2 ~ "1 year",
             age <6 ~ "2 to 5 years",
             age <11 ~ "6 to 10 years",
             age <15 ~ "11 to 14 years",
             TRUE ~ "15 to 17 years"
           )
  )  

table(peds_MVCs$age_cat, peds_MVCs$age)

# make AMS flag
peds_MVCs <- peds_MVCs %>% 
  mutate(ams = ifelse(lowest_gcs<15,1,0))


################################################################################
# Output data ----------------------------------------------
#

write_csv(peds_MVCs, "./Data/Peds_MVCs/Peds_MVCs-updated.csv")