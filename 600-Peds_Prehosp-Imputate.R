####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script examines missingness.  We remove values with >20% missing
#  and impute the others
#
####################################################################################

library(tidyverse)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis.csv")

###################################################################################
# Examine missingness ---------------------------------------------------------
#

# race, missing 60%
peds_MVCs %>% 
  group_by(race) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# sex, missing <1%
peds_MVCs %>% 
  group_by(sex) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# insurance, missing 60%
peds_MVCs %>% 
  group_by(insurance) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# region, missing 0%
peds_MVCs %>% 
  group_by(region) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# division, missing 0%
peds_MVCs %>% 
  group_by(division) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# level of care, missing <1%
peds_MVCs %>% 
  group_by(level_of_care) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# acuity, missing 53%
peds_MVCs %>% 
  group_by(acuity) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# lowest_GCS, missing 11.4%
peds_MVCs %>% 
  group_by(lowest_gcs) %>% 
  summarise(n=n(), prop=n()/nrow(.))

# time of day, missing 3.5%
peds_MVCs %>% 
  group_by(time_of_day) %>% 
  summarise(n=n(), prop=n()/nrow(.))

###################################################################################
# Remove frequent missing variables ------------------------------------------
#   Variables

# remove race
peds_MVCs <- peds_MVCs %>% select(-race)

# remove insurance
peds_MVCs <- peds_MVCs %>% select(-insurance)

# remove acuity
peds_MVCs <- peds_MVCs %>% select(-acuity)

###################################################################################
# Imputed occasional missing variables -----------------------------------------
#

table(peds_MVCs$level_of_care)

# add column for imputation
peds_MVCs <- peds_MVCs %>% mutate(imputated = ifelse(sex=="Unknown" | level_of_care=="Unknown" | lowest_gcs==-9 | time_of_day=="Unknown", 1, 0))

# sex, majority class Female
peds_MVCs <- peds_MVCs %>% mutate(sex = ifelse(sex=="Unknown", "Female", sex))

# level of care, majority class ALS
peds_MVCs <- peds_MVCs %>% mutate(level_of_care = ifelse(level_of_care=="Unknown", "ALS", level_of_care))

# gcs, majority class 15 and set flag if imputated
peds_MVCs <- peds_MVCs %>% mutate(impute_gcs = ifelse(lowest_gcs==-9, 1, 0),
                                  lowest_gcs = ifelse(lowest_gcs==-9, 15, lowest_gcs))

# update ams flag
peds_MVCs <- peds_MVCs %>% 
  mutate(ams = ifelse(lowest_gcs<15,1,0))

# recalculate ams
peds_MVCs <- peds_MVCs %>% mutate(ams = ifelse(lowest_gcs==15, 0, 1))

# time of day, majority class 12:00-17:59
peds_MVCs <- peds_MVCs %>% mutate(time_of_day = ifelse(time_of_day=="Unknown", "12:00-17:59", time_of_day))

################################################################################
# Output data ----------------------------------------------
#

write_csv(peds_MVCs, "./Data/Peds_MVCs/Peds_MVCs-analysis_imputed.csv")
