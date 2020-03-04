####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script reads in data from the NEMSIS 2017 data base then
#   filters by age (<18 years) and injury caused by a MVC (based on predefined 
#   injury codes).  This is considered the population of interest for this study,
#   so the patient IDs (PCRKey) are used to filter out the population of interest
#   for several tables containing data for analysis and the results are stored to
#   file.  This data set is large so we want to trim it to the elements necessary 
#   for analysis.
#
####################################################################################

library(tidyverse)

# clear memory
rm(list = ls())

###################################################################################
# Find patients meeting inclusion criteria for 2017 -----------------------------
# 1) age < 18 years
# 2) injury code involving MVC
#

# read in table with computed patient ages in years
computed <- read_csv("./Data/NEMSIS_2017-CSV_clean/computedelements.csv")

# examine data
computed %>% head(10)

# find patient ID for pediatric patients (age <18 years) 
peds_IDs <- computed %>% filter(ageinyear < 18) %>% select(PcrKey, ageinyear)

# read in injury codes assigned to patients
injuries <- read_csv("./Data/NEMSIS_2017-CSV_clean/factpcrcauseofinjury.csv")

# filter for pediatric patients only
peds_injuries <- inner_join(peds_IDs, injuries, by = "PcrKey")

# read list of injury codes that pertant to MVC occupants
ICD_occ_MVCs <- read_csv("./Data/Lookup_tables/IC10-MVC_occupant_codes-combined.csv")

# filter patient IDs for only those with injury codes related to MVCs
peds_MVCs_occ <- inner_join(peds_injuries, ICD_occ_MVCs, by="eInjury_01")

# store list patient IDs of pediatric patient involved in MVCs
peds_IDs <- peds_MVCs_occ %>% select(PcrKey)

# include only unique IDs (some patients had more than one MVC injury code listed)
peds_IDs <- peds_IDs %>% filter(duplicated(peds_IDs$PcrKey)==FALSE)

# write list of IDs to a file
write_csv(peds_IDs, "./Data/Peds_MVCs/NEMSIS2017-Peds_MVC_IDS.csv")

# clear memory
rm(list = ls())

###################################################################################
# Find patients meeting inclusion criteria for 2018 -----------------------------
# 1) age < 18 years
# 2) injury code involving MVC
#

# read in table with computed patient ages in years
computed <- read_csv("./Data/NEMSIS_2018-CSV_clean/computedelements.csv")

# examine data
computed %>% head(10)

# find patient ID for pediatric patients (age <18 years) 
peds_IDs <- computed %>% filter(ageinyear < 18) %>% select(PcrKey, ageinyear)

# read in injury codes assigned to patients
injuries <- read_csv("./Data/NEMSIS_2018-CSV_clean/factpcrcauseofinjury.csv")

# filter for pediatric patients only
peds_injuries <- inner_join(peds_IDs, injuries, by = "PcrKey")

# read list of injury codes that pertant to MVC occupants
ICD_occ_MVCs <- read_csv("./Data/Lookup_tables/IC10-MVC_occupant_codes-combined.csv")

# filter patient IDs for only those with injury codes related to MVCs
peds_MVCs_occ <- inner_join(peds_injuries, ICD_occ_MVCs, by="eInjury_01")

# store list patient IDs of pediatric patient involved in MVCs
peds_IDs <- peds_MVCs_occ %>% select(PcrKey)

# include only unique IDs (some patients had more than one MVC injury code listed)
peds_IDs <- peds_IDs %>% filter(duplicated(peds_IDs$PcrKey)==FALSE)

# write list of IDs to a file
write_csv(peds_IDs, "./Data/Peds_MVCs/NEMSIS2018-Peds_MVC_IDS.csv")

# clear memory
rm(list = ls())

###################################################################################
# Join IDs for 2017 and 2018  -----------------------------
#

# read in individual list
ped17 <- read_csv("./Data/Peds_MVCs/NEMSIS2017-Peds_MVC_IDS.csv")
ped18 <- read_csv("./Data/Peds_MVCs/NEMSIS2018-Peds_MVC_IDS.csv")

# join IDs (there are no duplicates)
full_ids <- ped17 %>% full_join(ped18)

# write list of IDs to a file
write_csv(full_ids, "./Data/Peds_MVCs/NEMSIS-Peds_MVC_IDS.csv")



