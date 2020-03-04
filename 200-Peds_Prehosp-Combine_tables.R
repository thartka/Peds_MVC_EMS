####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script reads combines data from various tables for the study, 
#   then stores combined data for subquent analysis.
#
####################################################################################

library(tidyverse)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables
pub <- read_csv("./Data/Peds_MVCs/Peds_MVC-pub_pcrevents.csv")
computed <- read_csv("./Data/Peds_MVCs/Peds_MVC-computedelements.csv")
race <- read_csv("./Data/Peds_MVCs/Peds_MVC-pcrpatientracegroup.csv")
vitals <- read_csv("./Data/Peds_MVCs/Peds_MVC-factpcrvital.csv")

################################################################################
# Preprocess data --------------------------------------------------------
#

# make duplicate race category "b'2514010'" means multiple races
dup_race <- race %>% select(PcrKey) %>% filter(duplicated(race$PcrKey))
race <- race %>% mutate(ePatient_14 = ifelse(PcrKey %in% dup_race$PcrKey, "2514010", ePatient_14))

# now remove race duplicates
race <- race %>% filter(duplicated(race$PcrKey)==FALSE)

# make gcs variable from 
gcs <- vitals %>% 
  mutate(eVitals_19=ifelse(.$eVitals_19 %in% c(7701001,7701003), NA, .$eVitals_19)) %>% 
  mutate(eVitals_20=ifelse(.$eVitals_20 %in% c(7701001,7701003), NA, .$eVitals_20)) %>% 
  mutate(eVitals_21=ifelse(.$eVitals_21 %in% c(7701001,7701003), NA, .$eVitals_21)) %>% 
  mutate(vitalsGCS = eVitals_19 + eVitals_20 + eVitals_21) %>% 
  select(PcrKey, vitalsGCS) %>% 
  filter(!is.na(vitalsGCS))

# get lowest gcs
gcs <- gcs %>% 
  group_by(PcrKey) %>% 
  summarise(lowestGCS = min(vitalsGCS)) 

###################################################################################
# Combine data ---------------------------------------------------------
#

# combine tables
peds_MVCs <- inner_join(pub, computed, by="PcrKey")
peds_MVCs <- peds_MVCs %>% left_join(race, by="PcrKey")
peds_MVCs <- peds_MVCs %>% left_join(gcs, by="PcrKey")

###################################################################################
# Store data ---------------------------------------------------------
#

write_csv(peds_MVCs, "./Data/Peds_MVCs/Peds_MVCs-Combined.csv")
