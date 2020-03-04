####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script selects the columns for analaysis and renames them.
#
####################################################################################

library(tidyverse)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables
peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-Combined.csv")


################################################################################
# Select columns and rename ----------------------------------------------
#

# copy columns for analysis
peds_MVCs <- peds_MVCs %>% 
  select(PcrKey,
         ePatient_14, # race
         ageinyear, # age
         ePatient_13, # sex
         ePayment_01, # insurance
         USCensusRegion, # region  
         USCensusDivision,
         eTimes_07, # time of arrival to patient
         eResponse_15, # level of care
         eDisposition_19, # acuity
         lowestGCS, # GCS
         eResponse_05, # type of response
         eDisposition_12 # disposition
  ) 

# rename columns
peds_MVCs <- peds_MVCs %>% rename(
  patientID = PcrKey,
  race = ePatient_14,
  age = ageinyear,
  sex = ePatient_13, 
  insurance = ePayment_01, 
  region = USCensusRegion,   
  division = USCensusDivision,
  arrival_time = eTimes_07, 
  level_of_care = eResponse_15, 
  acuity = eDisposition_19, 
  lowest_gcs = lowestGCS, 
  response_type = eResponse_05 
)

################################################################################
# Output data ----------------------------------------------
#

write_csv(peds_MVCs, "./Data/Peds_MVCs/Peds_MVCs-clean.csv")


