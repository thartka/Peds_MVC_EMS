####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script applies the exclusion criteria for the study.  
#  Exclusions: 1) Not scene response, 2) Not primary squad, 3) DOA, 4) In US
#
####################################################################################

library(tidyverse)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-updated.csv")

###################################################################################
# Apply exclusions ---------------------------------------------------------
#

# count cases
cases <- nrow(peds_MVCs)

# 1) Cases that were not scene responses
peds_MVCs <- peds_MVCs %>% filter(response_type=="Scene response")

# exlcuded cases at this step
cases - nrow(peds_MVCs)
cases <- nrow(peds_MVCs)

# 2) Not primary squad (assist)
peds_MVCs <- peds_MVCs %>% filter(dispo_MVC!="Not active")

# exlcuded cases at this step
cases - nrow(peds_MVCs)
cases <- nrow(peds_MVCs)

# 3) Died in field
peds_MVCs <- peds_MVCs %>% filter(dispo_MVC!="Death" & acuity != "Dead without Resuscitation Efforts (Black)")

# exlcuded cases at this step
cases - nrow(peds_MVCs)
cases <- nrow(peds_MVCs)

# 4) Cases outside the US (in territories)
peds_MVCs <- peds_MVCs %>% filter(division!="Territories")

# exlcuded cases at this step
cases - nrow(peds_MVCs)
cases <- nrow(peds_MVCs)

###################################################################################
# Calc transports ---------------------------------------------------------
#

# get counts
tran <- peds_MVCs %>% filter(trans==1) %>% nrow()
nontran <- peds_MVCs %>% filter(trans==0) %>% nrow()
all <- peds_MVCs %>% nrow()

# calc stats
tran/all
nontran/all

################################################################################
# Output data ----------------------------------------------
#

write_csv(peds_MVCs, "./Data/Peds_MVCs/Peds_MVCs-analysis.csv")