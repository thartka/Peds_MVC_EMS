####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script creates a plot of the distribution of reasons for non-transport
#  based on geographic division.  
#
####################################################################################

library(tidyverse)
library(ggthemes)

# clear memory
rm(list = ls())


###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis_imputed.csv")

# filter non-transports

peds_MVCs <- peds_MVCs %>% filter(dispo_MVC=="Non-transported")

###################################################################################
# Plot refusal reasons --------------------------------------------------------
#

dispo <- peds_MVCs %>% 
  group_by(division, eDisposition_12_desc) %>% 
  summarise(Freq=n()) %>%
  ungroup() %>% 
  group_by(division) %>% 
  mutate(Percent = 100*Freq/sum(Freq)) 
  


dispo$eDisposition_12_desc <- factor(dispo$eDisposition_12_desc,
                                         levels =c("Patient Treated, Released (AMA)",
                                            "Patient Refused Evaluation/Care (Without Transport)",
                                            "Patient Evaluated, No Treatment/Transport Required",
                                            "Patient Treated, Released (per protocol)",
                                            "Patient Treated, Transported by this EMS Unit",
                                            "Patient Refused Evaluation/Care (With Transport)",
                                            "Patient Treated, Transferred Care to Another EMS Unit",
                                            "Patient Treated, Transported by Law Enforcement",
                                            "Patient Treated, Transported by Private Vehicle"))

levels(dispo$eDisposition_12_desc)

ggplot(dispo, aes(x=division)) + 
  geom_bar(stat="identity",aes(y=Percent,fill=eDisposition_12_desc)) + 
  coord_flip() + 
  scale_fill_grey() +
  labs(fill = "Reason for Non-transport", y="Percent of Non-transports", x="Geographic Division") + 
  guides(fill = guide_legend(reverse=TRUE))


###################################################################################
# Plot nontransports by region --------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis_imputed.csv")

trans <- peds_MVCs %>% 
  group_by(division) %>% 
  summarise(freq=sum(trans==0)/n()) 

ggplot(trans, aes(x=division)) + 
  geom_bar(stat="identity",aes(y=freq)) + 
  coord_flip() + 
  scale_fill_grey() +
  labs(fill = "Percent Non-transport", y="Percent of Non-transports", x="Geographic Division")

###################################################################################
# Other analysis --------------------------------------------------------
#

# refusals data

dispo2 <- peds_MVCs %>% 
  group_by(eDisposition_12_desc, division) %>% 
  summarise(Freq=n()) %>%
  ungroup() %>% 
  group_by(division) %>% 
  mutate(Percent = 100*Freq/sum(Freq)) 

# transport data

dispo3 <- peds_MVCs %>% 
  group_by(trans, division) %>% 
  summarise(Freq=n()) %>%
  ungroup() %>% 
  group_by(division) %>% 
  mutate(Percent = 100*Freq/sum(Freq)) 
