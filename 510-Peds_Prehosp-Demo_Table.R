####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script produces a demographic table for the study population.
#
####################################################################################

library(tidyverse)
library(table1)
library(htmltools)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis.csv")

table(peds_MVCs$division)

######################################################################
#
# Variables prep ----------------------------------------------------
#

peds_MVCs <- peds_MVCs %>% 
  mutate(ams =
           case_when(
             lowest_gcs>=3 & lowest_gcs<15 ~ "Yes",
             lowest_gcs==15 ~ "No",             
             TRUE ~ "Unknown"
           )
  )

######################################################################
#
# Reorder categories -----------------------------------------------
#

peds_MVCs$age_cat <- factor(peds_MVCs$age_cat, levels = c("<1 year",
                                                           "1 year",
                                                           "2 to 5 years",
                                                           "6 to 10 years",
                                                           "11 to 14 years",
                                                           "15 to 17 years"))

peds_MVCs$race <-factor(peds_MVCs$race, levels = c("American Indian or Alaska Native",
                                                    "Asian",
                                                    "Black or African American",
                                                    "Hispanic or Latino",
                                                    "Native Hawaiian or Other Pacific Islander",
                                                    "White",
                                                    "Multiple races",
                                                    "Unknown"))

peds_MVCs$ams <-factor(peds_MVCs$ams, levels = c("Yes",
                                                  "No",
                                                  "Unknown"))

peds_MVCs$level_of_care <-factor(peds_MVCs$level_of_care, levels = c("BLS",
                                                                     "ALS",
                                                                     "Critical care",
                                                                     "Unknown"))

######################################################################
#
# Variables list ----------------------------------------------------
#

# make list of labels

variables=list(sex="Sex",
               age_cat="Age",
               race="Race/Ethnicity",
               ams="Altered mental status (lowest GCS<15)",
               level_of_care="Level of care",
               time_of_day="Time of day",               
               division="NEMSIS Region",
               eDisposition_12_desc="Disposition")

table(peds_MVCs$eDisposition_12_desc, peds_MVCs$trans)
table(peds_MVCs$ams, peds_MVCs$lowest_gcs)

######################################################################
#
# Formation varaible output ------------------------------------------
#

rndr <- function(x, name, ...) {
  
  # Keep categorical variables as percentages
  
  if (!is.numeric(x)) return(render.categorical.default(x))
  
  # Determine output for continuous variables
  
  frmt <- switch(name,
                 #front_row = "Mean",
                 render.continuous.default(x))
  
  parse.abbrev.render.code(c("", frmt))(x)
}


######################################################################
#
# Columns format -----------------------------------------------------
#

# Column designations

strata <- c(list(Overall=peds_MVCs), 
            list("Transported"=subset(peds_MVCs, trans==1)), 
            list("Not transported"=subset(peds_MVCs, trans==0)))

# create groups of columns

groups=list("", "", "")

grpspan = c(1, 1, 1)


######################################################################
#
# Create table --------------------------------------------------------
#

# Combine variables and groups into labels

labels <- list(variables=variables, groups=groups)

# create table

t <- table1(strata, labels, groupspan=grpspan, render=rndr)
t

# output to a file

cat(t, file="./Figures/Peds_MVC-table_1.html")

######################################################################
#
# Make demo graphic calculations----------------------------------------
#

# mean age and standard deviation

mean(peds_MVCs$age)

sd(peds_MVCs$age)

# percent female

peds_MVCs %>% 
  filter(sex!="Unknown") %>% 
  summarise(mean(sex=="Female"))
