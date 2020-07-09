####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script performs the logistic regression for the outcome of 
#   whether or not a pediatric patient was transported to the hospital after an MVC.
#
####################################################################################

library(tidyverse)
library(gdata)
library(faraway)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis_imputed.csv")

###################################################################################
# Filter out imputed data if necessary -----------------------------------------
#

filter_imputed <- FALSE
#filter_imputed <- TRUE

# filter out imputated
if(filter_imputed){peds_MVCs <- peds_MVCs %>% filter(imputated==FALSE)}

###################################################################################
# Create factors -----------------------------------------------------
#

# convert all strings to factors
#peds_MVCs <- peds_MVCs %>% mutate_if(is.character, as.factor)

# convert outcome to 0 (Transported) and 1 (Nontransported)
peds_MVCs <- peds_MVCs %>% mutate(dispo_MVC = ifelse(dispo_MVC=="Non-transported",1,0))

peds_MVCs$age_cat <- factor(peds_MVCs$age_cat, levels = c("<1 year",
                                                          "1 year",
                                                          "2 to 5 years",
                                                          "6 to 10 years",
                                                          "11 to 14 years",
                                                          "15 to 17 years"))

peds_MVCs$ams <- factor(peds_MVCs$ams)
levels(peds_MVCs$ams) <- c("No", "Yes")
peds_MVCs$ams <- relevel(peds_MVCs$ams,"Yes")

peds_MVCs$level_of_care <-factor(peds_MVCs$level_of_care, levels = c("BLS",
                                                                     "ALS",
                                                                     "Critical care",
                                                                     "Unknown"))

###################################################################################
# Perform regression -----------------------------------------------------
#

peds_glm <- glm(dispo_MVC ~ sex + age_cat + ams + level_of_care + time_of_day + division, 
                data = peds_MVCs, family = "binomial")

summary(peds_glm)

table(peds_MVCs$eDisposition_12_desc, peds_MVCs$dispo_MVC)

peds_MVCs %>% group_by(dispo_MVC) %>% summarise(n=n(), prop=n()/nrow(.))
peds_MVCs %>% filter(dispo_MVC==1) %>% group_by(eDisposition_12_desc) %>% summarise(n=n(), prop=n()/nrow(.))
peds_MVCs %>% group_by(eDisposition_12_desc) %>% summarise(n=n(), prop=n()/nrow(.))

table(peds_MVCs$ams, peds_MVCs$dispo_MVC)

###################################################################################
# Check diagnostics  -----------------------------------------------------
#

vif(peds_glm)

###################################################################################
# Create Regression Analysis Table -------------------------------------------
#

# source function for analysis
source("./Code/710-Regression_table_functions.R")

# variables for regression
vars <- c("age_cat","sex","ams","time_of_day","level_of_care","division")

# get output of univariate analysis in table format
uni_output <- univar_table(peds_MVCs, vars, "dispo_MVC")

# get output of multivariate analysis in table format
multi_output <- multivar_table(peds_MVCs, vars, "dispo_MVC")

# output tables
if(filter_imputed==TRUE){
  write_csv(uni_output, "./Data/Results/Peds_Prehosp-Univar_analysis.csv")
  write_csv(multi_output, "./Data/Results/Peds_Prehosp-Multivar_analysis.csv")
} else {
  write_csv(uni_output, "./Data/Results/Peds_Prehosp-Univar_analysis-imp.csv")
  write_csv(multi_output, "./Data/Results/Peds_Prehosp-Multivar_analysis-imp.csv")
}

