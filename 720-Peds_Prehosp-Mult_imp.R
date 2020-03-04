####################################################################################
# Project: Pediatic prehospital transport decisions
# Author: Thomas Hartka, MD
# Date: 12/13/19
# 
# Description: This script performs the logistic regression using multple imputation
#  as a sensitivity analysis
#
####################################################################################

library(tidyverse)
library(mice)

# clear memory
rm(list = ls())

###################################################################################
# Read in data ---------------------------------------------------------
#

# load data tables

peds_MVCs <- read_csv("./Data/Peds_MVCs/Peds_MVCs-analysis.csv")

peds_MVCs <- peds_MVCs %>% select(dispo_MVC, sex, age_cat, ams, lowest_gcs, level_of_care, time_of_day, division,)


###################################################################################
# Convert unknowns to NAs -----------------------------------------------------
#

unique(peds_MVCs$sex)
unique(peds_MVCs$level_of_care)
unique(peds_MVCs$time_of_day)
unique(peds_MVCs$ams)
unique(peds_MVCs$lowest_gcs)

table(peds_MVCs$sex)
table(peds_MVCs$level_of_care)
table(peds_MVCs$time_of_day)
table(peds_MVCs$ams)
table(peds_MVCs$lowest_gcs)

peds_MVCs$sex <- ifelse(peds_MVCs$sex == "Unknown", NA, peds_MVCs$sex)
peds_MVCs$level_of_care <- ifelse(peds_MVCs$level_of_care == "Unknown", NA, peds_MVCs$level_of_care)
peds_MVCs$time_of_day <- ifelse(peds_MVCs$time_of_day == "Unknown", NA, peds_MVCs$time_of_day)
peds_MVCs$lowest_gcs <- ifelse(peds_MVCs$lowest_gcs == -9, NA, peds_MVCs$lowest_gcs)

###################################################################################
# Create factors -----------------------------------------------------
#

# convert outcome to 1 (Transported) and 0 (Nontransported)
peds_MVCs <- peds_MVCs %>% mutate(dispo_MVC = ifelse(dispo_MVC=="Transported",1,0))

peds_MVCs$age_cat <- factor(peds_MVCs$age_cat, levels = c("<1 year",
                                                          "1 year",
                                                          "2 to 5 years",
                                                          "6 to 10 years",
                                                          "11 to 14 years",
                                                          "15 to 17 years"))

peds_MVCs$ams <- ifelse(peds_MVCs$lowest_gcs < 15, 1, 0)
peds_MVCs$ams <- factor(peds_MVCs$ams)
levels(peds_MVCs$ams) <- c("No", "Yes")
peds_MVCs <- peds_MVCs %>%  select(-lowest_gcs)

peds_MVCs$level_of_care <-factor(peds_MVCs$level_of_care, levels = c("BLS",
                                                                     "ALS",
                                                                     "Critical care"))


###################################################################################
# Imputate missing data -----------------------------------------------------
#

md.pattern(peds_MVCs)

peds_MVCs_imp <- mice(peds_MVCs, m=5, method = "pmm", seed = 314)


###################################################################################
# Perform regression -----------------------------------------------------
#

mdl <- with(peds_MVCs_imp, glm(dispo_MVC ~ sex + age_cat + ams + level_of_care + time_of_day + division, family = "binomial"))
mlr <- glm(dispo_MVC ~ sex + age_cat + ams + level_of_care + time_of_day + division, data = peds_MVCs, family = "binomial")

summary(pool(mdl))
sum <-  summary(pool(mdl), conf.int = TRUE, exponentiate = TRUE)


###################################################################################
# Create Regression Analysis Table -------------------------------------------
#

# source function for analysis
source("./Code/730-Regression_table_functions_imp.R")

# variables for regression
vars <- c("age_cat","sex","ams","time_of_day","level_of_care","division")

# get output of univariate analysis in table format
uni_output <- univar_table(peds_MVCs_imp, vars, "dispo_MVC")

# get output of multivariate analysis in table format
multi_output <- multivar_table(peds_MVCs_imp, vars, "dispo_MVC")

# output tables
write_csv(uni_output, "./Data/Results/Peds_Prehosp-Univar_analysis_imp.csv")
write_csv(multi_output, "./Data/Results/Peds_Prehosp-Multivar_analysis_imp.csv")
