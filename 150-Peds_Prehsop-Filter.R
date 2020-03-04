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

rm(list = ls())

##################################################################################
# Function to filter and join tables for study population ---------------
#
# Parameters:
#  input_file1 - first file name to read
#  input_file2 - second file name to read
#  output_file - file name to write to
#  key_column - name of column to filter on
#  key - values to keep in data set
#
# Returns:
#  None
#

filter_table_csv <- function(input_file1, input_file2, output_file, key_column, keys){
  
  # load table 1
  data_tab1 <- read_csv(input_file1)
  
  # filter for study population
  data_tab1 <- inner_join(data_tab1, keys, by=key_column)
  
  # load table 2
  data_tab2 <- read_csv(input_file2)
  
  # filter for study population
  data_tab2 <- inner_join(data_tab2, keys, by=key_column)
  
  # join tables
  data_full <- full_join(data_tab1, data_tab2)
  
  # clear intermediate tables
  data_tab1 <- NULL
  data_tab2 <- NULL
  
  # write out file
  write_csv(data_full, output_file)
  
  # clear table
  data_full <- NULL
}

###################################################################################
# Filter for study population -----------------------------
#  Tables are loaded, filtered, and stored to a different directory
# 

# read peds IDs
peds_IDs <- read_csv("./Data/Peds_MVCs/NEMSIS-Peds_MVC_IDS.csv")

# set directories
input_dir1 <- "./Data/NEMSIS_2017-CSV_clean/"
input_dir2 <- "./Data/NEMSIS_2018-CSV_clean/"
output_dir <- "./Data/Peds_MVCs/"

# filter computed elements table
file_name <- "computedelements.csv"
filter_table_csv(paste(input_dir1, file_name, sep=""), 
                 paste(input_dir2, file_name, sep=""), 
                 paste(output_dir, "Peds_MVC-", file_name, sep=""), 
                 "PcrKey", 
                 peds_IDs)

# filter published events table
file_name <- "pub_pcrevents.csv"
filter_table_csv(paste(input_dir1, file_name, sep=""), 
                 paste(input_dir2, file_name, sep=""), 
                 paste(output_dir, "Peds_MVC-", file_name, sep=""), 
                 "PcrKey", 
                 peds_IDs)

# filter patient race group table
file_name <- "pcrpatientracegroup.csv"
filter_table_csv(paste(input_dir1, file_name, sep=""), 
                 paste(input_dir2, file_name, sep=""), 
                 paste(output_dir, "Peds_MVC-", file_name, sep=""), 
                 "PcrKey", 
                 peds_IDs)

# filter vital signs table
file_name <- "factpcrvital.csv"
filter_table_csv(paste(input_dir1, file_name, sep=""), 
                 paste(input_dir2, file_name, sep=""), 
                 paste(output_dir, "Peds_MVC-", file_name, sep=""), 
                 "PcrKey", 
                 peds_IDs)