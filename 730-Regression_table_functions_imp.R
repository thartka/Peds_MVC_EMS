####################################################################################
# Source file: Regression table functions
# Author: Thomas Hartka, MD
# Date: 1/18/20
# 
# Description: These function produce univariate and multivariate tables for 
#   logistic regression modules using glm.  The output is provided in the form
#     var_name     odds           p_value
#     <variable>   X.XX(95%CI)    X.XX
#
####################################################################################

###################################################################################
# Function: univar_table
#
# Description: This function produces individual models for each variable passed
#  to the function in predictor_vars and calculates the odds ratio with 95% CI
#  along with the p-value.  The significant digits can be set separately for the ORs
#  and p-values.  The p-value threshold can also be set, under which p-values are
#  reported as "<0.01"/"<0.001"/etc.\
#
# Inputs:
#   uni_data - MATRIX data for analysis
#   predictor_vars - list of predictor variables to analyze
#   response_var - response variable
#   odds_sigdig (default=2) - significant digits for odds ration
#   p_sigdig (default=2) - significant digits for p-value
#   p_threshold (default=0.01) - threshold under which p-value is reported about <0.XXX
#
# Outputs:
#   uni_analysis - table with univariate regression information
#

univar_table <- function(uni_data, predictor_vars, response_var, odds_sigdig = 2, p_sigdig = 2, p_threshold = 0.01){
  
  # create blank data frame
  uni_analysis <- data.frame(
    var_name = character(),
    odds = character(),
    p_value = character()
  )
  
  # perform univariate analyses separately on all variables
  for(var in predictor_vars){
    
    # perform univariate analysis on each variable, substituting in name of the variable 
    # Uni_mod <- glm(substitute(r ~ p,list(r = as.name(response_var), p = as.name(var))), 
    #                data = uni_data, 
    #                family = "binomial")  
    
    uni_form <- paste(response_var," ~ ", var, sep = "")
    
    # perform univariate analysis on each variable, substituting in name of the variable 
    Uni_mod <- with(uni_data, glm(formula(format(uni_form)), family = "binomial"))
    
    # get 95% conf int
    sum <-  summary(pool(Uni_mod), conf.int = TRUE, exponentiate = TRUE)
    
    # loop through all terms
    for(i in 1:length(sum$term)){
      
      if(sum$term[[i]]=="(Intercept)"){
        # for the intercept enter place holder info  
        t_odds <-  "Ref."
        t_p_value <-  "-"
        
      } else {
        # else extract OR and p-value
        t_odds <- sum$estimate[[i]]
        t_p_value <- sum$p.value[[i]]
        
        # round cofficients
        t_odds <- round(t_odds, digits = odds_sigdig)
        
        # get upper and lower 95% CI
        t_odds_low <- round(sum$`2.5 %`[[i]], digits = odds_sigdig)
        t_odds_high <- round(sum$`97.5 %`[[i]], digits = odds_sigdig)
        
        # assemble value and CI
        t_odds <- paste(format(t_odds, nsmall = odds_sigdig), 
                        " (", format(t_odds_low, nsmall = odds_sigdig), "-", 
                        format(t_odds_high, nsmall = odds_sigdig), ")", sep="")
        
        # check if p-value is less than threshold
        if(t_p_value < p_threshold){
          
          # if less than threshold, set to "<0.XX1"
          t_p_value <- paste("<", p_threshold)
          
        } else {
          
          # else just round p-value
          t_p_value <- format(round(t_p_value, digits = p_sigdig), nsmall = p_sigdig)
        }
      }
      
      # add new data to table
      uni_analysis <- rbind(uni_analysis, 
                            data.frame(
                              var_name = as.character(sum$term[i]), 
                              odds = t_odds, 
                              p_value = as.character(t_p_value)))
    }
  }
  
  return(uni_analysis)
}

###################################################################################
# Function: multivar_table
#
# Description: This function produces a single models including all variables passed
#  to the function in predictor_vars and calculates the odds ratio with 95% CI
#  along with the p-value.  The significant digits can be set separately for the ORs
#  and p-values.  The p-value threshold can also be set, under which p-values are
#  reported as "<0.01"/"<0.001"/etc.\
#
# Inputs:
#   multi_data - MATRIX data for analysis
#   predictor_vars - list of predictor variables to analyze
#   response_var - response variable
#   odds_sigdig (default=2) - significant digits for odds ration
#   p_sigdig (default=2) - significant digits for p-value
#   p_threshold (default=0.01) - threshold under which p-value is reported about <0.XXX
#
# Outputs:
#   multi_analysis - table with multivariate regression information
#

multivar_table <- function(multi_data, predictor_vars, response_var, odds_sigdig = 2, p_sigdig = 2, p_threshold = 0.01){
  
  # create blank data frame
  multi_analysis <- data.frame(
    var_name = character(),
    odds = character(),
    p_value = character()
  )
  
  # create formula
  multi_form <- paste(response_var," ~ ", paste(predictor_vars, collapse = " + "), sep = "")

  # perform univariate analysis on each variable, substituting in name of the variable 
  multi_mod <- with(multi_data, glm(formula(format(multi_form)), family = "binomial"))
  #multi_mod <- with(multi_data, glm(dispo_MVC ~ sex + age_cat + ams + level_of_care + time_of_day + division, family = "binomial"))

  # get 95% conf int
  sum <-  summary(pool(multi_mod), conf.int = TRUE, exponentiate = TRUE)
  
  # loop through all terms
  for(i in 1:length(sum$term)){
    
    if(sum$term[[i]]=="(Intercept)"){
      # for the intercept enter place holder info  
      t_odds <-  "Ref."
      t_p_value <-  "-"
      
    } else {
      # else extract OR and p-value
      t_odds <- sum$estimate[[i]]
      t_p_value <- sum$p.value[[i]]
      
      # round cofficients
      t_odds <- round(t_odds, digits = odds_sigdig)
      
      # get upper and lower 95% CI
      t_odds_low <- round(sum$`2.5 %`[[i]], digits = odds_sigdig)
      t_odds_high <- round(sum$`97.5 %`[[1]], digits = odds_sigdig)
      
      # assemble value and CI
      t_odds <- paste(format(t_odds, nsmall = odds_sigdig), 
                      " (", format(t_odds_low, nsmall = odds_sigdig), "-", 
                      format(t_odds_high, nsmall = odds_sigdig), ")", sep="")
      
      # check if p-value is less than threshold
      if(t_p_value < p_threshold){
        
        # if less than threshold, set to "<0.XX1"
        t_p_value <- paste("<", p_threshold)
        
      } else {
        
        # else just round p-value
        t_p_value <- format(round(t_p_value, digits = p_sigdig), nsmall = p_sigdig)
      }
    }
    
    # add new data to table
    multi_analysis <- rbind(multi_analysis, 
                            data.frame(
                              var_name = as.character(sum$term[i]), 
                              odds = t_odds, 
                              p_value = as.character(t_p_value)))
  }
  
  
  return(multi_analysis)
}