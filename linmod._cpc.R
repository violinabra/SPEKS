library(readxl)
library(openxlsx)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(psych)
library(car)
library(broom)
library(nlme)

######################################################################
### Initialize Data Frames###
######################################################################
OD.df <- read_xlsx("/Users/ecuadorkian23/Desktop/Opioidp/logmod/OD_state.xlsx", sheet = "data")
DF_UVSR <- read_xlsx("/Users/ecuadorkian23/Desktop/Opioidp/logmod/UVSR_tp.xlsx", sheet = "Sheet 1 - UVSR_tp")
OP.df <- read_xlsx("/Users/ecuadorkian23/Desktop/Opioidp/Opioid_Prescribing_Rates_2022.xlsx", sheet = "Sheet 1 - Opioid_Prescribing_Ra") 
View(DF_UVSR)
######################################################################
###Combine Medicare data to population and overdose deaths data for analysis###
######################################################################
df_m <- OP.df.m %>%
  inner_join(
    DF_UVSR %>% select(State, Total_Population),
    by = c("Prscrbr_Geo_Desc" = "State"), 
    relationship = "many-to-many"
  ) %>%
  inner_join(
    OD.df %>% select(State_name, OD_deaths),
    by = c("Prscrbr_Geo_Desc" = "State_name"),
    relationship = "many-to-many"
  ) %>%
  distinct(across(c(Year,Prscrbr_Geo_Desc)), .keep_all = TRUE)

######################################################################
### Create new variables for analysis###
######################################################################
df_m$claims_percapita <- df_m$Tot_Opioid_Clms/df_m$Total_Population##claims scaled to total population
df_m$OD_percapita <- df_m$OD_deaths/df_m$Total_Population##overdose deaths scaled to total population


######################################################################
### Linear Regression of claims_percapita vs OD_percapita###
######################################################################
mod_cpc <- lm(OD_percapita ~ claims_percapita,  data = df_m)
summary(mod_cpc)

plot(mod_cpc,1)##Residuals vs Fitted plot of linear regression on mod_cpc object
plot(mod_cpc,2)##Q-Q plot of linear regression on mod_cpc object






