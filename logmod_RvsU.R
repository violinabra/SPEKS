library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(psych)
library(writexl)
library(sandwich)
library(lmtest)
library(car)
library(broom)
######################################################################
### Initialize Data Frames###
######################################################################
df_uvsr <- read_excel("/Users/ecuadorkian23/Desktop/Opioidp/logmod/UVSR.xlsx", sheet = "Data")
df_pr <- read_excel("/Users/ecuadorkian23/Desktop/Opioidp/logmod/poverty-rate-by-state-2025.xlsx", sheet = "Data") 
df_er <- read_excel("/Users/ecuadorkian23/Desktop/Opioidp/logmod/educational-attainment-by-state-2025.xlsx", sheet = "Data")
df_med_op <- read_excel("/Users/ecuadorkian23/Desktop/Opioidp/Opioid_Prescribing_Rates_2022.xlsx", sheet = "Sheet 1 - Opioid_Prescribing_Ra" )
######################################################################
### Data Processing###
######################################################################
df_med_op2 <- df_med_op[, c("Year", "Prscrbr_Geo_Lvl", "Prscrbr_Geo_Desc", "Breakout_Type", "Breakout","Tot_Opioid_Prscrbrs",
                         "Tot_Prscrbrs", "Tot_Opioid_Clms", "Tot_Clms")]

df_logmod <- df_med_op2 %>%
  filter(Prscrbr_Geo_Lvl == "State", Breakout == "Urban" | Breakout == "Rural") %>% 
  group_by(Year, Prscrbr_Geo_Desc, Breakout, Tot_Opioid_Prscrbrs) %>% 
  summarise(Total_OC = sum(Tot_Opioid_Clms, na.rm = TRUE))##Group together medicare data by State and Rural vs Urban

df_logmod_2 <- df_logmod %>% 
  inner_join(df_uvsr, by = c("Prscrbr_Geo_Desc" = "State",
                          "Breakout" = "Type")) %>% 
  inner_join(df_pr, by = c("Prscrbr_Geo_Desc" = "State")) %>% 
  inner_join(df_er, by = c("Prscrbr_Geo_Desc" = "State"))##inner join tables to one data frame for analysis


df_logmod_2$claims_inthousands <- df_logmod_2$Total_OC/1000 ##claims in thousands
df_logmod_2$claims_per_capita <- (df_logmod_2$Total_OC/df_logmod_2$Total_breakout)*100000 ##total opioid claims divided by total relative population times 100,000
df_logmod_2$povertyrate_recoded <- ifelse(df_logmod_2$Poverty_rate > 11.1, "Higher_poverty","Lower_poverty")##Re-coding for states above and below national poverty rate
df_logmod_2$bachelors_recoded <-  ifelse(df_logmod_2$W_bachelors_or_h > 35, "Above_na","Below_na")##Re-coding for states above or below national educational attainment 
######################################################################
### Log_model--create reference levels###
######################################################################

df_logmod_2$Breakout <- relevel(factor(df_logmod_2$Breakout), ref = "Urban")##dictate reference level
df_logmod_2$povertyrate_recoded <- relevel(factor(df_logmod_2$povertyrate_recoded), ref = "Lower_poverty")##dictate reference level
df_logmod_2$bachelors_recoded <- relevel(factor(df_logmod_2$bachelors_recoded), ref = "Above_na")##dictate reference level

levels(factor(df_logmod_2$Breakout))##check reference level
levels(factor(df_logmod_2$povertyrate_recoded))##check reference level
levels(factor(df_logmod_2$bachelors_recoded))##check reference level


######################################################################
### Logistic Regression of variables to test odds of a rural vs urban opioid claim###
######################################################################
mod_op <- glm(factor(Breakout) ~ claims_inthousands + povertyrate_recoded + bachelors_recoded, 
              data = df_logmod_2,
              family = binomial (link = "logit"))
summary(mod_op) 
exp(cbind(OR = coef(mod_op), confint(mod_op)))


######################################################################
### Robust SE for mod_op###
######################################################################
rob_mod_op <- vcovHC(mod_op,type = "HC0")
coeftest(mod_op, vcov. = rob_mod_op)




