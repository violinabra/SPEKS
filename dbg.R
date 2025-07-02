library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(psych)
library(writexl)
library(scales)

######################################################################
### Initialize Data Frames###
######################################################################
df_dbg_1 <- read_excel("/Users/ecuadorkian23/Desktop/Opioidp/FINAL/Opioid_Prescribing_Rates_2022.xlsx", sheet = "Sheet 1 - Opioid_Prescribing_Ra")

df_dbg_2 <- df_dbg_1[, c("Year", "Prscrbr_Geo_Lvl", "Prscrbr_Geo_Desc", "Breakout_Type", "Breakout","Tot_Opioid_Prscrbrs",
                             "Tot_Prscrbrs", "Tot_Opioid_Clms", "Tot_Clms")]
View(df_dbg_1)
######################################################################
### Data Processing###
######################################################################
df_dbg_3 <- df_dbg_2 %>%
  filter(Prscrbr_Geo_Lvl == "State" & (Breakout == "Urban" | Breakout == "Rural")) %>%
  group_by(Year, Prscrbr_Geo_Desc, Breakout, Tot_Opioid_Prscrbrs) %>% 
  summarise(Total_OC = sum(Tot_Opioid_Clms, na.rm = TRUE))## Grouping medicare opioid claims, state, breakout, and year

df_dbg_4 <- df_dbg_3 %>% 
  group_by(Year, Breakout) %>% 
  summarise(Sum_Opioid_Claims = sum(Total_OC, na.rm = TRUE))##Grouping medicare opioid claims by year and breakout for Double Bar Graph 
######################################################################
### Double Bar Graph of Rural vs Opioid Claims ###
######################################################################
# Step 2: Create the grouped bar chart
  ggplot(df_dbg_4, aes(x = factor(Year), y = Sum_Opioid_Claims, fill = Breakout)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_smooth(aes(group = Breakout, color = Breakout), method = "lm", se = FALSE, linetype = "dotted") +
  scale_fill_manual(values = c("Urban" = "lightblue", "Rural" = "salmon")) + 
  scale_color_manual(values = c("Urban" = "blue", "Rural" = "red")) + 
  scale_y_continuous(labels = comma) +  
  labs(
    title = "Medicare Part D Opioid Claims 2013 - 2022",
    x = "Year",
    y = "Total Opioid Claims",
  ) +
  theme_minimal()
  
  
