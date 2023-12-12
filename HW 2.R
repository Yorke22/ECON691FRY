#HOMEWORK 2
#Created by: FREDERICK R. YORKE
#Created on: DECEMBER 8, 2023

rm(list=ls())

#Load Libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(stargazer)

#Code for downloading a list of available variables

VAR <- load_variables(2021, "acs5", cache = TRUE)

#Filtering the list of variables to get the variables of Interest.
data<-VAR  %>%
  filter(grepl("ALLOCATION OF TRAVEL", concept)) 

#Obtaining the data of interest from ACS 5-Year API
vars<-c("B01001_001", "B01001_003", "B01001_004", "B01001_005", "B01001_006", # Total Population and Male under 18
        "B01001_027", "B01001_028", "B01001_029", "B01001_030", #Female under 18
        "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025", #Male over 64
        "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049", #Female over 64
        "B02001_002", "B02001_003", "B02001_005", # White, Black, Asian Population
        "B25087_001", "B25087_002", "B25058_001", # #Related to Housing Unit
        "B99084_001", "B99084_005", # Worked Related
        "B25088_001", "B06011_001") #Median Owner Cost and Median Income

#Command that pulls data from ACS under the years of interest
years <- c(2018, 2019, 2020, 2021)

for(i in years){
  acs <- get_acs(geography = "county",	#defines geography level of data 
                 variables = vars,	#specifics the data we want 
                 state = c(19, 20, 29, 31),	        
                 year = i,	        #denotes a particular year
                geometry = TRUE)	 

core <- acs %>%
  mutate(variable = case_when(variable=="B01001_001" ~ "Population",
                              variable=="B06011_001" ~ "Med_Inc",
                              variable=="B25058_001" ~ "Med_Rent",
                              variable=="B25088_001" ~ "Med_Cost",
                              TRUE ~ variable)) %>%
  select(-"moe") %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), names_from = "variable", values_from = "estimate") 

core <- core %>%
  group_by(GEOID) %>%
  mutate(per_und18 = (sum(c_across(B01001_003:B01001_006))+sum(c_across(B01001_027:B01001_030)))/Population,
         per_ovr64 = (sum(c_across(B01001_020:B01001_025))+sum(c_across(B01001_044:B01001_049)))/Population,
         per_blk   = (B02001_003)/Population,
         per_wht   = (B02001_002)/Population,
         per_asn   = (B02001_005)/Population,
         per_oth   = 1 - per_wht - per_blk - per_asn, # percentage Population(Other)
         per_mort  = (B25087_002)/B25087_001,
         per_wfh   = (B99084_005)/B99084_001,
         med_inc2 = Med_Inc/12,      # Monthly Median Income      
         Rent_Share = Med_Rent/med_inc2, #Share of Rent
         Afford = 0.33 - Rent_Share, # Affordability
         Population = Population/10000000,
         Med_Cost = Med_Cost/10000,
         Year = i) 
ifelse(i==years[1], CORE <- core, CORE <- rbind(CORE, core))
}
   

#PART 3 :Running the 6 Estimation Regressions

mod1 <- lm(Afford ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh,
           data = core)

mod2 <- lm(Rent_Share ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh,
           data = core)

mod1a <- lm(Afford ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh +
              factor(Year),
            data = CORE)

mod2a <- lm(Rent_Share ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh +
              factor(Year),
            data = CORE) 

mod1b <- lm(Afford ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh +
                factor(Year) + factor(GEOID),
              data = CORE)

mod2b <- lm(Afford ~ Population + Med_Cost + per_und18 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh +
                factor(Year) + factor(GEOID),
              data = CORE)

#Summary
summary(mod1)
summary(mod2)
summary(mod1a)
summary(mod2a)
summary(mod1b)
summary(mod2b)



#PART 4
# Displaying Result in Html Format
stargazer(mod1, type = "html",out = "./Analysis.html")  #The problem here is that you keep overwritting the file rather than saving different versions
stargazer(mod2, type = "html",out = "./Analysis.html")
stargazer(mod1a,type = "html",out = "./Analysis.html")
stargazer(mod2a,type = "html",out = "./Analysis.html")
stargazer(mod1b,type = "html",out = "./Analysis.html")
stargazer(mod2b,type = "html",out = "./Analysis.html")


#Latex Format
stargazer(mod1,type = "latex")  
stargazer(mod2,type = "latex") 
stargazer(mod1a,type = "latex")
stargazer(mod2a,type = "latex")
stargazer(mod1b,type = "latex")
stargazer(mod2b,type = "latex")
