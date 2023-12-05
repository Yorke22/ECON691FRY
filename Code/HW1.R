#ECON691 HOMEWORK 1
#FREDERICK R. YORKE

rm(list=ls())

library(tidycensus)
library(tidyverse)

var <- load_variables(2021, "acs5", cache = TRUE)

head(var)

#LOcating a file in a DATA
temp <- var %>%
  filter(grepl("MEDIAN", concept)) %>% 
  filter(grepl("GROSS RENT", concept)) 

vars <- c("B06011_001", "B25031_001")

acs <- get_acs(geography = "county", # defines geography level of data
               variables = vars, # specifies the data we want
               state = c(19,20,29,31),     # denotes the FOUR states assigned to me
               year = 2021, # denotes the year
               geometry = TRUE)

head(acs)

#Transforming the API data to a Wide-form dataframe
               
core <- acs %>%
  select(-moe) %>%
  mutate(variable = case_when(variable == "B06011_001"~ "Med_Inc",
                              variable == "B25031_001" ~ "Med_Rent",
                              TRUE ~ variable)) %>%
  #select(-"moe")%>%
  pivot_wider(id_cols = c("GEOID", "NAME","geometry"),
              names_from = "variable",
              values_from = "estimate") %>%
  mutate (med_inc2 = Med_Inc/12,      # Monthly Median Income      
          Rent_Share = Med_Rent/med_inc2, #Share of Rent
          Afford = Rent_Share - 0.33)

summary(core)


#Creating Maps for the Four states (Nebraska, Iowa, Missouri, Kansas)


#Map 1 
ggplot(core) + 
  geom_sf(aes(fill=Afford) + 
  scale_fill_gradient2()+
  theme_bw())

#Map 2 
ggplot(core) + 
  geom_sf(aes(fill=Med_Inc))

#Map 3 
ggplot(core) + 
  geom_sf(aes(fill=Med_Rent)) 


