# ECON691FRY


rm(list=ls())

library(tidycensus)
library(tidyverse)

var <- load_variables(2021, "acs5", cache = TRUE)

head(var)

#LOcating a file in a DATA
temp <- var %>%
  filter(grepl("MEDIAN", concept)) %>% # locating text pattern
  filter(grepl("GROSS RENT", concept)) # locating text pattern

#vars <- c("B06011_001", "B25031_001")

acs <- get_acs(geography = "county", # defines geography level of data
                variables = vars, # specifies the data we want
                state = 17,     # denotes the specific state
                year = 2021, # denotes the year
                geometry = TRUE) # downloada the TIGER shapefile data
  #Data is long_ Main identifier duplicated. Tough tp woork with.

# head(core) entered in console
#mutate used in tidyverse to modify or create a column


core <- acs %>%
  select(-moe) %>%
  mutate(variable = case_when(variable == "B06011_001"~ "Med_Inc",
                              variable == "B25031_001" ~ "Med_Rent",
                              TRUE ~ variable)) %>%
  pivot_wider(id_cols = c("GEOID", "NAME","geometry"),
              names_from = "variable",
              values_from = "estimate") %>%
  mutate (med_inc2 = Med_Inc/12,      # Monthly Median Income      
        Rent_Share = Med_Rent/med_inc2, #Share of Rent
        Afford = Rent_Share - 0.33)


#Tables and Graphs

ggplot(core) + #layering programs
  geom_sf(aes(fill=Afford)) #no need for x and y indication here

