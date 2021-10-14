library(pacman)
library(tidyverse)
library(lubridate)

trips <- read_csv("Trips_by_Distance.csv", col_types = cols(.default = "c", Date = "D")) #parsing error, may be causing problem with NA values?

table(is.na(trips$`State FIPS`)) #978 obs na
table(is.na(trips$`State Postal Code`)) #978 obs na
table(is.na(trips$`County FIPS`)) #all obs na
table(is.na(trips$`County Name`)) #all obs na

str(trips) 

trips[7:19] <- sapply(trips[7:19], as.numeric) #change type of cols

trips %>%
  filter(!is.na(`County Name`)) %>% 
  select(`County Name`) #look at formatting of county names for filtering

trips %>% 
  filter(`State Postal Code` == "NY") -> ny_trips #get NY only

#trips %>% 
#  filter(`County Name` == "Onondaga County") -> syr_trips

trips %>% 
  filter(Level == "National") -> nat_trips #get national only

nat_trips %>% 
  bind_rows(ny_trips) -> nat_ny_trips #bind together results

write.csv(nat_ny_trips, file = "nat_ny_trips.csv", na = "NA") #save as csv for sheets usage

##### Data Exploration/Validation

nat_ny_trips %>% 
  group_by(Level) %>% 
  summarise(count = n()) %>% View() #County State and National are accounted for

min_date <- min(nat_ny_trips$Date)
max_date <- max(nat_ny_trips$Date)

dates_total <- seq(min_date, max_date, by = "days")

table(nat_ny_trips$Date %in% dates_total) #all dates are present, none are missing

nrow(nat_ny_trips) == length(unique(nat_ny_trips$`Row ID`)) #number of unique GUIDs equals number of rows so each row is unique
