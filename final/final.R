# load packages ####

library(tidyverse)
library(arules)
library(arulesViz)
library(lubridate)
library(caret)
library(kernlab)
library(e1071)
library(rvest)
library(gt)
library(janitor)


# import the data ####

rebates <- read_csv("NYSERDA_Electric_Vehicle_Drive_Clean_Rebate_Data__Beginning_2017.csv")


# clean up the data ####

rebates %>% 
  select(-`Data through Date`) %>% #drop col
  mutate_all(tolower) %>% #ease of use lower case
  rename_all(tolower) %>% 
  rename("submit_date" = 1, #better col names
         "ev_type" = 6,
         "trans_type" = 7,
         "year_ghg_redux_mtco2e" = 8,
         "year_petrol_redux_gallons" = 9,
         "rebate_usd" = 10) %>% 
  mutate(submit_date = mdy(submit_date)) %>%  #convert to date format
  drop_na() %>% #drop na
  mutate(month = month(submit_date), #extract month/year
         year  = year(submit_date)) %>%
  select(-submit_date) -> clean_rebate #save as obj


# association rules mining ####

# this will determine the most common factors that lead in to receiving the maximum rebate
# I know there are other factors in determining rebates, such as public policy, but it may be a starting point for a story

clean_rebate %>% 
  mutate(across(.fns = as.factor)) %>% as("transactions") -> trans_rebate #need transaction matrix format

inspect(apriori(clean_rebate, parameter = list(supp = 0.1, conf = 0.8))) #rules generation algorithm

rules <- apriori(clean_rebate, #add more parameters for cleaner rules and regenerate them
                 parameter=list(supp=0.1, conf = 0.8, minlen=3), 
                 appearance = list(default="lhs",rhs="rebate_usd=2000"),
                 control = list(verbose=F))

rules<-sort(rules, decreasing=TRUE,by='lift') #sort by correlation

top_ten <- inspect(rules[c(1:10)]) #ave top 10 rules

top_ten

# all of the top ten rules have the Tesla Model 3 as a part of them -- seems that this is the most common vehicle that NY residents purchase/lease and receive the highest rebate level

# let's explore that a little more


# Research ####

#Seems like Tesla Model 3 is the most common way people are getting the full $2000 rebate in NY State. How does this work with the federal tax credit?

url <- "https://www.irs.gov/businesses/irc-30d-new-qualified-plug-in-electric-drive-motor-vehicle-credit"

fed <- read_html(url) %>% html_nodes(".table") %>% html_table() #scrape their site for the numbers
tesla_fed <- fed[[32]] #save table

# FROM IRS site: "Taxpayers may claim the full amount of the credit up the end of the first quarter after the quarter in which the manufacturer records its sale of the 200,000th qualified vehicle. For the second and third calendar quarters, taxpayers may claim 50% of the credit. For the fourth and fifth calendar quarters, taxpayers may claim 25% of the credit. No credit is allowed after the fifth quarter. Section 4.07 of Notice 2009-89 provides that a vehicle is not “acquired” before the date on which title passes under state law."

# $7500 until 12/31/2018, $3750 until 6/30/2019, $1875 until 12/31/2019, $0 in perpetuity

#Phasing out the Tesla rebates after 1/1/2020 might disincentivize people from buying them, let's see how before and after date splits matters...

#clean up data from table scraping
tesla_fed$`Qualifying Vehicle` = str_replace_all(string = tesla_fed$`Qualifying Vehicle`, pattern = "[0-9]", replacement = "")
tesla_fed$`Qualifying Vehicle` = str_replace_all(string = tesla_fed$`Qualifying Vehicle`, pattern = "-", replacement = "")
tesla_fed$`Qualifying Vehicle` = gsub(x = tesla_fed$`Qualifying Vehicle`, pattern = "\\s+", replacement = " ")

#build nice table visualization of data, may want to use it in the story
tesla_fed %>% 
  clean_names() %>%
  gt() %>% 
  tab_header(title = "Tesla Federal Tax Credit Phase Out Plan") %>%
  cols_label(qualifying_vehicle = "Qualifying Vehicle", 
             acquired_through_12_31_2018 = "Acquired Through 12/31/2018", 
             acquired_1_1_2019_through_6_30_2019 = "Acquired 1/1/2019 Through 6/30/2019", 
             acquired_7_1_2019_through_12_31_2019 = "Acquired 7/1/2019 Through 12/31/2019",
             credit_available_1_1_2020 = "Credit Available 1/1/2020") %>% 
  tab_source_note(source_note = "Source: Internal Revenue Service") %>% 
  tab_style(
    style = cell_borders(
      sides = c('left', 'right'),
      color = "#BBBBBB",
      weight = px(1),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
   ) %>% 
  tab_style(
    style = cell_fill(
      color = "#c3d6fa"
    ),
    locations = cells_body(
      columns = everything(),
      rows = c(2,4,6,8)
    )
  ) %>% 
  tab_style(
    style = list(cell_fill(
      color = "#002d62"
    ), cell_text(
      color = "#FFFFFF"
    )),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_fill(
      color = "#002d62"
    ), cell_text(
      color = "#FFFFFF",
      weight = "bold"
    )),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(
      align = "right"
    ),
    location = cells_source_notes()
  ) %>% 
  gtsave(
    "table.html"
  )

# Association Rules for Before and After 1/1/2020 ####

#These steps are all duplicated from the first run through, with the exception of the filtering for the date split

## Before 1/1/2020 ####

clean_rebate %>%
  filter(year < 2020) %>% 
  mutate(across(.fns = as.factor)) %>% as("transactions") -> trans_rebate_pre

inspect(apriori(trans_rebate_pre, parameter = list(supp = 0.1, conf = 0.8)))

rules_pre <- apriori(trans_rebate_pre, 
                     parameter=list(supp=0.1, conf = 0.8, minlen=3), 
                     appearance = list(default="lhs",rhs="rebate_usd=2000"),
                     control = list(verbose=F))

rules_pre<-sort(rules_pre, decreasing=TRUE,by='lift')

top_ten_pre <- inspect(rules_pre[c(1:10)])

top_ten_pre

# Tesla Model 3 still owns the top ten rules, appearing in just about every one (if not all of them)
# lift measure is even higher now, indicating a stronger correlation between the Tesla Model 3 and the max state rebate


## After 1/1/2020 ####

clean_rebate %>%
  filter(year >= 2020) %>% 
  mutate(across(.fns = as.factor)) %>% as("transactions") -> trans_rebate_post

inspect(apriori(trans_rebate_post, parameter = list(supp = 0.1, conf = 0.8)))

rules_post <- apriori(trans_rebate_post, 
                      parameter=list(supp=0.1, conf = 0.8, minlen=3), 
                      appearance = list(default="lhs",rhs="rebate_usd=2000"),
                      control = list(verbose=F))

rules_post<-sort(rules_post, decreasing=TRUE,by='lift')

top_ten_post <- inspect(rules_post[c(1:10)])

top_ten_post

# Tesla & the Model 3 appear in about half of the top ten rules, with a lower lift than the overall, though the metrics of the Model 3 make up the rest of the rules...
# this indicates that though purchases of teslas remained high (and even increased), other factors appeared more often when compared against the rebate earned


# Checking counts of purchases/lease ####

clean_rebate %>% 
  mutate(fed_rebate = case_when( #set ratio fo full tax credit for each purchase
    year < 2019 ~ 1,
    year == 2019 & month <= 6 ~ 0.5,
    year == 2019 & month >= 7 ~ 0.25,
    year >= 2020 ~ 0
  )) %>%
  filter(make == "tesla") %>% #select tesla only
  group_by(fed_rebate, month, year) %>% #group by for aggregation
  summarise(count = n()) %>% #aggregation
  mutate(date = paste0(month, "-", year), #parse date format
         date = my(date)) %>% 
  ungroup() %>% 
  select(-c(month, year)) -> total_teslas_bought #save out as obj

write_csv(total_teslas_bought, "total_teslas_bought.csv") #write to csv for import to datawrapper

ggplot(total_teslas_bought) + #plot the data here in R for quick visualization for personal validation
  geom_line(aes(date, count, color = fed_rebate)) + 
  theme_minimal()

# 2021 is kind of an outlier for plotting, because it was partially complete (data-wise), and the ongoing pandemic might have also had an effect on purchasing habits, even though the numbers for 2020 were extremely high
# however, it is very clear to see that purchases of teslas increased over time, even past the federal rebate end-date

# Checking how total rebate over time is considered for teslas ####

clean_rebate %>% 
  mutate(rebate_usd = as.numeric(rebate_usd), #reformat data
         rebate_usd = case_when(
    year < 2019 ~ rebate_usd + 7500, #add appropriate tax credit to every rebate
    year == 2019 & month <= 6 ~ rebate_usd + 3750,
    year == 2019 & month >= 7 ~ rebate_usd + 1875,
    year >= 2020 ~ rebate_usd
  )) %>%
  filter(make == "tesla") %>% #same process as before (continued below)
  group_by(month, year) %>% 
  summarise(sum = sum(rebate_usd)) %>% 
  mutate(date = paste0(month, "-", year),
         date = my(date)) %>% 
  ungroup() %>% 
  select(-c(month, year)) %>% 
  write_csv("total_rebate_tesla.csv")
  
  ggplot(data = read_csv("total_rebate_tesla.csv")) +
  geom_bar(aes(date, sum), stat = "identity") + 
  theme_minimal()

  