library(tidyverse)
library(lubridate)

setwd("~/School/Syracuse/Sr Sem 1/JNL 221")

##### Electric Vehicle Data #####
# https://catalog.data.gov/dataset/nyserda-electric-vehicle-drive-clean-rebate-data-beginning-2017
# created by: NY State Energy Research and Development Authority (NYSERDA)
# data through 7/9/21

ev_ny <- read_csv("NYSERDA_Electric_Vehicle_Drive_Clean_Rebate_Data__Beginning_2017.csv")

ev_ny %>% 
  select(-1) %>%
  mutate_all(tolower) %>% 
  rename_all(tolower) %>% 
  rename("submit_date" = 1,
         "ev_type" = 6,
         "trans_type" = 7,
         "year_ghg_redux_mtco2e" = 8,
         "year_petrol_redux_gallons" = 9,
         "rebate_usd" = 10) %>% 
  mutate(submit_date = mdy(submit_date)) -> ev_ny_clean

ev_ny_clean %>% 
  summarise_all(~sum(is.na(.)))

ev_ny_clean %>% 
  filter(!is.na(rebate_usd),
         !is.na(county),
         !is.na(trans_type)) -> ev_ny_clean

ev_ny_clean %>% 
  summarise_all(~sum(is.na(.)))

### arules association rules mining

library(arules)
library(arulesViz)

ev_ny_clean %>% 
  mutate(submit_date = month(submit_date),
         year_ghg_redux_mtco2e = as.numeric(year_ghg_redux_mtco2e),
         year_petrol_redux_gallons = as.numeric(year_petrol_redux_gallons),
         rebate_usd = as.numeric(rebate_usd)) -> ev_ny_month

ev_ny_month$year_ghg_redux_mtco2e <- cut(ev_ny_month$year_ghg_redux_mtco2e, breaks=c(-2,0,2,4), labels = c("Low", "Medium", "High"))

ev_ny_month$year_petrol_redux_gallons <- cut(ev_ny_month$year_petrol_redux_gallons, breaks=c(-100,250,500,1000), labels = c("Low", "Medium", "High"))

ev_ny_month$rebate_usd <- factor(ev_ny_month$rebate_usd)

cols <- c(1:7)

ev_ny_month[,cols] <- lapply(ev_ny_month[,cols], factor)

ev_ny_month %>% 
  select(-2:-3) -> temp_ev

evX <- as(temp_ev, "transactions")

summary(evX)

rules<-apriori(data=evX, parameter=list(supp=0.0675,conf = 0.8, minlen=2), 
               appearance = list(default="lhs",rhs="rebate_usd=2000"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by='lift')

inspect(rules)

subrules <- head(sort(rules, by="lift"), 10)

plot(subrules, method = "graph")

#association rules tell me that the higher the reductions of pollution and fuel consumption are the biggest factors in determining the rebate

top_rule <- inspect(rules[2])

#top_rule: occurs 26% of the time (11k+ times) (support)
#purchasing a battery-electric vehicle with high emission reductions and high gas reductions will result in max rebate 83% of the time (confidence)
#high emission reduction is >= 2 mtco2e/year , high gas reduction is >= 500 gallons/year
#this setup is 2x more likely to result in max rebate (lift)
#max rebate was $2000

#tesla, and specifically the model 3, are the most likely to garner the highest rebate offered

### by county

ev_ny_clean %>% 
  mutate(rebate_usd = as.numeric(rebate_usd)) %>% 
  group_by(county) %>% 
  summarise(rebate = sum(rebate_usd)) %>% 
  arrange(desc(rebate)) %>%
  slice(1:10) %>% 
  mutate(county = str_to_title(county)) %>%
ggplot()+
  geom_bar(aes(reorder(county,-rebate), rebate, fill=factor(ifelse(county=="Onondaga","Onondaga","Other"))), stat = "identity", show.legend = FALSE)+
  scale_fill_manual(name = "County", values=c("red","grey50")) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  theme_minimal() +
  xlab("Counties") +
  ylab("Total Rebates Given ($)") +
  ggtitle("Total Electric Vehicle Rebates Distributed By County in NY State, 2017-2021")

ggsave("ny_ev_county.jpg", device = "jpg")

### when is it best to buy a car to get the highest rebate in all of NY

ev_ny_clean %>% 
  mutate(submit_date = month(submit_date),
         year_ghg_redux_mtco2e = as.numeric(year_ghg_redux_mtco2e),
         year_petrol_redux_gallons = as.numeric(year_petrol_redux_gallons),
         rebate_usd = as.numeric(rebate_usd)) %>% 
  group_by(submit_date) %>% 
  summarise(avg_rebate = mean(rebate_usd)) %>%
  mutate(month_name = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
         quarter = c(rep("Q1", 3),rep("Q2", 3),rep("Q3", 3),rep("Q4", 3))) %>% 
  ggplot() +
  geom_bar(aes(reorder(month_name, -avg_rebate), avg_rebate, fill = quarter), stat = "identity")+
  scale_fill_viridis_d()+
  xlab("Month")+
  ylab("Average Rebate")+
  theme_minimal()+
  ggtitle("Average Rebate Paid by Month in NY State, 2017-2021")

date_ref <- tibble(month_name = factor(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
       month_number = c(1,2,3,4,5,6,7,8,9,10,11,12),
       quarter =c(rep("Q1", 3),rep("Q2", 3),rep("Q3", 3),rep("Q4", 3)))


ev_ny_clean %>% 
  mutate(submit_date = month(submit_date),
         year_ghg_redux_mtco2e = as.numeric(year_ghg_redux_mtco2e),
         year_petrol_redux_gallons = as.numeric(year_petrol_redux_gallons),
         rebate_usd = as.numeric(rebate_usd)) %>% 
  group_by(submit_date, county) %>% 
  summarise(avg_rebate = mean(rebate_usd)) %>%
  ungroup() %>% 
  left_join(.,date_ref, by = c("submit_date" = "month_number")) %>%
  mutate(county = str_to_title(county)) %>% 
  ggplot() +
  geom_tile(aes(county, month_name, fill = avg_rebate))+
  scale_fill_viridis_c()+
  theme(axis.text.x = element_text(angle = 90, vjust = .2, hjust = 1))+
  xlab("County")+
  ylab("Month")+
  ggtitle("Average Rebate Paid by Month in NY State, 2017-2021")+
  labs(fill = "Average Rebate")

ggsave("heatmap_county.jpg", device = "jpg")






##### Travel Data National During Covid #####
#https://catalog.data.gov/dataset/trips-by-distance
#Created by Bureau of Transportation Statistics
#data through

library(tidyverse)
library(lubridate)

trips <- read_csv("Trips_by_Distance.csv", col_types = cols(.default = "c", Date = "D"))

### monthly avg pct of people staying home by month

trip_2019 <- trips %>% 
  rename_all(tolower) %>%
  filter(year(date) == "2019") %>% 
  filter(`state postal code` %in% c(NA, "NY")) %>%
  group_by(month, level, `county name`) %>% 
  mutate(`population staying at home` = as.numeric(`population staying at home`),
         `population not staying at home` = as.numeric(`population not staying at home`)) %>% 
  summarise(pct_home = (`population staying at home`)/(`population staying at home` + `population not staying at home`)) %>% 
  mutate(pct_home = 100*pct_home) %>%
  filter(`county name` %in% c("Onondaga County", NA)) %>%
  group_by(month, level) %>% 
  summarise(avg_pct_home = mean(pct_home)) %>% 
  mutate(month = as.numeric(month)) %>%
  left_join(date_ref, by = c("month" = "month_number")) %>% 
  ggplot()+
  geom_bar(aes(month_name, avg_pct_home, fill = factor(level, levels = c("County", "State", "National"))), stat = "identity", position = "dodge")+
  theme_minimal()+
  labs(fill = "")+
  xlab("Month")+
  ylab("Average at Home (%)")

trip_2020 <- trips %>% 
  rename_all(tolower) %>%
  filter(year(date) == "2019") %>% 
  filter(`state postal code` %in% c(NA, "NY")) %>%
  group_by(month, level, `county name`) %>% 
  mutate(`population staying at home` = as.numeric(`population staying at home`),
         `population not staying at home` = as.numeric(`population not staying at home`)) %>% 
  summarise(pct_home = (`population staying at home`)/(`population staying at home` + `population not staying at home`)) %>% 
  mutate(pct_home = 100*pct_home) %>%
  filter(`county name` %in% c("Onondaga County", NA)) %>%
  group_by(month, level) %>% 
  summarise(avg_pct_home = mean(pct_home)) %>% 
  mutate(month = as.numeric(month)) %>%
  left_join(date_ref, by = c("month" = "month_number")) %>% 
  ggplot()+
  geom_bar(aes(month_name, avg_pct_home, fill = factor(level, levels = c("County", "State", "National"))), stat = "identity", position = "dodge")+
  theme_minimal()+
  labs(fill = "")+
  xlab("Month")+
  ylab("Average at Home (%)")

trip_2021 <- trips %>% 
  rename_all(tolower) %>%
  filter(year(date) == "2019") %>% 
  filter(`state postal code` %in% c(NA, "NY")) %>%
  group_by(month, level, `county name`) %>% 
  mutate(`population staying at home` = as.numeric(`population staying at home`),
         `population not staying at home` = as.numeric(`population not staying at home`)) %>% 
  summarise(pct_home = (`population staying at home`)/(`population staying at home` + `population not staying at home`)) %>% 
  mutate(pct_home = 100*pct_home) %>%
  filter(`county name` %in% c("Onondaga County", NA)) %>%
  group_by(month, level) %>% 
  summarise(avg_pct_home = mean(pct_home)) %>% 
  mutate(month = as.numeric(month)) %>%
  left_join(date_ref, by = c("month" = "month_number")) %>% 
  ggplot()+
  geom_bar(aes(month_name, avg_pct_home, fill = factor(level, levels = c("County", "State", "National"))), stat = "identity", position = "dodge")+
  theme_minimal()+
  labs(fill = "")+
  xlab("Month")+
  ylab("Average at Home (%)")

library(cowplot)
plot_grid(trip_2019, trip_2020, trip_2021,
          ncol = 3, nrow = 1,
          labels = c("2019", "2020", "2021"))

### average of each category for Onondaga County

trips %>% 
  filter(`County Name` == "Onondaga County") %>% 
  select(7:19) %>%
  mutate_all(as.numeric) -> onondaga_trips

df_summary <- as.data.frame(apply(onondaga_trips, 2, summary))

tibble(df_summary[4,]) -> test

ordering <- gsub("Number of Trips ", "", colnames(test)[4:13])

test %>% 
  pivot_longer(
    cols = everything(),
    names_to = "measure",
    values_to = "average"
  ) %>%
  slice(4:13) %>%
  mutate(measure = gsub("Number of Trips ", "", measure),
         measure = factor(measure, levels = ordering)) %>%
  ggplot()+
  geom_bar(aes(measure, average), stat = "identity")+
  theme(axis.text.x = element_text(angle = 15, vjust = .8, hjust = .5))+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  ylab("Occurrences")+
  xlab("Number of Trips")+
  ggtitle("Number of Trips Taken of Various Length During COVID-19 in Onondaga County")

ggsave("onondaga_trips.jpg", device = "jpg")
  
