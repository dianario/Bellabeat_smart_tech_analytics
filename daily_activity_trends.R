
# load relevant packages and data
library(tidyverse)
library(skimr)
library(janitor)
daily_activity <- read_csv("./fitabase_data/dailyActivity_merged.csv")

#explore data
View(daily_activity)
head(daily_activity)
str(daily_activity)

#Coerce Id col as character
daily_activity <- daily_activity %>% 
  mutate(Id = as.character(Id)) 
str(daily_activity)

# coerce ActivityDate as date
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = mdy(ActivityDate))
str(daily_activity)

skim_without_charts(daily_activity)

#Data validation

#Find out number of unique ID values 
unique_id_count <- daily_activity %>% 
  distinct(Id) %>% 
  summarise(count = n())
print(unique_id_count)

# The nunmber of unique IDs exceeded the number of users reported, so this is 
# investigated futher. 

print(daily_activity %>% distinct(Id), n=33)

# this has to be investigated further in other tables to see if this is real or a problem

# check date range 
date_range <-daily_activity %>% 
  summarise(StartDate = min(ActivityDate, na.rm = TRUE),
EndDate = max(ActivityDate, na.rm = TRUE))
print(date_range)
#date range is correct

#check the active minute data adds up
daily_activity <- daily_activity %>% 
  mutate(TotalMinutes = VeryActiveMinutes + FairlyActiveMinutes
         + LightlyActiveMinutes + SedentaryMinutes)
str(daily_activity)
# values below 1440 are observed, indicating that the user does not have the tracker active all day through the day

#The maxium value of active minutes falls within range
daily_activity %>% summarise(MaxMin =max(TotalMinutes))

#crate total active minutes column
daily_activity <- daily_activity %>% 
  mutate(TotalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)
str(daily_activity)

ggplot(data = daily_activity) + geom_point(mapping=aes(x=ActivityDate, y=TotalActiveMinutes)) 
 
  