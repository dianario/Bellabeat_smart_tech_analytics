
# load relevant packages and data
library(tidyverse)
library(skimr)
library(janitor)
daily_activity <- read_csv("./fitabase_data/dailyActivity_merged.csv")
daily_calories <- read_csv("./fitabase_data/dailyCalories_merged.csv")
daily_intensities <- read_csv("./fitabase_data/dailyIntensities_merged.csv")
daily_steps <- read_csv("./fitabase_data/dailySteps_merged.csv")

#explore data
str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(daily_steps)

# The daily activity data frame contains columns which appear in the other daily data frames. 
# This is confirmed by checking if the data in those columns is indeed identical. All the daily data
# is contained within the daily_activity data frame. In the case of daily_steps, 
# the only mismatch is located in column names, but all values are equal. 

daily_activity %>% select(Calories) %>% all.equal(daily_calories %>% select(Calories))
daily_activity %>% select(VeryActiveDistance, 
                          LightActiveDistance, 
                          SedentaryActiveDistance) %>% 
  all.equal(daily_intensities %>% 
              select(VeryActiveDistance, LightActiveDistance, SedentaryActiveDistance))
daily_activity %>%  select(VeryActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>% 
  all.equal(daily_intensities %>%  select(VeryActiveMinutes, LightlyActiveMinutes, SedentaryMinutes))
daily_activity %>% select(TotalSteps) %>% all.equal(daily_steps %>% select(StepTotal))


# Begin working with the daily_activity data frame.
##
##
##

# First change variable types. 
# Coerce Id col as character
daily_activity <- daily_activity %>% 
  mutate(Id = as.character(Id)) 
str(daily_activity)

# coerce ActivityDate col as date
daily_activity <- daily_activity %>% 
  mutate(ActivityDate = mdy(ActivityDate))
str(daily_activity)

# Data validation

#Find out number of unique ID values 
unique_id_count <- daily_activity %>% 
  distinct(Id) %>% 
  summarise(count = n())
print(unique_id_count)

# The number of unique IDs exceeded the number of users reported in data description, so this was
# investigated further. All the IDs are different, with no apparent typos. Therefore is is unlikely
# there are any mistakes. 

print(daily_activity %>% distinct(Id), n=33)



# check date range. The date range is correct
date_range <-daily_activity %>% 
  summarise(StartDate = min(ActivityDate, na.rm = TRUE),
EndDate = max(ActivityDate, na.rm = TRUE))
print(date_range)



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

ggplot(data = daily_activity) + 
  geom_line(mapping=aes(x=ActivityDate, y=TotalActiveMinutes, color=Id)) +
  labs(title = "Time spent active per user", 
  x = "Date", y ="Total Active Minutes")


average_active_time <- daily_activity %>%
  group_by(ActivityDate) %>%
  summarise(AverageActivity = mean(TotalActiveMinutes, na.rm = TRUE)) 

ggplot(data = average_active_time, aes(x = ActivityDate, y = AverageActivity)) +
  geom_line() + # Use geom_line() for line plot
  geom_point() + # Use geom_point() to show the actual data points
  theme_minimal() +
  labs(
    title = "",
    x = "Date",
    y = "Total"
  )
 
ggplot(data = daily_activity, aes(x = ActivityDate)) +
  geom_jitter(aes(y = TotalActiveMinutes), width = 0.1, height = 0, color = "black", alpha = 0.5) +
  geom_col(data = average_active_time, aes(y = AverageActivity), fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Daily Activity and Average Active Time",
    x = "Date",
    y = "Total Active Minutes"
  )
  