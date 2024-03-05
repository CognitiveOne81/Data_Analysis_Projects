#import Datasets
#Libraries
library(dplyr)
library(tidyverse)
library(readr)

# import dailyActivity_merged.csv

# import sleepDay_merged.csv

str(dailyActivity_merged)
str(sleepDay_merged)

# Check for duplicated observations
activity_duplicated_rows <- duplicated(dailyActivity_merged[, c("Id", "ActivityDate")])

# Print the duplicated rows
print(dailyActivity_merged[activity_duplicated_rows, ])


# Check for duplicated observations
sleep_duplicated_rows <- duplicated(sleepDay_merged[, c("Id", "SleepDay")])

# Print the duplicated rows
print(sleepDay_merged[sleep_duplicated_rows, ])
  
#Found Duplicate observations in sleepDay_Merged need to delete them
SleepDay_Merged_noDuplicates <- sleepDay_merged %>%
      distinct(Id, SleepDay, .keep_all = TRUE)

#Now I performed a left merge on SleepDay_data with dailyActivity_merged
Sleep_daily_Activity_Merged <- merge(SleepDay_Merged_noDuplicates,dailyActivity_merged , by.x = c("Id", "SleepDay"), by.y = c("Id", "ActivityDate"), all.x = TRUE)

#Now I created a new varible with 3 sub categories based on sleep statistics
#Separted Undersleepers sleep less then 7 hours, Good Sleepers sleep bewtween 7 and 9 hours, Over Sleepers sleep more then 9 hours
 
New_Sleep_Activity_Merged <- Sleep_daily_Activity_Merged %>%
       mutate(SleepCategory = case_when(
             TotalMinutesAsleep < 420 ~ "Under_Sleepers",
             between(TotalMinutesAsleep, 420, 540) ~ "Good_Sleepers",
             TotalMinutesAsleep > 540 ~ "Over_Sleepers",
             TRUE ~ "Other"  # Add an "Other" category for values outside the specified ranges
         ))
 
summary(New_Sleep_Activity_Merged)


New_Sleep_Activity_Merged$SleepCategory <- factor(
  New_Sleep_Activity_Merged$SleepCategory,
  levels = c("Under_Sleepers", "Good_Sleepers", "Over_Sleepers")
)

ggplot(New_Sleep_Activity_Merged, aes(x = SleepCategory, fill = SleepCategory)) +
  geom_bar() +
  labs(title = "Total Number of Different Sleeper Types")

ggplot(New_Sleep_Activity_Merged, aes(x = SleepCategory,y= Calories, fill = SleepCategory)) +
  geom_boxplot() +
  labs(title = "Calories Vs Sleep Category")
  