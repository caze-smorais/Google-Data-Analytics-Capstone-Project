getwd()
setwd("~/Documents/Google_data_analysis/Module 8/Project/2021")

library(tidyverse)  #helps wrangle data
# Use the conflicted package to manage conflicts
library(conflicted)
library(dplyr)
# Set dplyr::filter and dplyr::lag as the default choices
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# All csv files have been previously checked for spaces between the columns names

#=====================
# STEP 1: COLLECT DATA
#=====================

# # Upload Divvy datasets (csv files) here
jan <- read_csv("202301-divvy-tripdata.csv")
feb <- read_csv("202302-divvy-tripdata.csv")
mar <- read_csv("202303-divvy-tripdata.csv")
apr <- read_csv("202304-divvy-tripdata.csv")
may <- read_csv("202305-divvy-tripdata.csv")
jun <- read_csv("202306-divvy-tripdata.csv")
jul <- read_csv("202307-divvy-tripdata.csv")
aug <- read_csv("202308-divvy-tripdata.csv")
sep <- read_csv("202309-divvy-tripdata.csv")
oct <- read_csv("202310-divvy-tripdata.csv")
nov <- read_csv("202311-divvy-tripdata.csv")
dec <- read_csv("202312-divvy-tripdata.csv")

#====================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================

rides <- bind_rows(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
rides
#Sort the data frame by ride_length in ascending order by ride start



rides <- rides %>%
arrange(started_at)

rides


#======================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#======================================================

# Inspect the new table that has been created
colnames(rides)  # List of column names
nrow(rides)  # Number of rows
dim(rides)  # Dimensions of the data frame
head(rides)  #See the first 6 rows of the data frame.
tail(rides) #See the last 6 rows of the data frame
str(rides)  #See list of columns and data types (numeric, character, etc)
summary(rides)  #Statistical summary of data. Mainly for numerics

# Counting the number of NA's
na_count <- sum(is.na(rides)) 
na_count

# Counting the number of NA's for every single column (Identifying where the NA's are)
na_count_per_column <- colSums(is.na(rides))
na_count_per_column

# Removing the NA's (if NA's are on the columns we will use for analysis)
#clean_rides <- rides[complete.cases(rides),] # remove rows with NA's
#clean_rides <- droplevels(clean_rides) # remove unused levels from factors (if any)

#nrow(clean_rides)
#dim(clean_rides)

#if yes, correct code accordingly after this message

#checking if there is only two names on the member_casual column
unique(rides$member_casual)

# Adding columns that list the date, month, day, and year of each ride separately
rides$date <- as.Date(rides$started_at) #The default format is yyyy-mm-dd
rides$month <- format(as.Date(rides$date), "%m")
rides$day <- format(as.Date(rides$date), "%d")
rides$year <- format(as.Date(rides$date), "%Y")
rides$day_of_week <- format(as.Date(rides$date), "%A")

# Add a "ride_length" calculation to all_trips and convert to minutes for better information display (it's in seconds by default)
rides$ride_length <- difftime(rides$ended_at, rides$started_at, units = "mins")

# Inspect the structure of the columns
str(rides)

# Convert "ride_length" to numeric to run calculations on the data
rides$ride_length <- as.numeric(as.character(rides$ride_length))
is.numeric(rides$ride_length)


summary(rides)


# Removing "bad" data 
#removing rides with negatives values and considering only bike rides with more than 1 min

rides_v2 <- rides[!(rides$ride_length<1),]

nrow(rides)
nrow(rides_v2)

summary(rides_v2)

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================

# Descriptive analysis on ride_length (all figures in minutes)
mean(rides_v2$ride_length) #straight average (total ride length / rides)
median(rides_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(rides_v2$ride_length) #longest ride

#or in a shorter version
summary(rides_v2$ride_length)

# Compare members and casual users
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = mean)
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = median)
aggregate(rides_v2$ride_length ~ rides_v2$member_casual, FUN = max)

# Average ride time by each day for members vs casual users
rides_v2$day_of_week <- ordered(rides_v2$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
aggregate(rides_v2$ride_length ~ rides_v2$member_casual + rides_v2$day_of_week, FUN = mean)


# analyze ridership data by type and weekday
rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>%  # creates weekday field using wday() and specify week_start = 1 to start the week on Monday
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


# Creating a visualization of the number of rides by rider type week
rides_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>%  # creates weekday field using wday() and specify week_start = 1 to start the week on Monday
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = "drop") %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Weekday", y = "Number of Rides", fill = "Member Type", title = "Number of Rides by Member Type and Weekday 2023") +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 600000)) # This line changes the number format on the y-axis to display integral numbers instead exponential notation and a limitfor better information display



# Creating a visualization for average duration week
rides_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE, week_start = 1)) %>%  # creates weekday field using wday() and specify week_start = 1 to start the week on Monday
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = "drop") %>%
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(x = "Weekday", y = "Average Duration (minutes)", fill = "Member Type", title = "Average Ride Duration by Member Type and Weekday 2023")+
  scale_y_continuous(limits = c(0, 40))


#Creating a visualization of the number of rides by rider type for the year
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

rides_v2 %>%
  mutate(month = month(started_at)) %>%  # Extract month from started_at column
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = "drop") %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = factor(month, labels = month_names), y = number_of_rides, fill = member_casual)) +  # Convert month to month names
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Number of Rides", fill = "Member Type", title = "Number of Rides by Member Type and Month 2023") +
  scale_y_continuous(labels = scales::number_format(), limits = c(0, 500000))

# Creating a visualization for average duration year
rides_v2 %>%
  mutate(month = month(started_at)) %>%  # Extract month from started_at column
  group_by(member_casual, month) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length), .groups = "drop") %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = factor(month, labels = month_names), y = average_duration, fill = member_casual)) +  # Convert month to month names
  geom_col(position = "dodge") +
  labs(x = "Month", y = "Average Duration (minutes)", fill = "Member Type", title = "Average Ride Duration by Member Type and Month 2023") +
  scale_y_continuous(limits = c(0, 40))

#creating a pie chart to compare the number of members and casual rides during the year
member_counts <- table(rides_v2$member_casual)
percentages <- round(100 * member_counts / sum(member_counts), 1)
labels <- paste(names(member_counts), ": ", percentages, "%", sep = "")
pie(member_counts, labels = labels, main = "Distribution of The Rides by Member Type 2023")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================
# Create a csv file that we will visualize in Excel, Tableau, or my presentation software
counts <- aggregate(rides_v2$ride_length ~ rides_v2$member_casual + rides_v2$day_of_week, FUN = mean)
write.csv(counts, file = 'avg_ride_length.csv')
