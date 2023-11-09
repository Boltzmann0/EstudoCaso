#Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#Import Data
novembro_2022 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202211-divvy-tripdata.csv")
dezembro_2022 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202212-divvy-tripdata.csv")
janeiro_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202301-divvy-tripdata.csv")
fevereiro_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202302-divvy-tripdata.csv")
marco_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202303-divvy-tripdata.csv")
abril_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202304-divvy-tripdata.csv")
maio_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202305-divvy-tripdata.csv")
junho_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202306-divvy-tripdata.csv")
julho_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202307-divvy-tripdata.csv")
agosto_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202308-divvy-tripdata.csv")
setembro_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202309-divvy-tripdata.csv")
outubro_2023 <- read.csv("C:/Users/Lucas/OneDrive/EstudoCaso/202310-divvy-tripdata.csv")


#Data Validation
colnames(novembro_2022)
colnames(dezembro_2022)
colnames(janeiro_2023)
colnames(fevereiro_2023)
colnames(marco_2023)
colnames(abril_2023)
colnames(maio_2023)
colnames(junho_2023)
colnames(julho_2023)
colnames(agosto_2023)
colnames(setembro_2023)
colnames(outubro_2023)

# Total number of rows
sum(nrow(novembro_2022) + nrow(dezembro_2022) + nrow(janeiro_2023) 
    + nrow(fevereiro_2023) + nrow(marco_2023) + nrow(abril_2023) 
    + nrow(maio_2023) + nrow(junho_2023) + nrow(julho_2023)
    + nrow(agosto_2023) + nrow(setembro_2023) + nrow(outubro_2023))


# Combine data from all months into a single dataset
trip_final <- rbind(novembro_2022, dezembro_2022, janeiro_2023, fevereiro_2023, marco_2023, 
                    abril_2023, maio_2023, junho_2023, julho_2023, agosto_2023, setembro_2023, outubro_2023)


# Save the combined files
write.csv(trip_final,file = "C:/Users/Lucas/OneDrive/EstudoCaso/trip_final.csv",row.names = FALSE)

# Setting global variable size to inf
options(future.globals.maxSize = Inf)

# Check the structure of the combined dataset
str(trip_final)

# View the first few rows of the combined dataset
View(head(trip_final))

# View the last few rows of the combined dataset
View(tail(trip_final))

# Check the dimensions of the combined dataset
dim(trip_final)

# Summarize the data contained in the combined dataset
summary(trip_final)

# List the column names in the combined dataset
names(trip_final)

#Data Cleaning

#Count rows with "na" values
colSums(is.na(trip_final))

#Remove missing
clean_trip_final <- trip_final[complete.cases(trip_final), ]
#Remove duplicates
clean_trip_final <- distinct(clean_trip_final)
#Remove data with greater start_at than end_at
clean_trip_final<- clean_trip_final %>% 
  filter(started_at < ended_at)
#Remove na
clean_trip_final <- drop_na(clean_trip_final)
clean_trip_final <- remove_empty(clean_trip_final)
clean_trip_final <- remove_missing(clean_trip_final)

#Check Cleaned data
colSums(is.na(clean_trip_final))
View(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))

#Renaming column for better context
clean_trip_final <- rename(clean_trip_final, costumer_type = member_casual, bike_type = rideable_type)

#Separate date in date, day, month, year for better analysis
clean_trip_final$date <- as.Date(clean_trip_final$started_at)
clean_trip_final$week_day <- format(as.Date(clean_trip_final$date), "%A")
clean_trip_final$month <- format(as.Date(clean_trip_final$date), "%b_%y")
clean_trip_final$year <- format(clean_trip_final$date, "%Y")

#Separate column for time
clean_trip_final$time <- as.POSIXct(clean_trip_final$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final$time <- format(clean_trip_final$time, format = "%H:%M")

#Add ride length column
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

#Select the data we are going to use
clean_trip_final <- clean_trip_final %>% 
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length>1440,] 
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length<5,] 

#Save the cleaned data
write.csv(clean_trip_final,file = "clean_trip_final.csv",row.names = FALSE)