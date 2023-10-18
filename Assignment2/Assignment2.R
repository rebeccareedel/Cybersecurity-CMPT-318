#CMPT318 Assignment 2
library(dplyr)
library(lubridate)
library(zoo)

#PART 2
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data$Week = strftime(data$Date, format = "%V")
#print(data)

# Calculate the moving average for Global_intensity
data <- data %>%
  group_by(Week) %>%
  mutate(Moving_Avg_Global_intensity = rollmean(Global_intensity, k = 7, fill = NA))

print(data)

weekly_data <- aggregate(Moving_Avg_Global_intensity ~ Week, data, FUN = mean, na.rm = TRUE)
print(weekly_data)

