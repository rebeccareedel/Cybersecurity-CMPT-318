#CMPT318 Assignment 2
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)

#PART 2
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Week = strftime(data$Date, format = "%V")

# calculate the moving average to get smoothed week for Global_intensity
data = data %>%
  group_by(Week) %>%
  mutate(Moving_Avg_Global_intensity = rollmean(Global_intensity, k = 7, fill = NA))

# calculate average smoothed week
weekly_data = aggregate(Moving_Avg_Global_intensity ~ Week, data, FUN = mean, na.rm = TRUE)

# calculates anomaly score for every week
data$AnomalyScore = abs(data$Moving_Avg_Global_intensity - weekly_data$Moving_Avg_Global_intensity[match(data$Week, weekly_data$Week)])

# create anomaly score table
anomaly_table = data.frame(Week = data$Week, AnomalyScore = data$AnomalyScore)
anomaly_table = na.omit(anomaly_table)

# get week with highest anomaly score and the least anomaly score
most_anomalous_week = subset(anomaly_table, AnomalyScore == max(anomaly_table$AnomalyScore))
least_anomalous_week = subset(anomaly_table, AnomalyScore == min(anomaly_table$AnomalyScore))

# get highest and lowest anomaly score week data for graphing
most_anomalous_data = data[data$Week == most_anomalous_week$Week, ]
least_anomalous_data = data[data$Week == least_anomalous_week$Week, ]

# plot the data
ggplot(data, aes(x = Date, y = Moving_Avg_Global_intensity)) +
  geom_line(aes(color = "Smoothened Weeks")) +
  geom_line(data = most_anomalous_data, aes(x = Date, y = Moving_Avg_Global_intensity, color = "Most Anomalous Week")) +
  geom_line(data = least_anomalous_data, aes(x = Date, y = Moving_Avg_Global_intensity, color = "Least Anomalous Week")) +
  scale_color_manual(values = c("Smoothened Weeks" = "darkblue", "Most Anomalous Week" = "orange", "Least Anomalous Week" = "turquoise")) +
  labs(title = "Comparing Weekly Global Intensities", y = "Average Global Intensity") +
  theme_minimal()

