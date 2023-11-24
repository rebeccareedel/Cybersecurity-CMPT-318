# CMPT318 Assignment 3
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read table
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 

# convert data to be numeric
data$Date = as.numeric(data$Date, '%d/%m/%Y')
data$Time = as.numeric(data$Time, format = '%H:%M:%S')
data$weekno = strftime(data$Date, format = "%V")
data$weekday = strftime(data$Date, format = "%a")

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))

data_mon = scaled_data[scaled_data$weekday == 'Mon' & scaled_data$weekno == '01', ]
data_monall = scaled_data[scaled_data$weekday == 'Mon', ]

plot(data_mon$Time, data_mon$Global_reactive_power)

plot(data_monall$Time, data_monall$Global_reactive_power)

# determine a time window for a specific weekday that shows a
#clearly recognizable electricity consumption pattern over a time period  >120 &<240 minutes. 
# for global_reactive_power
# Extract the same time window for each week of the dataset

time_window = data_monall[data_monall$Time >= "2023-11-01 10:00:00" & data_monall$Time < "2023-11-01 14:00:00", ]
time_window = subset(time_window, select = c(Time, Global_active_power, Global_reactive_power, Voltage))
time_window = na.omit(time_window)

# train a number of univariate HMMs, with number of states >=3, <=16
n = rep(c(240),each=52)
model1 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 4, ntimes = n)
fit1 <- fit(model1)
summary(fit1)

model2 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 8, ntimes = n)
fit2 <- fit(model2)
summary(fit2)

model3 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 12, ntimes = n)
fit3 <- fit(model3)
summary(fit3)

model4 <- depmix(response = Global_reactive_power ~ 1, data = time_window, nstates = 16, ntimes = n)
fit4 <- fit(model4)
summary(fit4)

# NOTE: You may not need to train HMMs for each and every number of states within the range by
# making smart choices.

# For each HMM, compute the log-likelihood measure on the training dataset
print(logLik(fit1))
print(logLik(fit2))
print(logLik(fit3))
print(logLik(fit4))

# for each HMM, compute the Bayesian information criterion (BIC) = measure of complexity of model
print(BIC(fit1))
print(BIC(fit2))
print(BIC(fit3))
print(BIC(fit4))
# NOTE: want highest log-like and lowest BIC
#plot(1:4,c(BIC(fit1),BIC(fit2),BIC(fit3),BIC(fit4)),ty="b")
# Collect BIC and log-likelihood values for all fits
BIC_values <- c(BIC(fit1), BIC(fit2), BIC(fit3), BIC(fit4))
print(BIC_values)
LLH_values <- c(logLik(fit1), logLik(fit2), logLik(fit3), logLik(fit4))
print(LLH_values)
# Find the index of the maximum absolute difference between BIC and log-likelihood
best_model_index <- which.max(abs(BIC_values - LLH_values))
# Create a data frame with model numbers, BIC, and log-likelihood values
model_data <- data.frame(Model = 1:4, BIC = BIC_values, LogLik = LLH_values)

# Plot BIC and log-likelihood values
ggplot(model_data, aes(x = Model)) +
  geom_line(aes(y = BIC, color = "BIC"), linetype = "solid", linewidth = 1) +
  geom_line(aes(y = LogLik, color = "Log-Likelihood"), linetype = "solid", linewidth = 1) +
  scale_color_manual(values = c("BIC" = "darkblue", "Log-Likelihood" = "orange")) +
  labs(x = "Model", y = "Values") +
  theme_minimal() +
  ggtitle("BIC and Log-Likelihood Model Comparisons") +
  theme(plot.title = element_text(face = "bold")) +
  geom_vline(xintercept = best_model_index, linetype = "dotted", color = "turquoise", linewidth = 1)
 
