# CMPT318 Term Project Part 1
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read table
data = read.table('TermProjectData.txt', header =TRUE, sep =',') 

# PART 1. FEATURE ENGINEERING

# scale the raw data using standardization prior to applying PCA.
# convert data to be numeric
data$Date = as.Date(data$Date, '%d/%m/%Y')
data$Time = as.POSIXct(data$Time, format = '%H:%M:%S')
data$weekno = strftime(data$Date, format = "%V")
data$weekday = strftime(data$Date, format = "%a")

# scale data
scaled_data = data %>% mutate(across(where(is.numeric), scale))

# For deciding on the subset of variables that are most suitable for training your models,
# you need to perform a Principal Component Analysis (PCA)

# Choose a subset of the response variables for training of multivariate
# HMMs on normal electricity consumption data. 

# Provide a 1 proper rational for your final choice of response variables based on your PCA results. 

