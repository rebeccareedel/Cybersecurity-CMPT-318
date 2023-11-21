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

# For deciding on the subset of variables that are most suitable for training your models,
# you need to perform a Principal Component Analysis (PCA)

# Choose a subset of the response variables for training of multivariate
# HMMs on normal electricity consumption data. 

# Provide a 1 proper rational for your final choice of response variables based on your PCA results. 
