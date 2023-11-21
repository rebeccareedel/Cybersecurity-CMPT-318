# CMPT318 Term Project Part 1
# Authors: Mrinal Goshalia, Rebecca Reedel, Asmita Srivastava
install.packages("depmixS4")
library(depmixS4)
library(tidyverse)
library(dplyr)
library(ggplot2)

# read table
data = read.table('TermProjectData.txt', header =TRUE, sep =',') 