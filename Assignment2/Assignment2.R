#CMPT318 Assignment 2

#PART 2
data = read.table('Group_Assignment_1_Dataset.txt', header =TRUE, sep =',') 
data$Date <- as.Date(data$Date, '%d/%m/%Y')
data$Week = strftime(data$Date, format = "%V")
print(data)

weekly_data = aggregate(Global_intensity  ~ Week, data, sum)
print(weekly_data)
