setwd("D:/Ders/5_Spring 2022 Lectures/IE 360/Project")

# Set the date of which data is provided from the server.
date_data = '2022-06-02'

# Import required libraries

library(lubridate)
library(maditr)

# Read the data 
weather_file_name = paste(as.character(date_data),'_weather.csv', sep='')
prod_file_name = paste(as.character(date_data),'_production.csv', sep='')
weatherdata<- read.csv(weather_file_name)
prod_data<- read.csv(prod_file_name)

# Convert data from long to wide format
weather_data<-dcast(weatherdata, date + hour ~ variable+lat+lon, value.var = "value")

# Convert the datetime column to proper type
prod_data$datetime = as.POSIXct(paste(prod_data$date, prod_data$hour), format="%Y-%m-%d %H")
weather_data$datetime = as.POSIXct(paste(weather_data$date, weather_data$hour), format="%Y-%m-%d %H")
production = prod_data

# Merge the production and weather data
combined_data = merge(weather_data, prod_data, by='datetime', all.x = TRUE)
combined_data = data.table(combined_data)

# Fill the missing production values in the training data

# First
missings = combined_data[is.na(combined_data$date.y)]
x <- production[385:552,]
x = data.table(x)

x_mean = x[date != '2021-02-18' & date !='2021-02-24', .(mean(production)),hour]
combined_data[date.x=='2021-02-20']$production = x_mean$V1

# Second (3 consecutive days are missing)
x <- production[4441:4584,]
x = data.table(x)
x_mean = x[,(mean(production)),hour]
combined_data[date.x=='2021-08-09']$production = x_mean$V1
combined_data[date.x=='2021-08-10']$production = x_mean$V1
combined_data[date.x=='2021-08-11']$production = x_mean$V1

# Third (2 consecutive days are missing)
x <- production[8137:8280,]
x = data.table(x)
x_mean = x[,(mean(production)),hour]
combined_data[date.x=='2022-01-13']$production = x_mean$V1
combined_data[date.x=='2022-01-14']$production = x_mean$V1


combined_data = combined_data[date.x<'2022-06-01']


# Create a new variable, moving average smoothed production.
combined_data$capacity = 0

start = as.Date('2021-02-11')
end = as.Date(Sys.Date()-2)

rolling_mean_days = as.list(seq(start, end, by='1 day'))
l <- length(rolling_mean_days)
rolling_mean_no <- 21
m <- (rolling_mean_no-1)/2
for(i in m:(l)){
  for(j in 0:23){
    poi = mean(combined_data[(i-m)*24+(0:(rolling_mean_no-1))*24+1+j]$production)
    combined_data[(i)*24+j+1]$capacity <- poi
  }
}

# Fill the last days for new variable, with the latest available value.
fill_start = as.Date('2022-05-21')
fill_end = as.Date('2022-06-01')
autofill_mean_days = as.list(seq(fill_start, fill_end, by='1 day'))
last_means = combined_data[date.x==fill_start]$capacity
for (day in autofill_mean_days){
  combined_data[(date.x==day),]$capacity = last_means
}


# Fill the first days for new variable, with the first available value.
fill_start = as.Date('2021-02-01')
fill_end = as.Date('2021-02-11')
autofill_mean_days = as.list(seq(fill_start, fill_end, by='1 day'))
first_means = combined_data[date.x==fill_end]$capacity
for (day in autofill_mean_days){
  combined_data[(date.x==day),]$capacity = first_means
}


# Create normalized production variable.
combined_data[,normalized_production:=production/capacity]
combined_data[(capacity==0)]$normalized_production = 0

# Set normalize production values bigger than 3 to 0, because they are outliers.
combined_data[normalized_production>3,]$normalized_production <- 0

# Set outliers to 0.
combined_data[normalized_production>2 & hour.x ==5 , ]$normalized_production <- 0

# Create a month variable

combined_data[,month:=month(as.POSIXlt(combined_data$date.x, format="%Y-%m-%d"))]


# Create a binary temperature value. Takes value of 1 if mean of temperatures
# for each location is bigger than a specific value (300).

#combined_data[,binary_temp:=0]

#for (i in 1:length(combined_data[complete.cases(combined_data)]$TEMP_36.5_33.5)){
  
#  if((combined_data[i,TEMP_36.25_33]+combined_data[i,TEMP_36.5_33]+combined_data[i,TEMP_36.75_33]+combined_data[i,TEMP_36.25_33.25]+combined_data[i,TEMP_36.5_33.25]+combined_data[i,TEMP_36.75_33.25]+combined_data[i,TEMP_36.25_33.5]+combined_data[i,TEMP_36.5_33.5]+combined_data[i,TEMP_36.75_33.5])/9>=300){
 #   combined_data[i]$binary_temp<-1 
  #}   
  #else{
  #  combined_data[i]$binary_temp<-0 }
#}

# Convert type of month variable to factor.
combined_data$month <- as.factor(combined_data$month)

# Create separate datatables for each hour that we will make predictions for.

hoursix <- combined_data[hour.x==6]
hoursix <- hoursix[,-c(40,41,44)]

hourseven <- combined_data[hour.x==7]
hourseven <- hourseven[,-c(40,41,44)]

houreight <- combined_data[hour.x==8]
houreight <- houreight[,-c(40,41,44)]

hournine <- combined_data[hour.x==9]
hournine <- hournine[,-c(40,41,44)]

hourten <- combined_data[hour.x==10]
hourten <- hourten[,-c(40,41,44)]

houreleven <- combined_data[hour.x==11]
houreleven <- houreleven[,-c(40,41,44)]

hourtwelve <- combined_data[hour.x==12]
hourtwelve <- hourtwelve[,-c(40,41,44)]

hourthirteen <- combined_data[hour.x==13]
hourthirteen <- hourthirteen[,-c(40,41,44)]

hourfourteen <- combined_data[hour.x==14]
hourfourteen <- hourfourteen[,-c(40,41,44)]

hourfifteen <- combined_data[hour.x==15]
hourfifteen <- hourfifteen[,-c(40,41,44)]

hoursixteen <- combined_data[hour.x==16]
hoursixteen <- hoursixteen[,-c(40,41,44)]

hourseventeen <- combined_data[hour.x==17]
hourseventeen <- hourseventeen[,-c(40,41,44)]

houreighteen <- combined_data[hour.x==18]
houreighteen <- houreighteen[,-c(40,41,44)]

hournineteen <- combined_data[hour.x==19]
hournineteen <- hournineteen[,-c(40,41,44)]
