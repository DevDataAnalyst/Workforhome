library(readxl)
library(xlsx)
library(xlsxjars)
library(forecast)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(TSA) # time series libraries
library(tseries) #for runs.test

data_set <- read.csv("pollution_us_2000_2016.csv")

# Extract the NO2 AQI of Houston
Houston <- subset(data_set, City == "Houston" & County == "Harris", select = c(City, Date.Local, NO2.AQI, County))
head(Houston)
tail(Houston)

#Training Data
Houston$Date.Local <- as.Date(Houston$Date.Local) #make sure that your dates are of class Date or similar
Houston<- with(Houston, Houston[(Date.Local >= "2001-01-01" & Date.Local <= "2014-12-31"),]) #remove the data of 2000 and 2016
Houston<- Houston[order(Houston$Date.Local),] # oder the date by Date

#checking "NA" value
sum(is.na(Houston)) #if the result of the sum is 0, then there is no "NA" value.
Houston <- unique(Houston)

#calculating monthly averages for each year
yyyymm <- paste(format(as.POSIXlt(Houston$Date.Local), format = "%y-%m"), "01", sep = "-")
monthly_mean <- tapply(Houston$NO2.AQI, yyyymm, mean)
monthly_mean_train <- as.data.frame(monthly_mean)


#training TimeSeries
mean.ts <- ts(data = monthly_mean, start = c(2001, 1), end = c(2014,12), frequency = 12)

tsq_h = HoltWinters(mean.ts)

#Predictions for Quantity
tsq_hp = data.frame(predict(tsq_h,n.ahead = 12))

### combining the predictions -- Quantity
NO2_Predictions = data.frame(cbind(tsq_hp$fit))

#test data
Houston$Date.Local <- as.Date(Houston$Date.Local) #make sure that your dates are of class Date or similar
Houston<- with(Houston, Houston[(Date.Local >= "2015-01-01" & Date.Local <= "2015-12-31"),]) #remove the data of 2000 and 2016
Houston<- Houston[order(Houston$Date.Local),] # order the date by Date

#calculating monthly averages for each year
yyyymm <- paste(format(as.POSIXlt(Houston$Date.Local), format = "%y-%m"), "01", sep = "-")
monthly_mean <- tapply(Houston$NO2.AQI, yyyymm, mean)
monthly_mean <- as.data.frame(monthly_mean)

write.xlsx(NO2_Predictions,"Houston.xlsx",sheetName = "QTY",row.names = F,append = TRUE)








