### Homework 5 ###
library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyquant)
#Use the lumber price in US dataset  Download datasetfor Time Series Analysis using below steps.

# 1. Check for NAs, if you see any fill NAs using the mean.
library(readxl)
Lumber_Prices_in_US <- read_excel("~/Desktop/SMC/Advanced Analytics/LEC_TimeSeries/Lumber Prices in US.xlsx")
View(Lumber_Prices_in_US)

Lumber_Prices_in_US$`Wholesale Lumber Prices`[is.na(Lumber_Prices_in_US$`Wholesale Lumber Prices`)] = mean(Lumber_Prices_in_US$`Wholesale Lumber Prices`,na.rm = TRUE)
View(Lumber_Prices_in_US)
# the NAs were filled with mean value

which(is.na(Lumber_Prices_in_US$`Wholesale Lumber Prices`))
# the NAs were filled with mean value


#2. Once the dataset is complete, plot the time series and list your observations.
class(Lumber_Prices_in_US)
plot(Lumber_Prices_in_US$DATE, Lumber_Prices_in_US$`Wholesale Lumber Prices`)

#converting the dataset to timeseries
l_prices_ts = ts(Lumber_Prices_in_US[,colnames(Lumber_Prices_in_US)[2]], start = c(1913,1), frequency = 12)
class(l_prices_ts)

# Plot Inflation Time Series
autoplot(l_prices_ts) + ggtitle("Lumber Prices in US") + ylab("Lumber Prices")


# 3. Identify any outlier(s) - what method do you think would be appropriate to use for outlier detection? If you see any outlier - create a new dataset without outlier. If you found any outliers, you should have 2 datasets.
ggplot(data = Lumber_Prices_in_US, aes(x=Lumber_Prices_in_US$DATE, y=Lumber_Prices_in_US$`Wholesale Lumber Prices`)) + geom_point() + 
  geom_smooth(method = 'lm')

#library(fpp2)
tsoutliers(l_prices_ts)

l_prices = data.frame(Lumber_Prices_in_US)
class(l_prices)

colSums(is.na(l_prices))

# Smoothening with outliers with SMA = 5
l_prices$SMA5 = SMA(l_prices$Wholesale.Lumber.Prices, n = 5)
View(l_prices)
ggplot(data = l_prices, aes(x = `DATE`)) +
  geom_line(aes(y = SMA5), color = "blue") +
  geom_line(aes(y = Wholesale.Lumber.Prices), color = "red") +
  labs(title = "Lumber Price in US over Time with Moving Average",
       x = "Date",
       y = "Price") 


# Smoothening with outliers with SMA = 10
l_prices$SMA10 = SMA(l_prices$Wholesale.Lumber.Prices, n = 10)
#View(l_prices)
ggplot(data = l_prices, aes(x = `DATE`)) +
  geom_line(aes(y = SMA10), color = "blue") +
  geom_line(aes(y = Wholesale.Lumber.Prices), color = "red") +
  labs(title = "Lumber Price in US over Time with Moving Average",
       x = "Date",
       y = "Price") 


l_prices$WMA_3 = WMA(l_prices_ts, n=3, wts = c(0.2,0.3,0.5))

l_prices$WMA_4 = WMA(l_prices_ts, n=4, wts = c(0.1,0.1,0.3,0.5))
WMA_ds = data.frame(l_prices$WMA_4)

#View(l_prices)
wma3 = ts(l_prices$WMA_3)
wma4 = ts(l_prices$WMA_4)
sma10 = ts(l_prices$SMA10)
head(na.omit(sma10))

ggplot(data = l_prices, aes(x = `DATE`)) +
  geom_line(aes(y = sma10), color = "darkslategray") +
  geom_line(aes(y = wma4), color = 'darkorchid1') +
  geom_line(aes(y = wma3), color = 'blue') +
  geom_line(aes(y = Wholesale.Lumber.Prices), color = "red") +
  labs(title = "Lumber Price in US over Time with Moving Average",
       x = "Date",
       y = "Price") 

## The plot with SMA 10 definitely looks better, but I think if I go ahead with SMA 10 I might be loosing a lot of data, Hence I choose to go ahead with 
## WMA_4

#Test if the times series dataset is stationary 
adf.test(l_prices)
adf.test(na.omit(wma3))
adf.test(na.omit(wma4))
# Since th ep-value is more that 0.05 that is more than 5%, the data is not stationary

#The SMA 10 

# 5. Split your dataset into train and test datasets.

set.seed(123)

indexs = sample(2, nrow(WMA_ds), replace = T, prob = c(0.8, 0.2))
train_data = WMA_ds[indexs == 1,]
test_data = WMA_ds[indexs == 2,]


indexset = sample(2,nrow(l_prices_ts), replace = T, prob = c(0.8,0.2))
train_dataset = l_prices_ts[indexset == 1,]
test_dataset = l_prices_ts[indexset == 2,]


# 6. Create your model using the train dataset and check for residual plots (use: checkresiduals(your_model_name)). List your observations.

data_ts_arima = auto.arima(train_dataset, seasonal = TRUE)
summary(data_ts_arima)

wma_arima = auto.arima(train_data, seasonal = TRUE)
summary(wma_arima)


#check for resuduals
checkresiduals(data_ts_arima)

checkresiduals(wma_arima)

#The plot with WMA look well distributed than the original dataset

#7. Generate forecasts for the time period in testing dataset and calculate the Mean Absolute Error.
ts_fcst = forecast(test_dataset, h=12)

ts_frcst = forecast(test_data, h = 12)

plot(ts_fcst)
plot(ts_frcst)


# MAE for original dataset and WMA_4
mean(abs(l_prices[,2] - l_prices[,3]),na.rm = TRUE)

mean(abs(l_prices[,2] - l_prices[,6]),na.rm = TRUE)


# My findings:
#It is confusing to choose from smoothening method which one to choose, but WMA seems to be better option for time series data as we are mostly forecasting on the bases of
# latest fluctuations.
# By mistake I did MAE for all the data sets (SMA, WMA, original Dataset), I was able the see that WMA has least error despite SMA gave me better smoothening plot.
# I wish I had more time or may be I was more time efficient to work more on forecasting to make the forecaste narrower. 

# Assuming your time series data is stored in a data frame called 'data'
# and the timestamp column is named 'timestamp'

# Convert timestamp to a time series object
ts_data <- ts(data$beer_production, start = min(data$timestamp), frequency = 12)  # Assuming monthly data

# Define the proportion of data to be used for training
train_proportion <- 0.8  # 80% for training, 20% for testing

# Calculate the number of observations for training
train_size <- floor(length(ts_data) * train_proportion)

# Split the time series data into training and test sets
train_ts <- window(ts_data, start = start(ts_data), end = start(ts_data) + train_size - 1)
test_ts <- window(ts_data, start = start(ts_data) + train_size)

# Optionally, you can convert the train_ts and test_ts back to a data frame if needed
# train_df <- data.frame(timestamp = time(train_ts), beer_production = as.vector(train_ts))
# test_df <- data.frame(timestamp = time(test_ts), beer_production = as.vector(test_ts))
