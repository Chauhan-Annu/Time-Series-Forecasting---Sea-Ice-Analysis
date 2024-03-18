
install.packages("Kendall")

library(dplyr)
library(readr)
library(Kendall)
library(tseries)

#import and merge all CSV files into one data frame

data_all_N <- list.files(path = "C:/Users/owner/Documents/NOAA data/North",  # Identify all CSV files
                       pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%                              # Store all files in list
  bind_rows                                         # Combine data sets into one data set 

data_all_N      

df_north = as.data.frame(data_all_N)

sapply(df_north,class)
str(df_north)
dim(df_north)
View(df_north)

unique(df_north$year)
unique(df_north$mo)
unique(df_north$`data-type`)
unique(df_north$region)

boxplot(df_north$extent)
boxplot(df_north$area)

stem(df_north$extent)
stem(df_north$area)

hist(df_north$extent)

hist(df_north$area)

summary(df_north$extent)
summary(df_north$area)


df_north1 = df_north[df_north$area >=0 & df_north$year > 1978, ]
unique(df_north1$year)

attach(df_north)

#df_north2 = df_north1[order(df_north1$year,df_north1$mo),]


unique(df_north1$`data-type`)

boxplot(df_north1$extent,main="North Ice sea extent")
boxplot(df_north1$area,main="North Ice sea Area")

hist(df_north1$extent,main="North Ice sea extent")

hist(df_north1$area)

summary(df_north$year)
summary(df_north1$mo)
summary(df_north1$`data-type`)
summary(df_north1$extent)
summary(df_north1$area)

# Fri Sep 30 01:41:52 2022 ------------------------------

extent_north = df_north1$extent

#Converting to a time series
# There are two critical inputs we must give the function — frequency and start.

extent_north.timeseries = ts(data = extent_north,start = 1979 ,end = 2022 ,frequency = 12)

print(extent.timeseries)

plot(extent_north.timeseries,main="North Ice Extent timeseries Plot")

month_avg_north = df_north1 %>%
  mutate(month = mo ) %>%
  group_by(month) %>%
  summarize(month_avg = mean(extent))


month_extent_north.timeseries = ts(data = month_avg_north$month_avg,start = 1 ,end = 12 ,frequency = 1)

plot(month_extent_north.timeseries,xlab= "months",ylab = "Avg. Extent North Region")

# Testing for Presence of Trend and seasonality 

adf.test(extent_north.timeseries)  

lines(lowess(time(extent_north.timeseries), extent_north.timeseries), col='red')

MannKendall(extent_north.timeseries)

#The test statistic is -0.55, and the two-sided p-value is less than 0.05. 
#We reject the null hypothesis of the test and conclude that a trend exists 
#in the data because the p-value is much less than 0.05.

# We use SeasonalMannKendall Test to account for any seasonality in the data:
  
SeasonalMannKendall(extent_north.timeseries)

#The test statistic is -0.554, and the two-sided p-value is less than 0.05. 
#We reject the null hypothesis of the test and conclude that a seasonality exists 
#in the data because the p-value is much less than 0.05.

components_extent_north <- decompose(extent_north.timeseries)
plot(components_extent_north)

#Decomposing

#Immediately we can “decompose” the time series — which in this case means separating out the 3 main components that make up the time series:
#trend: the long-term trends in the data
#seasonal: the repeated seasonal signal adder
#random: the “left-over” components that aren’t expected from the seasonality or trend components.
#We can easily extract these components and plot them with:


#As we look at the decomposition components, we can visually see how they can add up to our “observed” value (our real values).




# data splitting 

extent_to_train = df_north1[df_north1$year < 2018,]
extent_train = extent_to_train$extent 
train.timeseries = ts(data = extent_train,start = 1979 ,end = 2017 ,frequency = 12)

extent_to_val = df_north1[df_north1$year >= 2018,]
extent_test = extent_to_val$extent 
test.timeseries = ts(data = extent_test,start = 2018 ,end = 2022 ,frequency = 12)


#Fitting with Holt-Winters


HW1 <- HoltWinters(train.timeseries)

HW1 <- HoltWinters(extent_north.timeseries, alpha=0.2, beta=0.1, gamma=0.1)

# Custom HoltWinters fitting
HW2 <- HoltWinters(train.timeseries, alpha=0.2, beta=0.1, gamma=0.1)



HW3 <- HoltWinters(train.timeseries,beta=FALSE, gamma=FALSE)

#Visually evaluate the fits

plot(extent_north.timeseries, ylab="North Hemisphere Ice extent", xlim=c(1979,2022))

lines(HW1$fitted[,1], lty=2, col="red")

lines(HW2$fitted[,1], lty=2, col="red")

plot(HW1)

HW1$SSE

plot(residuals(HW1))
plot(residuals(HW1, type='response'),main="North region Residual Plot")


#Predictions

HW1.pred <- predict(HW1, n.ahead = 49, prediction.interval = TRUE, level=0.95)


sse = sum((HW1.pred - (test.timeseries))^2)
sse


ssr = sum((HW1.pred - mean(test.timeseries))^2)

ssr


sst = sse + ssr


ssr/sst




#Visually evaluate the prediction
plot(extent.timeseries, ylab="North Hemisphere Ice extent", xlim=c(1979.5,2024))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW1.pred[,1], col="red")
lines(HW1.pred[,2], lty=2, col="orange")
lines(HW1.pred[,3], lty=2, col="orange")


##************************South Sea data analysis***********#

library(dplyr)
library(readr)
library(Kendall)

#import and merge all CSV files into one data frame

data_all_S <- list.files(path = "C:/Users/owner/Documents/NOAA data/South",  # Identify all CSV files
                         pattern = "*.csv", full.names = TRUE) %>% 
lapply(read_csv) %>%                              # Store all files in list
bind_rows                                         # Combine data sets into one data set 

data_all_S      

df_south = as.data.frame(data_all_S)

sapply(df_south,class)

str(df_south)
dim(df_south)
View(df_south)

unique(df_south$year)
unique(df_south$mo)
unique(df_south$`data-type`)
unique(df_south$region)

boxplot(df_south$extent)
boxplot(df_south$area)

hist(df_south$extent)

hist(df_south$area)

summary(df_south$extent)
summary(df_south$area)


df_south1 = df_south[df_south$area >=0 & df_south$year > 1978, ]
unique(df_south1$year)

#attach(df_south1)

#df_south2 = df_south1[order(df_south1$year,df_south1$mo),]


unique(df_south1$`data-type`)

boxplot(df_south1$extent,main="South Ice sea extent")
boxplot(df_south1$area,main="South Ice sea extent area")

hist(df_south1$extent,main="South Ice sea extent histogram")

hist(df_south1$area)

summary(df_south1$year)
summary(df_south1$mo)
summary(df_south1$extent)
summary(df_south1$area)

# Mon Dec 12 01:41:52 2022 ------------------------------

extent_south = df_south1$extent

#Converting to a time series
# There are two critical inputs we must give the function — frequency and start.

extent_south.timeseries = ts(data = extent_south,start = 1979 ,end = 2022 ,frequency = 12)

plot(extent_south.timeseries,main="South Region Ice Extent TimeSeries Plot")



month_avg_south = df_south1 %>%
  mutate(month = mo ) %>%
  group_by(month) %>%
  summarize(month_avg = mean(extent))


month_extent_south.timeseries = ts(data = month_avg_south$month_avg,start = 1 ,end = 12 ,frequency = 1)

plot(month_extent_south.timeseries,xlab= "months",ylab = "Avg. Extent South Region")





# Testing for Presence of Trend and seasonality 

lines(lowess(time(extent_south.timeseries), extent_south.timeseries), col='red')

adf.test(extent_south.timeseries)  

MannKendall(extent_south.timeseries)

#The test statistic is 0.587, and the two-sided p-value is less than 0.05. 
#We reject the null hypothesis of the test and conclude that a trend exists 
#in the data because the p-value is much less than 0.05.

# We use SeasonalMannKendall Test to account for any seasonality in the data:

SeasonalMannKendall(extent_south.timeseries)

#The test statistic is 0.597, and the two-sided p-value is less than 0.05. 
#We reject the null hypothesis of the test and conclude that a seasonality exists 
#in the data because the p-value is much less than 0.05.

components_extent_south <- decompose(extent_south.timeseries)
plot(components_extent_south)

#Decomposing

#Immediately we can “decompose” the time series — which in this case means separating out the 3 main components that make up the time series:
#trend: the long-term trends in the data
#seasonal: the repeated seasonal signal adder
#random: the “left-over” components that aren’t expected from the seasonality or trend components.
#We can easily extract these components and plot them with:


#As we look at the decomposition components, we can visually see how they can add up to our “observed” value (our real values).


# data splitting 

extent_to_train_south = df_south1[df_south1$year < 2018,]
extent_train_south = extent_to_train_south$extent 
train_south.timeseries = ts(data = extent_train_south,start = 1979 ,end = 2017 ,frequency = 12)

extent_to_val_south = df_south1[df_south1$year >= 2018,]
extent_test_south = extent_to_val_south$extent 
test_south.timeseries = ts(data = extent_test_south,start = 2018 ,end = 2022 ,frequency = 12)


#Fitting with Holt-Winters


HW1_south <- HoltWinters(train_south.timeseries)
# Custom HoltWinters fitting
HW2_south <- HoltWinters(train_south.timeseries, alpha=0.2, beta=0.1, gamma=0.1)

HW1_south <- HoltWinters(extent_south.timeseries, alpha=0.2, beta=0.1, gamma=0.1)

#Visually evaluate the fits

plot(train_south.timeseries, ylab="South sea Ice extent", xlim=c(1979,2017))

lines(HW1_south$fitted[,1], lty=2, col="yellow")

lines(HW2_south$fitted[,1], lty=2, col="red")

plot(HW1_south,main = "South Region Holt-Winters Filtering")

HW1_south$SSE


plot(residuals(HW1_south, type='response'),main="South region Residual Plot")


#Predictions


HW1_south.pred <- predict(HW1_south, n.ahead = 56, prediction.interval = TRUE, level=0.95)

df_south_pred = data.frame(HW1_south.pred)

str(df_south_pred)
str(extent_test_south)


sse = sum((df_south_pred$fit - (extent_test_south))^2)
sse


ssr = sum((df_south_pred$fit - mean(extent_test_south))^2)

ssr


sst = sse + ssr


ssr/sst




#Visually evaluate the prediction
plot(extent.timeseries, ylab="North Hemisphere Ice extent", xlim=c(1979.5,2024))
lines(HW1$fitted[,1], lty=2, col="blue")
lines(HW1.pred[,1], col="red")
lines(HW1.pred[,2], lty=2, col="orange")
lines(HW1.pred[,3], lty=2, col="orange")





















