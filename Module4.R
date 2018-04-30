# Week 3 discussion

library(forecast)
library(ggplot2)

# Read in data

snowfall <- readxl::read_xls("annual-snowfall-in-buffalo-19101.xls",skip = 13,col_names = F)
snowfallts <- ts(data = snowfall$X__2,start = c(1910,1),frequency = 1)

# Visualization

autoplot(snowfallts)

## The data seems (visually) to have a slight trend ; upward from 1920 onwards. The loess smoother seems to convey the same story. 


ggplot(data = snowfall,mapping = aes(x = X__1, y = X__2 )) + geom_line() + geom_smooth()

## It can be seen that the trend does not continue and levels off at atound 1960. It'll be usedul to try ETS models without seasonality - AAN, MAN. The damped and undamped model. Based on the look of the data, a damped model might work well.

## Lets split that data into training and test

training <- window(snowfallts, end = c(1966,1))
# AAN - Holts method

library(forecast)

## We'll use the ets() to make an optimized selection for alpha and beta parameters

AAN <- forecast::ets(training,model = "AAN")

MAN <- forecast::ets(training, model = "MAN")

AAN.damp <- forecast::ets(training,model = "AAN", damped = T)

MAN.damp <- forecast::ets(training, model = "MAN",damped = T)



## Based on the AIC's the MAN model seem to be a best model. However I am curious What does ets() say as the best model?


ets.fit <- forecast::ets(training,model = "ZZZ")

ets.damped <- forecast::ets(training,model = "ZZZ",damped = T)

Models <- list(AAN, MAN, AAN.damp, MAN.damp, ets.fit, ets.damped)

ETSSelections <- as.data.frame(t(sapply(Models,sweep::sw_glance)))

knitr::kable(ETSSelections)

## ets of MNN seemed to be the best so far. The forecast for the next 6 years are made.

forecast.ets <- forecast(ets.fit, h = 6)
autoplot(forecast.ets)

## Now lets fit an ARIMA model ... 
ARIMA.fit <- auto.arima(y = training)

summary(ARIMA.fit)

# ARIMA(0,1,1) which is a  single differencing that utilizes error of prior year to generate the forecast. This is almost like the ETS model. Below is the table of model performances

forecast.ARIMA <- forecast(ARIMA.fit, h = 6)
autoplot(forecast.ARIMA)
Accuracy.df <- rbind.data.frame(MNN.fit = accuracy(forecast.ets,snowfallts),
                                ARIMA = accuracy(forecast.ARIMA,snowfallts))

knitr::kable(round(Accuracy.df,2))

# It can be seen that ets and the ARIMA model are very close in its performances.
