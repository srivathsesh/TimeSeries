# Week 3 discussion

# Read in data

snowfall <- readxl::read_xls("annual-snowfall-in-buffalo-19101.xls",skip = 13,col_names = F)
snowfallts <- ts(data = snowfall$X__2,start = c(1910,1),frequency = 1)

# Visualization

plot(snowfallts)

## The data seems (visually) to have a slight trend ; upward from 1920 onwards. The loess smoother seems to convey the same story. 

library(ggplot2)
ggplot(data = snowfall,mapping = aes(x = X__1, y = X__2 )) + geom_point() + geom_smooth()

## It can be seen that the trend does not continue and levels off at atound 1960. It'll be usedul to try ETS models without seasonality - AAN, MAN. The damped and undamped model. Based on the look of the data, a damped model might work well.

# AAN - Holts method

library(forecast)

## We'll use the ets() to make an optimized selection for alpha and beta parameters

AAN <- forecast::ets(snowfallts,model = "AAN")

MAN <- forecast::ets(snowfallts, model = "MAN")

AAN.damp <- forecast::ets(snowfallts,model = "AAN", damped = T)

MAN.damp <- forecast::ets(snowfallts, model = "MAN",damped = T)

AAN

MAN

AAN.damp

MAN.damp

## Based on the AIC's the MAN model seem to be a best model. However I am curious What does ets() say as the best model?

## Oh! Thanks to Rahul Sangole who pointed me to this https://robjhyndman.com/hyndsight/aic/ . I realized I am needing to choose the models based on AIC because the smoothing parameters were optimized based on Maximum Likelihood estimate.

ets.fit <- forecast::ets(snowfallts,model = "ZZZ")

ets.damped <- forecast::ets(snowfallts,model = "ZZZ",damped = T)

ets.fit

ets.damped

## ets of MNN seemed to be the best so far.

plot(ets.fit)

Accuracy.df <- rbind.data.frame(AAN.fit = accuracy(AAN), AAN.damp.fit = accuracy(AAN.damp), 
                          MAN.fit = accuracy(MAN), MAN.damp.fit = accuracy(MAN.damp),
                          MNN.fit = accuracy(ets.fit), AAdN.fit = accuracy(ets.damped))

knitr::kable(round(Accuracy.df,2))