# Load data 
library(magrittr)
SANM <- read.csv(file = "SANM.csv")
SANM$Date <- as.Date(SANM$Date)
SANM$Datenum <- as.numeric(SANM$Date)

Newdata = data.frame(Datenum = seq(17624,by = 1,length.out = 30))
library(ggplot2)

ggplot(data = SANM, mapping = aes(x = Date, y = Close)) + geom_point()  + geom_smooth(mapping = aes(x = Date, y = Close),method = "loess")

ggplot(data = SANM, mapping = aes(x = Date, y = Close)) + geom_point()  + geom_smooth(mapping = aes(x = Date, y = Close),method = "lm")

loess.mdl <- loess(Close ~ Datenum, data = SANM,control = loess.control(surface = "direct"))
predicted <- predict(loess.mdl,newdata = Newdata)

Newdata$Close <- predicted

Stocks <- SANM %>% dplyr::select(Datenum,Close)
Stocks <- rbind.data.frame(Stocks, Newdata)
Stocks <- Stocks %>% dplyr::mutate(Date = lubridate::as_date(Datenum))

Stocks <- Stocks %>% dplyr::mutate(Predicted = c(rep("Actual",503),rep("Predicted",30)))

ggplot(data = Stocks, mapping = aes(x = Date, y = Close, color = Predicted)) + geom_point() + geom_smooth(method = "loess") + ggtitle("Stock price of SANMINA CORP (SANM)")
