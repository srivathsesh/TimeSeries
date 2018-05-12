ForecastMethods <- function(itm_id,shop_id){
  
  source("ArrangeData.R")
  library(dplyr)
  library(magrittr)
  library(forecast)
  library(sweep)
  #------------------------------------------------------------
  #                     Get data
  #-----------------------------------------------------------
  # needs MOnthlyShop.Items

  
tsdata <- ArrangeData(store_ids = shop_id,item_ids = itm_id)
#tsdata[which(is.na(tsdata[,"Price"])),"price"] <- 0
  #standardize column names
  colnames(tsdata) <- c("date_block_num","Sales","Price")
  
  #------------------------------------------------------------
  #                    Fit models
  #------------------------------------------------------------
  
  # naive fit
  
  tryCatch(expr = {naive.fit <- naive(tsdata[,"Sales"],h=1)
  nfct <- forecast(naive.fit,h = 1)
  fdf = data.frame(item_id = itm_id, 
                   shop_id = shop_id,
                   Method = "Naive",
                   Forecast = as.numeric(trunc(nfct$mean))
  )
  Performance = data.frame(accuracy(naive.fit))
  },
  error = function(e){
    fdf = data.frame(item_id = itm_id, 
                     shop_id = shop_id,
                     Method = "Naive",
                     Forecast = NA
    )
    Performance = data.frame(ME = NA, RMSE = NA, MAE = NA, MAPE = NA, MASE = NA, ACF1 = NA)
    },
    finally = print(paste("Naive model complete for item_id:",itm_id,"; shop_id:",shop_id))
  
  )
  
  
  
  # seasonal naive fit
  
  tryCatch(expr = {snaive.fit <- snaive(tsdata[,"Sales"])
  sfct <- forecast(snaive.fit,h=1)
  fdf <- rbind.data.frame(fdf,data.frame(item_id = itm_id, 
                              shop_id = shop_id,
                              Method = "SNaive",
                              Forecast = as.numeric(trunc(sfct$mean))
                              ))
   Performance %<>% rbind.data.frame(accuracy(snaive.fit))
  },
  error = function(e){
    fdf <- rbind(fdf,data.frame(item_id = itm_id, 
                                shop_id = shop_id,
                                Method = "SNaive",
                                Forecast = NA ))
    Performance %<>%  rbind.data.frame(ME = NA, RMSE = NA, MAE = NA, MAPE = NA, MASE = NA, ACF1 = NA)
  },
  finally = print(paste("Seasonal Naive model complete for item_id:",itm_id,"; shop_id:",shop_id))
  )
  
 
  # linear regression fit
  tryCatch(expr = {
    lm.fit <- tslm(Sales ~ Price,data = tsdata, singular.ok = T)
    lmfct <- forecast(lm.fit,h = 1,newdata = meanf(tsdata[,"Price"], h = 1)$mean)
    fdf <- rbind(fdf,data.frame(item_id = itm_id, 
                                shop_id = shop_id,
                                Method = "linear",
                                Forecast = as.numeric(trunc(lmfct$mean))))
    Performance %<>% rbind.data.frame(accuracy(lm.fit))
    },
  error = function(e){
    fdf <- rbind(fdf,data.frame(item_id = itm_id, 
                                shop_id = shop_id,
                                Method = "linear",
                                Forecast = NA ))
    Performance %<>%  rbind.data.frame(ME = NA, RMSE = NA, MAE = NA, MAPE = NA, MASE = NA, ACF1 = NA)
  },
  finally = print(paste("linear model complete for item_id:",itm_id,"; shop_id:",shop_id))
  )
  
  
  # auto.arima fit
  
  tryCatch(expr = {
    auto.arima.fit <- auto.arima(tsdata[,"Sales"])
    arimafct <- forecast(auto.arima.fit,h=1)
    fdf <- rbind(fdf,data.frame(item_id = itm_id, 
                                shop_id = shop_id,
                                Method = "arima.xreg",
                                Forecast = as.numeric(trunc(arimafct$mean ))))
    Performance %<>% rbind.data.frame(accuray(auto.arima.fit))
  },
  error = function(e){
    fdf <- rbind(fdf,data.frame(item_id = itm_id, 
                                shop_id = shop_id,
                                Method = "arima.xreg",
                                Forecast = NA ))
    Performance %<>%  rbind.data.frame(ME = NA, RMSE = NA, MAE = NA, MAPE = NA, MASE = NA, ACF1 = NA)
  },
  finally = print(paste("ARIMA model complete for item_id:",itm_id,"; shop_id:",shop_id))
  )
  
  
 return(cbind.data.frame(fdf,Performance))
  
  
}