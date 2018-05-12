RunForecastMethods <- function(x){
  tryCatch(expr = ForecastMethods(x$itm_id,x$shop_id), 
           error = function(e){data.frame(item_id = x$itm_id, 
                                          shop_id = x$shop_id,
                                          Method = "None",
                                          Forecast = NA ,
                                          ME = NA, RMSE = NA, MAE = NA, MAPE = NA, MASE = NA, ACF1 = NA)}
           )
}


