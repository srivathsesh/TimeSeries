
library(doParallel)
# distinct values of test was taken
test <- distinct(test)
# list of list of test rows

testing <- function(x){ list(itm_id = test$item_id[x], shop_id = test$shop_id[x])}
#testvalslist <- lapply(1:nrow(test), testing)
testvalslist <- lapply(1:214200, testing)

no_cores <- 25
cl <- makeCluster(no_cores)
clusterExport(cl = cl,varlist = list('MonthlySales.ItemShop','testvalslist'),envir =  .GlobalEnv)
clusterEvalQ(cl,{
  library(magrittr)
  library(dplyr)
  library(forecast)
  library(tidyr)
  library(sweep)
  source('ArrangeData.R')
  source('ForecastMethods.R')
  source('RunForecastMethods.R')
})
# packages <- c('dplyr','magrittr','forecast','tidyr','sweep')
# registerDoParallel(cl)
# 
# results <- foreach(i=1:20, .export=c('MonthlySales.ItemShop', 'ArrangeData','ForecastMethods','RunForecastMethods','testvalslist'), 
#                    .packages= packages) %dopar% {
#   RunForecastMethods(testvalslist[i])
# }

Results2 <- parLapply(cl,testvalslist,RunForecastMethods)

stopCluster(cl)


resultsdf2 <- purrr::reduce(Results2,bind_rows)
