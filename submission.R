# Make submisson

# forecats.rds contains the forecast R object resultsdf

report <- resultsdf %>% 
  # impute forecasts as zero
  dplyr::mutate(Forecast = ifelse(is.na(Forecast), 0, Forecast)) %>% 
  dplyr::select(item_id,shop_id,Method,RMSE) 
  
# Recreatee the submission file
submit <- resultsdf %>% 
  dplyr::select(item_id,shop_id) %>% 
  dplyr::distinct()
  
submit$ID = 1:nrow(submit)
submit$ID = submit$ID-1

submit %<>% dplyr::select(ID,shop_id,item_id) 



bestmdls <- report %>% group_by(shop_id,item_id) %>% 
  filter(rank(RMSE,ties.method = "first")==1)

fcts <- resultsdf %>% 
  dplyr::mutate(Forecast = ifelse(is.na(Forecast), 0, Forecast)) %>% 
  dplyr::select(item_id,shop_id,Method,Forecast) 

bestfct <- bestmdls %>% 
            dplyr::inner_join(.,fcts) %>% 
          dplyr::select(shop_id,item_id,RMSE,Forecast)

finalsubmission <- dplyr::inner_join(submit,bestfct) %>% 
  dplyr::select(ID,Forecast)

colnames(finalsubmission) <- c("ID","item_cnt_month")

write.csv(finalsubmission,"midterm.csv")

toyfit <- lm(MonthlyCts ~  city + item_id + Price + shop_id + date_block_num + lag(MonthlyCts,1) + item_category_id, singular.ok = T,data = MonthlySales.ItemShop)


