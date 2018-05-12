ArrangeData <- function(data = MonthlySales.ItemShop, CategorySuperlative = NULL, 
                        item_category_id = NULL, CategoryDesc = NULL,store_ids = NULL, 
                        item_ids = NULL,response = NULL, aggCriteria = NULL,Collapse = T,...){
  opts <- list(...) # How can we use High level Category filters?
  #
  #browser()
  # Get Item.Shops for aggregation
  if(!is.null(aggCriteria)){
    
    stopifnot(!is.null(response))
    criteria <- names(aggCriteria)
    if(is.element("item_ids",criteria)){
      aggItemIDs <- aggCriteria$item_ids
    } else{
      aggItemIDs <- NULL
    }
    
    if(is.element("CategorySuper", criteria)) {
      # using lexical scoping here
      aggItemIDs <- c(aggItemIDs,
                      ItemsReference %>% dplyr::select(item_id,!!aggCriteria$CatergorySuper) %>%
                        dplyr::filter_at(2,any_vars(as.integer(.)== 2)) %>% dplyr::select(item_id) %>% .$item_id)
      
      
    }
    if(is.element("category_id", criteria)){
      aggItemIDs <- c(aggItemIDs,
                      ItemsReference %>% dplyr::select(item_id,item_category_id) %>% 
                        dplyr::filter(item_category_id %in% !!aggCriteria$category_id) %>% 
                        .$item_id)
    }
    if(is.element("shop_ids",criteria)){
      aggShopIDs <- aggCriteria$shop_id
    } else aggShopIDs <- NULL
    
    if(is.element("location",criteria)) {
      locFlag = T
    } else locFlag = F
  }
  
  # filter by super category
  if(!is.null(CategorySuperlative)){
    data %<>% dplyr::filter_at(vars(which(colnames(.) %in% CategorySuperlative)),any_vars(as.integer(.)== 2))
  }
  
  # filter by item_category_id
  if(!is.null(item_category_id)){
    data %<>% dplyr::filter(item_category_id %in% !!item_category_id)
  }
  if(!is.null(CategoryDesc)){
    data %<>% dplyr::filter(Category %in% !!CategoryDesc)
  }
  if(!is.null(item_ids)) {
    data %<>% dplyr::filter(item_id %in% !!item_ids)
  }
  if(!is.null(store_ids)){
    data %<>% dplyr::filter(shop_id %in% !!store_ids)
  }
  
  # Aggregation
  
  if(!is.null(aggCriteria)){
    ResponseCriteria = names(response)
    if(all(is.element(c("shop_ids","item_ids"),ResponseCriteria))){
      Response <- data  %>% 
        dplyr::filter(item_id == !!response$item_id, shop_id == !!response$shop_id) %>% 
        dplyr::select(date_block_num,item_id,shop_id,MonthlyCts,Price) %>% 
        dplyr::arrange(date_block_num,item_id,shop_id) %>% 
        dplyr::mutate(Item.Shop = as.character(item_id + shop_id/100))
    } else{
      if(all(is.element(c("shop_ids","category_id"),ResponseCriteria))){
        Response <- data  %>% 
          dplyr::filter(item_category_id == !!response$category_id, shop_id == !!response$shop_id) %>% 
          dplyr::select(date_block_num,shop_id,item_category_id,MonthlyCts,Price) %>% 
          dplyr::arrange(date_block_num,item_category_id,shop_id) %>% 
          dplyr::group_by(date_block_num,item_category_id,shop_id) %>% 
          dplyr::summarise(MonthlyCts = sum(MonthlyCts, na.rm = T), Price = mean(Price,na.rm = T)) %>% 
          dplyr::mutate(itmCatid.ShopID = as.character(item_category_id + shop_id/100))
        
        
        Response %<>% dplyr::ungroup() %>%  
          tidyr::gather(variable, value, -c(date_block_num, itmCatid.ShopID)) %>% 
          tidyr::unite(temp, itmCatid.ShopID, variable) %>% 
          tidyr::spread(temp, value) %>% dplyr::select(-ends_with("_id"))
      }
      else{
        if(all(is.element(c("shop_ids","CategorySuper"),ResponseCriteria))){
          
          aggResItemIDs <-  ItemsReference %>% dplyr::select(item_id,!!ResponseCriteria$CatergorySuper) %>%
            dplyr::filter_at(2,any_vars(as.integer(.)== 2)) %>% dplyr::select(item_id) %>% .$item_id
          
          Response <- data  %>% 
            dplyr::filter(item_id == !!aggResItemIDs, shop_id == !!response$shop_id) %>% 
            dplyr::select(date_block_num,item_id,shop_id,MonthlyCts,Price) %>% 
            dplyr::arrange(date_block_num,item_id,shop_id) %>% 
            dplyr::group_by(date_block_num,item_id,shop_id) %>% 
            dplyr::summarise(MonthlyCts = sum(MonthlyCts, na.rm = T), Price = mean(Price,na.rm = T)) %>% 
            dplyr::mutate(Item.ShopID = as.character(item_id + shop_id/100))
          
          Response %<>% dplyr::ungroup() %>%  
            tidyr::gather(variable, value, -c(date_block_num, itmCatid.ShopID)) %>% 
            tidyr::unite(temp, Item.ShopID, variable) %>% 
            tidyr::spread(temp, value) #%>% dplyr::select(-ends_with("_id"))
        }
      }
    }
    
    responseitemids <- Response %>% dplyr::ungroup() %>% distinct(item_id) %>% .$item_id
    responseshopids <- Response %>% dplyr::ungroup() %>% distinct(shop_id) %>% .$shop_id
    
    Predictors.ItemIDs <- setdiff(aggItemIDs,responseitemids)
    Predictors.ShopIDs <- setdiff(aggShopIDs,responseshopids)
    
    Predictors <- data %>% 
      dplyr::filter(shop_id %in% !!Predictors.ShopIDs,
                    item_id %in% !!Predictors.ItemIDs) %>% 
      dplyr::arrange(date_block_num,item_id,shop_id)
    
    if(locFlag){
      if(nrow(Predictors) > 0){
        Predictors %<>% dplyr::group_by(date_block_num,item_id,city) %>% 
          dplyr::summarise(MonthlyCts = sum(MonthlyCts,na.rm = T),Price = mean(Price,na.rm = T))
        if(!Collapse){
          Predictors %<>% dplyr::mutate(Item.City = paste0(as.character(item_id),"_",city)) %>% 
            tidyr::gather(variable,value, -c(date_block_num,Item.City)) %>% 
            tidyr::unite(temp,Item.City,variable) %>%
            tidyr::spread(temp,value) 
        } else {
          Predictors %<>%  dplyr::left_join(.,ItemsReference[,c("item_id","Category")]) %>% 
            dplyr::group_by(date_block_num,Category,city) %>% 
            dplyr::summarise(MonthlyCts = sum(MonthlyCts,na.rm = T), Price = mean(Price, na.rm = T)) %>% 
            tidyr::unite(catCity,Category,city) %>% 
            tidyr::gather(variable,value, -c(date_block_num,catCity)) %>% 
            tidyr::unite(temp,catCity,variable) %>%
            tidyr::spread(temp,value)
        }
        
        
      } else {
        Predictors <- data %>% dplyr::group_by(date_block_num,item_id,city) %>% 
          dplyr::summarise(MonthlyCts = sum(MonthlyCts,na.rm = T),Price = mean(Price,na.rm = T))
        if(!Collapse){
          dplyr::mutate(Item.City = paste0(as.character(item_id),"_",city)) %>% 
            tidyr::gather(variable,value, -c(date_block_num,Item.City)) %>% 
            tidyr::unite(temp,Item.City,variable) %>%
            tidyr::spread(temp,value)
        } else {
          Predictors %<>%  dplyr::left_join(.,ItemsReference[,c("item_id","Category")]) %>% 
            dplyr::group_by(date_block_num,Category,city) %>% 
            dplyr::summarise(MonthlyCts = sum(MonthlyCts,na.rm = T), Price = mean(Price, na.rm = T)) %>% 
            tidyr::unite(catCity,Category,city) %>% 
            tidyr::gather(variable,value, -c(date_block_num,catCity)) %>% 
            tidyr::unite(temp,catCity,variable) %>%
            tidyr::spread(temp,value)
        }
      }
      
      
    } else{
      Predictors %<>% dplyr::group_by(date_block_num,item_id,shop_id) %>% 
        dplyr::summarise(MonthlyCts = sum(MonthlyCts,na.rm = T),Price = mean(Price,na.rm = T)) %>% 
        dplyr::mutate(Item.Shop = as.character(item_id + shop_id/100)) %>% 
        tidyr::gather(variable,value, -c(date_block_num,Item.Shop)) %>% 
        tidyr::unite(temp,Item.City,variable) %>%
        tidyr::spread(temp,value)
    }
    
    #browser()
    
    ArrangedData <- Response %>% dplyr::left_join(Predictors)
    
  } else {
    # selection
    data %<>% dplyr::mutate(Item.Shop = as.character(item_id + shop_id/100)) %>% 
      dplyr::select(date_block_num,Item.Shop,MonthlyCts,Price)
    
    # spread 
    
    ArrangedData <-  data %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(Cusum = cumsum(MonthlyCts)) %>% 
      dplyr::filter(Cusum > 0)
      
      
  }
  
  
  if(nrow(ArrangedData) > 0 ){
    mindatenum <- min(ArrangedData$date_block_num)
    ArrangedData %<>% 
      dplyr::select(-Cusum) %>% 
    tidyr::gather(variable,value, -c(date_block_num,Item.Shop)) %>%
      tidyr::unite(temp,Item.Shop,variable) %>%
      tidyr::spread(temp,value)
    yr <- floor(mindatenum/11) + 2013
    mm <- mindatenum %% 11
    ts(ArrangedData,start = c(yr,mm), frequency = 12)
  } else{
    print(paste("No data for shop: ",shop_ids, "and item_id", item_ids))
  }
  
  
}