DFP_getDeliveryInfo <- function(
                                  start_year = as.numeric(format(Sys.Date()-31, "%Y")),
                                  start_month = as.numeric(format(Sys.Date()-31, "%m")),
                                  start_day = as.numeric(format(Sys.Date()-31, "%d")),
                                  end_year = as.numeric(format(Sys.Date()-1, "%Y")),
                                  end_month = as.numeric(format(Sys.Date()-1, "%m")),
                                  end_day = as.numeric(format(Sys.Date()-1, "%d")),
                                  filter = NULL,
                                  column = 'AD_SERVER_IMPRESSIONS'
){
  dfpAuth()
  start_date <- list(year=start_year, month=start_month, day=start_day)
  end_date <- list(year=end_year, month=end_month, day=end_day)
  
  if(is.null(filter)){
    #pagePos
    pagePos_request <- list(reportJob =
                              list(reportQuery =
                                     list(dimensions = 'LINE_ITEM_NAME',
                                          dimensions = 'AD_UNIT_ID',
                                          dimensions = 'CUSTOM_CRITERIA',
                                          adUnitView = 'FLAT',
                                          columns = column, 
                                          startDate = start_date,
                                          endDate = end_date,
                                          dateRangeType = 'CUSTOM_DATE'
                                          #, statement=list(query="WHERE LINE_ITEM_NAME = '26334-1_13157_NelsonsSpeed_M-BTOL-SR-T-320x80-WESTMI'")
                                     )))
    
    #geo
    geo_request <- list(reportJob =
                          list(reportQuery =
                                 list(dimensions = 'LINE_ITEM_NAME',
                                      dimensions = 'REGION_NAME',
                                      adUnitView = 'FLAT',
                                      columns = column, 
                                      startDate = start_date,
                                      endDate = end_date,
                                      dateRangeType = 'CUSTOM_DATE'
                                      #, statement=list(query="WHERE LINE_ITEM_NAME = '26334-1_13157_NelsonsSpeed_M-BTOL-SR-T-320x80-WESTMI'")
                                 )))
    getAdvertiserList <- list(reportJob =
                                list(reportQuery =
                                       list(dimensions = 'LINE_ITEM_NAME',
                                            dimensions = 'ADVERTISER_NAME',
                                            columns = column, 
                                            startDate = start_date,
                                            endDate = end_date,
                                            dateRangeType = 'CUSTOM_DATE'
                                            #, statement=list(query="WHERE LINE_ITEM_NAME = '26334-1_13157_NelsonsSpeed_M-BTOL-SR-T-320x80-WESTMI'")
                                       )))
  } else{
    #pagePos
    pagePos_request <- list(reportJob =
                              list(reportQuery =
                                     list(dimensions = 'LINE_ITEM_NAME',
                                          dimensions = 'AD_UNIT_ID',
                                          dimensions = 'CUSTOM_CRITERIA',
                                          adUnitView = 'FLAT',
                                          columns = column, 
                                          startDate = start_date,
                                          endDate = end_date,
                                          dateRangeType = 'CUSTOM_DATE',
                                          statement=list(query=filter)
                                     )))
    
    #geo
    geo_request <- list(reportJob =
                          list(reportQuery =
                                 list(dimensions = 'LINE_ITEM_NAME',
                                      dimensions = 'REGION_NAME',
                                      adUnitView = 'FLAT',
                                      columns = column, 
                                      startDate = start_date,
                                      endDate = end_date,
                                      dateRangeType = 'CUSTOM_DATE',
                                      statement=list(query=filter)
                                 )))
    getAdvertiserList <- list(reportJob =
                                list(reportQuery =
                                       list(dimensions = 'LINE_ITEM_NAME',
                                            dimensions = 'ADVERTISER_NAME',
                                            columns = column, 
                                            startDate = start_date,
                                            endDate = end_date,
                                            dateRangeType = 'CUSTOM_DATE',
                                            statement=list(query=filter)
                                       )))
  }
  
  getAdvertiserList_data <- API_exponential_backoff_retry(dfp_full_report_wrapper(getAdvertiserList))
  names(getAdvertiserList_data)[3:5] <- c("drop1","drop2","drop3")
  if(nrow(getAdvertiserList_data) == 0){
    for(i in 1:ncol(getAdvertiserList_data)){
      getAdvertiserList_data[1,i] <- 'NA'
    }
  }
  

  pagePos_data <- API_exponential_backoff_retry(dfp_full_report_wrapper(pagePos_request))
  pagePos_data <- pagePos_data[grepl("^pos",pagePos_data$Dimension.CUSTOM_CRITERIA),]
  
  geo_data <- API_exponential_backoff_retry(dfp_full_report_wrapper(geo_request))
  
  geo_data2 <- geo_data %>% left_join(getAdvertiserList_data, by = "Dimension.LINE_ITEM_NAME") %>% dplyr::select(-drop1,-drop2,-drop3)
  pagePos_data2 <- pagePos_data %>% left_join(getAdvertiserList_data, by = "Dimension.LINE_ITEM_NAME") %>% dplyr::select(-drop1,-drop2,-drop3)
  
  
  del_list <- list(pagePos_data = pagePos_data2, geo_data = geo_data2)
  return(del_list)
}