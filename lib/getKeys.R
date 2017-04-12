DFP_getKeys <- function(){
  dfpAuth()
  
  #get predefined geography id's from DFP
  geoids_ids <- DFP_getGeoInfo(targetable = TRUE)
  
  
  #get a dataframe of unique AD_UNIT_ID's to pass to future queries. Get details about each for easier sampling
  #old way below
  # report_request_data <- list(reportJob=
  #                               list(reportQuery=
  #                                      list(dimensions='AD_UNIT_ID', 
  #                                           adUnitView='FLAT',
  #                                           columns='TOTAL_INVENTORY_LEVEL_IMPRESSIONS', 
  #                                           dateRangeType='LAST_MONTH')))
  # 
  # ad_unit_ids_download <- API_exponential_backoff_retry(dfp_full_report_wrapper(report_request_data))
  # 
  # ad_unit_ids <- ad_unit_ids_download %>%
  #   mutate(business = DFP_catBusiness(ad_unit_ids_download$Dimension.AD_UNIT_NAME)) %>%
  #   mutate(mobile = DFP_catMobileDesk(ad_unit_ids_download$Dimension.AD_UNIT_NAME)) %>%
  #   mutate(section = DFP_catSection(ad_unit_ids_download$Dimension.AD_UNIT_NAME)) %>%
  #   select(AD_UNIT_ID = Dimension.AD_UNIT_ID, AD_UNIT_NAME = Dimension.AD_UNIT_NAME, business, mobile, section)
  
  #get a dataframe of unique AD_UNIT_ID's to pass to future queries. Get details about each for easier sampling
  #new way----------------------------------------
  ad_unit_ids <- DFP_getAdUnitInfo() %>% filter(name %in% c('BoatTrader','YachtWorld','Boats'))
  
  # Determine Key-Value Ids ---------------------------------------------------------------------------------------
  
  # first pull all the keys you want by name (edit the list to add more)
  # Did it this way because there were more keys than just marine.
  # in future may not need to filter any, and pull down all information
  key_ids_dataset <- NULL
  names <- c('class', 'condition', 'length','make','Page','pos','Seller', 'site_version', 'state', 'type','year', 'zip_code')
  for (n in names){
    request_data <- list('filterStatement'=list(query=paste0("WHERE name='", n, "'")))
    temp <- API_exponential_backoff_retry(dfp_getCustomTargetingKeysByStatement(request_data))
    key_ids_dataset <- rbind(key_ids_dataset, temp)
  }
  
  # second pull the values for each of the Keys
  # PULL ALL THE CUSTOM TARGETING VALUES OF THOSE KEYS
  value_ids_dataset <- NULL
  for (i in key_ids_dataset$id){
    request_data <- list('filterStatement'=list(query=paste0("WHERE customTargetingKeyId=", i)))
    temp <- API_exponential_backoff_retry(dfp_getCustomTargetingValuesByStatement(request_data))
    value_ids_dataset <- rbind(value_ids_dataset, temp)
  }
  
  keyVal_ids <- key_ids_dataset %>% 
    select(customTargetingKeyId = id, customerTargetingName =name, customTargetingDisplay = displayName, customTargetingType = type, customTargetingStatus = status) %>%
    right_join(value_ids_dataset, by = 'customTargetingKeyId')
  
  
  keys <- list(geoids_ids = geoids_ids,ad_unit_ids =ad_unit_ids,keyVal_ids = keyVal_ids)
  
  return(keys)
}
