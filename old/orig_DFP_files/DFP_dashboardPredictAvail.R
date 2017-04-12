DFP_dashboardPredictAvail <- function(){

  ##########get keys to cat data
  print("getting keys")
  keys <- DFP_getKeys()
  
  #categorize keys
  print("categorizing...")
  geo <- DFP_geoCat(geo_data = NULL, geoInformation = keys$geoids_ids)
  US_keys <- geo %>% filter(Country == 'US',type == 'STATE') %>% select(id)
  CA_key <- geo %>% filter(Country == 'CA',type == 'PROVINCE') %>% select(id)
  Int_keys <- geo %>% filter(!Country %in% c('US','CA'),type == 'COUNTRY') %>% select(id)
  
  geoKeys <- rbind(US_keys,CA_key,Int_keys)
  
  Ad_pos <- DFP_getDeliveryInfo()$pagePos_data
  Ad_pos$Pos <- DFP_catPos(Ad_pos$Dimension.CUSTOM_CRITERIA)
  Ad_pos$business <- DFP_catBusiness(Ad_pos$Dimension.AD_UNIT_NAME)
  Ad_pos$MobDesk <- DFP_catMobileDesk(Ad_pos$Dimension.AD_UNIT_NAME)
  Ad_pos$section <- DFP_catSection(Ad_pos$Dimension.AD_UNIT_NAME)
  
  adUnitPos <- Ad_pos %>% select(adUnitId = Dimension.AD_UNIT_ID, posValueID = Dimension.CUSTOM_TARGETING_VALUE_ID) %>% distinct(adUnitId,posValueID)
  
  
  
  
      
      
      
      print("getting forecast data...")
      allData <- NULL
      for(j in 1:nrow(adUnitPos)){
        
        print(paste0(j," of ",nrow(adUnitPos)))    
        
        targeting <- DFP_constructInventoryTargetingList(adUnitId=as.character(adUnitPos$adUnitId[j]),
                                                         geotarget= as.character(geoKeys$id),
                                                         criterias=list('136027'= as.character(adUnitPos$posValueID[j])
                                                                        #this has brnad factor targeting                 ,'136267'= c('115765528387','115765518067','115765518787','115765541107')
                                                         ),
                                                         keys = keys
        )
        
        
        
        one_hypothetical_line_item <- DFP_constructLineItem(startDate= Sys.Date() + 1,
                                                            endDate= Sys.Date() + 31,
                                                            deliveryRateType='EVENLY',
                                                            frequencyCaps=NULL,
                                                            lineItemType='STANDARD',
                                                            priority=8,
                                                            costType='CPM',
                                                            primaryGoal=list(goalType='LIFETIME',
                                                                             unitType='IMPRESSIONS', 
                                                                             units=1000), 
                                                            targeting=list(geoTargeting=targeting$geoTargeting, 
                                                                           inventoryTargeting=targeting$inventoryTargeting, 
                                                                           technologyTargeting=targeting$technologyTargeting,
                                                                           customTargeting=targeting$customTargeting)
        )
        
        request_data <- list(lineItem=one_hypothetical_line_item,
                             forecastOptions=list(includeTargetingCriteriaBreakdown='true', 
                                                  includeContendingLineItems='true'))
        
        result <- API_exponential_backoff_retry(dfp_getAvailabilityForecast(request_data, as_df = F, verbose=F))
        
        someData <- NULL
        for(i in 1:length(names(result$rval))){
          temp <- NULL; temp2 <- NULL
          if(names(result$rval)[[i]] == 'targetingCriteriaBreakdowns'){
            if(result$rval[[i]]$targetingDimension == 'GEOGRAPHY'){
              temp <- geo %>% filter(id == result$rval[[i]]$targetingCriteriaId)
              temp2 <- cbind.data.frame(temp, avilImps = as.numeric(result$rval[[i]]$availableUnits), matchedImps = as.numeric(result$rval[[i]]$matchedUnits))
              someData <- rbind(someData,temp2)
            }
          }
        }
        
        #update here
        #
        # need to bring in page/pos data to link everything up
        
        Pos <- Ad_pos %>% filter(Dimension.AD_UNIT_ID == adUnitPos$adUnitId[j], Dimension.CUSTOM_TARGETING_VALUE_ID == adUnitPos$posValueID[j]) %>% select(Pos) %>% distinct(Pos)
        business <- Ad_pos %>% filter(Dimension.AD_UNIT_ID == adUnitPos$adUnitId[j], Dimension.CUSTOM_TARGETING_VALUE_ID == adUnitPos$posValueID[j]) %>% select(business) %>% distinct(business)
        MobDesk <- Ad_pos %>% filter(Dimension.AD_UNIT_ID == adUnitPos$adUnitId[j], Dimension.CUSTOM_TARGETING_VALUE_ID == adUnitPos$posValueID[j]) %>% select(MobDesk) %>% distinct(MobDesk)
        Section <- Ad_pos %>% filter(Dimension.AD_UNIT_ID == adUnitPos$adUnitId[j], Dimension.CUSTOM_TARGETING_VALUE_ID == adUnitPos$posValueID[j]) %>% select(section) %>% distinct(section)
        
        
        #
        #
        temp3 <- cbind.data.frame(AD_UNIT_ID = adUnitPos$adUnitId[j], POS_ID = adUnitPos$posValueID[j],Pos = Pos, business = business, MobDesk = MobDesk, Section = Section, someData)
        #
        #
        allData <- rbind(allData,temp3)
      }
      
      newData <- allData %>% 
        mutate(key = paste0("DFP_",AD_UNIT_ID,"-",POS_ID,"-",id,"-",as.character(Sys.Date()-1)), 
               date = as.character(Sys.Date()-1), salesgroup = "DFP_FORECAST", advertiser = "DFP_FORECAST", campaign = "DFP_FORECAST",
               international = ifelse(Country %in% c('CA','US'),0,1), imps = matchedImps*.85) %>%
        select(key, date, business, salesgroup, advertiser, campaign, geo = Geo,country = Country, state = name, area = Area,international, pos = Pos, section, imps)
      
      
      return(newData)
}