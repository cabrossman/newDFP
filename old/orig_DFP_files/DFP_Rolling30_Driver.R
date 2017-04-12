DFP_Rolling30_Driver <- function(){
  #define start and end times
  start_year <- as.numeric(format(Sys.Date()-31, "%Y"))
  start_month <- as.numeric(format(Sys.Date()-31, "%m"))
  start_day <- as.numeric(format(Sys.Date()-31, "%d"))
  start_date <- list(year=start_year, month=start_month, day=start_day)
  
  end_year <- as.numeric(format(Sys.Date()-1, "%Y"))
  end_month <- as.numeric(format(Sys.Date()-1, "%m"))
  end_day <- as.numeric(format(Sys.Date()-1, "%d"))
  end_date <- list(year=end_year, month=end_month, day=end_day)
  
  startYMD <- as.integer(format(Sys.Date()-31, "%Y%m%d"))
  endYMD <- as.integer(format(Sys.Date()-1, "%Y%m%d"))
  
  # 
  # 
  # start_year <- 2016
  # start_month <- 4
  # start_day <- 1
  # start_date <- list(year=start_year, month=start_month, day=start_day)
  # 
  # end_year <- 2016
  # end_month <- 4
  # end_day <- 30
  # end_date <- list(year=end_year, month=end_month, day=end_day)
  
  
  #get all lines
  # lines <- DFP_getAllMarineLineItems()
  #filter lines which where active in last 30 days
  # linesNames <- lines[ifelse(is.na(lines$endYMD),99999999,lines$endYMD) >= startYMD & ifelse(is.na(lines$startYMD),-99999999,lines$startYMD) <= endYMD, 3]
  # 
  print("downloading network delivery")
  marineLines <- DFP_getAllMarineLineItems()
  networkLines <- marineLines %>% filter(grepl("^adx",name,ignore.case = TRUE))
  
  
  filter = paste0("WHERE LINE_ITEM_NAME IN (", 
                  paste0(paste0("'",networkLines$name,"'"), collapse=','), 
                  ")")
  
  
  dfpNetworkAds <- DFP_getDeliveryInfo(start_year = start_year, start_month = start_month, start_day = start_day,
                                       end_year = end_year, end_month = end_month, end_day = end_day,
                                       filter = filter, column = 'TOTAL_LINE_ITEM_LEVEL_IMPRESSIONS')
  
  network_pagePos <- dfpNetworkAds$pagePos_data
  network_geo <- dfpNetworkAds$geo_data
  
  #rename for binding
  names(network_pagePos)[7] <- 'Column.AD_SERVER_IMPRESSIONS'
  names(network_geo)[5] <- 'Column.AD_SERVER_IMPRESSIONS'
  
  print("downloading non network delivery")
  del <- DFP_getDeliveryInfo(start_year = start_year, start_month = start_month, start_day = start_day, 
                             end_year = end_year, end_month = end_month, end_day = end_day)
  
  DelByPagePos <- rbind(del$pagePos_data,network_pagePos)
  DelByGeo <- rbind(del$geo_data,network_geo)
  
  print("categorizing delivery")
  #make adjustments to DelByPagePos to fit OAS framework
  DelByPagePos$Pos <- DFP_catPos(DelByPagePos$Dimension.CUSTOM_CRITERIA)
  DelByPagePos$business <- DFP_catBusiness(DelByPagePos$Dimension.AD_UNIT_NAME)
  DelByPagePos$MobDesk <- DFP_catMobileDesk(DelByPagePos$Dimension.AD_UNIT_NAME)
  DelByPagePos$section <- DFP_catSection(DelByPagePos$Dimension.AD_UNIT_NAME)
  DelByPagePos$salesgroup <- DFP_catSalesGroup(cbind.data.frame(Campaign = DelByPagePos$Dimension.LINE_ITEM_NAME, 
                                                                Advertiser = DelByPagePos$Dimension.ADVERTISER_NAME))
  names(DelByPagePos)[1] <- 'campaign';names(DelByPagePos)[7] <- 'impByPagePos';
  names(DelByPagePos)[8] <- "Advertiser"; names(DelByPagePos)[6] <- "Page"
  keep <- c('Advertiser','campaign','salesgroup','Page','Pos','impByPagePos','section','business','MobDesk')
  DelByPagePos <- DelByPagePos[,keep]
  
  #campaigns have same names across desktop and mobile. Need to sum up before joining with Geo
  DelByPagePos2 <- DelByPagePos %>% select(-Page,-MobDesk) %>% group_by(Advertiser, campaign, salesgroup, Pos, section, business) %>% summarize(impByPagePos = sum(impByPagePos))
  
  #make adjustments to geo_data to fit OAS framework
  geoInformation <- DFP_getGeoInfo()
  DelByGeo <- DFP_geoCat(geo_data = DelByGeo,geoInformation = geoInformation)
  DelByGeo$salesgroup <- DFP_catSalesGroup(cbind.data.frame(Campaign = DelByGeo$Dimension.LINE_ITEM_NAME, Advertiser = DelByGeo$Dimension.ADVERTISER_NAME))
  names(DelByGeo)[1] <- 'campaign'; names(DelByGeo)[2] <- 'State'
  names(DelByGeo)[5] <- 'impByGeo'; names(DelByGeo)[6] <- 'Advertiser'
  keep <- c('Advertiser','campaign','salesgroup','Geo','impByGeo','NoGeoExist','Country','State','Area')
  DelByGeo <- DelByGeo[,keep]
  
  #join pagePos & GeoData
  
  DelByPagePosSectionGeo <- DFP_joinGeo_PagePos(DelByPagePos2,DelByGeo)
  DelByPagePosSectionGeo <- DelByPagePosSectionGeo %>% mutate(international = ifelse(Country %in% c('US','CA'), 0,1), date = as.character(Sys.Date() - 1)) %>% mutate(key = paste0("DFP_",business,campaign,Geo,Pos,section,date))
  DelByPagePosSectionGeo <- DelByPagePosSectionGeo %>% select(key, date, business,salesgroup = salesgroup.x,advertiser = Advertiser,campaign,geo = Geo,country = Country,state = State,area = Area,international,pos = Pos, section, imps = EstImpByPosSecGeo)
  DelByPagePosSectionGeo <- DelByPagePosSectionGeo[,c("key","date","business","salesgroup","advertiser","campaign","geo","country","state","area","international","pos","section","imps")]
  
  
  return(DelByPagePosSectionGeo)
  
  
}
