##################SQL server connections###############
rm(list = ls())
options(stringsAsFactors = FALSE)
suppressMessages(require(forecast))
suppressMessages(library(XML))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(sqldf))
suppressMessages(library(ggplot2))
suppressMessages(library(broom))


path <- getwd()

#dfp auth
dfpAuth <- function(){
  suppressMessages(library("rdfp"))
  options(rdfp.network_code = "29685107")
  options(rdfp.application_name = "MyApp")
  options(rdfp.client_id = "992031099147-71td3s047cpnmikis4qald76o4l56476.apps.googleusercontent.com")
  options(rdfp.client_secret = "Vlt2eQlMsRNeHvE8UfVA9qSy")
  dfp_auth()
}

redShiftAuth <- function(){
  suppressMessages(require(RJDBC))
  #CLASSPATH <- paste0(path,"mysql-connector-java-5.1.39-bin.jar")
  CLASSPATH <- "C:\\Users\\christopher.brossman\\Documents\\Projects\\work\\newDFP\\RedshiftJDBC42-1.1.17.1017.jar"
  driver <- JDBC("com.amazon.redshift.jdbc42.Driver",classPath= CLASSPATH," ")
  URL = "jdbc:redshift://sapi-37-dominion.cmizbsfmzc6w.us-east-1.redshift.amazonaws.com:5439/sapi_797"
  UID = "sapi_workspace_49554"
  PASS = "zJ2mU6aP6tM7oF0b"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}

API_exponential_backoff_retry <- function(expr, n = 5, verbose = FALSE){
  
  for (i in seq_len(n)) {
    
    result <- try(eval.parent(substitute(expr)), silent = FALSE)
    
    if (inherits(result, "try-error")){
      
      backoff <- runif(n = 1, min = 0, max = 2 ^ i - 1)
      if(verbose){
        message("Error on attempt ", i,
                ", will retry after a back off of ", round(backoff, 2),
                " seconds.")
      }
      Sys.sleep(backoff)
      
    } else {
      if(verbose){
        message("Succeed after ", i, " attempts")
      }
      break 
    }
  }
  
  if (inherits(result, "try-error")) {
    message("Failed after max attempts")
    result <- NULL
  } 
  
  return(result)
} 

DFP_constructLineItem <- function(startDate,
                                  endDate,
                                  deliveryRateType,
                                  frequencyCaps,
                                  lineItemType,
                                  priority,
                                  costType,
                                  creativePlaceholders,
                                  primaryGoal, 
                                  targeting){
  final <- list()
  final$startDateTime <- DFP_date2DateTimeObj(startDate, daytime='beginning')
  final$endDateTime <- DFP_date2DateTimeObj(endDate, daytime='end')
  
  if(!is.null(deliveryRateType))
    final$deliveryRateType <- deliveryRateType
  
  if(is.null(frequencyCaps$maxImpressions) ||
     is.null(frequencyCaps$numTimeUnits) ||
     is.na(frequencyCaps$maxImpressions) || 
     is.na(frequencyCaps$numTimeUnits)){
    frequencyCaps <- NULL
  }
  if(!is.null(frequencyCaps))
    final$frequencyCaps <- frequencyCaps
  
  if(!is.null(lineItemType))
    final$lineItemType <- lineItemType
  if(!is.null(priority))
    final$priority <- priority
  if(!is.null(costType))
    final$costType <- costType
  
  if(!is.null(primaryGoal))
    final$primaryGoal <- primaryGoal
  if(!is.null(targeting))
    final$targeting <- targeting
  
  finalfinal <- list(lineItem=final)
  
  return(finalfinal)
}


DFP_constructInventoryTargetingList <- function(adUnitId, geotarget, criterias, keys){
  
  final <- list(geoTargeting=NULL, 
                inventoryTargeting=NULL, 
                technologyTargeting=NULL,
                customTargeting=NULL)
  
  
  #first to handle is geographic targeting. Accepts a vector of geography Id's and formats this for DFP
  cols <- c('id','type','canonicalparentid','name')
  geo <- keys$geoids_ids
  geo <- as.matrix(t(geo[geo$id %in% geotarget,cols]))
  colnames(geo) <- rep('targetedLocations',ncol(geo))
  rownames(geo)[4] <- 'displayName'
  
  temp <- NULL
  for(i in 1:ncol(geo)){
    temp <- list(targetedLocations=list(id=geo[1,i], 
                                        type=geo[2,i], 
                                        displayName=geo[4,i]
    )
    )
    final$geoTargeting <- append(final$geoTargeting,temp)
  }
  
  
  #target the particular ad units
  final$inventoryTargeting$targetedAdUnits <- list(adUnitId=adUnitId, includeDescendants='true')
  
  nonnull_criterias <- list()
  ii <- 1
  for(i in 1:length(criterias)){
    if(!is.null(criterias[[i]])){
      nonnull_criterias[[names(criterias)[i]]] <- criterias[[i]]
      ii <- ii + 1
    }
  }
  criterias <- nonnull_criterias
  
  if (length(criterias)>0){
    indiv_possibilities <- expand.grid(criterias, stringsAsFactors = F)
    ct_object <- vector("list", nrow(indiv_possibilities)+1)
    ct_object[[1]] <- 'OR'
    for(i in 1:nrow(indiv_possibilities)){
      one_ct_object <- vector("list", ncol(indiv_possibilities)+2)
      one_ct_object[[1]] <- 'AND'
      one_ct_object[[length(one_ct_object)]] <- c('type'="CustomCriteriaSet")
      names(one_ct_object) <- c('logicalOperator', rep('children', ncol(indiv_possibilities)), '.attrs')
      for(j in 1:ncol(indiv_possibilities)){
        one_ct_object[[j+1]] <- list(keyId=names(indiv_possibilities)[j], 
                                     valueIds=indiv_possibilities[i,j],
                                     operator='IS',
                                     `.attrs`=c('type'="CustomCriteria"))
      }
      ct_object[[i+1]] <- one_ct_object
    }
    names(ct_object) <- c('logicalOperator', rep('children', nrow(indiv_possibilities)))
    final$customTargeting <- ct_object
  }
  return(final)
}

DFP_date2DateTimeObj <- function(this_date,
                                 daytime=c('beginning','end'), 
                                 timeZoneID='America/New_York'){
  
  this_hour <- if(daytime=='beginning') 0 else 23
  this_minute <- if(daytime=='beginning') 0 else 59
  this_second <- if(daytime=='beginning') 0 else 59
  
  x <- list(date=list(year=as.integer(format(this_date, '%Y')), 
                      month=as.integer(format(this_date, '%m')), 
                      day=as.integer(format(this_date, '%d'))), 
            hour=if(this_hour == 0 & this_date == Sys.Date()) pmin(hour(Sys.time()+hours(1)), 23) else this_hour, 
            minute=if(this_minute == 0 & this_date == Sys.Date()) pmin(minute(Sys.time()), 59) else this_minute,
            second=if(this_second == 0 & this_date == Sys.Date()) pmin(as.integer(second(Sys.time())), 59) else this_second,
            timeZoneID=timeZoneID)
  return(x)
}


#load in key data from redshift
conn <- redShiftAuth()
adUnitPos <- dbGetQuery(conn, "select * from dfp_ad_unit_ids")
geoKeys <- dbGetQuery(conn, "select * from dfp_geoids")
keyVal_ids <- dbGetQuery(conn, "select * from dfp_keyVal_ids")
keys <- list(adUnitPos = adUnitPos, geoids_ids = geoKeys, ad_unit_ids = keyVal_ids)
dbDisconnect(conn)


##### for shiney
### Need UI to have single select box for various ways to filter down an Ad Unit
# today this is business, portal 
#https://github.dominionenterprises.com/christopher-brossman/BannerOpportunityDashboard/blob/master/DFP_supportFunctions/DFP_dashboardPredictAvail.R



  for(j in 1:nrow(adUnitPos)){
    
    print(paste0(j," of ",nrow(adUnitPos)))    
    
    targeting <- DFP_constructInventoryTargetingList(adUnitId=as.character(adUnitPos$adUnitId[j]),
                                                     geotarget= as.character(geoKeys$id),
                                                     criterias=list('136027'= as.character(keyVal_ids$posValueID[j])
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
  


