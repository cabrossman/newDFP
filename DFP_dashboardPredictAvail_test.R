
  #check inventory for boattrader mobile, details = 35074267
  #in the US = 2840
  #pos=Bottom => 136027 = c('74322824947')
  #make in jeannu, beneateau => '136267'= c('115765528387','115765518067')


    targeting <- DFP_constructInventoryTargetingList(adUnitId=as.character(35074267),
                                                     geotarget= as.character(2840),
                                                     criterias=list('136027'= c('74322824947')
                                                                    ,'136267'= c('115765528387','115765518067')
                                                                    #this has brnad factor targeting                 ,'136267'= c('115765528387','115765518067','115765518787','115765541107')
                                                     ),
                                                     keys = keys
    )
    
######new keys##########################################################################################
    
    #57248-1_14810_TotalConcepts_BO-ROS-RT1-300x250
    #
    #check inventory for boats ad unit = 262822239
    #in Dayton OH, South Bend-Elkhart In, indianapolis IN, Toledo OH, Ft. Wayne IN ==> '200542','200588','200527','200547','200509'
    #page in Advanced serach, boats editorial, boats details, home, or search ==>591399 =  '209335954599','209335948599','209335950039','209335947639','209335952199'
    #site_version is US ==> 591639 = 209335960839
    #pos is box-1 ==> 589839 = 209332942359
    targeting <- DFP_constructInventoryTargetingList(adUnitId=c('262822239'),
                                                     geotarget=c('200542','200588','200527','200547','200509'),
                                                     criterias=list('591399'= c('209335954599','209335948599','209335950039','209335947639','209335952199')
                                                                    ,'591639'= c('209335960839')
                                                                    , '589839'= c('209332942359')
                                                                    #this has brnad factor targeting                 ,'136267'= c('115765528387','115765518067','115765518787','115765541107')
                                                     ),
                                                     keys = keyList, boolString = 'None'
    )
    
    
    
    
    #start = 2/22/2017
    #end = 3/22/17
    # imps = 3756
    #check inventory for boats ad unit = 262822239
    #in the US = '2840'
    #page in search results or details ==>'591399'= c('209335950039','209335948599')
    #pos is box-1 ==> 589839 = 209332942359
    #make is beneteau, absolute ==> 589479 = c('209336397639','209336374839')
    #class is 'AftCabin or Airboat ==> '590679' = c('209334298599','209334298839')
    targeting <- DFP_constructInventoryTargetingList(adUnitId=c('262822239'),
                                                     geotarget=c('2840'),
                                                     criterias=list('591399'= c('209335950039','209335948599')
                                                                    , '589839'= c('209332942359')
                                                                    , '589479' = c('209336397639','209336374839')
                                                                    , '590679' = c('209334298599','209334298839')
                                                     ),
                                                     keys = keyList
                                                     #,
                                                     #boolString = 'make-OR-class'
    )
    
    
    
    targeting <- DFP_constructInventoryTargetingList(adUnitId=c('262822239'),
                                                     geotarget=c('2840'),
                                                     criterias=list('589839'= c('209332942599')
                                                                    , '589479'= c('209336503239','209336420439','209336556519','209336595639','209336465799','209336392599','209336540439','209336565159')
                                                                    , '591399' = c('209335948599','209335950039')
                                                                    , '591639' = c('209335959159','209335959399','209335959639','209335957959','209335957719','209335958679','209335960599','209335958199','209335960839')
                                                                    , '590679' = c('209334320679','209334342759')
                                                     ),
                                                     keys = keyList
                                                     ,
                                                     boolString = 'make-OR-class'
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
    
 