forecastImpressions <- function(adUnitVec,
                                geoVec,
                                criteriasListOfNamedVecs,
                                keyList,
                                boolStringChar = 'make-OR-class, rest=AND',
                                start_date = Sys.Date() + 1,
                                end_date = Sys.Date() + 31,
                                deliveryRateTypeChar = 'EVENLY',
                                frequencyCapsChar = NULL,
                                lineItemTypeChar = 'STANDARD',
                                priorityInt = 8,
                                costTypeChar = 'CPM',
                                goalTypeChar = 'LIFETIME',
                                unitTypeChar='IMPRESSIONS',
                                unitsInt=1000
                                )
  {
  
  targeting <- DFP_constructInventoryTargetingList(adUnitId=adUnitVec,
                                                   geotarget=geoVec,
                                                   criterias=criteriasListOfNamedVecs,
                                                   keys = keyList,
                                                   boolString = boolStringChar
  )
  
  one_hypothetical_line_item <- DFP_constructLineItem(startDate= start_date,
                                                      endDate= end_date,
                                                      deliveryRateType=deliveryRateTypeChar,
                                                      frequencyCaps=frequencyCapsChar,
                                                      lineItemType=lineItemTypeChar,
                                                      priority=priorityInt,
                                                      costType=costTypeChar,
                                                      primaryGoal=list(goalType=goalTypeChar,
                                                                       unitType=unitTypeChar, 
                                                                       units=unitsInt), 
                                                      targeting=list(geoTargeting=targeting$geoTargeting, 
                                                                     inventoryTargeting=targeting$inventoryTargeting, 
                                                                     technologyTargeting=targeting$technologyTargeting,
                                                                     customTargeting=targeting$customTargeting)
  )
  
  request_data <- list(lineItem=one_hypothetical_line_item,
                       forecastOptions=list(includeTargetingCriteriaBreakdown='true', 
                                            includeContendingLineItems='true'))
  
  result <- API_exponential_backoff_retry(dfp_getAvailabilityForecast(request_data, as_df = F, verbose=F))
  
  return(result)
}