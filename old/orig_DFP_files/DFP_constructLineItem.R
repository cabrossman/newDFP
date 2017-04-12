
# second create a handy function for constructing the line items
# because it's really compicated to get the format just right, so
# use thsi function to simply plug in the values you care about
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