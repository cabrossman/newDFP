DFP_constructInventoryTargetingList <- function(adUnitId, geotarget, criterias, keys, boolString){
  
  final <- list(geoTargeting=NULL, 
                inventoryTargeting=NULL, 
                technologyTargeting=NULL,
                customTargeting=NULL)
  
  #adjusted!
  #first to handle is geographic targeting. Accepts a vector of geography Id's and formats this for DFP
  if(!is.null(geotarget)){
    final$geoTargeting <- targeting_geo(geotarget, keys)
  }
  
  
  #target the particular ad units
  if(!is.null(adUnitId)){
    final$inventoryTargeting$targetedAdUnits <- list(adUnitId=adUnitId, includeDescendants='true')
  }
  
  #get custom targeting
  if(!is.null(criterias)){
    final$customTargeting <- targeting_custom(criterias,boolString)
  }
  
  return(final)
}

