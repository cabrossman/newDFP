
#first to handle is geographic targeting. Accepts a vector of geography Id's and formats this for DFP

targeting_geo <- function(geoIdVec, keys){
  geo <- keys$geoids_ids %>% filter(id %in% geoIdVec) %>% select(id, type, displayName = name) %>% t() %>% as.matrix()
  colnames(geo) <- rep('targetedLocations',ncol(geo))
  
  
  #convert into list
  geoTargetList <- NULL
  for(i in 1:ncol(geo)){
    temp <- list(targetedLocations=list(id=geo[1,i], 
                                        type=geo[2,i], 
                                        displayName=geo[3,i])
    )
    geoTargetList <- append(geoTargetList,temp)
  }
  
  
  return(geoTargetList)
}




######################################################
# test function
########################################################
# targets <- unlist(line_item_detail_Del$rval[[15]]$targeting$geoTargeting[1,])
# 
# geoTarget <- targeting_geo(targets,keyList)
# comparison <- line_item_detail_Del$rval[[15]]$targeting$geoTargeting
# matequal <- function(x, y){
#   is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
# }
# matequal(geoTarget, comparison)
# ncol(geoTarget); ncol(comparison)