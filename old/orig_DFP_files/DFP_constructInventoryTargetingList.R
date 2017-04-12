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

