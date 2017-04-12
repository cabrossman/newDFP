DFP_getGeoInfo <- function(targetable = NULL){
  
  if(is.null(targetable)){
    request_data <- list(selectStatement=
                           list(query=paste('select Id, Name,', 
                                            'CanonicalParentId, CountryCode,',
                                            "Type from Geo_Target")))
  } else if(targetable == TRUE){
    request_data <- list(selectStatement=
                           list(query=paste('select Id, Name,', 
                                            'CanonicalParentId, CountryCode,',
                                            "Type from Geo_Target WHERE targetable = true")))
  }
  
  dfp_select_result <- API_exponential_backoff_retry(dfp_select(request_data)$rval)
  final_result <- dfp_select_parse(dfp_select_result)
  return(final_result)
  
}





