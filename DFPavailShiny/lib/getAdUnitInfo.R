DFP_getAdUnitInfo <- function(){
  request_data <- list(selectStatement=
                         list(query=paste('select Id, Name',
                                          "from Ad_Unit")))
  
  dfp_select_result <- API_exponential_backoff_retry(dfp_select(request_data)$rval)
  final_result <- dfp_select_parse(dfp_select_result)
  return(final_result)
}