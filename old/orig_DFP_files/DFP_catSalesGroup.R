DFP_catSalesGroup <- function(df){
  
  df$DDM <- ifelse(grepl("^adx",tolower(df$Campaign)),1,0)
  df$Passback <- ifelse(grepl("passback",tolower(df$Campaign)),1,0)
  df$Default <- ifelse(grepl("default",tolower(df$Campaign)),1,0)
  df$House <- ifelse(grepl("^house-",tolower(df$Campaign)),1,ifelse(grepl("house",tolower(df$Advertiser)),1,0))
  df$salesgroup <- ifelse(df$House > 0,"House",ifelse(df$Default > 0, "unknown_default",ifelse(df$Passback >0, "Passback",ifelse(df$DDM > 0, "DDM", "Marine"))))
  
  
  
  #newly built to capture default data by geo                          
  newDefault <- c('M-BTOL-ALL-SR-1x1-House-Top', 'M-YW-ALL-SR-1X1-House-liner1', 'BTOL-FP-ALL-1x1-HOUSE-HERO', 'M-BTOL-FP-ALL-1x1-HOUSE-HERO', 'M-BTOL-DT-ALL-1x1-HOUSE-BOTTOM', 'M-YW-DT-ALL-1x1-HOUSE-BOTTOM', 'YW-SR-ALL-1x1-HOUSE-BOTTOM')
    df$salesgroup <- ifelse(df$Campaign %in% newDefault,"known_default",df$salesgroup)
  
  return(df$salesgroup)
}



