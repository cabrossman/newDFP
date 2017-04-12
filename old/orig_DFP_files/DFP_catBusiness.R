DFP_catBusiness <- function(vector){
  cat <- vector
  cat <- NULL
  for(i in 1:length(vector)){
    if(grepl('boattrader',vector[i], ignore.case = TRUE)){
      cat[i] <- 'BT'
    } else if (grepl('yacht',vector[i], ignore.case = TRUE)){
      cat[i] <- 'YW'
    } else if (grepl('YW',vector[i], ignore.case = TRUE)){
      cat[i] <- 'YW'
    } else if (grepl('boats',vector[i], ignore.case = TRUE)){
      cat[i] <- 'BC'
    } else {
      cat[i] <- 'other'
    }
  }
  
  return(cat)
}