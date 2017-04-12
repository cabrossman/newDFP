DFP_catSection <- function(vector){
  cat <- vector
  cat <- NULL
  for(i in 1:length(vector)){
    if(grepl('SearchResult',vector[i], ignore.case = TRUE)){
      cat[i] <- 'SR'
    } else if (grepl('SR',vector[i], ignore.case = TRUE)){
      cat[i] <- 'SR'
    } else if (grepl('frontpage',vector[i], ignore.case = TRUE)){
      cat[i] <- 'HOME'
    } else if (grepl('Details',vector[i], ignore.case = TRUE)){
      cat[i] <- 'DT'
    } else {
      cat[i] <- 'OTHER'
    }
  }
  
  return(cat)
}