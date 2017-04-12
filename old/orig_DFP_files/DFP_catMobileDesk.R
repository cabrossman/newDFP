DFP_catMobileDesk <- function(vector){
  cat <- vector
  cat <- NULL
  for(i in 1:length(vector)){
    if(grepl('desktop',vector[i], ignore.case = TRUE)){
      cat[i] <- 'desktop'
    } else if (grepl('mobile',vector[i], ignore.case = TRUE)){
      cat[i] <- 'mobile'
    } else {
      cat[i] <- 'other'
    }
  }
  
  return(cat)
}