DFP_catPos <- function(vector){
  a <- tolower(str_extract(vector, "(?<==).*"))
  return(a)
}