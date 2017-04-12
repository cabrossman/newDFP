pullKeysFromDB <- function(){
  #connect to redshift
  conn <- redShiftAuth(path)
  #query db
  geo <- dbGetQuery(conn, "select * from geoids_ids")
  adU <- dbGetQuery(conn, "select * from ad_unit_ids")
  kv <- dbGetQuery(conn, "select * from keyVal_ids")
  
  keyList <- list(geoids_ids = geo, ad_unit_ids = adU, keyVal_ids = kv)
  
  dbDisconnect(conn)
  
  return(keyList)
}