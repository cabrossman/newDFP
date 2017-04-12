# app-inventory-self-service/global.R
# Copyright 2017
# Boats Group, All rights reserved.

# taken from the following example
# http://www.r-bloggers.com/mimicking-a-google-form-with-a-shiny-app/

#set global options
options(stringsAsFactors=FALSE, scipen=999)
options(warn=-1)

#source functions and required packages.
source(paste0(getwd(),'/lib/AuthFunctions.R'))


########################
# below is an alternative way to force a restart in the code. The way I have implmented is to "touch restart.txt" file with a CRON job in linux.
# creating a new blank restart.txt file with an updated timestamp forces a reboot of the server for updated keys 
########################
# pollData <- reactivePoll(1800000, session=NULL,
#                          checkFunc = function() {
#                            as.Date(Sys.time()-hours(6), tz='EST')
#                          },
#                          valueFunc = function() {
#                            #connect to redshift
#                            conn <- redShiftAuth(path)
#                            #query db
#                            geo <- dbGetQuery(conn, "select * from geoids_ids")
#                            adU <- dbGetQuery(conn, "select * from ad_unit_ids")
#                            kv <- dbGetQuery(conn, "select * from keyVal_ids")
#                            
#                            rateCard <- dbGetQuery(conn, "select * from RateCard")
#                            boatsRegions <- dbGetQuery(conn, "select * from boatsRegions")
#                            countryCodes <- dbGetQuery(conn, "select * from countryCodes")
#                            brandclassbuckets <- dbGetQuery(conn, "select * from brand_class_buckets")
#                            
#                            dbDisconnect(conn)
#                            
#                            keyList <- list(geoids_ids = geo, ad_unit_ids = adU, keyVal_ids = kv,
#                                               rate_card = rateCard, boats_regions = boatsRegions, country_codes = countryCodes, brand_class_buckets = brandclassbuckets)
#                            
#                            return(keyList)
#                            
#                          })
# 
# keyList <- reactive({pollData()})


#function to pull data from database. Returns a list. This is used to populate the selections options/rate logic in the application
valueFunc = function() {
  #connect to redshift
  conn <- redShiftAuth(path)
  #query db
  geo <- dbGetQuery(conn, "select * from geoids_ids")
  adU <- dbGetQuery(conn, "select * from ad_unit_ids")
  kv <- dbGetQuery(conn, "select * from keyVal_ids")

  rateCard <- dbGetQuery(conn, "select * from RateCard")
  boatsRegions <- dbGetQuery(conn, "select * from boatsRegions")
  countryCodes <- dbGetQuery(conn, "select * from countryCodes")
  brandclassbuckets <- dbGetQuery(conn, "select * from brand_class_buckets")
  targetingBySite <- dbGetQuery(conn,"SELECT * FROM targetingBySite")

  dbDisconnect(conn)

  keyList <- list(geoids_ids = geo, ad_unit_ids = adU, keyVal_ids = kv,
                  rate_card = rateCard, boats_regions = boatsRegions, 
                  country_codes = countryCodes, brand_class_buckets = brandclassbuckets,
                  targetsBySite = targetingBySite)

  return(keyList)

 }

 keyList <- valueFunc()

#connect app for api calls
dfpAuth()

