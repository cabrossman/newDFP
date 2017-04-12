# app-inventory-self-service/global.R
# Copyright 2016
# Dominion Enterprises, All rights reserved.

# taken from the following example
# http://www.r-bloggers.com/mimicking-a-google-form-with-a-shiny-app/

options(stringsAsFactors=FALSE, scipen=999)

suppressWarnings(suppressPackageStartupMessages(library(RPostgreSQL)))
suppressWarnings(suppressPackageStartupMessages(library(XML)))
suppressWarnings(suppressPackageStartupMessages(library(shiny)))
suppressWarnings(suppressPackageStartupMessages(library(shinyjs)))
suppressWarnings(suppressPackageStartupMessages(library(shinysky)))
suppressWarnings(suppressPackageStartupMessages(library(methods)))
suppressWarnings(suppressPackageStartupMessages(library(scales)))
suppressWarnings(suppressPackageStartupMessages(library(plyr)))
suppressWarnings(suppressPackageStartupMessages(library(dplyr)))
suppressWarnings(suppressPackageStartupMessages(library(RCurl)))
suppressWarnings(suppressPackageStartupMessages(library(digest)))
suppressWarnings(suppressPackageStartupMessages(library(lubridate)))
suppressWarnings(suppressPackageStartupMessages(library(stringi)))
suppressWarnings(suppressPackageStartupMessages(library(googlesheets)))
suppressWarnings(suppressPackageStartupMessages(library(rdfp)))
suppressWarnings(suppressPackageStartupMessages(library(bpaalib)))

# app lib
source("app-inventory-self-service-lib.R")

# uis lib
source("mobile_dfp_ui.R")
source("desktop_oas_ui.R")

res <- createDFPAPISession()

# rdfp_options <- readRDS("./data/rdfp_options.rds")
# options(rdfp.network_code = rdfp_options$network_code)
# options(rdfp.application_name = rdfp_options$application_name)
# options(rdfp.client_id = rdfp_options$client_id)
# options(rdfp.client_secret = rdfp_options$client_secret)
# 
# dfp_auth(token = "./data/rdfp_token.rds")

# which fields get saved 
fieldsAll <- c("adUnitId", "size", "priority", "costType", 
               "daterange", "goalType", "unitType", 
               "units", "deliveryRateType", 
               "homesdma", "status", "pos", "geo_breakdown", "monthly_breakdown")

# which fields are mandatory
fieldsMandatory <- c("size")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

adUnitNamesMap <- c('Homes_Mobile ROS'=34976467, 
                    'Homes_Mobile SearchResults'=34976707, 
                    'Homes_Mobile Details'=34976587)

suppressMessages(gs_auth(token = "googlesheets_token.rds", verbose = FALSE))

# register the google sheet for logging
tryCatch({
    logging_wkbk <- gs_retry(gs_title('Mobile Inventory Requests'), n = 3)
  },error=function(e){
    print(e)
})
