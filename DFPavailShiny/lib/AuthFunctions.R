##################SQL server connections###############
#rm(list = ls())
options(stringsAsFactors = FALSE)
suppressMessages(require(tidyverse))
suppressMessages(library(XML))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(sqldf))
suppressMessages(library(shiny))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))
suppressMessages(library(plyr))
suppressMessages(library(stringi))

path <- paste0(getwd(),'/lib/')
file.sources = list.files(path,pattern = "*.R")
file.sources <- file.sources[! file.sources %in% 'AuthFunctions.R']
for(i in 1:length(file.sources)){
  source(paste0(path,'\\',file.sources[i]))
}
# rm(file.sources,i)

#dfp auth
dfpAuth <- function(){
  suppressMessages(library("rdfp"))
  options(rdfp.network_code = "252108799")
  options(rdfp.application_name = "Brossman_API")
  options(rdfp.client_id = "992031099147-71td3s047cpnmikis4qald76o4l56476.apps.googleusercontent.com")
  options(rdfp.client_secret = "Vlt2eQlMsRNeHvE8UfVA9qSy")
  dfp_auth()
}

redShiftAuth <- function(path){
  suppressMessages(require(RJDBC))
  #CLASSPATH <- paste0(path,"mysql-connector-java-5.1.39-bin.jar")
  CLASSPATH <- paste0(path,"RedshiftJDBC42-1.1.17.1017.jar")
  driver <- JDBC("com.amazon.redshift.jdbc42.Driver",classPath= CLASSPATH," ")
  URL = "jdbc:redshift://sapi-37-dominion.cmizbsfmzc6w.us-east-1.redshift.amazonaws.com:5439/sapi_797"
  UID = "sapi_workspace_76383"
  PASS = "sQ6xJ5iB6tI6bW6g"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}

redShiftAuth2 <- function(path){
  suppressMessages(require(RJDBC))
  #CLASSPATH <- paste0(path,"mysql-connector-java-5.1.39-bin.jar")
  CLASSPATH <- paste0(path,"RedshiftJDBC42-1.1.17.1017.jar")
  driver <- JDBC("com.amazon.redshift.jdbc42.Driver",classPath= CLASSPATH," ")
  URL = "jdbc:redshift://sapi-37-dominion.cmizbsfmzc6w.us-east-1.redshift.amazonaws.com:5439/sapi_797"
  UID = "sapi_workspace_13468"
  PASS = "eM8oW0fS2aM5mN8b"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}

rm(file.sources,i)