##################SQL server connections###############
rm(list = ls())
options(stringsAsFactors = FALSE)
suppressMessages(require(forecast))
suppressMessages(library(XML))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(sqldf))
suppressMessages(library(ggplot2))
suppressMessages(library(broom))
# suppressMessages(library(plyr))

path <- getwd()
file.sources = list.files(path,pattern = "*.R")
file.sources <- file.sources[! file.sources %in% 'AuthFunctions.R']
for(i in 1:length(file.sources)){
  source(paste0(path,'\\',file.sources[i]))
}
# rm(file.sources,i)

sqlServerAuth <- function(){
  suppressMessages(require(RJDBC))
  CLASSPATH <- "C:\\Users\\christopher.brossman\\Downloads\\Microsoft JDBC Driver 4.0 for SQL Server\\sqljdbc_4.0\\enu\\sqljdbc4.jar"
  driver <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver",classPath= CLASSPATH," ")
  URL = "jdbc:sqlserver://10.93.42.22"
  UID = "DDM_User"
  PASS = "XRe8Pu"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}
# mysql connection
mySQLAuth <- function(){
  suppressMessages(require(RJDBC))
  CLASSPATH <- "C:\\Users\\christopher.brossman\\Downloads\\mysql-connector-java-5.1.39\\mysql-connector-java-5.1.39-bin.jar"
  driver <- JDBC("com.mysql.jdbc.Driver",classPath= CLASSPATH," ")
  URL = "jdbc:mysql://imtdb_slave.boats.local/imt"
  UID = "imt_ro"
  PASS = "imt_ro"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}
# siteCat connection
siteCatAuth <- function(){
  suppressMessages(library(RSiteCatalyst))
  key = "christopher.brossman:Dominion Enterprises"
  secret = "a12552cbea82f242e0f93f2317fb0516"
  SCAuth(key = key, secret = secret)
}
#sc_report suite
# report_Suite <- 'demidas'


#OAS_auth
oasAuth <- function(){
  suppressMessages(library(roas))
  my_credentials <- oas_build_credentials(account = 'Dominion',username='BrossmanC', password = 'cabr1234@')
  return(my_credentials)
}

#dfp auth
dfpAuth <- function(){
  suppressMessages(library("rdfp"))
  options(rdfp.network_code = "29685107")
  options(rdfp.application_name = "MyApp")
  options(rdfp.client_id = "992031099147-71td3s047cpnmikis4qald76o4l56476.apps.googleusercontent.com")
  options(rdfp.client_secret = "Vlt2eQlMsRNeHvE8UfVA9qSy")
  dfp_auth()
}

redShiftAuth <- function(){
  suppressMessages(require(RJDBC))
  #CLASSPATH <- paste0(path,"mysql-connector-java-5.1.39-bin.jar")
  CLASSPATH <- "C:\\Users\\christopher.brossman\\Documents\\Projects\\work\\newDFP\\RedshiftJDBC42-1.1.17.1017.jar"
  driver <- JDBC("com.amazon.redshift.jdbc42.Driver",classPath= CLASSPATH," ")
  URL = "jdbc:redshift://sapi-37-dominion.cmizbsfmzc6w.us-east-1.redshift.amazonaws.com:5439/sapi_797"
  UID = "sapi_workspace_49554"
  PASS = "zJ2mU6aP6tM7oF0b"
  con <- dbConnect(driver, URL, UID, PASS)
  return(con)
}

rm(file.sources,i,path)