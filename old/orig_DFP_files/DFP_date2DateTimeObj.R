# This is more complicated to understand, hand coding the values into a list to submit availability request ------------------------------------

# first, it's helpful to have a function that takes the current time
# and converts it into a DFP-friendly list
# you'll note that we add an hour if requesting for today because
# forecasts must "start" in the future. If you use the exact time right now
# when you create the object, then it will already be in the past by the time you 
# submit to DFP
DFP_date2DateTimeObj <- function(this_date,
                                 daytime=c('beginning','end'), 
                                 timeZoneID='America/New_York'){
  
  this_hour <- if(daytime=='beginning') 0 else 23
  this_minute <- if(daytime=='beginning') 0 else 59
  this_second <- if(daytime=='beginning') 0 else 59
  
  x <- list(date=list(year=as.integer(format(this_date, '%Y')), 
                      month=as.integer(format(this_date, '%m')), 
                      day=as.integer(format(this_date, '%d'))), 
            hour=if(this_hour == 0 & this_date == Sys.Date()) pmin(hour(Sys.time()+hours(1)), 23) else this_hour, 
            minute=if(this_minute == 0 & this_date == Sys.Date()) pmin(minute(Sys.time()), 59) else this_minute,
            second=if(this_second == 0 & this_date == Sys.Date()) pmin(as.integer(second(Sys.time())), 59) else this_second,
            timeZoneID=timeZoneID)
  return(x)
}
