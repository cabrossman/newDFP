# app-inventory-self-service/server.R
# Copyright 2016
# Dominion Enterprises, All rights reserved.

keys <- queryPostgresDB('select * from ad_ops.customtargetingkeys')
values <- queryPostgresDB('select * from ad_ops.customtargetingvalues')
emails <- c('jeff.nelson@homes.com', 
            'tamara.gebre@homes.com', 
            'kristen.rowland@homes.com', 
            'brent.matson@homes.com', 
            'steve.mortimer@homes.com', 
            'pete.zuchowski@homes.com',
            'collin.magee@homes.com',
            'xiaoshan.liu@dominionenterprises.com',
            'carol.tucker@homes.com', 
            'erin.ruane@homes.com', 
            'jay.williams@homes.com', 
            'eric.hinkle@homes.com', 
            'angel.frazier@homes.com', 
            'chuck.goode@homes.com')

shinyServer(function(input, output, session) {

  requests_made <- 0
  
  run_start_datetime <- Sys.time()
  
  cancel.onSessionEnded <- session$onSessionEnded(function() {
    tryCatch({
      this_ip <- NULL
      try({
        isolate({ this_ip <- input$idinfo})
        if(is.null(this_ip) || is.na(this_ip) || this_ip==''){
          this_ip <- '123'
        }
      })
      if(get_os()=='linux'){
        try({
          # take the script end time
          run_end_datetime <- Sys.time()
          # calculate run_total_time_in_min
          run_total_time_in_min <- as.numeric(round(difftime(run_end_datetime, run_start_datetime, units="mins"), 4))
          isolate({this_query <- parseQueryString(session$clientData$url_search)})
          jsonBody <- sprintf('{
                              "session_minutes": %f,
                              "requests_made": %i,
                              "uuid": "%s",
                              "appnumb": "%s",
                              "app_name": "inventory-self-service",
                              "ip_address": "%s",
                              "keen": {
                              "addons": [
                              {
                              "name" : "keen:ip_to_geo",
                              "input" : {
                              "ip" : "ip_address"
                              },
                              "output" : "ip_geo_info"
                              }
                              ]
                              }
        }',  run_total_time_in_min, 
                              as.integer(ifelse(exists('requests_made'), requests_made, 0)),
                              ifelse(is.null(this_query$sid), '',  this_query$sid),
                              ifelse(is.null(this_query$anum), '', this_query$anum),
                              ifelse(is.null(this_ip), '123', this_ip))
          h <- basicHeaderGatherer()
          t <- basicTextGatherer()
          httpHeader <- c("Accept-Type"="application/json", 'Content-Type'="application/json")
          curlPerform(url="https://api.keen.io/3.0/projects/56099dee672e6c3c9869845a/events/inventory_self_service_session_tracking?api_key=4338065540698c1e19638552e814a2112da854c13dcb88fd04908adbfbe3030d042bd8a071a4d21ebd48db80f9dc0a75a05c0e2002b624584caf34c6e85cdebbb230e98cb06f0957980cf6dc7ea237eb20be4777109c00034e720653bf980f91cadc971b4a760df132e5b32ac97aed33", 
                      httpheader=httpHeader, 
                      headerfunction = h$update, 
                      writefunction = t$update, 
                      ssl.verifypeer=F, postfields=jsonBody)
        })
        try({
          f <- paste0('/srv/shiny-server/apps/inventory-self-service/data/openconnections/', this_query$anum, '_', this_query$sid)
          unlink(f, recursive = FALSE, force = TRUE)
        })
      }
    }, error=function(e){
      cat('error')
    })
  })
    
  # update the dropdowns
  observe({
    updateSelectizeInput(session, 
                         inputId='email', label="Send Breakdown email to: ",
                         selected = NULL,
                         choices = emails,
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Pick one or more email addresses',
                                        dropdownParent = 'body'))
    valueslist <- values %>% filter(customtargetingkeyid=='166747') %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId='homesdma', label="Homes DMA Search Term Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a Homes DMA',
                                        dropdownParent = 'body'))
    valueslist <- values %>% filter(customtargetingkeyid=='166507') %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId="city", label="Homes City Search Term Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a City',
                                        dropdownParent = 'body'))
    valueslist <- values %>% filter(customtargetingkeyid=='170467') %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId="county", label="Homes County Search Term Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a County',
                                        dropdownParent = 'body'))
    valueslist <- values %>% filter(customtargetingkeyid=='153907') %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId="state", label="Homes State Search Term Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a State',
                                        dropdownParent = 'body'))
    valueslist <- values %>% filter(customtargetingkeyid=='166147') %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId="status", label="Listing Type Search Term Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a Listing Type',
                                        dropdownParent = 'body'))
  })
  
  observe({
    if (input$size=='300x250'){
      filter_list <- c('Bottom')
    } else if (input$size=='320x50'){
      filter_list <- c('Liner2', 'Middle')
    } else if (input$size=='320x150'){
      filter_list <- c('Liner1')
    } else {
      filter_list <- c('LeftText', 'RightText')
    }
    valueslist <- values %>% filter(customtargetingkeyid=='136027', 
                                    name %in% filter_list) %>% select(name,id) %>% arrange(name) %>% as.data.frame()
    updateSelectizeInput(session, 
                         inputId="pos", label="Position Targeting: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names=as.character(valueslist$name)), 
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter a Position',
                                        dropdownParent = 'body'))
  }, priority = 99)
  
  # goal unit type changes based on cost type
  observe({
    if(as.integer(input$priority) < 6){
      shinyjs::show("pct_sign")
      updateRadioButtons(session, 
                         "unitType", "Unit Type: ", 
                         c("Impressions"='IMPRESSIONS'), inline=T)
      updateNumericInput(session, "units", "Goal as % of Total Impressions", value=50, min=0, max=100, step=5)
    } else {
      shinyjs::hide("pct_sign")
      if(input$costType=='CPM'){
        updateRadioButtons(session, 
                           "unitType", "Unit Type: ", 
                           c("Impressions"='IMPRESSIONS'), inline=T)
        updateNumericInput(session, "units", "Impression Goal Count", value=1000, min=0, max=10000000000, step=1000)
      } else {
        updateRadioButtons(session, 
                           "unitType", "Unit Type: ", 
                           c("Clicks"='CLICKS'), inline=T)
        updateNumericInput(session, "units", "Click Goal Count", value=100, min=0, max=1000000, step=25)
      }
    }
  })
  
  # when ad size changes update the position spots available
  observe({
    if (input$size=='300x250'){
      filter_list <- c('Bottom')
      updateRadioButtons(session, 
                         inputId="adUnitId", label="Ad Unit: ", 
                         choices=c('Homes_Mobile ROS'=34976467, 
                                   'Homes_Mobile SearchResults'=34976707, 
                                   'Homes_Mobile Details'=34976587), 
                         selected=34976467)
    } else if (input$size=='320x50'){
      filter_list <- c('Liner2', 'Middle')
      updateRadioButtons(session, 
                         inputId="adUnitId", label="Ad Unit: ", 
                         choices=c('Homes_Mobile ROS'=34976467, 
                                   'Homes_Mobile SearchResults'=34976707, 
                                   'Homes_Mobile Details'=34976587), 
                         selected=34976467)
    } else if (input$size=='320x150'){
      filter_list <- c('Liner1')
      updateRadioButtons(session, 
                         inputId="adUnitId", label="Ad Unit: ", 
                         choices=c('Homes_Mobile SearchResults'=34976707), 
                         selected=34976707)
    } else {
      filter_list <- c('LeftText', 'RightText')
      updateRadioButtons(session, 
                         inputId="adUnitId", label="Ad Unit: ", 
                         choices=c('Homes_Mobile Details'=34976587), 
                         selected=34976587)
    }
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      t <- constructInventoryTargetingList(adUnitId=input$adUnitId, 
                                           usgeoip=input$usgeoip,
                                           criterias=list('136027'=input$pos,
                                                          '166147'=input$status,
                                                          '166747'=if(input$homesdmaexclusionflag) values %>% 
                                                            filter(customtargetingkeyid=='166747') %>% 
                                                            filter(!(id %in% input$homesdma)) %>% .$id else input$homesdma,
                                                          '153907'=if(input$stateexclusionflag) values %>% 
                                                            filter(customtargetingkeyid=='153907') %>% 
                                                            filter(!(id %in% input$state)) %>% .$id else input$state,
                                                          '170467'=if(input$countyexclusionflag) values %>% 
                                                            filter(customtargetingkeyid=='170467') %>% 
                                                            filter(!(id %in% input$county)) %>% .$id else input$county,
                                                          '166507'=if(input$cityexclusionflag) values %>% 
                                                            filter(customtargetingkeyid=='166507') %>% 
                                                            filter(!(id %in% input$city)) %>% .$id else input$city))
      
      goalType <- if(as.integer(input$priority)<6) 'DAILY' else 'LIFETIME'
      unitType <- if(as.integer(input$priority)<6 | input$costType=='CPM') 'IMPRESSIONS' else 'CLICKS'
      units <- if(!is.null(input$units) && is.finite(input$units) && input$units>0) input$units else 1
                                                          
      hypothetical_line_item <- constructLineItem(startDate=input$daterange[1],
                                                  endDate=input$daterange[2],
                                                  deliveryRateType=input$deliveryRateType,
                                                  frequencyCaps=list(maxImpressions=input$frequencycapunits, 
                                                                     numTimeUnits=input$frequencycaptimeunits, 
                                                                     timeUnit=input$frequencycaptimeframe),
                                                  lineItemType=if(as.integer(input$priority)<6) 'SPONSORSHIP' else 'STANDARD',
                                                  priority=input$priority,
                                                  costType=input$costType,
                                                  creativePlaceholders=input$size,
                                                  primaryGoal=list(goalType=goalType,
                                                                   unitType=unitType, 
                                                                   units=units), 
                                                  targeting=t)
      
      request_data <- list(lineItem=hypothetical_line_item,
                           forecastOptions=list(includeTargetingCriteriaBreakdown='true', 
                                                includeContendingLineItems='true'))
      
      r <- dfp_getAvailabilityForecast(request_data, as_df = F, verbose=F)
      requests_made <<- requests_made + 1
      units_for_goal_calc <- if(!is.null(input$units) && is.finite(input$units)) input$units else 0
      goal_text <- if(units_for_goal_calc==0) 'Not specified' else paste0(comma(if(as.integer(input$priority)<6) 
                                                                            as.integer(units_for_goal_calc/100 * as.numeric(r$rval$matchedUnits)) else 
                                                                              as.integer(r$rval$reservedUnits)), ' ', tolower(r$rval$unitType))
      availability_txt <- paste0('<div class="one-third-div"><h2>Availability</h2><ul><li>',
                                 paste0(
                                   c(paste0('<b>Matched</b>: ', comma(as.integer(r$rval$matchedUnits)), ' ', tolower(r$rval$unitType)),
                                     paste0('<b>Available</b>: ', comma(as.integer(r$rval$availableUnits)), ' ', tolower(r$rval$unitType)),
                                     paste0('<b>Possible</b>: ', comma(as.integer(r$rval$possibleUnits)), ' ', tolower(r$rval$unitType)), 
                                     paste0('<b>Goal</b>: ', goal_text)), 
                                   collapse='</li><li>'), '</li></ul></div>')
      definitions_txt <- paste0('<div class="two-third-div"><h2>Definitions</h2><ul><li>',
                                paste0(
                                  c('<b>Matched</b> -- How many units satisfy all specified criteria.',
                                    paste0('<b>Available</b> -- How many units can be booked without affecting any other line items. Booking more than this number ', 
                                           'can cause lower and same priority line items to underdeliver.'), 
                                    paste0('<b>Possible</b> -- How many units can be booked without affecting any higher priority line items. Booking more than', 
                                           'this number can cause the line item to underdeliver.')),
                                  collapse='</li><li>'), '</li></ul></div>')
      
      if(length(r$rval[grepl("contendingLineItems", names(r$rval))])>0){
        contending_stats <- ldply(r$rval[grepl("contendingLineItems", names(r$rval))], .fun = function(x) {
          x <- as.data.frame(x, stringsAsFactors = F)
        }, .id = NULL)
        # take, at most, 10 contending items
        line_item_list <- dfp_getLineItemsByStatement(list(filterStatement=
                                                        list(query=paste0("WHERE id IN (", 
                                                                          paste0(head(contending_stats$lineItemId, 10), 
                                                                                 collapse=',') ,")"))))
        contending_html <- create_contending_html(line_item_list, contending_stats)
        contending_txt <- paste0('<h2>Contending Items</h2><ol><li>', paste0(contending_html$txt, collapse='</li><li>'), '</li></ol><hr/>')
      } else {
        contending_txt <- ''
      }
      
      session$sendCustomMessage(type = "myCallbackHandlerPushResultTxt1", paste0(availability_txt, definitions_txt))
      session$sendCustomMessage(type = "myCallbackHandlerPushResultTxt2", contending_txt)
      
      shinyjs::show("availability_result")
      shinyjs::show("contending_result")
      shinyjs::show("clearform_msg")
      session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
      shinyjs::hide("submit_msg")
      
      tryCatch({
        # log the request to common google doc
        logging_wkbk <- logging_wkbk %>% 
          gs_add_row(ws="Request Log", 
                     input = c(format(Sys.time(),'%m-%d-%Y %X'),
                               input$size, 
                               paste0(names(adUnitNamesMap[adUnitNamesMap %in% input$adUnitId]), collapse="|"),
                               input$priority,
                               input$costType,
                               format(input$daterange[1], '%m-%d-%Y'),
                               format(input$daterange[2], '%m-%d-%Y'),
                               input$usgeoip,
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='166747', 
                                                               valueids=input$homesdma), collapse="|"),
                               input$homesdmaexclusionflag,
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='153907', 
                                                               valueids=input$state), collapse="|"),
                               input$stateexclusionflag,
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='166507', 
                                                               valueids=input$city), collapse="|"),
                               input$cityexclusionflag,
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='170467', 
                                                               valueids=input$county), collapse="|"),
                               input$countyexclusionflag,
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='166147', 
                                                               valueids=input$status), collapse="|"),
                               paste0(convertKeyValueIdToLabel(valuesmap=values, 
                                                               keyid='136027', 
                                                               valueids=input$pos), collapse="|"),
                               units,
                               unitType,
                               goalType,
                               input$deliveryRateType,
                               input$frequencycapunits,
                               input$frequencycaptimeunits, 
                               input$frequencycaptimeframe,
                               input$monthly_breakdown,
                               input$geo_breakdown,
                               paste0(input$email, collapse="|")))
      
      }, error=function(e){
        stop('Error Logging to Google Sheet. Will not affect inventory numbers.')
      })
      
      if(input$geo_breakdown | input$monthly_breakdown){
        this_email <- input$email
        if(is.null(this_email) || this_email==''){
          stop('Must provide an email to receive breakdowns')
        }
        this_start <- input$daterange[1]
        this_end <- input$daterange[2]
        this_geo_breakdown <- input$geo_breakdown
        this_monthly_breakdown <- input$monthly_breakdown
        save(this_email, this_start, this_end, 
             this_monthly_breakdown, this_geo_breakdown, request_data, 
             file=paste0('/srv/shiny-server/apps/inventory-self-service/queue/', 
                         sprintf("%s_%s.RData", 
                                 humanTime(), 
                                 digest(this_email))))
        shinyjs::show("breakdown_msg")
      } else {
        shinyjs::hide("breakdown_msg")
      }
    },
    error = function(err) {
      # clear results on error to reduce confusion
      session$sendCustomMessage(type = "myCallbackHandlerPushResultTxt1", "")
      session$sendCustomMessage(type = "myCallbackHandlerPushResultTxt2", "")
      # show the error text
      session$sendCustomMessage(type = "myCallbackHandlerSendError", 
                                gsub('Error in doTryCatch\\(return\\(expr\\), name, parentenv, handler\\): |Error in value\\[\\[3L\\]\\]\\(cond\\): ', '', err))
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
      session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::reset("form")
    # clear the bottom portion
    session$sendCustomMessage(type = "myCallbackHandlerClearResultTxt", "reset")
    shinyjs::hide("clearform_msg")
  })
})