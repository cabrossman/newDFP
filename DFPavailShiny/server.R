shinyServer(function(input, output, session) {
  # memory placeholder for global list variable r
  r <- NULL
  
################################################################
# reactive data inputs based on ad unit setup. only the page is not in here, as that is done based on alias
################################################################
  
  #bucket reactive
  dataSourceBucket <- reactive({
    #brand/class bucket
    x <- keyList$ad_unit_ids %>% mutate_each(funs(as.character)) %>% filter(id == input$adUnitId) %>% select(site = name) %>% 
      inner_join(keyList$targetsBySite, by='site') %>% filter(key == 'bucket') %>% distinct(value) %>% 
      arrange(value) %>% mutate(bucket = value) %>% select(id = value, name = bucket)
    return(x)
    
  })
  
  #make reactive
  dataSourceMake <- reactive({
    #make
    a <- keyList$keyVal_ids %>% filter(customertargetingname == 'make', status=='ACTIVE')  %>% distinct(customertargetingname, name, id)
    b <- keyList$ad_unit_ids %>% mutate_each(funs(as.character)) %>% filter(id == input$adUnitId) %>% select(site = name) %>% 
      inner_join(keyList$targetsBySite, by='site') %>% inner_join(a, by=c('key' = 'customertargetingname', 'value'='name')) %>%
      select(id, name = value) %>% distinct() %>% arrange(name)
    return(b)
    
  })
  
  #class reactive
  dataSourceClass <- reactive({
    #class
    a <- keyList$keyVal_ids %>% filter(customertargetingname == 'class', status=='ACTIVE')  %>% distinct(customertargetingname, name, id)
    b <- keyList$ad_unit_ids %>% mutate_each(funs(as.character)) %>% filter(id == input$adUnitId) %>% select(site = name) %>% 
      inner_join(keyList$targetsBySite, by='site') %>% inner_join(a, by=c('key' = 'customertargetingname', 'value'='name')) %>%
      select(id, name = value) %>% distinct() %>% arrange(name)
    return(b)
    
  })
  
  #pos reactive
  dataSourcePos <- reactive({
    #class
    a <- keyList$keyVal_ids %>% filter(customertargetingname == 'pos', status=='ACTIVE')  %>% distinct(customertargetingname, name, id)
    b <- keyList$ad_unit_ids %>% mutate_each(funs(as.character)) %>% filter(id == input$adUnitId) %>% select(site = name) %>% 
      inner_join(keyList$targetsBySite, by='site') %>% inner_join(a, by=c('key' = 'customertargetingname', 'value'='name')) %>%
      select(id, name = value) %>% distinct() %>% arrange(name)
    return(b)
    
  })
  
  #page reactive
  dataSourcePage <- reactive({
    a <- keyList$keyVal_ids %>% filter(customertargetingname == 'Page', status=='ACTIVE') %>% 
      mutate(boats = ifelse(grepl('^BOATS',displayname)|!grepl('^BT|^YW',displayname),1,0)) %>%
      mutate(bt = ifelse(grepl('^BT',displayname)|!grepl('^BOATS|^YW',displayname),1,0)) %>%
      mutate(yw = ifelse(grepl('^YW',displayname)|!grepl('^BOATS|^BT',displayname),1,0))
    
      boats_data <- a %>% filter(boats == 1) %>% select(id, name = displayname) %>% distinct() %>% arrange(name)
      yw_data <- a %>% filter(yw == 1) %>% select(id, name = displayname) %>% distinct() %>% arrange(name)
      bt_data <- a %>% filter(bt == 1) %>% select(id, name = displayname) %>% distinct() %>% arrange(name)
      
    site_selected <- keyList$ad_unit_ids %>% mutate_each(funs(as.character)) %>% filter(id == input$adUnitId) %>% select(site = name) %>% as.character()
    
    if(site_selected == 'Boats'){
      return(boats_data)
    } else if(site_selected == 'BoatTrader'){
      return(bt_data)
    } else{
      return(yw_data)
    }

  })
  
  # update the dropdowns
  observe({
    ################################################################################################################################################
    #ad units populated by ad unit table in global
    ################################################################################################################################################

    ################################################################################################################################################
    #key values
    ################################################################################################################################################
    #brand/class bucket reactive
    valueslist <- dataSourceBucket()
    updateSelectizeInput(session,
                         inputId='brand_class', label="Brand/Class Bucket(s): ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Brand/Class Buckets',
                                        dropdownParent = 'body'))
    
    #position reactive
    valueslist <- dataSourcePos()
    updateSelectizeInput(session,
                         inputId='pos', label="Position(s): ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter positions',
                                        dropdownParent = 'body'))
    
    #page reactive
    valueslist <- dataSourcePage()
    updateSelectizeInput(session,
                         inputId='Page', label="Page(s): ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Pages',
                                        dropdownParent = 'body'))
    
    #site_version, reactive is on the UI side
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'site_version', status=='ACTIVE') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='site_version', label="Site Version(s): ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Site Version',
                                        dropdownParent = 'body'))

    #make reactive
    valueslist <- dataSourceMake()
    updateSelectizeInput(session,
                         inputId='make', label="Make selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter makes',
                                        dropdownParent = 'body'))
    
    #class reactive
    valueslist <- dataSourceClass()
    updateSelectizeInput(session,
                         inputId='class', label="Class selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter classes',
                                        dropdownParent = 'body'))
    
    #type -- not reactive. Available for all ad units
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'type', status=='ACTIVE') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='type', label="Type selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter types',
                                        dropdownParent = 'body'))
    
    #length -- not reactive. Available for all ad units
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'length', status=='ACTIVE') %>% distinct(id,name) %>% mutate(s1 = ifelse(grepl('^[0-9]{3}',name),1,0)) %>% arrange(s1,name) %>% select(id,name)
    updateSelectizeInput(session,
                         inputId='length', label="Length selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter lengths',
                                        dropdownParent = 'body'))
    
    #year -- not reactive. Available for all ad units
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'year', status=='ACTIVE') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='year', label="Year selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter years',
                                        dropdownParent = 'body'))
    
    #condition -- not reactive. Available for all ad units
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'condition', status=='ACTIVE') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='condition', label="Condition selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter condition',
                                        dropdownParent = 'body'))
    
    #state reactive handled in UI
    valueslist <- keyList$keyVal_ids %>% filter(customertargetingname == 'state', status=='ACTIVE') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='stateBTsearch', label="State on BT search selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter states',
                                        dropdownParent = 'body'))
    
    ################################################################################################################################################
    #geo keys
    ################################################################################################################################################
    #Continent
    valueslist <- keyList$country_codes %>% distinct(continent) %>% filter(continent != 'Unknown') %>% select(id = continent) %>% mutate(name = id) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='CONTINENT', label="Continent selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Continents',
                                        dropdownParent = 'body'))
    
    #boats custom region
    valueslist <- keyList$boats_regions %>% arrange(desc(countrycode),boats_region) %>% distinct(boats_region) %>% select(id = boats_region) %>% mutate(name = id) 
    updateSelectizeInput(session,
                         inputId='BOATS_REGION', label="Boats Region selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Continents',
                                        dropdownParent = 'body'))
    
    #Country
    valueslist <- keyList$geoids_ids %>% filter(type == 'COUNTRY') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='COUNTRY', label="Country selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Countries',
                                        dropdownParent = 'body'))
    
    
    #State/Province -- state from US provice from CA
    valueslist <- keyList$geoids_ids %>% filter(type %in% c('STATE','PROVINCE'), countrycode %in% c('US','CA')) %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='STATE-PROVINCE', label="State-Province selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter State-Province',
                                        dropdownParent = 'body'))
    
    #DMA
    valueslist <- keyList$geoids_ids %>% filter(type == 'DMA_REGION',countrycode=='US') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='DMA_REGION', label="DMA selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter DMAs',
                                        dropdownParent = 'body'))
    
    #COUNTY in FL
    valueslist <- keyList$geoids_ids %>% filter(type == 'COUNTY',countrycode=='US',canonicalparentid=='21142') %>% distinct(id,name) %>% arrange(name)
    updateSelectizeInput(session,
                         inputId='COUNTY', label="Counties in FL selections: ",
                         selected = NULL,
                         choices = structure(as.character(valueslist$id), names = as.character(valueslist$name)),
                         options = list(openOnFocus = TRUE,
                                        hideSelected = TRUE,
                                        placeholder = 'Enter Counties',
                                        dropdownParent = 'body'))
    
    
  })
  
  # goal unit type changes based on cost type
  observe({
    if(as.integer(input$priority) < 6){ # these are sponsorships where booking is percentage based
      updateNumericInput(session, "units", "Goal as % of Total Impressions", value=100, min=0, max=100, step=5)
    } else {
      if(input$costType=='CPM'){ # these are based on actual figures/impressions
        updateRadioButtons(session, "unitType", "Unit Type: ",  c("Impressions"='IMPRESSIONS'), inline=T)
        updateNumericInput(session, "units", "Impression Goal Count", value=10000, min=0, max=10000000000, step=1000)
      } else {
        updateRadioButtons(session, "unitType", "Unit Type: ", c("Clicks"='CLICKS'), inline=T)
        updateNumericInput(session, "units", "Click Goal Count", value=100, min=0, max=1000000, step=25)
      }
    }
  })
  

  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    
    # Save the data (show an error message in case of error)
    try({
      
      #build criterias list
      criterias <- NULL
      #check to see if any custom criteria has been selected
      totCustomCriterias <- length(input$class) + length(input$condition) + length(input$length) + length(input$make) + length(input$Page) + 
        length(input$pos) + length(input$site_version) + length(input$stateBTsearch) + length(input$type) + length(input$year) + length(input$brand_class)
      
      
      if(totCustomCriterias > 0){#if we have custom criterias
        #create null list to hold values
        criterias <- list()
        #if we have classes or bucket
        if(length(input$class)>0 | length(input$brand_class)>0){
          #get distinct list of classes in the class field or bucket field
          bucketclasskeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'class', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          classkeys <- unique(c(input$class, bucketclasskeys))
          #append to criterias
          criterias[['590679']] <- classkeys[classkeys != ""]
        }
        #append condition to criterias
        if(length(input$condition)>0){criterias[['590799']] <- input$condition}
        #append length to criterias
        if(length(input$length)>0){criterias[['591279']] <- input$length}
        #append make or brandclass make to criterias
        if(length(input$make)>0  | length(input$brand_class)>0){
          #get distinct list of makes
          bucketmakekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'make', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          makekeys <- unique(c(input$make, bucketmakekeys))
          
          #pass to criterias
          criterias[['589479']] <- makekeys[makekeys != ""]
        }
        #append page to criterias
        if(length(input$Page)>0){criterias[['591399']] <- input$Page}
        #pos
        if(length(input$pos)>0){criterias[['589839']] <- input$pos}
        #site version
        if(length(input$site_version)>0){criterias[['591639']] <- input$site_version}
        #bt search
        if(length(input$stateBTsearch)>0){criterias[['591759']] <- input$stateBTsearch}
        #type or brand class
        if(length(input$type)>0 | length(input$brand_class) > 0){
          #get unique list of type
          buckettypekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'type', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          if(NROW(buckettypekeys)>0 | length(input$type)>0){
            typekeys <- unique(c(input$type, buckettypekeys))
            
            criterias[['591879']] <- typekeys[typekeys != ""]
          }
          
        }
        #year
        if(length(input$year)>0){criterias[['591999']] <- input$year}
      }
      
      #build geographic vector
      gvec <- NULL
      #check to see if any geo entry
      if(length(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, input$CONTINENT, input$BOATS_REGION))>0){
        
        #get ids and unique of all geo entries. GEO ARE OR criterias
        geoContinent <- keyList$geoids_ids %>% filter(type == 'COUNTRY') %>% inner_join(keyList$country_codes, by="countrycode") %>% 
          filter(continent %in% input$CONTINENT) %>% select(id) %>% list() %>% unlist() %>% as.character()
        
        geoBoats <- keyList$boats_regions %>% filter(boats_region %in% input$BOATS_REGION) %>% select(id)  %>% list() %>% unlist() %>% as.character()
        
        gvec <- unique(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, geoContinent, geoBoats))
      }
      
      #make API call
      r <<-   forecastImpressions(adUnitVec = input$adUnitId,
                                 geoVec = gvec,
                                 criteriasListOfNamedVecs=criterias,
                                 keyList = keyList,
                                 boolStringChar = input$booleanLogic,
                                 start_date = input$daterange[1],
                                 end_date = input$daterange[2],
                                 deliveryRateTypeChar = input$deliveryRateType,
                                 #frequencyCapsChar = NULL,
                                 frequencyCapsChar=list(maxImpressions=input$frequencycapunits,
                                                    numTimeUnits=input$frequencycaptimeunits,
                                                    timeUnit=input$frequencycaptimeframe),
                                 lineItemTypeChar = if(as.integer(input$priority)<6) 'SPONSORSHIP' else 'STANDARD',
                                 priorityInt = input$priority,
                                 costTypeChar = input$costType,
                                 goalTypeChar = if(as.integer(input$priority)<6) 'DAILY' else 'LIFETIME',
                                 unitTypeChar=if(as.integer(input$priority)<6 | input$costType=='CPM') 'IMPRESSIONS' else 'CLICKS',
                                 unitsInt=if(!is.null(input$units) && is.finite(input$units) && input$units>0) input$units else 1
                                 )
      
      #output
      #global rate entered. Used in other parts, but only updated on submit
      rate_entered <<- ifelse(input$costType=='CPM',input$CPM/1000, input$CPM)
      
      #adjust units used in goal
      units_for_goal_calc <- if(!is.null(input$units) && is.finite(input$units)) input$units else 0
      #create goal text for HTML output
      goal_text <- if(units_for_goal_calc==0) 'Not specified' else paste0(comma(if(as.integer(input$priority)<6) 
        as.integer(units_for_goal_calc/100 * as.numeric(r$rval$matchedUnits)) else 
          as.integer(r$rval$reservedUnits)), ' ', tolower(r$rval$unitType))
      
      #did we have enough impressions/clicks to meet goal?
      guarenteed <- ifelse(as.integer(input$priority)<6,'', ifelse(as.numeric(r$rval$possibleUnits) > units_for_goal_calc,' : <b>Sufficient</b> possible impression to meet goal',' : <b>Insufficient</b> possible impressions to meet goal'))
        
      #output of avails HTML
      output$availability_txt <-renderText({  paste0('<div class="avails-div"><h2>Availability</h2><ul><li>',
                                 paste0(
                                   c(paste0('<b>Matched</b>: ', comma(as.integer(r$rval$matchedUnits)), ' ', tolower(r$rval$unitType),' : <b>Total Cost</b>: $',comma(round(as.numeric(r$rval$matchedUnits)*rate_entered,2))),
                                     paste0('<b>Available</b>: ', comma(as.integer(r$rval$availableUnits)), ' ', tolower(r$rval$unitType),' : <b>Total Cost</b>: $',comma(round(as.numeric(r$rval$availableUnits)*rate_entered,2))),
                                     paste0('<b>Possible</b>: ', comma(as.integer(r$rval$possibleUnits)), ' ', tolower(r$rval$unitType),' : <b>Total Cost</b>: $',comma(round(as.numeric(r$rval$possibleUnits)*rate_entered,2))),
                                     paste0('<b>Goal</b>: ', goal_text,guarenteed)), 
                                   collapse='</li><li>'), '</li></ul></div>')
      })
      
      #output of definitions HTML
      output$definitions_txt <- renderText({  paste0('<div class="definitions-div"><h2>Definitions</h2><ul><li>',
                                paste0(
                                  c('<b>Matched</b> -- How many units satisfy all specified criteria.',
                                    paste0('<b>Available</b> -- How many units can be booked without affecting any other line items. Booking more than this number ', 
                                           'can cause lower and same priority line items to underdeliver.'), 
                                    paste0('<b>Possible</b> -- How many units can be booked without affecting any higher priority line items. Booking more than', 
                                           'this number can cause the line item to underdeliver.')),
                                  collapse='</li><li>'), '</li></ul></div>')
      })
      
      #output of contending line items
      output$contending_txt <- renderText({
      
        #get contending line items 
        if(length(r$rval[grepl("contendingLineItems", names(r$rval))])>0){
          contending_stats <- ldply(r$rval[grepl("contendingLineItems", names(r$rval))], .fun = function(x) {
            x <- as.data.frame(x, stringsAsFactors = F)
          }, .id = NULL)
          # take, at most, 10 contending items
          #make API call
          line_item_list <- dfp_getLineItemsByStatement(list(filterStatement=
                                                               list(query=paste0("WHERE id IN (", 
                                                                                 paste0(head(contending_stats$lineItemId, 10), 
                                                                                        collapse=',') ,")"))))
          #get contending HTML
          contending_html <- create_contending_html(line_item_list, contending_stats)
          contending_txt <- paste0('<h2>Contending Items - Showing ', ifelse(NROW(contending_stats)>10,10,NROW(contending_stats)),' of ',NROW(contending_stats),'</h2><ol><li>', paste0(contending_html$txt, collapse='</li><li>'), '</li></ol><hr/>')
        } else {
          #if no contending line items
          contending_txt <- ''
        }
        
      
      })
      #show buttons and jump to bottom of page
      session$sendCustomMessage(type = "myCallbackHandlerShowButton", "reset")
      session$sendCustomMessage(type = "myCallbackHandlerJumpToBottom", "reset")
      
    }) #try
  }) #submit button
  
  
##################################################################################################  
  #download contending line items and details
##################################################################################################    
  download_dat <- reactive({
    #must have some input to make this reactive.
    submitNum <- input$submit
    
    # force busy indicator here 
    session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
    
    #get details of contending
    contending_stats <- ldply(r$rval[grepl("contendingLineItems", names(r$rval))], .fun = function(x) {
      x <- as.data.frame(x, stringsAsFactors = F)
    }, .id = NULL)
    
    
    # download all contending line items and details
    line_item_list <- dfp_getLineItemsByStatement(list(filterStatement=
                                                         list(query=paste0("WHERE id IN (", 
                                                                           paste0(contending_stats$lineItemId, 
                                                                              collapse=',') ,")"))))
    
    #make list into DF
    df <- createContendingLineItemDF(line_item_list)
    
    # join specifics for contending line items
    df2 <- df %>% inner_join(contending_stats,by=c("id"="lineItemId"))
    
    # force busy indicator here
    session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
    
    return(df2)
  })
  
  #download button of contending line items
  output$downloadContending <- downloadHandler(
    filename = function() { 
      paste0('contending-',Sys.Date(), '.csv') 
    },
    content = function(file) {
      
        df2 <- download_dat()      
        #make list a DF
        write_csv(df2, file, na="")
      },
    contentType = "text/csv"
  )
  
  

##################################################################################################  
#download monthly Breakdown
##################################################################################################  
monthlyBreakdown <- reactive({
  #must have some input to make this reactive.
  submitNum <- input$submit
  
  # force busy indicator here 
  session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
  try({
    
      #build criterias list
      criterias <- NULL
      
      #check for any criterias
      totCustomCriterias <- length(input$class) + length(input$condition) + length(input$length) + length(input$make) + length(input$Page) + 
        length(input$pos) + length(input$site_version) + length(input$stateBTsearch) + length(input$type) + length(input$year) + length(input$brand_class)
      
      
      if(totCustomCriterias > 0){# if some criterias
        #create null list
        criterias <- list()
        #if bucket or class get unique
        if(length(input$class)>0 | length(input$brand_class)>0){
          bucketclasskeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'class', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          classkeys <- unique(c(input$class, bucketclasskeys))
          
          criterias[['590679']] <- classkeys[classkeys != ""]
        }
        #condition
        if(length(input$condition)>0){criterias[['590799']] <- input$condition}
        #length
        if(length(input$length)>0){criterias[['591279']] <- input$length}
        #if bucket or make get unique
        if(length(input$make)>0  | length(input$brand_class)>0){
          bucketmakekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'make', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          makekeys <- unique(c(input$make, bucketmakekeys))
          
          criterias[['589479']] <- makekeys[makekeys != ""]
        }
        #page
        if(length(input$Page)>0){criterias[['591399']] <- input$Page}
        #pos
        if(length(input$pos)>0){criterias[['589839']] <- input$pos}
        #site version
        if(length(input$site_version)>0){criterias[['591639']] <- input$site_version}
        #BT state
        if(length(input$stateBTsearch)>0){criterias[['591759']] <- input$stateBTsearch}
        #if type or bucket get unique
        if(length(input$type)>0 | length(input$brand_class) > 0){
          
          buckettypekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'type', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          if(NROW(buckettypekeys)>0 | length(input$type)>0){
            typekeys <- unique(c(input$type, buckettypekeys))
            
            criterias[['591879']] <- typekeys[typekeys != ""]
          }
          
        }
        #if year get unique
        if(length(input$year)>0){criterias[['591999']] <- input$year}
      }
      
      #build geographic vector
      gvec <- NULL
      
      if(length(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, input$CONTINENT, input$BOATS_REGION))>0){
        
        #get unique of all entries
        geoContinent <- keyList$geoids_ids %>% filter(type == 'COUNTRY') %>% inner_join(keyList$country_codes, by="countrycode") %>% 
          filter(continent %in% input$CONTINENT) %>% select(id) %>% list() %>% unlist() %>% as.character()
        
        geoBoats <- keyList$boats_regions %>% filter(boats_region %in% input$BOATS_REGION) %>% select(id)  %>% list() %>% unlist() %>% as.character()
        
        gvec <- unique(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, geoContinent, geoBoats))
      }
      
      # turn a date into a 'monthnumber' relative to an origin
      monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01")) 
                            return(lt$year*12 + lt$mon) } 
      # compute a month difference as a difference between two monnb's
      mondf <- function(d1, d2) { return(monnb(d2) - monnb(d1)) }
      #compute difference
      mCnt <- mondf(input$daterange[1], input$daterange[2])
      
      #make a table
      t <- NULL
      for(q in 0:mCnt){
        if(q==0){
          s = input$daterange[1]
          e = input$daterange[1]
          day(e) = days_in_month(e)
        } else if(q==mCnt){
          s = input$daterange[1]
          month(s) = month(s) +q
          day(s) = 1
          e = input$daterange[2]
        } else {
          s = input$daterange[1]
          month(s) = month(s) +q
          day(s) = 1
          e = s
          day(e) <- days_in_month(e)
        }
        
        temp <- cbind.data.frame(start = s, end = e)
        t <- rbind(t,temp)
      }
        #API call for each month/partial month. store in temp table. append to fData, return fData
        fData <-NULL
        for(z in 1:NROW(t)){
          r <-   forecastImpressions(adUnitVec = input$adUnitId,
                                     geoVec = gvec,
                                     criteriasListOfNamedVecs=criterias,
                                     keyList = keyList,
                                     boolStringChar = input$booleanLogic,
                                     start_date = t$start[z],
                                     end_date = t$end[z],
                                     deliveryRateTypeChar = input$deliveryRateType,
                                     #frequencyCapsChar = NULL,
                                     frequencyCapsChar=list(maxImpressions=input$frequencycapunits,
                                                            numTimeUnits=input$frequencycaptimeunits,
                                                            timeUnit=input$frequencycaptimeframe),
                                     lineItemTypeChar = if(as.integer(input$priority)<6) 'SPONSORSHIP' else 'STANDARD',
                                     priorityInt = input$priority,
                                     costTypeChar = input$costType,
                                     goalTypeChar = if(as.integer(input$priority)<6) 'DAILY' else 'LIFETIME',
                                     unitTypeChar=if(as.integer(input$priority)<6 | input$costType=='CPM') 'IMPRESSIONS' else 'CLICKS',
                                     unitsInt=if(!is.null(input$units) && is.finite(input$units) && input$units>0) input$units else 1
          )
          
          temp <- cbind.data.frame(startDate = t$start[z], endDate = t$end[z],
                                   matchedUnits = r$rval$matchedUnits, availableUnits = r$rval$availableUnits, possibleUnits = r$rval$possibleUnits,
                                   unitType = r$rval$unitType, 
                                   RevMatchedUnits = as.numeric(r$rval$matchedUnits)*rate_entered,
                                   RevAvailableUnits= as.numeric(r$rval$availableUnits)*rate_entered, 
                                   RevPossibleUnits = as.numeric(r$rval$possibleUnits)*rate_entered
          )
          
          fData <- rbind(fData, temp)
          
        } #end of loop
      
      # force busy indicator here
      session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
      
      return(fData)


   }) # end of try



    
  }) #end of reactive
  

  #download button for monthly breakdown
  output$beakdownMonthly <- downloadHandler(
    filename = function() { 
      paste0('beakdownMonthly-',Sys.Date(), '.csv') 
    },
    content = function(file) {
      
      monthlyBreakdown <- monthlyBreakdown()      
      #make list a DF
      write_csv(monthlyBreakdown, file, na="")
    },
    contentType = "text/csv"
  )
  
##################################################################################################  
#download position Breakdown
##################################################################################################  
  posBreakdown <- reactive({
    #must have some input to make this reactive.
    submitNum <- input$submit
    
    # force busy indicator here 
    session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
    try({
      
      #build criterias list
      criterias <- NULL
      #check for any criterias
      totCustomCriterias <- length(input$class) + length(input$condition) + length(input$length) + length(input$make) + length(input$Page) + 
        length(input$pos) + length(input$site_version) + length(input$stateBTsearch) + length(input$type) + length(input$year) + length(input$brand_class)
      
      
      if(totCustomCriterias > 0){#if some criterias
        #empty list to hold criterias
        criterias <- list()
        #if bucket or class get unique
        if(length(input$class)>0 | length(input$brand_class)>0){
          bucketclasskeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'class', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          classkeys <- unique(c(input$class, bucketclasskeys))
          
          criterias[['590679']] <- classkeys[classkeys != ""]
        }
        #condition
        if(length(input$condition)>0){criterias[['590799']] <- input$condition}
        #length
        if(length(input$length)>0){criterias[['591279']] <- input$length}
        #make or bucket get unique
        if(length(input$make)>0  | length(input$brand_class)>0){
          bucketmakekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'make', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          makekeys <- unique(c(input$make, bucketmakekeys))
          
          criterias[['589479']] <- makekeys[makekeys != ""]
        }
        #page
        if(length(input$Page)>0){criterias[['591399']] <- input$Page}
        #no position -- we loop through later
        #if(length(input$pos)>0){criterias[['589839']] <- input$pos}
        #site_version
        if(length(input$site_version)>0){criterias[['591639']] <- input$site_version}
        #bt state
        if(length(input$stateBTsearch)>0){criterias[['591759']] <- input$stateBTsearch}
        #if type or class get unique
        if(length(input$type)>0 | length(input$brand_class) > 0){
          
          buckettypekeys <- keyList$brand_class_buckets %>% filter(customertargetingname == 'type', bucket %in% input$brand_class) %>% 
            distinct(id) %>% list() %>% unlist() %>% as.character()
          
          if(NROW(buckettypekeys)>0 | length(input$type)>0){
            typekeys <- unique(c(input$type, buckettypekeys))
            
            criterias[['591879']] <- typekeys[typekeys != ""]
          }
          
        }
        #year
        if(length(input$year)>0){criterias[['591999']] <- input$year}
      }
      
      #build geographic vector
      gvec <- NULL
      
      if(length(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, input$CONTINENT, input$BOATS_REGION))>0){
        
        #get unique of all entries
        geoContinent <- keyList$geoids_ids %>% filter(type == 'COUNTRY') %>% inner_join(keyList$country_codes, by="countrycode") %>% 
          filter(continent %in% input$CONTINENT) %>% select(id) %>% list() %>% unlist() %>% as.character()
        
        geoBoats <- keyList$boats_regions %>% filter(boats_region %in% input$BOATS_REGION) %>% select(id)  %>% list() %>% unlist() %>% as.character()
        
        gvec <- unique(c(input$COUNTRY, input$`STATE-PROVINCE`, input$DMA_REGION, input$COUNTY, geoContinent, geoBoats))
      }
      
      #if no pos entered - return overall
      if(length(input$pos)==0){
        fData <- cbind.data.frame(pos = 'no pos seleced',
                                  matchedUnits = r$rval$matchedUnits, availableUnits = r$rval$availableUnits, possibleUnits = r$rval$possibleUnits,
                                  unitType = r$rval$unitType, 
                                  RevMatchedUnits = as.numeric(r$rval$matchedUnits)*rate_entered,
                                  RevAvailableUnits= as.numeric(r$rval$availableUnits)*rate_entered, 
                                  RevPossibleUnits = as.numeric(r$rval$possibleUnits)*rate_entered
        )
      } else{
        #loop through each position entered, store in temp table, append to fData. Return fData
        fData <-NULL
        for(z in 1:length(input$pos)){
          criterias[['589839']] <- input$pos[z]
          
          r <-   forecastImpressions(adUnitVec = input$adUnitId,
                                     geoVec = gvec,
                                     criteriasListOfNamedVecs=criterias,
                                     keyList = keyList,
                                     boolStringChar = input$booleanLogic,
                                     start_date = input$daterange[1],
                                     end_date = input$daterange[2],
                                     deliveryRateTypeChar = input$deliveryRateType,
                                     #frequencyCapsChar = NULL,
                                     frequencyCapsChar=list(maxImpressions=input$frequencycapunits,
                                                            numTimeUnits=input$frequencycaptimeunits,
                                                            timeUnit=input$frequencycaptimeframe),
                                     lineItemTypeChar = if(as.integer(input$priority)<6) 'SPONSORSHIP' else 'STANDARD',
                                     priorityInt = input$priority,
                                     costTypeChar = input$costType,
                                     goalTypeChar = if(as.integer(input$priority)<6) 'DAILY' else 'LIFETIME',
                                     unitTypeChar=if(as.integer(input$priority)<6 | input$costType=='CPM') 'IMPRESSIONS' else 'CLICKS',
                                     unitsInt=if(!is.null(input$units) && is.finite(input$units) && input$units>0) input$units else 1
          )
          
          #get name from id
          pos_name <- keyList$keyVal_ids %>% filter(id == input$pos[z]) %>% select(name) %>% distinct() %>% as.character()
          
          temp <- cbind.data.frame(pos = pos_name,
                                   matchedUnits = r$rval$matchedUnits, availableUnits = r$rval$availableUnits, possibleUnits = r$rval$possibleUnits,
                                   unitType = r$rval$unitType, 
                                   RevMatchedUnits = as.numeric(r$rval$matchedUnits)*rate_entered,
                                   RevAvailableUnits= as.numeric(r$rval$availableUnits)*rate_entered, 
                                   RevPossibleUnits = as.numeric(r$rval$possibleUnits)*rate_entered
          )
          
          fData <- rbind(fData, temp)
          
        } #end of loop
      }
      
      
      # force busy indicator here
      session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
      
      return(fData)
      
      
    }) # end of try
    
    
    
    
  }) #end of reactive
  
  
  #download button for position breakdown
  output$posBreakdown <- downloadHandler(
    filename = function() { 
      paste0('beakdownPos-',Sys.Date(), '.csv') 
    },
    content = function(file) {
      
      posBreakdown <- posBreakdown()      
      #make list a DF
      write_csv(posBreakdown, file, na="")
    },
    contentType = "text/csv"
  )
  
##################################################################################################   
#download geo breakdown
##################################################################################################  
  geoBreakdown <- reactive({
    #must have some input to make this reactive.
    submitNum <- input$submit
    
    # force busy indicator here 
    session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
    
    #for submit button API - loop through availabile breakdowns. Find geography. Bind all together in DF and return that DF
    dat <- NULL
    for(m in 1:length(r$rval)){
      if(names(r$rval)[m] == 'targetingCriteriaBreakdowns'){
        if(r$rval[m]$targetingCriteriaBreakdowns$targetingDimension == 'GEOGRAPHY'){
          temp <- cbind.data.frame(Geo = r$rval[m]$targetingCriteriaBreakdowns$targetingCriteriaName,
                                   availableUnits = r$rval[m]$targetingCriteriaBreakdowns$availableUnits,
                                   matchedUnits = r$rval[m]$targetingCriteriaBreakdowns$matchedUnits,
                                   availableRev = as.numeric(r$rval[m]$targetingCriteriaBreakdowns$availableUnits)*rate_entered,
                                   matchedRev = as.numeric(r$rval[m]$targetingCriteriaBreakdowns$matchedUnits)*rate_entered
          )
          dat <- rbind(dat,temp)
        }
      }
    }
    #if user is a hockey puck and doesnt enter any geo - return overall and say no geo selected
    if(is.null(dat)){
      dat <- cbind.data.frame(Geo = 'No Geo Selected',
                              availableUnits = r$rval$availableUnits,
                              matchedUnits = r$rval$matchedUnits,
                              availableRev = as.numeric(r$rval$availableUnits)*rate_entered,
                              matchedRev = as.numeric(r$rval$matchedUnits)*rate_entered
      )
    }
    
    # force busy indicator here
    session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
    
    return(dat)
  })
  
  #download button for geo breakdown
  output$beakdownGeo <- downloadHandler(
    filename = function() { 
      paste0('beakdownGeo-',Sys.Date(), '.csv') 
    },
    content = function(file) {
      
      df2 <- geoBreakdown()      
      #make list a DF
      write_csv(df2, file, na="")
    },
    contentType = "text/csv"
  )
  
##################################################################################################  
# download custom breakdown
##################################################################################################  
  customBreakdown <- reactive({
    #must have some input to make this reactive.
    submitNum <- input$submit
    
    # force busy indicator here 
    session$sendCustomMessage(type = "myCallbackHandlerAddBusy", "reset")
    
    #for submit button API - loop through availabile custom breakdowns Find custom Bind all together in DF and return that DF
    datCust <- NULL
    for(m in 1:length(r$rval)){
      if(names(r$rval)[m] == 'targetingCriteriaBreakdowns'){
        if(r$rval[m]$targetingCriteriaBreakdowns$targetingDimension == 'CUSTOM_CRITERIA'){
          temp <- cbind.data.frame(custom_criteria = r$rval[m]$targetingCriteriaBreakdowns$targetingCriteriaName,
                                   availableUnits = r$rval[m]$targetingCriteriaBreakdowns$availableUnits,
                                   matchedUnits = r$rval[m]$targetingCriteriaBreakdowns$matchedUnits,
                                   availableRev = as.numeric(r$rval[m]$targetingCriteriaBreakdowns$availableUnits)*rate_entered,
                                   matchedRev = as.numeric(r$rval[m]$targetingCriteriaBreakdowns$matchedUnits)*rate_entered
          )
          datCust <- rbind(datCust,temp)
        }
      }
    }
    #if no criterias - give overall to user
    if(is.null(datCust)){
      datCust <- cbind.data.frame(Geo = 'No Criteria Selected',
                              availableUnits = r$rval$availableUnits,
                              matchedUnits = r$rval$matchedUnits,
                              availableRev = as.numeric(r$rval$availableUnits)*rate_entered,
                              matchedRev = as.numeric(r$rval$matchedUnits)*rate_entered
      )
    }
    
    # force busy indicator here
    session$sendCustomMessage(type = "myCallbackHandlerRemoveBusy", "reset")
    
    return(datCust)
  })
  
  #download button for custom criterias
  output$breakdownCustom <- downloadHandler(
    filename = function() { 
      paste0('beakdownCustom-',Sys.Date(), '.csv') 
    },
    content = function(file) {
      
      df2 <- customBreakdown()      
      #make list a DF
      write_csv(df2, file, na="")
    },
    contentType = "text/csv"
  )
  
##################################################################################################  
# rate card logic
##################################################################################################  
  
  output$rateCard <- renderText({
    
    rateKV <- keyList$rate_card %>% inner_join(keyList$keyVal_ids, by=c('pos'='name')) %>% select(pos,boatscom,geo,georate,id)
    
    if(input$costType=='CPC'){ 
      # no rate card for CPC
      rateText <- paste0('Rate Card Rate: The rate-card rate is only available for type of CPM')
    }
    else if(length(input$pos)!=1){ 
      #rate card only available only one position
      rateText <- paste0('Rate Card Rate: Please select only one rate-card position')
    } 
    else{ 
      #if meet this criteria....
      
      #rate card only available for certain positions
      if(input$pos %in% rateKV$id){
        
        #cost of geographic targeting
        if(length(c(input$COUNTY,input$DMA_REGION)) > 0){
          # has dma or county
          rate <- rateKV %>% filter(id == input$pos, geo == 'DMA') %>% select(georate) %>% as.numeric()
        } else if(length(c(input$`STATE-PROVINCE`)) > 0){
          #state
          rate <- rateKV %>% filter(id == input$pos, geo == 'State') %>% select(georate) %>% as.numeric()
        } else if(length(input$BOATS_REGION)>0){
          #region
          rate <- rateKV %>% filter(id == input$pos, geo == 'Region') %>% select(georate) %>% as.numeric()
        } else{
          #national
          rate <- rateKV %>% filter(id == input$pos, geo == 'National') %>% select(georate) %>% as.numeric()
        }
        
        #add length cost
        if(length(input$length)>0){
          rate = rate + 4
        }
        
        #add year cost
        if(length(input$year)>0){
          rate = rate + 4
        }
        
        #add condition cost
        if(length(input$condition)>0){
          rate = rate + 4
        }
        
        #add BT search cost
        if(length(input$stateBTsearch)>0){
          rate = rate + 4
        }
        
        #add make/class cost
        if(length(input$make)>0 | length(input$class)>0){
          rate = rate + 6 
        }
        
        #add brand/class cost
        if(length(input$brand_class)>0){
          rate = rate + 2 
        }
        
        rateText <- paste0('Rate Card Rate: $',rate)
      } else{ #if not a rate card position
        rateText <- paste0('Rate Card Rate: Please Select Only One RateCard Position')
      }
      
    }
    
    #format text for UI
    return(paste0('<b>',rateText,'</b>'))
  })

}) #end of file

