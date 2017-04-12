# app-inventory-self-service/app-inventory-self-service-lib.R
# Copyright 2016
# Dominion Enterprises, All rights reserved.

date2DFPDateTimeObj <- function(this_date,
                                daytime=c('beginning','end'), 
                                timeZoneID='America/New_York'){
  
  this_hour <- if(daytime=='beginning') 0 else 23
  this_minute <- if(daytime=='beginning') 0 else 59
  this_second <- if(daytime=='beginning') 0 else 59
  
  x <- list(date=list(year=as.integer(format(this_date, '%Y')), 
                      month=as.integer(format(this_date, '%m')), 
                      day=as.integer(format(this_date, '%d'))), 
            hour=if(this_hour == 0 & this_date == Sys.Date()) pmin(hour(Sys.time()+hours(2)), 23) else this_hour, 
            minute=if(this_minute == 0 & this_date == Sys.Date()) pmin(minute(Sys.time()), 59) else this_minute,
            second=if(this_second == 0 & this_date == Sys.Date()) pmin(as.integer(second(Sys.time())), 59) else this_second,
            timeZoneID=timeZoneID)
  return(x)
}

convertKeyValueIdToLabel <- function(valuesmap, keyid, valueids){
  return(valuesmap %>% 
          filter(customtargetingkeyid==keyid) %>% 
          filter(id %in% valueids) %>% .$name)
}

constructLineItem <- function(startDate,
                              endDate,
                              deliveryRateType,
                              frequencyCaps,
                              lineItemType,
                              priority,
                              costType,
                              creativePlaceholders,
                              primaryGoal, 
                              targeting){
  final <- list()
  final$startDateTime <- date2DFPDateTimeObj(startDate, daytime='beginning')
  final$endDateTime <- date2DFPDateTimeObj(endDate, daytime='end')

  if(!is.null(deliveryRateType))
    final$deliveryRateType <- deliveryRateType
  
  if(is.null(frequencyCaps$maxImpressions) ||
     is.null(frequencyCaps$numTimeUnits) ||
     is.na(frequencyCaps$maxImpressions) || 
     is.na(frequencyCaps$numTimeUnits)){
    frequencyCaps <- NULL
  }
  if(!is.null(frequencyCaps))
    final$frequencyCaps <- frequencyCaps
  
  if(!is.null(lineItemType))
    final$lineItemType <- lineItemType
  if(!is.null(priority))
    final$priority <- priority
  if(!is.null(costType))
    final$costType <- costType
  
  split_size <- strsplit(creativePlaceholders, 'x')
  final$creativePlaceholders$size <- list(width=as.integer(split_size[[1]][1]), 
                                          height=as.integer(split_size[[1]][2]), 
                                          isAspectRatio='false')
  final$creativePlaceholders$expectedCreativeCount <- 1
  final$creativePlaceholders$creativeSizeType <- 'PIXEL'
  
  if(!is.null(primaryGoal))
    final$primaryGoal <- primaryGoal
  if(!is.null(targeting))
    final$targeting <- targeting
  
  finalfinal <- list(lineItem=final)
  
  return(finalfinal)
}

constructInventoryTargetingList <- function(adUnitId, usgeoip, criterias){
  
  final <- list(geoTargeting=NULL, 
                inventoryTargeting=NULL, 
                technologyTargeting=NULL,
                customTargeting=NULL)
  
  if(usgeoip=='Yes'){
    final$geoTargeting <- list(targetedLocations=list(id="2840", 
                                                      type="COUNTRY", 
                                                      displayName="United States"))
  }
  
  final$inventoryTargeting$targetedAdUnits <- list(adUnitId=adUnitId, includeDescendants='true')
  
  nonnull_criterias <- list()
  ii <- 1
  for(i in 1:length(criterias)){
    if(!is.null(criterias[[i]])){
      nonnull_criterias[[names(criterias)[i]]] <- criterias[[i]]
      ii <- ii + 1
    }
  }
  criterias <- nonnull_criterias
  
  if (length(criterias)>0){
    indiv_possibilities <- expand.grid(criterias, stringsAsFactors = F)
    ct_object <- vector("list", nrow(indiv_possibilities)+1)
    ct_object[[1]] <- 'OR'
    for(i in 1:nrow(indiv_possibilities)){
      one_ct_object <- vector("list", ncol(indiv_possibilities)+2)
      one_ct_object[[1]] <- 'AND'
      one_ct_object[[length(one_ct_object)]] <- c('type'="CustomCriteriaSet")
      names(one_ct_object) <- c('logicalOperator', rep('children', ncol(indiv_possibilities)), '.attrs')
      for(j in 1:ncol(indiv_possibilities)){
        one_ct_object[[j+1]] <- list(keyId=names(indiv_possibilities)[j], 
                                     valueIds=indiv_possibilities[i,j],
                                     operator='IS',
                                     `.attrs`=c('type'="CustomCriteria"))
      }
      ct_object[[i+1]] <- one_ct_object
    }
    names(ct_object) <- c('logicalOperator', rep('children', nrow(indiv_possibilities)))
    final$customTargeting <- ct_object
  }
  return(final)
}

create_contending_html <- function(line_item_list, contending_stats){
  res <- ldply(tail(line_item_list[grepl("rval", names(line_item_list))]$rval,-2), .fun=function(x, contending){
    # accept a list of line items gotten by statement and 
    # and return an html summary of each presented in a list
    txt <- paste0('<b>', x$name, '</b> with Status: ', x$status, '<br>', 
                  '<u>Contending</u>: ', comma(as.integer(contending[contending$lineItemId==x$id,2])), ' ', 
                  gsub('contending', '', names(contending)[2]),'<br>',
                  '<u>Priority</u>: ', stri_trans_totitle(x$lineItemType), ' - ', x$priority, '<br>',
                  '<u>Cost</u> ', dollar(round(as.numeric(x$costPerUnit$microAmount)/1000000)), ' ', x$costType,
                  ' with <u>Budget</u> of ', dollar(round(as.numeric(x$budget$microAmount)/1000000)),'<br>',
                  'For more info click <a href="https://www.google.com/dfp/29685107#delivery/LineItemDetail/', 
                  'orderId=', x$orderId, '&lineItemId=', x$id, '" target="_blank">HERE</a> to view this line item in DFP<br>')
    return(data.frame(txt=txt, contending=as.integer(contending[contending$lineItemId==x$id,2]), stringsAsFactors = F))
  }, .id=NULL, contending=contending_stats)
  res <- res[order(res$contending, decreasing=T),]
  return(res)
}


simpleCap <- function(x) {
  x <- gsub("_", " ", tolower(x))
  s <- strsplit(x, " ")[[1]]
  r <- paste(toupper(substring(s, 1,1)), substring(s, 2),
             sep="", collapse=" ")
  gsub('Ecpm', 'ECPM', gsub('Cpc', 'CPC', gsub('Cpm', 'CPM', gsub('Ctr', 'CTR', r))))
}

build_credentials <- function(account, username, password){
  return(paste0('<?xml version="1.0" encoding="UTF-8"?>
                <env:Envelope xmlns:n1="http://api.oas.tfsm.com/" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:env="http://schemas.xmlsoap.org/soap/envelope/">
                <env:Body>
                <n1:OasXmlRequest xmlns:n1="http://api.oas.tfsm.com/">
                <String_1>', account,  '</String_1>
                <String_2>', username, '</String_2>
                <String_3>', password, '</String_3>'))
}

request_builder <- function(credentials, adxml_request){
  return(paste0(credentials, 
                '<String_4>
                <![CDATA[
                <?xml version="1.0"?>',
                adxml_request, 
                ']]>
                </String_4>
                </n1:OasXmlRequest>
                </env:Body>
                </env:Envelope>'))
}

inventory_by_site <- function(credentials){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "Site"), parent = child1)
  c3 = newXMLNode("Keywords", attrs = c(type = "Site"), parent = child2)
  c6 = newXMLNode("Table", "ListStatistics", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  #   sss <- xmlToList(ss)
  #   # check has "Request OK."
  #   stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response/Site")[[1]])))
  #   # based on the action=List type=, parse
  #   View(xmlToDataFrame(nodes = getNodeSet(ss, "//List/Site")))
  # for reports 
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
  
}

position_delivery_info <- function(credentials, 
                                   site_id, 
                                   start_date, 
                                   end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Report"), parent = top)
  child2 = newXMLNode("Report", attrs = c(type = "Site"), parent = child1)
  c3 = newXMLNode("Id", site_id, parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)
  c6 = newXMLNode("Table", "Delivery.Site.Base.T254.01", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  #   sss <- xmlToList(ss)
  #   # check has "Request OK."
  #   stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response/Site")[[1]])))
  #   # based on the action=List type=, parse
  #   View(xmlToDataFrame(nodes = getNodeSet(ss, "//List/Site")))
  # for reports 
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
  
}

campaign_revenue_info <- function(credentials, 
                                  campaign_id=NULL,
                                  report_type=c('Executive Summary', 
                                                'Campaign Information',
                                                'Campaign Booked Revenue Summary',
                                                'Revenue By Campaign',
                                                'Advertiser and Agency Information',
                                                'Revenue By Creative',
                                                'Revenue By Creative by Site',
                                                'Network Revenue by Site'),
                                  start_date=NULL, 
                                  end_date=NULL){
  
  if (report_type=='Executive Summary'){
    table <- 'Revenue.Campaign.Base.T100.03'
  } else if (report_type=='Campaign Information'){
    table <- 'Revenue.Campaign.Base.T110.02'
  } else if (report_type=='Campaign Booked Revenue Summary'){
    table <- 'Revenue.Campaign.Base.T135.03'
  } else if (report_type=='Revenue By Campaign'){
    table <- 'Revenue.Campaign.Base.T145.02'
  } else if (report_type=='Advertiser and Agency Information'){
    table <- 'Revenue.Campaign.Base.T150.07'
  } else if (report_type=='Revenue by Creative'){
    table <- 'Revenue.Campaign.Base.T960.01'
  } else if (report_type=='Revenue by Creative by Site'){
    table <- 'Revenue.Campaign.Base.T960.02'
  } else if (report_type=='Network Revenue by Site'){
    table <- 'Revenue.Campaign.Network.T160.05'
  } else {
    stop('wrong report type')
  }
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Report"), parent = top)
  child2 = newXMLNode("Report", attrs = c(type = "Campaign"), parent = child1)
  if (!is.null(campaign_id)){
    c3 = newXMLNode("Id", campaign_id, parent = child2)
  }
  if (!is.null(start_date)){
    c4 = newXMLNode("StartDate", start_date, parent = child2)
  }
  if (!is.null(end_date)){
    c5 = newXMLNode("EndDate", end_date, parent = child2)
  }
  c6 = newXMLNode("Table", table, parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  # for reports 
  ns <- tail(getNodeSet(ss, "//row"),-3)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}



#Location View
#Campaign Detail View
campaign_geodelivery_info <- function(credentials, 
                                      campaign_id,
                                      location_type=c('State', 'City', 'Zip', 'DMA', 'MSA', 'Country'),
                                      start_date, 
                                      end_date){
  
  if (location_type=='State'){
    table <- 'Delivery.Campaign.Geotargeting.T324.01'
  } else if (location_type=='City'){
    table <- 'Delivery.Campaign.Geotargeting.T326.01'
  } else if (location_type=='Zip'){
    table <- 'Delivery.Campaign.Geotargeting.T328.01'
  } else if (location_type=='DMA'){
    table <- 'Delivery.Campaign.Geotargeting.T334.01'
  } else if (location_type=='MSA'){
    table <- 'Delivery.Campaign.Geotargeting.T332.01'
  } else if (location_type=='Country'){
    table <- 'Delivery.Campaign.Geotargeting.T322.01'
  } else {
    stop('wrong location type')
  }
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Report"), parent = top)
  child2 = newXMLNode("Report", attrs = c(type = "Campaign"), parent = child1)
  c3 = newXMLNode("Id", campaign_id, parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)
  c6 = newXMLNode("Table", table, parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  # for reports 
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}





list_sections2 <- function(credentials, id='Homes') {
  
  #campaign list
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Section"), parent = top)
  child2 = newXMLNode("Database", attrs = c(action = "list"), parent = child1)
  child3 = newXMLNode("SearchCriteria", parent = child2)
  child4 = newXMLNode("Id", id, parent = child3)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response/Section")[[1]])))
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//List/Section")))
}

# Type, Status, Id (exact), Url, SectionId, AdvertiserId, AgencyId, CampaignGroupId, 
# StartDate, EndDate, WhenCreated(condition), WhenModified(condition), PriorityLevel (condition)
# Completion, Reach, PurchaseOrder, InsertionOrderId, CampaignManager

my_search_criteria=list(newXMLNode("Status", 'L'),
                        newXMLNode("Url", 'www.homes.com'),
                        newXMLNode("WhenModified", attrs = c(condition = "GE"), '2015-06-01'), 
                        newXMLNode("WhenModified", attrs = c(condition = "LE"), '2015-10-07'))

list_campaigns <- function(credentials, search_criteria) {
  
  #campaign list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "Campaign"), parent = adxml_node)
  campaign_node = newXMLNode("Campaign", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = campaign_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # check has "Request OK."
  stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response/Campaign")[[1]])))
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//List/Campaign")))
}

list_dma_codes <- function(credentials, search_criteria=NULL) {
  
  #dma list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "Dma"), parent = adxml_node)
  campaign_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = campaign_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # check has "Request OK."
  stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response//Dma")[[1]])))
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/Dma")))
}

list_state_codes <- function(credentials, search_criteria=NULL) {
  
  #dma list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "State"), parent = adxml_node)
  campaign_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = campaign_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # check has "Request OK."
  stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response//State")[[1]])))
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/State")))
}

list_city_codes_by_country <- function(credentials, country='US') {
  
  #city list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "City"), parent = adxml_node)
  campaign_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", attrs = c(pageIndex = "2"),parent = campaign_node)
  country_node = newXMLNode("Country", parent = search_criteria_node)
  country_code_node = newXMLNode("Code", country, parent = country_node)
  
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  total_rows <- as.numeric(sss$Response$List$.attrs['totalNumberOfEntries'])
  numberOfRows <- as.numeric(sss$Response$List$.attrs['numberOfRows'])
  if (total_rows>numberOfRows){
    total_pages <- total_rows %/% numberOfRows
    if(total_rows %% numberOfRows >0)
      total_pages <- total_pages + 1
    final<-NULL
    for (i in 1:total_pages){
      
      adxml_node = newXMLNode("AdXML")
      request_node = newXMLNode("Request", attrs = c(type = "City"), parent = adxml_node)
      campaign_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
      search_criteria_node = newXMLNode("SearchCriteria", attrs = c(pageIndex = as.character(i)),parent = campaign_node)
      country_node = newXMLNode("Country", parent = search_criteria_node)
      country_code_node = newXMLNode("Code", country, parent = country_node)
      
      r <- as(adxml_node, "character")
      
      xmlBody <- request_builder(credentials=credentials, 
                                 adxml_request=r)
      
      h <- basicHeaderGatherer()
      t <- basicTextGatherer()
      httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
      curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
                  httpheader=httpHeader, 
                  headerfunction = h$update, 
                  writefunction = t$update, 
                  ssl.verifypeer=F, postfields=xmlBody)
      
      #pull out the results and format as XML
      doc <- xmlTreeParse(t$value(), asText=T)
      s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
      ss <- xmlParse(s)
      sss <- xmlToList(ss)
      res <- xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/City"))
      final <- rbind(final, res)
    }
  }
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/City")))
}


list_msa_codes <- function(credentials, search_criteria=NULL) {
  
  #dma list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "Msa"), parent = adxml_node)
  campaign_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = campaign_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # check has "Request OK."
  stopifnot(grepl('^Request OK.', xmlValue(getNodeSet(ss, "//Response//Msa")[[1]])))
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/Msa")))
}

list_pages <- function(credentials, search_criteria) {
  
  #campaign list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "Page"), parent = adxml_node)
  database_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = database_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/Page")))
}


list_sections <- function(credentials, search_criteria) {
  
  #campaign list
  adxml_node = newXMLNode("AdXML")
  request_node = newXMLNode("Request", attrs = c(type = "Section"), parent = adxml_node)
  database_node = newXMLNode("Database", attrs = c(action = "list"), parent = request_node)
  search_criteria_node = newXMLNode("SearchCriteria", parent = database_node)
  for (node in search_criteria){
    addChildren(search_criteria_node, node)
  }
  r <- as(adxml_node, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  sss <- xmlToList(ss)
  # based on the action=List type=, parse
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/List/Section")))
}

camp_pos_delivery_info <- function(credentials, 
                                   campaign_id, 
                                   start_date, 
                                   end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Report"), parent = top)
  child2 = newXMLNode("Report", attrs = c(type = "Campaign"), parent = child1)
  c3 = newXMLNode("Id", campaign_id, parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)
  c6 = newXMLNode("Table", "Delivery.Campaign.Base.T254.01", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
  
}

read_campaign <- function(credentials,
                          campaign_id){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Campaign"), parent = top)
  child2 = newXMLNode("Campaign", attrs = c(action = "read"), parent = child1)
  child3 = newXMLNode("Overview", parent = child2)
  c4 = newXMLNode("Id", campaign_id, parent = child3)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  overview <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Overview")))
  schedule <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Schedule")))
  schedule$Sections <- paste0(sort(unlist(xmlToDataFrame(getNodeSet(ss, "//Campaign/Schedule/Sections/SectionId")))), collapse="|")
  content <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Content")))
  pages <- tryCatch({ pages <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Pages")))}, error=function(e){return(NULL)})
  target <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Target")))
  exclude <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Exclude")))
  billing <- as.list(xmlToDataFrame(nodes = getNodeSet(ss, "//Campaign/Billing")))
  # for reports 
  res <- list(Overview=overview, Schedule=schedule, Content=content, Pages=pages, Target=target, Exclude=exclude, Billing=billing)
  return(res)
  
}

read_page <- function(credentials,
                      Url){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Page"), parent = top)
  child2 = newXMLNode("Database", attrs = c(action = "read"), parent = child1)
  child3 = newXMLNode("Page", parent = child2)
  child4 = newXMLNode("Url", Url, parent = child3)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  return(xmlToDataFrame(nodes = getNodeSet(ss, "//Response/Page")))
  return(res)
  
}

pos_booked_inventory <- function(credentials,
                                 position, 
                                 start_date, 
                                 end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "Position"), parent = child1)
  c3 = newXMLNode("Id", position, parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)
  c6 = newXMLNode("Table", "BookedForecastSummary", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

pos_inventory <- function(credentials,
                          position, 
                          start_date, 
                          end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "Position"), parent = child1)
  c3 = newXMLNode("Keywords", attrs = c(type = "Position"), parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)
  c6 = newXMLNode("Table", "ListStatistics", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

site_geoinventory <- function(credentials,
                              location_type=c('State', 'City', 'Zip', 'DMA', 'MSA', 'Country'),
                              site_id,
                              positions,
                              start_date, 
                              end_date){
  
  if (location_type=='State'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T130.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T125.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='City'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T140.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T135.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='Zip'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T150.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T145.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='DMA'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T170.03'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T165.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='MSA'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T180.03'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T175.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='Country'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Site.Geotargeting.T120.09'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Site.Geotargeting.T115.02'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else {
    stop('wrong location type')
  }
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "GeoInventory"), parent = top)
  child2 = newXMLNode("GeoInventory", attrs = c(type = "Site", maxRow="100000"), parent = child1)
  c2 = newXMLNode("Id", site_id, parent = child2)
  for(i in 1:length(positions)){
    addChildren(child2, newXMLNode("Position", positions[i]))
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", table, parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-6)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}


section_at_position_searchable_list <- function(credentials,
                                                keyword=NULL,
                                                position=NULL, 
                                                start_date, 
                                                end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "SectionAtPosition"), parent = child1)
  if (!is.null(keyword)){
    c2 = newXMLNode("Keywords", keyword, attrs = c(type = "Section"), parent = child2)
  }
  if (!is.null(position)){
    c3 = newXMLNode("Position", position, parent = child2)
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", "ListStatistics", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- getNodeSet(ss, "//row")
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

section_geoinventory <- function(credentials,
                                 location_type=c('State', 'City', 'Zip', 'DMA', 'MSA', 'Country'),
                                 section,
                                 positions, 
                                 start_date, 
                                 end_date){
  
  if (location_type=='State'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T130.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T125.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='City'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T140.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T135.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='Zip'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T150.04'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T145.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='DMA'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T170.03'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T165.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='MSA'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T180.03'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T175.01'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else if (location_type=='Country'){
    if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
      table <- 'Inventory.Section.Geotargeting.T120.09'
    } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
      table <- 'Inventory.Section.Geotargeting.T115.02'
    } else {
      stop('mixing dates past and future. reset the dates')
    }
  } else {
    stop('wrong location type')
  }
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "GeoInventory"), parent = top)
  child2 = newXMLNode("GeoInventory", attrs = c(type = "Section", maxRow="100000"), parent = child1)
  c2 = newXMLNode("Id", section, parent = child2)
  for(i in 1:length(positions)){
    addChildren(child2, newXMLNode("Position", positions[i]))
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", table, parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-6)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

search_inventory <- function(credentials,
                             keywords,
                             positions, 
                             start_date, 
                             end_date){
  
  if (as.Date(start_date) > Sys.Date() & as.Date(end_date) > Sys.Date()){
    type <- 'KeywordForecast'
    table <- 'Inventory.Keyword.Search.T220.01'
  } else if (as.Date(start_date) < Sys.Date() & as.Date(end_date) < Sys.Date()) {
    type <- 'KeywordStatistics'
    table <- 'Inventory.Keyword.Search.T210.01'
  } else {
    stop('mixing dates past and future. reset the dates')
  }
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "SearchInventory"), parent = top)
  child2 = newXMLNode("SearchInventory", attrs = c(type = type, maxRow="100"), parent = child1)
  for(i in 1:length(keywords)){
    addChildren(child2, newXMLNode("Keywords", keywords[i]))
  }
  for(i in 1:length(positions)){
    addChildren(child2, newXMLNode("Position", positions[i]))
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", table, parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=my_credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-6)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

search_inventory_booked <- function(credentials,
                                    keywords,
                                    positions, 
                                    start_date, 
                                    end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "SearchInventory"), parent = top)
  child2 = newXMLNode("SearchInventory", attrs = c(type = "KeywordBooked", maxRow="100"), parent = child1)
  for(i in 1:length(keywords)){
    addChildren(child2, newXMLNode("Keywords", keywords[i]))
  }
  for(i in 1:length(positions)){
    addChildren(child2, newXMLNode("Position", positions[i]))
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", 'Inventory.Keyword.Search.T102.05', parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-6)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}


# page at position
# www.homes.com/content/listing.cfm@Middle
# Overview = campaigns
# very helpful to find out campaigns at that position
# BookedForecastSummary shows all campaigns, CPC, etc.
# DetailStatistics = Avg Per Day
# DetailForecast = For each day Total Impresssions, Booked, Available
page_at_position_searchable_list <- function(credentials,
                                             keyword,
                                             position=NULL, 
                                             start_date, 
                                             end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "PageAtPosition"), parent = child1)
  c2 = newXMLNode("Keywords", keyword, attrs = c(type = "Page"), parent = child2)
  if (!is.null(position)){
    c3 = newXMLNode("Position", position, parent = child2)
  }
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", "ListStatistics", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- getNodeSet(ss, "//row")
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}


page_pos_booked_forecast_summary <- function(credentials,
                                             page='www.homes.com/content/listing.cfm',
                                             position='Bottom1', 
                                             start_date, 
                                             end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "PageAtPosition"), parent = child1)
  c2 = newXMLNode("Id", paste0(page, '@', position), parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", "BookedForecastSummary", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-3)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}


page_pos_detailstatistics <- function(credentials,
                                      page='www.homes.com/content/listing.cfm',
                                      position='Bottom1', 
                                      start_date, 
                                      end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "PageAtPosition.Detail"), parent = child1)
  c2 = newXMLNode("Id", paste0(page, '@', position), parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", "DetailStatistics", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials,
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

page_pos_detailforecast <- function(credentials,
                                    page='www.homes.com/content/listing.cfm',
                                    position='Bottom1', 
                                    start_date, 
                                    end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Inventory"), parent = top)
  child2 = newXMLNode("BasicInventory", attrs = c(type = "PageAtPosition.Detail"), parent = child1)
  c2 = newXMLNode("Id", paste0(page, '@', position), parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", "DetailForecast", parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}

section_impr_by_keyword <- function(credentials,
                                    section,
                                    positions, 
                                    start_date, 
                                    end_date){
  
  top = newXMLNode("AdXML")
  child1 = newXMLNode("Request", attrs = c(type = "Report"), parent = top)
  child2 = newXMLNode("Report", attrs = c(type = "Section"), parent = child1)
  c3 = newXMLNode("Id", section, parent = child2)
  c4 = newXMLNode("StartDate", start_date, parent = child2)
  c5 = newXMLNode("EndDate", end_date, parent = child2)  
  c6 = newXMLNode("Table", 'Delivery.Section.Search.T810.01', parent = child2)
  r <- as(top, "character")
  
  xmlBody <- request_builder(credentials=credentials, 
                             adxml_request=r)
  
  h <- basicHeaderGatherer()
  t <- basicTextGatherer()
  httpHeader <- c('soapAction'='OasXmlRequest', "Accept-Type"="text/xml", 'Content-Type'="text/xml")
  curlPerform(url="https://openadstream18.247realmedia.com/oasapi/OaxApi", 
              httpheader=httpHeader, 
              headerfunction = h$update, 
              writefunction = t$update, 
              ssl.verifypeer=F, postfields=xmlBody)
  
  #pull out the results and format as XML
  doc <- xmlTreeParse(t$value(), asText=T)
  s <- xmlToList(doc)$Body.OasXmlRequestResponse.result
  ss <- xmlParse(s)
  ns <- tail(getNodeSet(ss, "//row"),-4)
  res <- xmlToDataFrame(ns, collectNames=F)
  return(res)
}
