#steve's stolen function
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
                  'For more info click <a href="https://www.google.com/dfp/252108799#delivery/LineItemDetail/', 
                  'orderId=', x$orderId, '&lineItemId=', x$id, '" target="_blank">HERE</a> to view this line item in DFP<br>')
    return(data.frame(txt=txt, contending=as.integer(contending[contending$lineItemId==x$id,2]), stringsAsFactors = F))
  }, .id=NULL, contending=contending_stats)
  res <- res[order(res$contending, decreasing=T),]
  return(res)
}