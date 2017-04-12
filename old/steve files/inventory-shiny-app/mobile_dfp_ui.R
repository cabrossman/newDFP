mobile_dfp <- tabPanel("Mobile (DFP)", 
  fluidRow(
    column(12,
           tags$br(),
           div(id = "form",
               div(class='first-div',
                   h3("Ad Unit Setup"),
                   hr(),
                   radioButtons("size", "Ad Unit Size: ",
                                choices=c("300x250 (SRP-Bottom;DT-Bottom)"='300x250',  "320x150 (SRP-Liner1)"='320x150', 
                                          "320x50 (SRP-Liner2;DT-Middle)"='320x50', "145x15 (DT-LeftText;DT-RightText)"='145x15'), 
                                selected='300x250'),
                   radioButtons(inputId="adUnitId", 
                                label="Ad Unit: ", 
                                # this won't work on the server by pulling from global.R
                                choices=c('Homes_Mobile ROS'=34976467, 
                                          'Homes_Mobile SearchResults'=34976707, 
                                          'Homes_Mobile Details'=34976587), 
                                selected=NULL),
                   radioButtons("priority", "Priority: ", selected=8,
                                c('Sponsorship - 4'=4, 'Standard - 6 (High)'=6, 'Standard - 8 (Normal)'=8, 'Standard - 10 (Low)'=10)),
                   radioButtons("costType", "Cost Type: ", 
                                c('CPM', 'CPC'), inline=T)
               ),
               div(class='second-div',
                   h3("Geographical Targeting"),
                   hr(),
                   radioButtons(inputId='usgeoip', 'Only accept US Traffic (based on GeoIP)?', choices=c('Yes','No'), selected='Yes', inline=T),
                   selectizeInput(inputId="homesdma", label="Homes DMA Search Term Targeting: ", choices=NULL, selected=NULL, multiple=TRUE),
                   checkboxInput(inputId='homesdmaexclusionflag', label='EXCLUDE the DMAs selected above'),
                   selectizeInput(inputId="state", label="Homes State Search Term Targeting: ", choices=NULL, selected=NULL, multiple=TRUE),
                   checkboxInput(inputId='stateexclusionflag', label='EXCLUDE the states selected above'),
                   selectizeInput(inputId="county", label="Homes County Search Term Targeting: ", choices=NULL, selected=NULL, multiple=TRUE),
                   checkboxInput(inputId='countyexclusionflag', label='EXCLUDE the counties selected above'),
                   selectizeInput(inputId="city", label="Homes City Search Term Targeting: ", choices=NULL, selected=NULL, multiple=TRUE),
                   checkboxInput(inputId='cityexclusionflag', label='EXCLUDE the cities selected above'),
                   selectizeInput(inputId="status", label="Listing Type Search Term Targeting: ", choices=NULL, selected=NULL, multiple=TRUE),
                   selectizeInput(inputId="pos", label="Position Targeting: ", choices=NULL, selected=NULL, multiple=TRUE)
               ),
               div(class='third-div',
                   h3("Goal Details"),
                   hr(),
                   #radioButtons("goalType", "Goal Type: ", choices=c('Daily', 'Lifetime'), selected=NULL),
                   #radioButtons("unitType", "Unit Type: ", 
                   #              c("Impressions"='IMPRESSIONS', "Clicks"='CLICKS'), inline=T),
                   tags$div(
                     div(style="width:70%;display:inline-block;", numericInput("units", "Impression Goal Count", value=1000, 
                                                                               min=0, max=10000000000, 
                                                                               step=1000, width="100%")),
                     div(style="width:10%;display:inline-block;", div(id="pct_sign", style="display: inline-block;", "%"))
                   ),
                   radioButtons("deliveryRateType", "Delivery Rate Type: ", selected='FRONTLOADED',
                                choices=c("Evenly"='EVENLY', "Frontloaded"='FRONTLOADED', 
                                          "As Fast As Possible"='AS_FAST_AS_POSSIBLE'), inline=T),
                   tags$div(
                     tags$p(style="display:block;max-width:100%;margin-bottom:5px;font-weight:700;", "Frequency Capping: "),
                     div(style="width:20%;display:inline-block;", numericInput("frequencycapunits", "Limit", value=NULL, min=0, max=10000000000, step=1)),
                     div(style="width:25%;display:inline-block;", tags$p(style="text-align:center;", "impressions every")),
                     div(style="width:20%;display:inline-block;", numericInput("frequencycaptimeunits", "", value=NULL, min=0, max=10000000000, step=1)),
                     div(style="width:30%;display:inline-block;", selectInput("frequencycaptimeframe", "", selectize = FALSE,
                                                       selected='DAYS', choices=c('Minutes'='MINUTE', 'Hours'='HOUR',
                                                                              'Days'='DAY', 'Weeks'='WEEK',
                                                                              'Months'='MONTH', 'Lifetime'='LIFETIME')))
                   ),
                   tags$br(),
                   dateRangeInput("daterange", "Date range:",
                                  start  = format(ceiling_date(Sys.Date(),'month'),'%Y-%m-%d'),
                                  end    = format(ceiling_date(Sys.Date(),'month')+months(1),'%Y-%m-%d'),
                                  min    = format(Sys.Date(),'%Y-%m-%d'),
                                  max    = "2020-12-31",
                                  format = "M d yyyy",
                                  separator = " - "),
                   checkboxInput('monthly_breakdown', 'Breakdown for Each Month'),
                   checkboxInput('geo_breakdown', 'Breakdown for Each Geography'),
                   selectizeInput(inputId="email", label="Send Breakdown email to: ", choices=NULL, selected=NULL, multiple=TRUE),
                   actionButton("submit", "Submit Your Request", class = "btn-primary"),
                   br(), br(), br(), br(),
                   shinyjs::hidden(
                     div(id = "submit_msg", "Waiting for DFP Response..."),
                     div(id = "error",
                         div(br(), tags$b("Warning: "), span(id = "error_msg")))
                   ),
                   shinyjs::hidden(
                     div(id = "breakdown",
                         div(br(), tags$b("Note: "), span(id = "breakdown_msg", "Emailing Breakdown Data")))
                   ),
                   shinyjs::hidden(
                     div(id = "clearform_msg",
                         actionLink("submit_another", "Click here to Clear form"))
                   )
               )
           )
    )
  ),
  hr(),
  fluidRow(
    column(12, 
           shinyjs::hidden(
             div(id = "availability_result")
           )
    )
  ),
  hr(),
  fluidRow(
    column(12, 
           shinyjs::hidden(
             div(id = "contending_result")
           )
    )
  )
)