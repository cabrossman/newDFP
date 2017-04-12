
shinyUI(

  fluidPage(
    #head
    #connects to CSS, and images. Below is javascript used in the app as well
    tags$head(
      #google tag manager
      HTML('

           <!-- Google Tag Manager -->'),
      tags$script("(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
                  new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                  j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
                  'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
                  })(window,document,'script','dataLayer','GTM-MJ9CBBQ');"),
            HTML('<!-- End Google Tag Manager -->
                 
                 '),
      
      #CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css" ),     
      tags$link(rel="shortcut icon", type="image/x-icon", href="boatsicon.ico"),
      tags$link(rel="icon", type="image/x-icon", href="boatsicon.ico"),
      
      #bunch of custom JS
      tags$script('
                  Shiny.addCustomMessageHandler("myCallbackHandlerAddBusy",
                  function(reset) {
                  $("html").addClass("shiny-busy");
                  });'),
            tags$script('
                        Shiny.addCustomMessageHandler("myCallbackHandlerRemoveBusy",
                        function(reset) {
                        $("html").removeClass("shiny-busy");
                        });'),
            tags$script('
                        Shiny.addCustomMessageHandler("myCallbackHandlerHideButton",
                        function(reset) {
                        $("#downloadContending").addClass("my-hidden");
                        $("#beakdownMonthly").addClass("my-hidden");
                        $("#beakdownGeo").addClass("my-hidden");
                        $("#breakdownCustom").addClass("my-hidden");
                        $("#posBreakdown").addClass("my-hidden");
                        });'),
            tags$script('
                        Shiny.addCustomMessageHandler("myCallbackHandlerShowButton",
                        function(reset) {
                        $("#downloadContending").removeClass("my-hidden");
                        $("#beakdownMonthly").removeClass("my-hidden");
                        $("#beakdownGeo").removeClass("my-hidden");
                        $("#breakdownCustom").removeClass("my-hidden");
                        $("#posBreakdown").removeClass("my-hidden");
                        });'),
            tags$script('
                        Shiny.addCustomMessageHandler("myCallbackHandlerJumpToBottom",
                        function(reset) {
                        $("html, body").scrollTop( $(document).height() );
                        });')
            ),
    
    ## google tag manager at start of <body>
    # HTML('<!-- Google Tag Manager (noscript) -->
    #        <noscript><iframe src="https://www.googletagmanager.com/ns.html?id=GTM-MJ9CBBQ"
    #      height="0" width="0" style="display:none;visibility:hidden"></iframe></noscript>
    #      <!-- End Google Tag Manager (noscript) -->'),
    HTML('
         <!-- Google Tag Manager (noscript) -->'),
    tags$noscript(
      tags$iframe(src="https://www.googletagmanager.com/ns.html?id=GTM-MJ9CBBQ",
                  height="0", width="0", style="display:none;visibility:hidden")
    ),
    HTML('<!-- End Google Tag Manager (noscript) -->
         '),

  
  #busy indicator used throughout most of the app
  shinysky::busyIndicator(text = "Calculation in progress ... ", wait = 0),

  
  #title
  titlePanel(title=div(img(src="boatslogo.png"), "Inventory Self Service"), windowTitle = 'Inventory Self Service'),
  
  #start of HTML Body (i think)
  fluidRow(

    column(12,
           tags$br(),
           div(id = "form",
               div(class='ad-unit-setup-div',
                   h3("Ad Unit Setup"),
                   hr(),
                   
                   #ad unit: pulled in from global file
                   radioButtons("adUnitId", "Ad Unit(s): ",choices=structure(as.character(keyList$ad_unit_ids$id), names = as.character(keyList$ad_unit_ids$name)), selected='254335239'),
                   
                   #pos
                   selectizeInput(inputId="pos", label="Position(s): ", choices=NULL, selected=NULL, multiple=TRUE),
                   #page
                   selectizeInput(inputId="Page", label="Page(s): ", choices=NULL, selected=NULL, multiple=TRUE),
                   
                   #site version -only show this panel if the result is not boattrader (id = 254335239)
                   conditionalPanel(
                     condition = "input.adUnitId != '254335239'",
                     selectizeInput(inputId="site_version", label="Site Version(s): ", choices=NULL, selected=NULL, multiple=TRUE)
                   ),
                   #priority
                   radioButtons("priority", "Priority: ", selected=8,
                                c('Sponsorship - 4'=4, 'Standard - 6 (High)'=6, 'Standard - 8 (Normal)'=8, 'Standard - 10 (Low)'=10)),
                   #cost type
                   radioButtons("costType", "Cost Type: ", 
                                c('CPM', 'CPC'), inline=T)
               ),
               div(class='geo-targeting-div',
                   #Geographical Targeting - Between Selections Are Considered 'OR' Conditions
                   h3("Geographical Targeting"),
                   hr(),
                   #continent
                   selectizeInput(inputId='CONTINENT', label="Continent selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #country
                   selectizeInput(inputId='COUNTRY', label="Country selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #boats custom regions
                   selectizeInput(inputId='BOATS_REGION', label="Boats Region selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #states in the US or provices in CA
                   selectizeInput(inputId='STATE-PROVINCE', label="State-Province selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #US DMA's
                   selectizeInput(inputId='DMA_REGION', label="DMA selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #counties in FL.
                   selectizeInput(inputId='COUNTY', label="Counties in FL selections: ", choices=NULL, selected=NULL, multiple=TRUE)
               ),
               div(class='custom-targeting-div',
                   h3("Custom Targeting"),
                   hr(),
                   #custom brand classes
                   selectizeInput(inputId='brand_class', label="Brand/Class Bucket(s): ", choices=NULL, selected=NULL, multiple=TRUE),
                   #makes
                   selectizeInput(inputId="make", label="Make selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #class
                   selectizeInput(inputId="class", label="Class selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #type
                   selectizeInput(inputId="type", label="Type selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #length
                   selectizeInput(inputId="length", label="Length selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #year
                   selectizeInput(inputId="year", label="Year selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #condition
                   selectizeInput(inputId="condition", label="Condition selections: ", choices=NULL, selected=NULL, multiple=TRUE),
                   #state if on boat trader. Only show if the ad unit is boattrader
                   conditionalPanel(
                     condition = "input.adUnitId =='254335239'",
                     selectizeInput(inputId='stateBTsearch', label="State on BT search selections: ", choices=NULL, selected=NULL, multiple=TRUE)
                   ),
                   #boolean logic to understand how to interpret custom targeting API call
                   radioButtons("booleanLogic", "Select Boolean Logic", 
                                c('all AND','make-OR-class','make-OR-type','class-OR-type','make-OR-class-OR-type'),
                                selected = 'make-OR-class',inline=T)

               ),
               div(class='goal-details-div',
                   h3("Goal Details"),
                   hr(),
                   #impression goal count
                   numericInput("units", "Impression Goal Count", value=10000,min=0, max=10000000000,step=1000),
                   # tags$div(
                   # 
                   #   
                   # div(style="width:70%;display:inline-block;", numericInput("units", "Impression Goal Count", value=1000,
                   #                                                           min=0, max=10000000000,
                   #                                                           step=1000, width="100%")),
                   # div(style="width:10%;display:inline-block;", div(id="pct_sign", style="display: inline-block;", ""))
                   # ),
                   
                   #Delivery Rate Type
                   radioButtons("deliveryRateType", "Delivery Rate Type: ", selected='EVENLY',
                                choices=c("Evenly"='EVENLY', "Frontloaded"='FRONTLOADED', 
                                          "As Fast As Possible"='AS_FAST_AS_POSSIBLE'), inline=T),
                   #Frequency Capping
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
                   #date range: will go out ~5 years in future
                   dateRangeInput("daterange", "Date range:",
                                  start  = format(ceiling_date(Sys.Date(),'month'),'%Y-%m-%d'),
                                  end    = format(ceiling_date(Sys.Date(),'month')+months(1),'%Y-%m-%d'),
                                  min    = format(Sys.Date(),'%Y-%m-%d'),
                                  max    = paste0(format(Sys.Date() + 5*365,'%Y'),'-12-31'),
                                  format = "M d yyyy",
                                  separator = " - "),
                   
                   #rate used in calculation of rate card
                   numericInput("CPM", "CPM/CPC:", 10, min = 1, max = 50),
                   
                   #dynamic output of rate card rate from server function
                   htmlOutput("rateCard"),
                   #submit button
                   actionButton("submit", "Submit Your Request", class = "btn-primary"),
                   br(), br(), br(), br()
                   ),
               div(class='availability-output-div',
                   hr(),
                   #output of availability HTML
                   htmlOutput('availability_txt'),
                   #output of definitions HTML
                   htmlOutput('definitions_txt')

               ),
               div(class='AvailabilityButtons',
                  #download buttons
                   downloadButton('beakdownMonthly', 'Download Breakdown by Month', class='my-hidden'),
                   downloadButton('posBreakdown', 'Download Breakdown by Position', class='my-hidden'),
                   downloadButton('beakdownGeo', 'Download Breakdown by Geography', class='my-hidden'),
                   downloadButton('breakdownCustom', 'Download Breakdown by Custom Criteria', class='my-hidden')
               
               ),
               
               div(class='contending',
                   hr(),
                   br(),
                   #download button of contending line items
                   downloadButton('downloadContending', 'Download All Contending Line Items & Line Details', class='my-hidden'),
                   #html output of contending line items
                   htmlOutput('contending_txt')

               )
               
           )
    )
  )
  
))