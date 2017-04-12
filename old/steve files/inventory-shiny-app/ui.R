# app-inventory-self-service/ui.R
# Copyright 2016
# Dominion Enterprises, All rights reserved.

shinyUI(
  fluidPage(
    tags$title("Inventory Self Service"),
    tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css'),
              tags$link(rel="shortcut icon", type="image/x-icon", href="homes-logo.ico"),
              tags$link(rel="icon", type="image/x-icon", href="homes-logo.ico"),
              tags$script('
      			              Shiny.addCustomMessageHandler("myCallbackHandlerPushResultTxt1",
                          function(text) {
                          document.getElementById("availability_result").innerHTML = text;
                          });'), 
              tags$script('
      			              Shiny.addCustomMessageHandler("myCallbackHandlerSendError",
                          function(text) {
                          document.getElementById("error_msg").innerHTML = text;
                          });'), 
              tags$script('
      			              Shiny.addCustomMessageHandler("myCallbackHandlerPushResultTxt2",
                          function(text) {
                          document.getElementById("contending_result").innerHTML = text;
                          });'), 
              tags$script('
      			              Shiny.addCustomMessageHandler("myCallbackHandlerClearResultTxt",
                          function(reset) {
                          document.getElementById("availability_result").innerHTML = "";
                          document.getElementById("contending_result").innerHTML = "";
                          });'),
              tags$script('
      			            Shiny.addCustomMessageHandler("myCallbackHandlerAddBusy",
                        function(reset) {
                        $("html").addClass("shiny-busy");
                        });'),
              tags$script('
    			              Shiny.addCustomMessageHandler("myCallbackHandlerRemoveBusy",
    			              function(reset) {
    			              $("html").removeClass("shiny-busy");
    			              });')),
    shinyjs::useShinyjs(),
    br(),
    inputIdInfo("idinfo"),
    busyIndicator(text="Pulling data from DFP", wait=0),
    mainPanel(
      tabsetPanel(id = "tabs1",
                  mobile_dfp,
                  desktop_oas
      )
    )
    )
  )