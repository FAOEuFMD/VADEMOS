#####################################################################################################
## UI and server for the app.R
## pilar.riusmunoz@fao.org May 2022~
#####################################################################################################

###################################################
# 0 - Load libraries
###################################################
library(shiny)
library(shinythemes)
library(knitr)
library(shinyWidgets)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(tibble)
library(rhandsontable)
library(ggplot2)
#library(googlesheets4)
library(sendmailR)
options(shiny.reactlog=TRUE)
library(highcharter)
# library(sp)
# library(leaflet)
# library(leaflet.extras)
# library(raster)
# library(sf)
# library(rworldxtra)
library(mapboxapi)
library(remotes)
library(Rcpp)
library(mapboxer)
library(shinyscreenshot)
library(janitor)
library(glue)

###################################################
# 1 - Source files
###################################################
source("vaccinationmodelfixed.R") # modeling script
source("plotOutputs.R") # functions to get plotOutputs
source("UIParts.R") # UI for 'tool' tab
source("UIForms.R") # UI for 'Survey' and 'Feedback' tab

###################################################
# 2 - Define UI for the app
###################################################
# Define login credentials
credentials = data.frame(
  username_id = c("eufmd", "eufmd1", "turkey", "iran", "georgia", "guest"),
  passod   = sapply(c("mypass", "mypass1", "turfmdvc^2021!", "irnfmdvc!2021#", "geofmdvc_2021%", "eufmdguest"), password_store),
  permission  = c("basic", "advanced", "basic", "basic", "basic", "basic"),
  stringsAsFactors = F
)



ui <- fluidPage(
  # Use CSS file to customize the interface
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "model.css"), # color themes
    tags$link(rel="shortcut icon", href="FAVICON.png") # eufmd favicon
  ),
  theme = shinytheme("united"), # basic shiny theme
  chooseSliderSkin(skin="Flat", color="#073f23"), # to avoid sliderInput's blue color
  
  # Interface
  navbarPage(title = div(img(src='EuFMD.png', # Add EuFMD logo ver. 1/Sep/2021 at the left-top, height should be 50px
                             style="margin-top: -10px; padding-left:10px;padding-right:10px;padding-bottom:10px",
                             height = 50)),
             windowTitle="FMD Vaccine Demand Estimation Tool",
             id="tabs",
             
             
             
             # ----------------------------------
             # NAVBAR TAB PANEL 1 - LOGIN PAGE
             tabPanel("Login", fluidPage(),
                      # Main login screen
                      loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                                       wellPanel(
                                         tags$h2("VADEMOS-FMD: Test version 1.07", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                                         br(),
                                         textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                                         passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                                         br(),
                                         div(
                                           style = "text-align: center;",
                                           actionButton("login", "SIGN IN", style = "color: white; background-color:#16834c;
                                                        padding: 10px 15px; width: 150px; cursor: pointer;
                                                        font-size: 18px; font-weight: 600;"),
                                           shinyjs::hidden(
                                             div(id = "nomatch",
                                                 tags$p("Oops! Incorrect username or password!",
                                                        style = "color: red; font-weight: 600; 
                                                        padding-top: 5px;font-size:16px;", 
                                                        class = "text-center"))),
                                           br(),
                                           br()
                                           ) #div
                                           ) # wellPanel
                                         ) #div
                ), #tabPanel
             
             # ----------------------------------
             # NAVBAR TAB PANEL 2 - ABOUT PAGE
             # description/help guide for the tool ?
             tabPanel("About",
                      style="text-align: justify;",
                       includeHTML("about.Rhtml")
             ), # tabpanel 2
             
             # ----------------------------------
             # NAVBAR TAB PANEL 3 - SURVEY PAGE (default: hidden)
             tabPanel("Survey",
                      # Form appears only when a user logs in 
                      form <- div(class="collect_info",
                                  style = "width: 900px; max-width: 100%; margin: 0 auto; padding: 20px;",
                                  userdata()
                                 ) #div
                       ),
             
             # ----------------------------------
             # NAVBAR TAB PANEL 4 - TOOL PAGE (default: hidden)
             tabPanel("Tool",
                      #h5("You must login to access the tool"),
                      # Form appears only when a user logs in
                      tool <- div(class = "vademos", tool())
                      ),
              
             tabPanel("Contact",
                      contactpage <- div(id = "contactpage", feedback(),
                                         style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;"
                      ) #div
                      ) #tabPanel

            ), # navbar
        
        br(),
        br(),
        # FOOTER
        div(id='footer',
            class = "footer",
            style = "background-color: #16834c; color: white;",
            includeHTML("footer.html")
        ) # div=footer
        
) # UI end


###################################################
# 3 Server; R functions that run/respond to UI
###################################################
server <- function(input, output, session) {
  
  hideTab(inputId = "tabs", target = "Survey")
  hideTab(inputId = "tabs", target = "Tool")
  
  ########################
  # Login outputs
  ########################
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(1000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }
  })

  
  observe({
    req(USER$login == TRUE)
    hideTab(inputId = "tabs", target = "Login") # hide main login page
    showTab(inputId = "tabs", target = "Survey")
    showTab(inputId = "tabs", target = "Tool")
    appendTab(inputId = "tabs", tabPanel(uiOutput("logoutbtn")))
    updateTabsetPanel(session, "tabs", selected="About") # select data collecting tab as active tab
  })

  observeEvent(input$submitbutton, {
    req(USER$login == TRUE)
    updateTabsetPanel(session, "tabs", selected="Tool") # select tool tab as active tab
  })
  
  observeEvent(input$feedbackbutton, {
    updateTabsetPanel(session, "tabs", selected="Contact") # select tool tab as active tab
  })
  
  ########################
  ## logout button
  output$logoutbtn <- renderUI({
    req(USER$login)
    actionButton("logoutbtn", "Log out",
                 icon = icon("sign-out"),
                 style = "color:white; border-color:transparent; background-color: #262625;
                 margin-top: -10px; margin-bottom: -10px; padding: 2; float:right")
  })
  
  observeEvent(input$logoutbtn, {
    req(USER$login)
    session$reload()
  })
  
  

  
  ########################
  # Reactive elements
  # Update input params
  ########################
  #adding region and subregion Pili August 2022
  
  
  # 
  # 
  # output$Subregion <- renderUI({
  #   # if ('All Regions' %in% input$Region) {
  #   #   reg <- data
  #   # } else {
  #     reg <- data %>%
  #       filter(
  #         Region %in% input$Region)
  #   
  #   subreg <- sort(unique(reg$Subregion))
  #   
  #   # render selectizeInput
  #   selectizeInput(
  #     inputId = 'Subregion',
  #     label = NULL,
  #     choices = c(subreg),
  #     multiple = FALSE,
  #     selected = NULL)
  # })
  
  observe({
    
    input$Region
    filtereddata<-data%>%
      filter(data$Region %in% input$Region)
    
    
    updateSelectInput(session, "Subregion",
                      choices = sort(filtereddata$Subregion),
                      selected = NULL
                      
    )
    
  })
  
  observe({
    
    #region <- input$Region
    input$Subregion
    filtereddata2<-data%>%
      filter(data$Subregion %in% input$Subregion)
    
    
    updateSelectInput(session, "country_selected",
                      choices = sort(filtereddata2$Country_name),
                      selected = NULL
                      
    )
    
  })

  
  dat <- reactive({
    
    if (input$country_selected != "Turkey"){
      data[(data$Country_name==input$country_selected) &
           (data$Year==input$year_selected), ]
      
    } else if (input$country_selected == "Turkey"){
      req(input$subnational_selected)
      data[(data$Country_name==input$country_selected) &
             (data$SubnationalRegion==input$subnational_selected) &
             (data$Year==input$year_selected), ]
    }
  })
  
  prevdat <- reactive({
    if (input$country_selected != "Turkey"){
      data[(data$Country_name==input$country_selected), ]
    } else if (input$country_selected == "Turkey"){
      req(input$subnational_selected)
      data[(data$Country_name==input$country_selected) & (data$SubnationalRegion==input$subnational_selected), ]
    }
  })
  
  
  # Sidebar tab 1 - vaccine schedules
  output$vschedule_lr_as <- renderUI({
      textInput("vschedule_lr_as","Large ruminant (> 1 year)",
                value=unique(dat()[dat()$Species=="LR", "VaccineSchedule_AS"])) # VS_LR for selected country & year
  }) #renderUI
  
  output$vschedule_sr_as <- renderUI({
    textInput("vschedule_sr_as","Small ruminant (> 1 year)",
              value=unique(dat()[dat()$Species=="SR", "VaccineSchedule_AS"]))
  }) #renderUI
  
  output$vschedule_p_as <- renderUI({
    textInput("vschedule_p_as","Pig (> 1 year)",
              value=unique(dat()[dat()$Species=="P", "VaccineSchedule_AS"]))
  }) #renderUI
  
  output$vschedule_lr_ys <- renderUI({
    textInput("vschedule_lr_ys","Large ruminant (< 12 months)",
              value=unique(dat()[dat()$Species=="LR", "VaccineSchedule_YS"])) # VS_LR for selected country & year
  }) #renderUI
  
  output$vschedule_sr_ys <- renderUI({
    textInput("vschedule_sr_ys","Small ruminant (< 12 months)",
              value=unique(dat()[dat()$Species=="SR", "VaccineSchedule_YS"]))
  }) #renderUI
  
  output$vschedule_p_ys <- renderUI({
    textInput("vschedule_p_ys","Pig (< 12 months)",
              value=unique(dat()[dat()$Species=="P", "VaccineSchedule_YS"]))
  }) #renderUI
  
  # sector proportions
  output$sectorpropdairy <- renderUI({
      sliderInput("sectorpropdairy","Dairy",
                  min=0, max=100,
                  value=round(mean(prevdat()$PropDairy, na.rm=TRUE)*100),
                  step=1,
                  post="%")
  }) #renderUI
  
  output$sectorpropbeef <- renderUI({
      sliderInput("sectorpropbeef","Beef",
                  min=0, max=100,
                  # value=round(mean(prevdat()$PropBeef, na.rm=TRUE)*100),
                  value=100-input$sectorpropdairy,
                  #to not go over 100%
                  step=1,
                  post="%")
  }) #renderUI
  
  output$sectorpropsmall <- renderUI({
    req(prevdat())
    sliderInput("sectorpropsmall","Smallholders",
                min=0, max=100,
                # value=round(0.1, 0),
                value=100-input$sectorpropbeef-input$sectorpropdairy,
                # to adjust to 100%
                step=1,
                post="%")
  }) #renderUI
  
  # youngstock proportion
  output$ysproplr <- renderUI({
      sliderInput("ysproplr","Large Ruminants (< 12 months)",
                  min=0, max=100,
                  value=round(mean(prevdat()[prevdat()$Species=="LR", "PropYS"], na.rm=TRUE)*100),
                  step=1,
                  post="%")
  }) #renderUI
  
  output$yspropsr <- renderUI({
      sliderInput("yspropsr","Small Ruminants (< 12 months)",
                  min=0, max=100,
                  value=round(mean(prevdat()[prevdat()$Species=="SR", "PropYS"], na.rm=TRUE)*100),
                  step=1,
                  post="%")
  }) #renderUI
  
  output$yspropp <- renderUI({
      sliderInput("yspropp","Pigs (< 12 months)",
                  min=0, max=100,
                  value=round(mean(prevdat()[prevdat()$Species=="P", "PropYS"], na.rm=TRUE)*100),
                  step=1,
                  post="%")
  }) #renderUI
  
  
  ########################
  # Reactive elements
  # to update inputs
  ########################
  reactiveinputtable <- reactive({
    countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
    year_to_predict = as.integer(input$year_selected)
    
    if (countrycode != "TUR"){
      get_inputtable(data, countrycode, year_to_predict)
    } else if (countrycode == "TUR"){
      req(input$subnational_selected)
      get_inputtable(data, countrycode, year_to_predict, input$subnational_selected)
    }
  })

  reactiveinputtable_reduced <- reactive({
    get_reducedtable(reactiveinputtable())
    })
  
  # PCP, Livestock Pops, Total outbreaks in editable datatable
  output$inputtable <- renderDT({
    DT::datatable(reactiveinputtable_reduced(),
                  rownames = FALSE, editable = list(target = "row", disable = list(columns = c(0))),
              options=list(dom = 't', pageLength = 20), selection="none")
  })
  
  # ----------------------------------
  # Initial pcp selection from the selected country & year
  # pcp selection
  output$pcp_selected <- renderUI({
    radioButtons("pcp_selected","Expected PCP-FMD stage",
                 choices=list('1'=1, '2'=2, '3'=3, '4'=4, 'Above'=5),
                 selected=as.integer(unique(tail(v$data$PCP, 1))),
                 inline=TRUE # horizontal buttons
    )
  }) # renderUI
  
  # ----------------------------------
  # Whenever a country or year selected -> save the inputtable as csv
  observeEvent({input$country_selected
                input$year_selected
                input$subnational_selected}, {
    
    # ----------------------------------
    # Interactive plot 1: PCP projection
    output$pcps <- renderPlot({
        get_plot_pcp(prevdat(),
                     reactiveinputtable_reduced(),
                     input$country_selected, input$year_selected)
    }, bg="transparent") # pcp plot
    
    # ----------------------------------
    # Interactive plot 2: Livestock population
    output$pops <- renderPlot({
      get_plot_pop(prevdat(),
                   reactiveinputtable(),
                   input$country_selected, input$year_selected)
    }, bg="transparent") # pop plot
    
    # ----------------------------------
    # Interactive plot 3: FMD Total Outbreaks
    output$outbreaks <- renderPlot({
      get_plot_on(prevdat(),
                  reactiveinputtable_reduced(),
                  input$country_selected, input$year_selected)
    }, bg="transparent") # outbreak plot
    
    })
  
  
  # ----------------------------------
  # When any cells are edited:
  
  v <- reactiveValues(data=NULL)
  observe({
    req(reactiveinputtable_reduced())
    v$data <- reactiveinputtable_reduced()
  })
  
  observeEvent(input$inputtable_cell_edit, {

    #get values
    info = input$inputtable_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    
    if(k < 0){ #pass if negative
      print("Please select positive values")
    } else {
      v$data[i, j+1] <- k # adjust values
    }
    
    # Update selected PCP
    updateRadioButtons(session = session, inputId = "pcp_selected",
                       selected = as.integer(unique(tail(v$data$PCP, 1))))
    
    # Update PCP plot
    output$pcps <- renderPlot({
      get_plot_pcp(prevdat(),
                   v$data,
                   input$country_selected, input$year_selected)
    }, bg="transparent")
    
    # Update Pop plot
    output$pops <- renderPlot({
      get_plot_pop(prevdat(),
                   get_mutated_inputtable(data.frame(v$data)),
                   input$country_selected, input$year_selected)
    }, bg="transparent")
    
    # Update ON plot
    output$outbreaks <- renderPlot({
      get_plot_on(prevdat(),
                  v$data,
                  input$country_selected, input$year_selected)
    }, bg="transparent")
  })
  
  
  # ----------------------------------
  # PCP stage related sliderinputs
  # delphi input - q2
  pcp_reactive <- eventReactive(input$pcp_selected, {
    pcp <- as.numeric(input$pcp_selected)
    pcpstr <- sprintf('_%s_m', pcp)
    delphi[grepl(pcpstr, names(delphi))]
  })
  
  output$prophylactic_vc_lr <- renderUI({ 
    req(prevdat())
    q2_LR <- unique(pcp_reactive()[grepl('Q2_LR', names(pcp_reactive()))])
    q2_LR_mode <- unique(q2_LR[grepl('most_likely', names(q2_LR))])
    sliderInput("prophylactic_vc_lr","Large ruminants",
                min=0, max=100,
                value=as.numeric(q2_LR_mode)*100,
                step=1,
                post="%")
  })
  
  output$prophylactic_vc_sr <- renderUI({
    req(prevdat())
    q2_SR <- unique(pcp_reactive()[grepl('Q2_SR', names(pcp_reactive()))])
    q2_SR_mode <- unique(q2_SR[grepl('most_likely', names(q2_SR))])
    sliderInput("prophylactic_vc_sr","Small ruminants",
                min=0, max=100,
                value=as.numeric(q2_SR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$prophylactic_vc_p <- renderUI({
    req(prevdat())
    q2_P <- unique(pcp_reactive()[grepl('Q2_P', names(pcp_reactive()))])
    q2_P_mode <- unique(q2_P[grepl('most_likely', names(q2_P))])
    sliderInput("prophylactic_vc_p","Pigs",
                min=0, max=100,
                value=as.numeric(q2_P_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  # delphi input - q3
  output$outbreak_vc_lr <- renderUI({
    req(prevdat())
    q3_LR <- unique(pcp_reactive()[grepl('Q3_LR', names(pcp_reactive()))])
    q3_LR_mode <- unique(q3_LR[grepl('most_likely', names(q3_LR))])
    sliderInput("outbreak_vc_lr","Large ruminants",
                min=0, max=100,
                value=as.numeric(q3_LR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$outbreak_vc_sr <- renderUI({
    req(prevdat())
    q3_SR <- unique(pcp_reactive()[grepl('Q3_SR', names(pcp_reactive()))])
    q3_SR_mode <- unique(q3_SR[grepl('most_likely', names(q3_SR))])
    sliderInput("outbreak_vc_sr","Small ruminants",
                min=0,  max=100,
                value=as.numeric(q3_SR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$outbreak_vc_p <- renderUI({
    req(prevdat())
    q3_P <- unique(pcp_reactive()[grepl('Q3_P', names(pcp_reactive()))])
    q3_P_mode <- unique(q3_P[grepl('most_likely', names(q3_P))])
    sliderInput("outbreak_vc_p","Pigs",
                min=0, max=100,
                value=as.numeric(q3_P_mode)*100,
                step=1,
                post="%")
  }) #renderUI

  
  ########################
  # Reactive elements
  # to update outputs
  ########################

  # results <- eventReactive(input$updatebutton, {
  #   countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
  #   year_to_predict = as.integer(input$year_selected)
  # 
  #     get_results(data, countrycode, year_to_predict,
  #                 input_vs_lr_as=as.numeric(input$vschedule_lr_as),
  #                 input_vs_sr_as=as.numeric(input$vschedule_sr_as),
  #                 input_vs_p_as=as.numeric(input$vschedule_p_as),
  #                 input_vs_lr_ys=as.numeric(input$vschedule_lr_ys),
  #                 input_vs_sr_ys=as.numeric(input$vschedule_sr_ys),
  #                 input_vs_p_ys=as.numeric(input$vschedule_p_ys),
  #                 input_prop_lr_ys=as.numeric(input$ysproplr)/100,
  #                 input_prop_sr_ys=as.numeric(input$yspropsr)/100,
  #                 input_prop_p_ys=as.numeric(input$yspropp)/100,
  #                 input_propdairy=as.numeric(input$sectorpropdairy)/100,
  #                 input_propbeef=as.numeric(input$sectorpropbeef)/100,
  #                 input_propsmall=as.numeric(input$sectorpropsmall)/100,
  #                 #input_pcp=as.integer(input$pcp_selected),
  #                 subnational=as.character(input$subnational_selected),
  #                 input_vc_lr=as.numeric(input$prophylactic_vc_lr)/100,
  #                 input_vc_sr=as.numeric(input$prophylactic_vc_sr)/100,
  #                 input_vc_p=as.numeric(input$prophylactic_vc_p)/100,
  #                 input_oc_lr=as.numeric(input$outbreak_vc_lr)/100,
  #                 input_oc_sr=as.numeric(input$outbreak_vc_sr)/100,
  #                 input_oc_p=as.numeric(input$outbreak_vc_p)/100,
  #                 edited=FALSE, df_edited=NA
  #     )
  #   }
  # )
  
  # # Listen when input table is modified and run button was clicked
  results <- eventReactive(input$updatebutton, {
    countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
    year_to_predict = as.integer(input$year_selected)
    
    #if (input$input_cell_edit){

    get_results(data, countrycode, year_to_predict,
                input_vs_lr_as=as.numeric(input$vschedule_lr_as),
                input_vs_sr_as=as.numeric(input$vschedule_sr_as),
                input_vs_p_as=as.numeric(input$vschedule_p_as),
                input_vs_lr_ys=as.numeric(input$vschedule_lr_ys),
                input_vs_sr_ys=as.numeric(input$vschedule_sr_ys),
                input_vs_p_ys=as.numeric(input$vschedule_p_ys),
                input_prop_lr_ys=as.numeric(input$ysproplr)/100,
                input_prop_sr_ys=as.numeric(input$yspropsr)/100,
                input_prop_p_ys=as.numeric(input$yspropp)/100,
                input_propdairy=as.numeric(input$sectorpropdairy)/100,
                input_propbeef=as.numeric(input$sectorpropbeef)/100,
                input_propsmall=as.numeric(input$sectorpropsmall)/100,
                #input_pcp=as.integer(input$pcp_selected),
                subnational=as.character(input$subnational_selected),
                input_vc_lr=as.numeric(input$prophylactic_vc_lr)/100,
                input_vc_sr=as.numeric(input$prophylactic_vc_sr)/100,
                input_vc_p=as.numeric(input$prophylactic_vc_p)/100,
                input_oc_lr=as.numeric(input$outbreak_vc_lr)/100,
                input_oc_sr=as.numeric(input$outbreak_vc_sr)/100,
                input_oc_p=as.numeric(input$outbreak_vc_p)/100,
                edited=TRUE,
                df_edited=data.frame(v$data)
    )
      
    
  }
  )
  
  # df_paramtable <- eventReactive(input$updatebutton, {
  #   results()[[1]] # dataframe of all previous + future years up to the 'year_to_predict'
  # })
  # 
  # df_year_predicted <- eventReactive(input$updatebutton, {
  #   results()[[2]]
  # })
  
  
  
  vaccine_estimates <- eventReactive(input$updatebutton, {
    vaccine_estimates <- results()[[3]] # result dataframe of mc output
  })
  
  
  
  
  mcresult <- eventReactive(input$updatebutton, {
    mcresult <- results()[[4]] # mc output - distribution of parameters
  })
  
 
########################################
##########  Rob - Addition of a table that resume the vaccine demand by species (16/02/22)
  vaccine_estimatesS <- eventReactive(input$updatebutton, {
    vaccine_estimatesS <- results()[[5]]
  })
  
  
########################################
####Directing buttons
  
  # Direct to the result tab when submit button is activated
  observeEvent(input$updatebutton, {
    updateTabsetPanel(session = session, inputId = "maintabset", selected = "Result")
    shinyjs::runjs("window.scrollTo(0, 0)")
  })
  
  # Direct to the result tab when aggregate button is activated
  observeEvent(input$aggregatebutton, {
    updateTabsetPanel(session = session, inputId = "maintabset", selected = "Map")
    shinyjs::runjs("window.scrollTo(0, 300)")
  })
  
  # Direct to the input tab when back button is activated
  observeEvent(input$backbutton, {
    updateTabsetPanel(session = session, inputId = "maintabset", selected = "Parameters")
    # v$data <- input$inputtable
  })
 
  
#########################################
##### Reseting  
  
  # Reset the parameters when reset button is pressed
  observeEvent(input$resetbutton, {
    # Reset input widgets
    shinyjs::reset("inputs1")
    
    # Reset datatable
    output$inputtable <- renderDT({
      DT::datatable(reactiveinputtable_reduced(), 
                    rownames = FALSE, editable = TRUE,
                    options=list(dom = 't', pageLength = 20), selection="none")
    })
    

    v$data <- reactiveinputtable_reduced()

    
    # Reset plots
    output$pcps <- renderPlot({
      get_plot_pcp(prevdat(),
                   reactiveinputtable_reduced(),
                   input$country_selected, input$year_selected)
    }, bg="transparent") # pcp plot
    
    # ----------------------------------
    # Interactive plot 2: Livestock population
    output$pops <- renderPlot({
      get_plot_pop(prevdat(),
                   reactiveinputtable(),
                   input$country_selected, input$year_selected)
    }, bg="transparent") # pop plot
    
    output$outbreaks <- renderPlot({
      get_plot_on(prevdat(),
                  reactiveinputtable_reduced(),
                  input$country_selected, input$year_selected)
    }, bg="transparent") # outbreak plot
    
    
  }, ignoreInit = TRUE)
  
  
  
  ########################
  # Result table
  ########################
  # Result table
  reformatted <- reactive({
                reformatted_table <- reformat_datatable(vaccine_estimates()) # function to reformat the dataframe
                datatable(reformatted_table[reformatted_table$Year>2021, ],
                          rownames=FALSE, editable=FALSE, extensions = 'Buttons', # copy / download buttons
                          options = list(dom = 'Bt',
                                         buttons = list('copy', 'csv', 'excel', 'pdf')
                            ))
    })
  
  output$resulttable <- DT::renderDataTable({
    if (input$updatebutton>0) {
      isolate(reformatted())
    }
    }) # rendertable
  
  
  
  # Result table by species - added by Rob (16/02/22)
  
  reformatted2 <- reactive({
    reformatted_table2 <- reformat_datatable2(vaccine_estimatesS()) # function to reformat the dataframe
    datatable(reformatted_table2[reformatted_table2$Year>2021, ],
              rownames=FALSE, editable=FALSE, extensions = 'Buttons', # copy / download buttons
              options = list(dom = 'Bt',
                             buttons = list('copy','csv', 'excel', 'pdf')
              ))
  })
  
  output$resulttable2 <- DT::renderDataTable({
    if (input$updatebutton>0) {
      isolate(reformatted2())
    }
  }) # rendertable
  
  
  
  ########################
  # Plots in result tab
  ########################
  # Plot simulated vaccine numbers # Modified by Rob (16/02/22)
  histplotoutput <- reactive({
       output$histresult <- renderPlot({
      hist(mcresult()$Total_vaccines/10^6, main="", xlab="", ylab="", ylim = c(0,22000), yaxt = "n", ann = FALSE)
      title(main = paste("Predicted Total Vaccines in",input$year_selected ), line = 2, font.lab=2, cex.main = 1.5 )
      title(xlab = "Total Vaccines (in millions)", font.lab=2, cex.lab = 1.2, sub = "Blue line = Median, Red lines = CI 0.025 & 0.975")
      abline(v= median(mcresult()$Total_vaccines/10^6), col="blue", lwd=2)
      # abline(v= vaccine_estimates$CI_low/10^6, col="red")
      # abline(v = low/10^6, col = "red", lwd=3, lty=2)
      abline(v= quantile(apply(unmc(mcresult()$Total_vaccines)/10^6, 1, median), probs = 0.025), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
      abline(v= quantile(apply(unmc(mcresult()$Total_vaccines)/10^6, 1, median), probs = 0.975), col = "red", lwd=3, lty=2) # Rob 01/03/22: Calculate the CI interval around the median (median uncertainty)
    }) #renderPlot
  }) # reactive
  
  
  output$histresult <- renderPlot({
    if (input$updatebutton>0) {
      isolate(histplotoutput())
    }
  }) # renderPlot
  
  # Plot simulated vaccine numbers
  trajplotoutput <- reactive({
    output$trajresult <- renderPlot({
      countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
      plot_totalvaccine_traj(countrycode, vaccine_estimates())
    })
  }) # reactive
  output$trajresult <- renderPlot({
    if (input$updatebutton>0) {
      isolate(trajplotoutput())
    }
  }) # renderPlot

  # Plot Simulated Large ruminants vaccines # added by Rob (16/02/22)
  
  histresultLRoutput <- reactive({
    output$histresultLR <- renderPlot({
      countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
      plot_totalvaccine_LG_traj(mcresult)+
        title(main = paste("Predicted Vaccines for Large Ruminants in", input$year_selected), line = 3.3, font.lab=2, cex.main = 1.3 )
    })
  }) # reactive
  
  
  output$histresultLR <- renderPlot({
    if (input$updatebutton>0) {
      isolate(histresultLRoutput())
    }
  }) # renderPlot
  
  # Plot Simulated SMALL Ruminants vaccines # added by Rob (16/02/22)
  
  histresultSMoutput <- reactive({
    output$histresultSR <- renderPlot({
      countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
      plot_totalvaccine_SR_traj(mcresult)+
        title(main = paste("Predicted Vaccines for Small Ruminants in", input$year_selected ), line = 3.3, font.lab=2, cex.main = 1.3 )
    })
  }) # reactive
  
  
  output$histresultSR <- renderPlot({
    if (input$updatebutton>0) {
      isolate(histresultSMoutput())
    }
  }) # renderPlot
  
  
  
  # Plot Simulated PIG vaccines # added by Rob (16/02/22)
  
  histresultPoutput <- reactive({
    output$histresultP <- renderPlot({
      countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
      plot_totalvaccine_P_traj(mcresult)
      title(main = paste("Predicted Vaccines for Pigs in", input$year_selected), line = 3.3, font.lab=2, cex.main = 1.3 )
    })
  }) # reactive
  
  
  output$histresultP <- renderPlot({
    if (input$updatebutton>0) {
      isolate(histresultPoutput())
    }
  }) # renderPlot
  
  
  # Plot Simulated Emergency Vaccines # added by Rob (16/02/22)
  
  histresultEVoutput <- reactive({
    output$histresultEV <- renderPlot({
      countrycode = unique(data[data$Country_name == input$country_selected, "Country_code"])
      plot_totalvaccine_EV_traj(mcresult)
      title(main = paste("Predicted Emergency Vaccines in", input$year_selected), line = 3.3, font.lab=2, cex.main = 1.3 )
    })
  }) # reactive
  
  
  output$histresultEV <- renderPlot({
    if (input$updatebutton>0) {
      isolate(histresultEVoutput())
    }
  }) # renderPlot
  
  
  #Pie chart for youngstock proportions (Pili June 2022)   

  #   
  #   Name<-c("Vaccines for Youngstock Large Ruminants"," Vaccines for Youngstock Small Ruminants",
  #           "Vaccines for Youngstock Pigs ") #, "Vaccines for Adults Large Ruminants"," Vaccines for Adults Small Ruminants",
  #          # "Vaccines for Adult Pigs ","other") 
  #   Pct<-c(input$ysproplr, input$yspropsr, input$yspropp)
  #   
  #   hc <- df %>% 
  #     hchart("pie", innersize= "50%", hcaes(x = Name,  y = Pct), title = "Youngstock Proportion Vaccine Estimation")
  #   # hc_plotOptions(innersize="50%", startAngle=90, endAngle=90,center=list('50%', '75%'),size='110%')
  #   hc
  # }) #render pie chart


  ###Half pie Youngstock and Adult proportions 7th variable is only dummy if even numbers highchart displays error hence the extra redundant variable
  output$pieCHART<- renderHighchart({
    Name<-c("Adult Pigs","Adult Large Ruminants", "Adult Small Ruminants"
            ,"Youngstock Large Ruminants ", "Youngstock Small Ruminants", "Youngstock Pigs ", "Other")
    Pct<-c(((vaccine_estimates()$TotalVaccine)/(100-input$yspropp)),((vaccine_estimates()$TotalVaccine)/(100-input$ysproplr)),
           ((vaccine_estimates()$TotalVaccine)/(100-input$yspropsr)), (vaccine_estimates()$TotalVaccine/input$ysproplr),
           (vaccine_estimates()$TotalVaccine/input$yspropsr), (vaccine_estimates()$TotalVaccine/input$yspropp),  (vaccine_estimates()$TotalVaccine/0))
   
   #   
     df<-data.frame(Name,Pct)
    
    highchart()%>%
    hc_add_series(type = "pie", data = df, hcaes(x = Name, y = Pct), startAngle = -90, endAngle = 90) %>%
      hc_tooltip(valueDecimals = 0) %>%
      hc_yAxis(allowDecimals = FALSE) %>%
      
                         
      hc_legend(
      align = "center",
      layout = "proximate",
      x= 1,
      y=100
    )
  
  }) #render pie chart
  
  
 # Bar chart for sector proportions (Pili June 2022) highchart shows erro when showing 3 variables hece we add an empty one 
    
    
    output$barCHART<- renderHighchart({
   
    Sector<-c("Beef","Dairy","Smallholders","")
    Vaccines<-c((vaccine_estimates()$TotalVaccines/input$sectorpropsmall), 
    (vaccine_estimates()$TotalVaccines/input$sectorpropdairy),
    (vaccine_estimates()$TotalVaccines/input$sectorpropbeef),(vaccine_estimates()$TotalVaccines/0))
    year<-vaccine_estimates()$Year
    df<-data.frame(Sector, Vaccines, year)

    hc <- df %>%
      hchart("column", hcaes(x = Sector, y = Vaccines, group= year))%>%
      hc_tooltip(valueDecimals = 0) 
    
  }) #render pie chart
  
  
  
  #########################
  #Map output##############
  #########################
#add the reactive function to filter the data to visualize
   
    observeEvent(input$mapyear, {
      data <- data[data$Year == input$mapyear,]
    })  
   
    
    
  #Mapbox Carto (thirdparty) api address + token to render custom UN compliant map  

  url<- "https://api.mapbox.com/styles/v1/pilipili/cl4h4ht3r001015t5xf2cuu25/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoicGlsaXBpbGkiLCJhIjoiY2wyOWdka2JwMGd5NjNrbzg1NXRndW1mbiJ9.Msfezuof3dDDJJ7xNwe-qQ"

 
  
   map_out <-reactive ({
    
    map<-leaflet(data=data) %>%
      setView(15, 46, zoom = 3) %>%
      
    addTiles(urlTemplate = url) %>%
      #addHeatmap(data=data$TotalOutbreaks)
    
      #addLegend(position= 'bottomright', colors, values,  opacity= 0.5)
      addCircles(popup= paste('Outbreaks', data$TotalOutbreaks))
      #addMarkers(popup= paste('Outbreaks', data$TotalOutbreaks))
    
    #addPolygons(data = sub_shape,fill = TRUE, fillColor = colors$color, fillOpacity = .8, stroke = TRUE, weight = 3, color = "white", dashArray = c(5,5),popup = pops
  })


   output$worldmap <- renderLeaflet({
     map_out()
                                  

    })
   
 
   
   observeEvent(input$map_groups, {
     worldmap <- leafletProxy("map") %>% clearControls()
     
     if (input$map_groups == 'PCP (default)'){
       my_map <- my_map %>%
         addLegend(
           "bottomright",
           pal = palpcp,
           values = World@data$PCP,
           opacity= 1, 
           title= 'PCP Stage')
     }else{
       my_map <- my_map %>%
         addLegend(
           "bottomright",
           pal = palpool,
           values = world@data$Pool,
           opacity= 1, title= 'Pool')
     }
   }) 
 
  ##########################
  #Aggregated results table  
  #########################
   agresulttab <- data.frame()
   agresulttable<-data.frame()
   
   
   saveData <- function(data) {
     
     data <- as.data.frame(data)
     if (exists("agresulttab")) {
       agresulttab <<- dplyr::bind_rows(agresulttab, data )
       
     } else {
       agresulttab <<- data
     }
   }
  
   loadData <- function() {
     if (exists('agresulttab')) {
       
       agresulttable <- dplyr::bind_rows(agresulttab, agresulttable)
       adorn_totals(agresulttable, "row") #adds the row with totals
       
     }
     
   }
   
   
   observeEvent(input$aggregatebutton, {
   
  saveData(na.omit(vaccine_estimates()))
  
   output$agresulttable <- DT::renderDataTable({
     
     datatable(loadData(), rownames=FALSE, extensions= 'Buttons',
               options = list(dom = 'Bt', buttons = list('copy', 'csv', 'excel', 'pdf')))

    }) # rendertable
 
    
    
     }) #observe agg button


  ############################################
  #Remove last row of Aggregated results table
  ############################################

  newagresulttable <- agresulttable[-c(1),]
  deleterow<- function() {
    if (exists("agresulttable")) {
      #agresulttable<- agresulttable[-nrow(agresulttable),]
      agresulttable<- agresulttable[-c(1),]

  }}

  observeEvent(input$removeRow, {

    deleterow()


    output$agresulttable <- DT::renderDataTable({

    datatable(agresulttable, rownames=FALSE, extensions= 'Buttons',
                options = list(dom = 'Bt', buttons = list('copy', 'csv', 'excel', 'pdf')))


  }) # rendertable


  })

  
###serotype
  # output$serotype <- renderUI({
  #   tags$iframe(seamless="seamless", 
  #               src= "https://tableau.apps.fao.org/#/views/CurrentPCPMap_July7_16571953067940/Serotypes?:iid=2:showVizHome=no:embed=true ",
  #               width=1200, 
  #               height=800)
  # })
  
  
 
  ########################
  # Send message to mail
  ########################
  observeEvent(input$sendbutton, {
      req(input$message)
      print('comming soon')
      # gsheet <- googlesheets4::gs4_get('1hZoP2odMA9rQIWmkVJdVBGpnYXsGlY_ghq0JCdl_ZmQ')
      # googlesheets4::sheet_append(gsheet, data = input$message, sheet=1)
    
      # Use isolate() to avoid dependency on input$sendbutton
      # isolate({
      #     from <- as.character(input$contact_email) #input$contact_email
      #     to <- "hyojung.lee@fao.org"
      #     subject <- paste("VADEMOS got a new message")
      #     body <- input$message                    
      #     smtp=list(host.name="smtp.office365.com")
      #     sendmail(from=from,to=to,subject=subject,msg=body,control=smtp)
      # })
    })

  
  
  } # server

shinyApp(ui, server)
