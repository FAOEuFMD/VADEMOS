#####################################################################################################
## UI and server for the app.R
## pilar.riusmunoz@fao.org August 2024~
#####################################################################################################


###################################################
# 0 - Load libraries
###################################################
library(RMySQL) #Mysql connection
library(data.table) #reads csv and table functions
library(readxl)  #reads excels
library(DBI)
library(dplyr)
library(tidyverse)
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
library(ggiraph)  # For interactive graphics
library(plotly)
library(sendmailR)
options(shiny.reactlog=TRUE)
library(highcharter)
library(sp)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(raster)
library(sf)
library(rworldxtra)
library(mapboxapi)
library(tmap)   
library(remotes)
library(mapboxer)
library(shinyscreenshot)
library(janitor)
library(glue)
library(rsconnect)
library(renv)
library(bslib)
library(httr)
library(geojsonio)
library(shinycssloaders)




###################################################
# 1 - Source Data
###################################################
# Load environment variables credentials in the .Renviron file
db_host <- Sys.getenv("DB_HOST")
db_user <- Sys.getenv("DB_USER")
db_user2 <- Sys.getenv("DB_USER2")
db_password <- Sys.getenv("DB_PASSWORD")
db_password2 <- Sys.getenv("DB_PASSWORD2")
db_port <- Sys.getenv("DB_PORT")
db_name <- Sys.getenv("DB_NAME")
db_name2 <- Sys.getenv("DB_NAME2")

###################################################
# 2 - Establish Connection
###################################################

#pcp<-read_excel("pcp_2024.xlsx")
outbreak<-read_excel("Outbreaks_Wahis.xlsx")
# Replace '-' with 0 and transform the Cases column to numeric (integer)
outbreak$Cases <- as.integer(gsub("-", "0", outbreak$Cases))
outbreak$Outbreaks <- as.integer(gsub("-", "0", outbreak$`New outbreaks`))

delphi <- read.csv('delphi-round1.csv')

###################################################
# General UI
###################################################
source("UIParts.R")

ui <- fluidPage(
  # Use CSS file to customize the interface
  tags$head(
    tags$title("VADEMOS"),  # Set the title of the web page
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"), # color themes
    tags$link(rel="shortcut icon", href="FAVICON.png") # eufmd favicon
  ),
  theme = shinytheme("united"), # basic shiny theme
  chooseSliderSkin(skin="Flat", color="#073f23"), # to avoid sliderInput's blue color
  
  # Interface
  
  
  navbarPage(
             id = "main_navbar",  # Add an ID to the navbarPage
             title = div(img(src = 'EuFMD_2023_white.png',
                             style = "margin: -20px 0 0 0; padding: 0;", height = 70),
                         style = "display: flex; align-items: left;"  ),
   
             # NAVBAR TAB PANEL 1 - ABOUT PAGE
             tabPanel("About",
                      style = "text-align: justify; margin-left: 20px; margin-right: 20px;",
                      
                      # Buttons at the top for navigation
                      div(
                        actionButton("sum", "Summary", class = "btn-custom"),          # Button for Summary
                        actionButton("param", "Parameters", class = "btn-custom"),     # Button for Parameters
                        actionButton("math", "Mathematical Models", class = "btn-custom"),  # Button for Mathematical Models     
                        actionButton("go_to_tool", "Tool", class = "btn-custom") #only used if no navbar
                      ),
                      
                      # Step-by-step presentation within the 'About' tab
                      div(
                        # Hidden input to store the step value
                        hidden(textInput("step", NULL, value = 1)),
                        
                        # Step 1: Include first chunk of HTML (Summary)
                        conditionalPanel(
                          condition = "input.step == '1'",  # Summary section
                          includeHTML("www/about_part1.Rhtml")
                        ),
                        
                        # Step 2: Include second chunk of HTML (Parameters)
                        conditionalPanel(
                          condition = "input.step == '2'",  # Parameters section
                          includeHTML("www/about_part2.Rhtml")
                        ),
                        
                        # Step 3: Include third chunk of HTML (Mathematical Models)
                        conditionalPanel(
                          condition = "input.step == '3'",  # Mathematical Models section
                          includeHTML("www/about_part3.Rhtml")
                        )
                      )  # div
             )  # tabPanel about
             , # tabPanel about
             
             
             
             
             # ----------------------------------
             # NAVBAR TAB PANEL 2 - TOOL PAGE 
             tabPanel("Tool",
                      tool <- div(class = "vademos", tool())
                      ),
              
             tabPanel("Contact",
                      div(id = "contactpage", 
                          style = "width: 100%; max-width: 100%; margin: 0 auto; padding: 20px;",
                          tags$iframe(
                            width = "1000px", 
                            height = "500px", 
                            src = "https://forms.office.com/e/yAY1SsSJsk?embed=true", 
                            frameborder = "0", 
                            marginwidth = "0", 
                            marginheight = "0", 
                            style = "border: none; max-width:100%; max-height:100vh", 
                            allowfullscreen = NA, 
                            webkitallowfullscreen = NA, 
                            mozallowfullscreen = NA, 
                            msallowfullscreen = NA
                          )
                      )
             )

            ), # navbar
        
        br(),
        br(),
        # FOOTER
        div(id='footer',
            class = "footer",
            style = "background-color: #073f23; color: white;",
            includeHTML("www/footer.html")
        ) # div=footer
        
) # UI end fluid page


###################################################
# 3 Server; R functions that run/respond to UI
###################################################
server <- function(input, output, session) {
  
  ###################About presentation##################  
  # Server part
  step <- reactiveVal(1)  # Initialize step to 1
  
  # Update hidden input with the current step
  observe({
    updateTextInput(session, "step", value = step())
  })
  
  # Step navigation buttons
  observeEvent(input$param, { step(2) })  # Go to parameters
  observeEvent(input$math, { step(3) })   # Go to mathematical models
  observeEvent(input$sum, { step(1) })    # Go back to summary
  
###################Help Side Bar##################
  # Reactive value to store the current help content
  current_help <- reactiveVal("www/help.Rhtml")
  
  
  # Update help content based on button click
  observeEvent(input$help1, {
    current_help("www/help1.Rhtml")
    # JavaScript to scroll the sidebar to the top
    session$sendCustomMessage("scroll-sidebar", list())
  })
  
  # Update help content based on button click
  observeEvent(input$help2, {
    current_help("www/help2.Rhtml")
    
  })
  
  observeEvent(input$help3, {
    current_help("www/help3.Rhtml")
    
  })
  
  observeEvent(input$help4, {
    current_help("www/help4.Rhtml")
    
  })
  
  
  
  # Render the help content in the sidebar
  output$help_content <- renderUI({
    req(current_help())
    includeHTML(current_help())
  })
  
  observeEvent(input$go_to_tool, {
    updateTabsetPanel(session, "main_navbar", selected = "Tool")
  })
 
#################STEP1##########################  
  
  # Filter subregions according to selected region
  observe({
    req(input$Region)  # Ensure Region input is available
    # Connect to AWS RDS
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = Sys.getenv("DB_NAME1"), # or DB_NAME2 if you want the other DB
                     host = Sys.getenv("DB_HOST"),
                     port = as.numeric(Sys.getenv("DB_PORT")),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD"))
    
    
    # Query to fetch distinct subregions based on selected regions
    query_subregions <- paste("SELECT DISTINCT subregion FROM VADEMOS.countries WHERE region IN ('", 
                              paste(input$Region, collapse = "','"), "')", sep = "")
    
    # Fetch filtered subregions from the database
    filtered_subregions <- dbGetQuery(con, query_subregions)
    
    # Update the Subregion picker input with the fetched data
    updatePickerInput(session, "Subregion", choices = filtered_subregions$subregion)
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
  })
  
  # Filter countries according to selected subregion
  observe({
    req(input$Subregion)  # Ensure Subregion input is available
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = Sys.getenv("DB_NAME1"), # or DB_NAME2 if you want the other DB
                     host = Sys.getenv("DB_HOST"),
                     port = as.numeric(Sys.getenv("DB_PORT")),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD"))
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Query to fetch distinct countries based on selected subregions
    query_countries <- paste("SELECT DISTINCT name_un FROM VADEMOS.countries WHERE subregion IN ('", 
                             paste(input$Subregion, collapse = "','"), "')", sep = "")
    
    # Fetch filtered countries from the database
    filtered_countries <- dbGetQuery(con, query_countries)
    
    # Update the Country picker input with the fetched data
    updatePickerInput(session, "Country", choices = filtered_countries$name_un)
  })
  
 
  
 

  # Reactive expression to filter forecast data based on year and country selections
  forecasted_data <- reactive({
    req(input$year_selected, input$Country, input$Species)
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = Sys.getenv("DB_NAME1"), # or DB_NAME2 if you want the other DB
                     host = Sys.getenv("DB_HOST"),
                     port = as.numeric(Sys.getenv("DB_PORT")),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD"))
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Query to filter the forecast data from the database based on the selected year, country, and species
    query <- sprintf(
      "SELECT * FROM VADEMOS.forecast_data 
     WHERE Year IN (%s) 
       AND Country IN (%s) 
       AND Specie IN (%s)",
      paste(shQuote(input$year_selected), collapse = ", "),
      paste(shQuote(input$Country), collapse = ", "),
      paste(shQuote(input$Species), collapse = ", ")
    )
    
    # Execute the query and fetch the data
    result <- dbGetQuery(con, query)
    
    return(result)
  })
  
  
      
  # Render the population prediction table
  output$forecasttable <- renderDT({
    data <- forecasted_data()
    
    
    
    datatable(data, escape = FALSE, extensions = 'Buttons', options = list(
      dom = 'Bfrtip',  # Add buttons to the top of the table
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      pageLength = 10,
      dom = 't',       # 't' stands for table only (removes "Show entries" and search box)
      ordering = FALSE,  # Disable ordering (sorting) of columns
      paging = FALSE,    # Disable pagination
      scrollX = TRUE,
      # Hide the 'ID' column
      columnDefs = list(list(visible = FALSE, targets = 1))),
      editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,6,7,8,9,10)))
  )
  })
  
  # Reactive value to store help text
  help_text <- reactiveVal("* Select on Step 1 to see values.")
  
  # Output the dynamic help text
  output$dynamic_help_text <- renderText({
    help_text()
  })

  # Update help text when the edit button is clicked
  observeEvent(input$edit_values, {
    # Update help text based on the action of clicking the edit button
    help_text("* Double click on Forecasted Value cell to edit value.")
  })
  
  
  # Observer triggered when the "Save" button is clicked
  observeEvent(input$save_pops, {

    # Check if the cell edit information is available
    if (!is.null(input$forecasttable_cell_edit)) {

      # Access the current forecasted data
      new_data <- forecasted_data()


      # Update the forecast_data based on the edited input
      edited_data <- input$forecasttable_cell_edit

      # Check if there are any edits
      if (nrow(edited_data) > 0) {
        for (i in seq_len(nrow(edited_data))) {
          info <- edited_data[i, ]  # Get each edit info
          # Ensure that we are only updating valid cells
          if (info$row <= nrow(new_data) && info$col <= ncol(new_data)) {
            new_data[info$row, info$col] <- info$value
          }
        }
        
        # Replace the reactive data with the updated data
        forecasted_data <<- reactiveVal(new_data)
        print("Forecasted data updated.")
        print(forecasted_data)
      

        # Notify the user of successful save
        showNotification("Changes have been saved successfully!", type = "message", duration = 5)
      } else {
        print("No edits made.")
        showNotification("No changes were made.", type = "warning", duration = 5)  # Notify if no edits were made
      }
    } else {
      print("No edited data available.")
      showNotification("No edited data available.", type = "error", duration = 5)  # Notify if no edited data
    }
  })


  # Reactive expression to filter faostat data based on year, area, and item selections
  faostat_data <- reactive({
    req(input$Country, input$Species)
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = Sys.getenv("DB_NAME1"), # or DB_NAME2 if you want the other DB
                     host = Sys.getenv("DB_HOST"),
                     port = as.numeric(Sys.getenv("DB_PORT")),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD"))
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Query to filter the faostat data from the database based on the selected year, area, and item
    query <- sprintf(
      "SELECT Year, Area, Item, Value FROM VADEMOS.faostat_animal 
     WHERE Area IN (%s) 
       AND Item IN (%s)",
      paste(shQuote(input$Country), collapse = ", "),
      paste(shQuote(input$Species), collapse = ", ")
    )
    
    # Execute the query and fetch the data
    result <- dbGetQuery(con, query)
    
    return(result)
  })
  

  
  # ----------------------------------
  # # Interactive plot 1: Livestock population

    output$pops <- renderPlotly({
      
      # Request selected countries and species
      selected_countries <- input$Country
      selected_species <- input$Species
      
      get_plot_pop(faostat_data(), forecasted_data(), selected_countries, selected_species)
    })
    
  get_plot_pop <- function(df1, df2, selected_countries, selected_species) {
    # Ensure df1 and df2 have the correct columns
    if (!all(c("Year", "Value", "Area", "Item") %in% colnames(df1))) {
      stop("df1 is missing required columns: Year, Value, Area, Item")
    }
    if (!all(c("Year", "Forecasted Value", "Country", "Specie") %in% colnames(df2))) {
      stop("df2 is missing required columns: Year, Forecasted Value, Country, Specie")
    }
    
    # Filter data based on selected countries and species
    df1_filtered <- df1 %>%
      filter(Area %in% selected_countries, Item %in% selected_species)
    
    df2_filtered <- df2 %>%
      filter(Country %in% selected_countries, Specie %in% selected_species)
    
    # Create the plot
    plot_ly() %>%
      add_trace(data = df1_filtered, x = ~Year, y = ~Value, type = 'scatter', mode = 'lines',
                split = ~Area,  # This will create separate lines for each country
                text = ~paste("Country:", Area, "<br>Species:", Item, "<br>Year:", Year, "<br>Value:", Value),
                hoverinfo = 'text', name = ~paste(Area, 'FAO Stats')) %>%
      add_trace(data = df2_filtered, x = ~Year, y = ~`Forecasted Value`, type = 'scatter', 
                mode = 'markers', marker = list(color = '#36454F', size = 7, opacity = 1),
                text = ~paste("Country:", Country, "<br>Species:", Specie, "<br>Year:", 
                              Year, "<br>Forecasted Value:", `Forecasted Value`),
                hoverinfo = 'text',name = 'Forecasted') %>%
      layout(
        title = list(text = "Livestock Population and Forecast", x = 0, xanchor = 'left'),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Population"),
        legend = list(x = 0.1, y = 0.9)  # Adjust position as needed
      )
  }

  
  
  #################STEP2########################## 
  # Helper function to generate renderUI for each species and schedule type
  generateVaccineScheduleUI <- function(species, schedule_type, label) {
    renderUI({
      # Set default value to "2" for large ruminants, otherwise "1"
      default_value <- ifelse(species == "LR", "2", "1")
      selectInput(
        inputId = paste0("vschedule_", tolower(species), "_", tolower(schedule_type)),
        label = label,
        choices = c("1", "2"),
        selected = default_value  # Default selection
      )
    })
  }
  
  # Sidebar tab 1 - vaccine schedules
  output$vschedule_lr_as <- generateVaccineScheduleUI("LR", "AS", "Large ruminant (> 1 year)")
  output$vschedule_sr_as <- generateVaccineScheduleUI("SR", "AS", "Small ruminant (> 1 year)")
  output$vschedule_p_as <- generateVaccineScheduleUI("P", "AS", "Pig (> 1 year)")
  output$vschedule_lr_ys <- generateVaccineScheduleUI("LR", "YS", "Large ruminant (< 12 months)")
  output$vschedule_sr_ys <- generateVaccineScheduleUI("SR", "YS", "Small ruminant (< 12 months)")
  output$vschedule_p_ys <- generateVaccineScheduleUI("P", "YS", "Pig (< 12 months)")
  
  # Create a reactive value to store user selections
  user_vaccine_schedule <- reactiveValues(selections = list())
  
  # Observe changes in the vaccine schedule inputs
  observe({
    user_vaccine_schedule$selections$lr_as <- input$vschedule_lr_as
    user_vaccine_schedule$selections$sr_as <- input$vschedule_sr_as
    user_vaccine_schedule$selections$p_as <- input$vschedule_p_as
    user_vaccine_schedule$selections$lr_ys <- input$vschedule_lr_ys
    user_vaccine_schedule$selections$sr_ys <- input$vschedule_sr_ys
    user_vaccine_schedule$selections$p_ys <- input$vschedule_p_ys

  })
  
  
  
  ########################################### 
  # youngstock proportion
  
  output$ysproplr <- renderUI({
    sliderInput("ysproplr","Large Ruminants (< 12 months)",
                min=0, max=100,
                value=30,
                step=1,
                post="%")
  }) #renderUI
  
  output$yspropsr <- renderUI({
    sliderInput("yspropsr","Small Ruminants (< 12 months)",
                min=0, max=100,
                value=20,
                step=1,
                post="%")
  }) #renderUI
  
  output$yspropp <- renderUI({
    sliderInput("yspropp","Pigs (< 12 months)",
                min=0, max=100,
                value=20,
                step=1,
                post="%")
  }) #renderUI
  
  # Create a reactive value to store youngstock proportions
  youngstock_proportions <- reactiveValues(selections = list())
  
  # Observe changes in the youngstock proportion sliders
  observe({
    youngstock_proportions$selections$lr <- input$ysproplr  # Large Ruminants
    youngstock_proportions$selections$sr <- input$yspropsr  # Small Ruminants
    youngstock_proportions$selections$p <- input$yspropp     # Pigs
    
  })
  
  
  ########################STEP3###########################
  # pcp 
  
  output$pcp_selected <- renderUI({
    radioButtons("pcp_selected","Select PCP-FMD stage to view default coverage",
                 choices=list('1'=1, '2'=2, '3'=3, '4'=4, 'Above'=5),
                 selected=NULL,
                 inline=TRUE # horizontal buttons
    )
  }) # renderUI
  
  
  # ----------------------------------
  # PCP stage related sliderinputs
  # delphi input - q2
  pcp_reactive <- eventReactive(input$pcp_selected, {
    pcp <- as.numeric(input$pcp_selected)
    pcpstr <- sprintf('_%s_m', pcp)
    delphi[grepl(pcpstr, names(delphi))]
  })
  
  output$prophylactic_vc_lr <- renderUI({ 
 
    q2_LR <- pcp_reactive()[grepl('Q2_LR', names(pcp_reactive()))] #here i removed unique
    q2_LR_mode <- unique(q2_LR[grepl('most_likely', names(q2_LR))])
    sliderInput("prophylactic_vc_lr","Large ruminants",
                min=0, max=100,
                value=as.numeric(q2_LR_mode)*100,
                step=1,
                post="%")
  })
  
  output$prophylactic_vc_sr <- renderUI({

    q2_SR <- pcp_reactive()[grepl('Q2_SR', names(pcp_reactive()))] #here i removed unique
    q2_SR_mode <- unique(q2_SR[grepl('most_likely', names(q2_SR))])
    sliderInput("prophylactic_vc_sr","Small ruminants",
                min=0, max=100,
                value=as.numeric(q2_SR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$prophylactic_vc_p <- renderUI({
    
    q2_P <- pcp_reactive()[grepl('Q2_P', names(pcp_reactive()))] #here i removed unique
    q2_P_mode <- unique(q2_P[grepl('most_likely', names(q2_P))])
    sliderInput("prophylactic_vc_p","Pigs",
                min=0, max=100,
                value=as.numeric(q2_P_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  # delphi input - q3
  output$outbreak_vc_lr <- renderUI({
    
    q3_LR <- pcp_reactive()[grepl('Q3_LR', names(pcp_reactive()))] #here i removed unique
    q3_LR_mode <- unique(q3_LR[grepl('most_likely', names(q3_LR))])
    sliderInput("outbreak_vc_lr","Large ruminants",
                min=0, max=100,
                value=as.numeric(q3_LR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$outbreak_vc_sr <- renderUI({
    
    q3_SR <- pcp_reactive()[grepl('Q3_SR', names(pcp_reactive()))]#here i removed unique
    q3_SR_mode <- unique(q3_SR[grepl('most_likely', names(q3_SR))])
    sliderInput("outbreak_vc_sr","Small ruminants",
                min=0,  max=100,
                value=as.numeric(q3_SR_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  
  output$outbreak_vc_p <- renderUI({
    
    q3_P <- pcp_reactive()[grepl('Q3_P', names(pcp_reactive()))]#here i removed unique
    q3_P_mode <- unique(q3_P[grepl('most_likely', names(q3_P))])
    sliderInput("outbreak_vc_p","Pigs",
                min=0, max=100,
                value=as.numeric(q3_P_mode)*100,
                step=1,
                post="%")
  }) #renderUI
  

  
  
  
###########PCP data 
  pcp_data <- reactive({
    req(input$Country)
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = db_name2,  # or db_name if you want the other DB
                     host = db_host,
                     port = as.numeric(db_port),
                     user = db_user2,
                     password = db_password2)
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Query to filter the forecast data from the database based on the selected country
    query <- sprintf(
      "SELECT * FROM PCP.PCP_DB 
     WHERE Country IN (%s)",
      paste(shQuote(input$Country), collapse = ", ")
    )
    
    # Execute the query and fetch the data
    result <- dbGetQuery(con, query)
    
    # Add numeric mapping for PCP_Stage
    result$PCP_Stage_Numeric <- case_when(
      result$PCP_Stage == "PCP-0" ~ 0,
      result$PCP_Stage == "PCP-1" ~ 1,
      result$PCP_Stage == "PCP-2" ~ 2,
      result$PCP_Stage == "PCP-3" ~ 3,
      result$PCP_Stage == "PCP-4" ~ 4,
      result$PCP_Stage == "PCP-1-Provisional" ~ 0.5,
      result$PCP_Stage == "PCP-2-Provisional" ~ 1.5,
      result$PCP_Stage == "PCP-3-Provisional" ~ 2.5,
      result$PCP_Stage == "PCP-4-Provisional" ~ 3.5,
      TRUE ~ NA_real_  # Handle unexpected cases
    )
    # Remove rows with NA in PCP_Stage
    result <- result[!is.na(result$PCP_Stage), ]
    
    return(result)
  })
  
  
  
  output$pcp_table <- renderDT({
    # Access the reactive pcp_data
    data <- pcp_data()
    
    # Filter for the year 2025 directly
    pcp_2025 <- data[data$Year == 2025, ]
    
    # Print for debugging
    print(pcp_2025)
    
    # Render the DataTable with editable cells
    datatable(
      pcp_2025, 
      options = list(
        pageLength = 10, 
        dom = 't', 
        columnDefs = list(list(visible = FALSE, targets = c(1,3,4,5,7,8,9)))  # Adjust column visibility
      ),
      editable = list(target = 'cell', disable = list(columns = 2))
    )
  })
  
  
  
  
  
  # Define a named vector that maps each stage (including intermediate values like 0.5, 1.5, etc.) to a color
  pcpcolors <- c(
    "0"= '#fd030e',
    "0.5" = '#fd030e',  # Red for stage 0.5
    "1" = '#ef8125',    # Orange for stage 1
    "1.5" = '#ef8125',  # Orange for stage 1.5
    "2" = '#fccc19',    # Yellow for stage 2
    "2.5" = '#fccc19',  # Yellow for stage 2.5
    "3" = '#4f8e32',    # Green for stage 3
    "3.5" = '#4f8e32',  # Green for stage 3.5
    "4" = '#15592b'     # Dark green for stage 4
  )
  
  get_plot_pcp_lines <- function(df) {
    # Check if 'PCP-FMD Stage' exists in the dataframe
    if (!"PCP_Stage" %in% colnames(df)) {
      stop("'PCP_Stage' column is missing in the dataframe")
    }
    
    
    # Initialize the plot
    p <- plot_ly()
    
    # Get unique countries
    countries <- unique(df$Country)
    
    # Add traces for each country
    for (i in seq_along(countries)) {
      country <- countries[i]
      country_data <- df[df$Country == country, ]
      
      # Add a bar trace for the current country
      p <- p %>%
        add_bars(
          data = country_data,
          x = ~Year,
          y = ~PCP_Stage_Numeric,
          color = ~as.factor(PCP_Stage_Numeric),  # Color by PCP Stage
          colors = pcpcolors,
          name = country,
          text = ~paste("PCP Stage:", PCP_Stage),
          hoverinfo = "text",
          visible = if (i == 1) TRUE else "legendonly"  # Show only the first country by default
        )
    }
    
    # Adjust layout to remove dropdown and focus on legend interactivity
    p <- p %>%
      layout(
        title = list(
          text = "PCP-FMD Progression by Country",
          x = 0,
          xanchor = "left"
        ),
        xaxis = list(title = "Year"),
        yaxis = list(
          title = "PCP-FMD Stage",
          tickmode = "linear",
          tickvals = seq(0, 4, by = 0.5),
          ticktext = as.character(seq(0, 4, by = 0.5)),
          range = c(0, 4)
        ),
        showlegend = TRUE  # Use legend for toggling countries
      )
    
    return(p)
  }
  # Server logic
  output$pcps <- renderPlotly({
    req(pcp_data())  # Ensure that the reactive function has data
    
    # Use the reactive function to get filtered data
    df <- pcp_data()
    
    ## Validate that there is data to plot
    validate(
      need(nrow(df) > 0, "No data available to plot as selected Region does not have FMD-PCP Stage. Continue to next step")
    )
    
    # Generate the plot using the function defined earlier
    get_plot_pcp_lines(df)
  })
  
 
  
  
  
  
  
  ########################STEP3###########################
  # Emergency vaccination km radius and outbreak info 
  # Server: Access the selected radius value using input$radius
  observe({
    radius_selected <- input$radius
   
  })
  
  # Reactive expression to filter forecast data based on year and country selections
  outbreaks_data <- reactive({
    req(input$Country, input$Species)  # Ensure both inputs are available
    
    # Filter the outbreak data based on the selected country and species
    result <- outbreak %>%
      filter(Country %in% input$Country, 
             Species %in% input$Species)  # Filter by both country and species
    
    return(result)
  })
  
  
  # Render the outbreaks table
  output$outbreaktable <- renderDT({
    # Replace NA with 0, convert to numeric, and handle non-numeric values
    outbreaks_summary <- outbreaks_data() %>%
      # Filter out rows where 'New Outbreaks' or 'Cases' could not be coerced to numeric
      filter(!is.na(Outbreaks) & !is.na(Cases)) %>%
      group_by(Country, Species) %>%
      summarize(
        `Average Outbreaks` = round(mean(Outbreaks, na.rm = TRUE),0),
        `Average Cases` = round(mean(Cases, na.rm = TRUE),0),
        .groups = 'drop'  # Ungroup after summarizing to avoid grouped output
      )
    
    # Select and arrange the columns to display in the DataTable
    outbreaks_summary %>%
      dplyr::select(Country, Species, `Average Outbreaks`, `Average Cases`)
  }, options = list(
    pageLength = 10,
    dom = 't',       # 't' stands for table only (removes "Show entries" and search box)
    ordering = FALSE,         # Disable ordering (sorting) of columns
    paging = FALSE,           # Disable pagination
    scrollX = TRUE
  ), editable = FALSE)
  # ----------------------------------
  
  
####################################################################
######################Results#######################################

  get_results <- function() {
    # Step 1: Retrieve the forecasted data
    forecast_data <- forecasted_data() %>% dplyr::select(2:5)  # Select columns 2, 3, 4, 5 (Country, Year, Specie, Forecasted Value)
    
    # Print the forecast data
    print("Step 1: Forecast data:")
    print(forecast_data)
    
    # Step 2: Initialize an empty results dataframe
    results <- data.frame(Country = character(),
                          Year = numeric(),
                          Specie = character(),
                          Forecasted_Value = numeric(),
                          Youngstock_Coverage = numeric(),
                          Adult_Coverage = numeric(),
                          VaccineRequirement = numeric(),
                          stringsAsFactors = FALSE)
    
    # Step 3: Loop through each row in forecast_data to calculate vaccine requirements
    for (i in seq_len(nrow(forecast_data))) {
      country <- forecast_data[i, "Country"]
      year <- forecast_data[i, "Year"]
      specie <- forecast_data[i, "Specie"]
      forecast_value <- round(as.numeric(forecast_data[i, "Forecasted Value"]), 2) # Forecasted population value
      # Print current row being processed
      print(paste("Processing row:", i))
      print(paste("Country:", country, "Year:", year, "Specie:", specie, "Forecasted Value:", forecast_value))
      
      # Step 4: Get young/adult stock proportions and vaccination schedule based on specie
      if (specie %in% c("Cattle", "Buffalo", "Camels")) {
        ys_prop <- as.numeric(youngstock_proportions$selections$lr)  # Large ruminants proportion
        adult_prop <- 100 - ys_prop  # Adult proportion
        ys_vac_schedule <- as.numeric(user_vaccine_schedule$selections$lr_ys)
        adult_vac_schedule <- as.numeric(user_vaccine_schedule$selections$lr_as)
      } else if (specie %in% c("Goats", "Sheep")) {
        ys_prop <- as.numeric(youngstock_proportions$selections$sr)  # Small ruminants proportion
        adult_prop <- 100 - ys_prop  # Adult proportion
        ys_vac_schedule <- as.numeric(user_vaccine_schedule$selections$sr_ys)
        adult_vac_schedule <- as.numeric(user_vaccine_schedule$selections$sr_as)
      } else if (specie == "Swine / pigs") {
        ys_prop <- as.numeric(youngstock_proportions$selections$p)   # Pigs proportion
        adult_prop <- 100 - ys_prop  # Adult proportion
        ys_vac_schedule <- as.numeric(user_vaccine_schedule$selections$p_ys)
        adult_vac_schedule <- as.numeric(user_vaccine_schedule$selections$p_as)
      } else {
        next  # Skip any unrecognized species
      }
      
      # Print the proportions and vaccine schedules
      print("Step 2: Proportions and Vaccine Schedules")
      print(paste("Youngstock Vaccine Schedule:", ys_vac_schedule, "Adult Vaccine Schedule:", adult_vac_schedule))
      
      # Step 5: Calculate youngstock and adult populations
      youngstock_value <- forecast_value * (ys_prop / 100)
      adultstock_value <- forecast_value * (adult_prop / 100)
      
      # Print youngstock and adultstock values
      print(paste("Youngstock Value:", youngstock_value, "Adultstock Value:", adultstock_value))
      
     
      
      # Step 7: Retrieve PCP stage and coverage percentage for the country
      pcp_filtered <- pcp_data()  # Call the reactive function to get PCP data
      pcp_filtered_country <- pcp_filtered[pcp_filtered$Country %in% input$Country, ]  # Filter for selected countries
      pcp_2025 <- pcp_filtered_country[pcp_filtered_country$Year == 2025, ]  # Filter for the year 2024
      
      # Print the filtered PCP data for debugging
      print("Step 3: PCP Data for 2025")
      
      
      # Pull the numeric PCP stage as numeric values
      pcp_stage <- as.numeric(pcp_2025$`PCP-FMD Stage Numeric`)  # Directly access the numeric stage
      
      # Print the PCP stage for debugging
      print(pcp_stage)
      
      # Retrieve coverage percentages based on specie
      coverage <- switch(specie,
                         "Cattle" = as.numeric(input$prophylactic_vc_lr) / 100,
                         "Buffalo" = as.numeric(input$prophylactic_vc_lr) / 100,
                         "Camels" = as.numeric(input$prophylactic_vc_lr) / 100,
                         "Other camelids" = as.numeric(input$prophylactic_vc_lr) / 100,
                         "Goats" = as.numeric(input$prophylactic_vc_sr) / 100,
                         "Sheep" = as.numeric(input$prophylactic_vc_sr) / 100,
                         "Swine / pigs" = as.numeric(input$prophylactic_vc_p) / 100,
                         NA)  # Default case
      print(paste("Coverage:",coverage))

      # Step 6: Calculate vaccine requirements for youngstock and adultstock
      youngstock_vaccine_requirement <- youngstock_value * ys_vac_schedule * coverage
      adultstock_vaccine_requirement <- adultstock_value * adult_vac_schedule * coverage
      
      # Print vaccine requirements
      print(paste("Youngstock Vaccine Requirement:", youngstock_vaccine_requirement, 
                  "Adultstock Vaccine Requirement:", adultstock_vaccine_requirement))      
      # Step 8: Calculate total vaccine requirements
      total_vaccine_requirement <- round(youngstock_vaccine_requirement + adultstock_vaccine_requirement, 0)
      # Print the total vaccine requirement
      print(paste("Total Vaccine Requirement:", total_vaccine_requirement))
      
      # Step 9: Append the calculated results to the results dataframe
      results <- rbind(results, data.frame(Country = country,
                                           Year = year,
                                           Specie = specie,
                                           Population_Value = format(round(forecast_value),
                                                                     big.mark = ",", scientific = FALSE, trim = TRUE),
                                           Vaccine_Requirement = format(round(total_vaccine_requirement), 
                                                                        big.mark = ",", scientific = FALSE, trim = TRUE),
                                           Youngstock_Coverage = format(round(youngstock_vaccine_requirement),
                                                                        big.mark = ",", scientific = FALSE, trim = TRUE),  # Actual youngstock to vaccinate
                                           Adult_Coverage = format(round(adultstock_vaccine_requirement),
                                                                   big.mark = ",", scientific = FALSE, trim = TRUE)))       # Actual adults to vaccinate
                                           
    }
    
    print("Final results:")
    print(results)
    
    return(results)
  }
  
  
  # Direct to the result tab when submit button is activated
  observeEvent(input$resultsbutton, {
    updateTabsetPanel(session = session, inputId = "maintabset", selected = "Results")
    shinyjs::runjs("window.scrollTo(0, 0)")
    # Trigger the help action
    current_help("www/help5.Rhtml")
    
  })
  

  
  ########################
  # Result table
  ########################
  
  shared_results <- reactiveVal(NULL)  # Store results data
  
  
  # Observer triggered when the "resultsbutton" is pressed
  observeEvent(input$resultsbutton, {
    
    # Call the get_results function
    results <- get_results()
    shared_results(results)
    
    
  
    
    # render the results in a table:
    output$resultstable <- renderDT({
      datatable(results, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',  # Add buttons to the top of the table
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
        pageLength = 10,
        dom = 't',       # 't' stands for table only (removes "Show entries" and search box)
        ordering = FALSE,  # Disable ordering (sorting) of columns
        paging = TRUE,    # Enable pagination
        scrollX = TRUE    # Horizontal scrolling
      ))
      
    
  })
    
  }) #observe event results button
  
  
  #########################
  #Map output##############
  #########################
  
  observeEvent(input$mapbutton, {
    # Show the spinner
    shinyjs::show("loading")
    # Call the get_results function
    results <- shared_results()
    req(results)
    con <- dbConnect(RMySQL::MySQL(),
                     dbname = Sys.getenv("DB_NAME1"), # or DB_NAME2 if you want the other DB
                     host = Sys.getenv("DB_HOST"),
                     port = as.numeric(Sys.getenv("DB_PORT")),
                     user = Sys.getenv("DB_USER"),
                     password = Sys.getenv("DB_PASSWORD"))
    # Ensure the connection is closed after the query
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Query density data
    density_query <- sprintf(
      "SELECT * FROM VADEMOS.density WHERE NAME_0 IN (%s) AND Specie IN (%s)",
      paste(shQuote(results$Country), collapse = ", "),
      paste(shQuote(unique(results$Specie)), collapse = ", ")
    )
    density_data <- dbGetQuery(con, density_query)
    print(density_data)
    
    # Merge results with density data
    merged <- merge(results, density_data, by.x = c("Country", "Specie"), by.y = c("NAME_0", "Specie"))
    merged <- merged %>% mutate(Vaccine_Requirement = as.numeric(gsub(",", "", Vaccine_Requirement))) %>%
                         mutate(Vaccine_Requirement = round((Density / 100) * Vaccine_Requirement, 0))
    
    selected_countries <- unique(merged$CNTY)
    
  
    
    ##########################code to fetch from API#########################################3
    CNTY_filter <-CNTY_filter <- paste(sprintf("'%s'", selected_countries), collapse = ", ")
    CNTY_filter <- gsub(" ", "", CNTY_filter)  # Remove any spaces

    # Debug: Print the filter to ensure it's correct
    print(paste("CNTY Filter:",CNTY_filter))

    # Manually construct the polygon URL with the filter

    polygon_url <- sprintf(
      "https://geoservices.un.org/arcgis/rest/services/ClearMap_WebTopo/MapServer/110/query?where=CNTY%%20IN%%20(%s)&outFields=CNTY,ADM1_Name&returnGeometry=true&f=geojson",CNTY_filter)
    #Debug: Print the constructed URL
    print(paste("Constructed Polygon URL:", polygon_url))

    #polygon_url <- "https://geoservices.un.org/arcgis/rest/services/ClearMap_WebTopo/MapServer/110/query?where     =CNTY='TCD'&outFields=CNTY,ADM1_Name&returnGeometry=true&f=geojson"

    # Fetch polygon data with error handling
    tryCatch({
       response <- GET(polygon_url)
       geojson_text <- content(response, as = "text", type = "application/geo+json")
       # Validate and convert to sf object
       geojson_data <- jsonlite::fromJSON(geojson_text, simplifyVector = FALSE)


    if (is.null(geojson_data$features) || length(geojson_data$features) == 0) {
      shinyjs::hide("loading")
      showNotification("No polygons available for the selected countries.", type = "warning")
      return()
    }

    

   
    # Convert to sf object and add polygons to the map
    sf_data <- geojsonsf::geojson_sf(geojson_text)
    
   
    
    # Filter the sf_data based on selected countries
    sf_data <- sf_data %>% filter(CNTY %in% selected_countries)
    
    
    # Merge sf_data (polygon data) with merged_data (density and vaccine requirement) on ADMIN1_Name
    merged_sf_data <- merge(sf_data, merged, by = "ADM1_Name", all.x = TRUE)
    
    # Print the merged data for debugging purposes
    
      print(merged_sf_data)
      
    
    
      
      
      # Render the leaflet map with density-based coloring
      output$worldmap <- renderLeaflet({
        
        # Create color palette based on Density values
        palette <- colorNumeric("Greens", domain = merged_sf_data$Density)
        
        # Render leaflet map
        leaflet() %>%
          #setView(lng = -0.027987, lat = 16.263981, zoom = 4) %>%  # Set initial view
          addEsriTiledMapLayer(url = "https://geoservices.un.org/arcgis/rest/services/ClearMap_WebTopo/MapServer") %>%
          
          # Add polygons and assign colors based on Density
          addPolygons(data = merged_sf_data,
                      color = "black",  # Polygon border color
                      weight = 1,
                      opacity = 1,
                      fillColor = ~palette(Density),  # Use palette function to color based on Density
                      fillOpacity = 0.7,
                      highlightOptions = highlightOptions(weight = 2, color = "white", fillOpacity = 0.7),
                      
                      # # Labels: Format each field on a separate line
                      # label = ~HTML(paste0(
                      #   "<strong>Area:</strong> ", ADM1_Name, "<br>",
                      #   "<strong>Density:</strong> ", Density, "%<br>",
                      #   "<strong>Vaccine Requirement:</strong> ", Vaccine_Requirement
                      # )),
                      # labelOptions = labelOptions(
                      #   style = list("font-weight" = "normal", padding = "3px 8px"),
                      #   textsize = "13px",
                      #   direction = "auto",
                      #   opacity = 0.9
                      # ),
                      layerId = ~ADM1_Name
                      
          ) %>%
          
          # Add a legend to represent density-based coloring
          addLegend(pal = palette, values = merged_sf_data$Density, opacity = 0.7, title = "Density", position = "bottomright")
      }) #map output
      
      # Hide the loading spinner after calculations are done
      shinyjs::hide("loading")
      
      # Reactive value to store the selected area
      selected_area <- reactiveVal(NULL)
      
      observeEvent(input$worldmap_shape_click, {
        click <- input$worldmap_shape_click
        req(click)
        
        # Get the clicked area ID
        area_id <- click$id
        
        # Update the reactive value with the new selection
        selected_area(area_id)
        
        # Filter data based on the selected area
        filtered_data <- merged %>% filter(ADM1_Name == selected_area())
        print(filtered_data)
        
        output$detailstable <- renderUI({
          # Create a custom HTML table
          tableHTML <- lapply(1:nrow(filtered_data), function(i) {
            tags$table(
              tags$tr(tags$th("Country"), tags$td(filtered_data$Country[i])),
              tags$tr(tags$th("Specie"), tags$td(filtered_data$Specie[i])),
              tags$tr(tags$th("ADM1_Name"), tags$td(filtered_data$ADM1_Name[i])),
              tags$tr(tags$th("Density"), tags$td(filtered_data$Density[i])),
              tags$tr(tags$th("head_km2"), tags$td(filtered_data$head_km2[i])),
              tags$tr(tags$th("Vaccine_Requirement_"), tags$td(filtered_data$Vaccine_Requirement[i])),
              tags$tr(tags$th("----"), tags$td("----"))  # Separator row
            )
          })
          do.call(tagList, tableHTML)
        }) #details table
        
        # output$detailstable <- renderDT({
        #   datatable(filtered_data, options = list(
        #     columnDefs = list(
        #       list(visible = FALSE, targets = c(0, 3, 4, 5, 6, 7, 8, 9))  # Hide the first 9 columns (0 to 8)
        #     ),
        #     dom = 't',  # 't' stands for table only (removes "Show entries", search box, and pagination)
        #     paging = FALSE,  # Disable pagination
        #     ordering = FALSE  # Disable column ordering
        #   ))
        # }) #details table
       
      }) #map click
      
      # Observe full table button and map display
      observeEvent(input$fulltablebutton, {
      
        # Render the updated table with the highlighting logic
        output$fulltable <- renderDT({
          datatable(
            merged,
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',  # Add buttons to the top of the table
              buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
              columnDefs = list(list(visible = FALSE, targets =c(0,3,4,6,7,8,9))  # Hide the first 9 columns 
                      ),
              dom = 't',       # Table only (no search, pagination, etc.)
              paging = FALSE,  # No pagination
              ordering = FALSE  # Disable sorting
              
            ) 
          ) 
        })
      })
    
    }, error = function(e) {
      shinyjs::hide("loading")
      showNotification("The UN Geoservice map is currently unavailable. Please try again in a few minutes.", type = "error")
      message("Error fetching polygon data: ", e$message)
      return(NULL)  # Ensure the rest of the code does not execute
    })
      
    })#observe event

      
  # Close the connection when the Shiny session ends
  
  # session$onSessionEnded(function() {
  #   dbDisconnect(con)
  # })
  
  
#########################################
##### Dowload Report  
  
 


    
    

  
  
 
  ########################
  # Send message to mail
  ########################
  observeEvent(input$sendbutton, {
      req(input$message)
      print('comming soon')
      
    })
  
  
  
  
  } # server

shinyApp(ui, server)
