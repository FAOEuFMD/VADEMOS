#####################################################################################################
## UI and server for the app.R
## pilar.riusmunoz@fao.org August 2024~
#####################################################################################################


###################################################
# 0 - Load libraries
###################################################

library(bslib)
library(data.table) #reads csv and table functions
library(DBI)
library(dplyr)
library(DT)
library(future)
library(future.apply)
library(geodata)
library(geojsonio)
library(glue)
library(httr)
library(janitor)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(plotly)
library(raster)
library(readxl)  #reads excels
library(renv)
library(rhandsontable)
library(RMySQL) #Mysql connection
library(rsconnect)
library(sf)
library(shiny)
library(shinycssloaders)
library(shinylogs)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(sp)
library(tibble)
library(tidyverse)



###################################################
# 1 - Establish Connection 
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
# 2 - Source Data
###################################################


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
  
  library(DBI)
  library(RMySQL)  # Use RPostgres if AWS uses PostgreSQL
  
  # Set up async processing with future
  # Use only 1 worker on shinyapps.io free tier to avoid memory issues
  if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
    plan(sequential)  # No parallel processing on server (saves memory)
  } else {
    plan(multisession, workers = 2)  # 2 workers locally (reduced from 3)
  }
  
  # Create persistent cache directory for GADM data
  cache_dir <- file.path(getwd(), "gadm_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    message("Created GADM cache directory: ", cache_dir)
  }
  
  # Function to manage cache size (keep it under 50MB locally, 30MB on server)
  manage_cache_size <- function(max_size_mb = 50) {
    # Use smaller cache on shinyapps.io
    if (Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps") {
      max_size_mb <- 30
    }
    
    cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
    if (length(cache_files) == 0) return()
    
    # Get file info
    file_info <- file.info(cache_files)
    total_size_mb <- sum(file_info$size) / (1024^2)
    
    if (total_size_mb > max_size_mb) {
      message(paste("Cache size:", round(total_size_mb, 2), "MB - cleaning old files..."))
      
      # Sort by last access time (oldest first)
      file_info$path <- rownames(file_info)
      file_info <- file_info[order(file_info$atime), ]
      
      # Remove oldest files until under limit
      for (i in seq_len(nrow(file_info))) {
        if (total_size_mb <= max_size_mb) break
        
        file_size_mb <- file_info$size[i] / (1024^2)
        file.remove(file_info$path[i])
        total_size_mb <- total_size_mb - file_size_mb
        message(paste("  - Removed old cache file:", basename(file_info$path[i])))
      }
      
      # Force garbage collection to free memory
      gc()
      message(paste("Cache cleaned. New size:", round(total_size_mb, 2), "MB"))
    }
  }
  
  # Check and manage cache size on startup
  tryCatch({
    manage_cache_size(max_size_mb = 50)
  }, error = function(e) {
    message("Cache management error: ", e$message)
  })
  
  # NO memory cache - use disk only to save RAM on free tier
  
  # Function to load GADM data from disk cache or download if needed
  get_gadm_data <- function(country_code, silent = FALSE) {
    cache_file <- file.path(cache_dir, paste0(country_code, "_adm1.rds"))
    
    # Check if file exists in disk cache
    if (file.exists(cache_file)) {
      message(paste("  - Loading from disk cache:", country_code))
      if (!silent) {
        showNotification(
          paste("Loading", country_code, "from cache..."),
          type = "default",
          duration = 3,
          closeButton = FALSE,
          id = paste0("loading_", country_code)
        )
      }
      tryCatch({
        sf_data <- readRDS(cache_file)
        return(sf_data)
      }, error = function(e) {
        message(paste("  - Error reading cache file, re-downloading:", country_code))
        file.remove(cache_file)  # Remove corrupted file
      })
    }
    
    # If not in cache, download from GADM
    message(paste("  - Downloading from GADM:", country_code))
    if (!silent) {
      showNotification(
        paste("Downloading administrative boundaries for", country_code, "..."),
        type = "default",
        duration = NULL,  # Stay until dismissed
        closeButton = FALSE,
        id = paste0("download_", country_code)
      )
    }
    
    gadm_data <- geodata::gadm(country = country_code, level = 1, path = tempdir())
    sf_data <- sf::st_as_sf(gadm_data)
    
    # Save to disk cache for future use
    tryCatch({
      saveRDS(sf_data, cache_file)
      message(paste("  - Saved to disk cache:", country_code))
      if (!silent) {
        removeNotification(id = paste0("download_", country_code))
        showNotification(
          paste("Successfully loaded", country_code),
          type = "default",
          duration = 2,
          closeButton = FALSE
        )
      }
    }, error = function(e) {
      message(paste("  - Warning: Could not save to cache:", e$message))
      if (!silent) {
        removeNotification(id = paste0("download_", country_code))
      }
    })
    
    return(sf_data)
  }
  
  store_logs_in_db <- function(logs) {
    # Extract only session-related information
    session_logs <- logs$session
    
    # Convert session_logs to a data frame (if not already)
    session_logs <- as.data.frame(session_logs)
    
    # Keep only required columns (if they exist)
    required_cols <- c("app", "user", "server_connected", "sessionid", "server_disconnected")
    session_logs <- session_logs[, intersect(names(session_logs), required_cols), drop = FALSE]
    
    # Connect to AWS database
    con <- dbConnect(
      MySQL(),  
      host = Sys.getenv("DB_HOST"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD"),
      dbname = Sys.getenv("DB_NAME"),
      port = as.integer(Sys.getenv("DB_PORT"))
    )
    
    # Write logs to the AWS database
    dbWriteTable(con, "usage", session_logs, append = TRUE, row.names = FALSE)
    
    # Close the connection
    dbDisconnect(con)
  }
  
  # Track usage and store logs in AWS
  track_usage(
    storage_mode = store_custom(FUN = function(logs) {
      store_logs_in_db(logs)  # Pass only session logs to the function
    })
  )
  
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
  
  # Reactive value to store country data with ISO3CD
  filtered_countries_data <- reactiveVal(NULL)
  
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
    
    # Query to fetch distinct countries with ISO3CD based on selected subregions
    query_countries <- paste("SELECT DISTINCT name_un, ISO3CD FROM VADEMOS.countries WHERE subregion IN ('", 
                             paste(input$Subregion, collapse = "','"), "')", sep = "")
    
    # Fetch filtered countries from the database
    filtered_countries <- dbGetQuery(con, query_countries)
    
    # Store the data in reactive value
    filtered_countries_data(filtered_countries)
    
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
    
    
    
    datatable(data, escape = FALSE, extensions = 'Buttons', editable = list(target = 'cell', disable = list(columns = c(0,1,2,3,4,6,7,8,9,10))),
    options = list(
      dom = 'Bfrtip',  # Add buttons to the top of the table
      buttons = list(
        list(extend = 'csv', filename = "VADEMOS_Livestock_Forecast"),
        list(extend = 'excel', filename = "VADEMOS_Livestock_Forecast"),
        list(extend = 'pdf', filename = "VADEMOS_Livestock_Forecast"),
        list(extend = 'print', filename = "VADEMOS_Livestock_Forecast")
      ),
      pageLength = 10,
      dom = 't',       # 't' stands for table only (removes "Show entries" and search box)
      ordering = FALSE,  # Disable ordering (sorting) of columns
      paging = FALSE,    # Disable pagination
      scrollX = TRUE,
      # Hide the 'ID' column
      columnDefs = list(list(visible = FALSE, targets = c(1,10)))))
  
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
                split =  ~interaction(Area, Item),  # This will create separate lines for each country
                text = ~paste("Country:", Area, "<br>Species:", Item, "<br>Year:", Year, "<br>Value:", Value),
                hoverinfo = 'text', name = ~paste(Area, "-", Item, 'FAO Stats')) %>%
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
        choices = c("1", "2", "3", "4"),  # Now allowing up to 4 doses per year
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
    sliderInput("ysproplr","Large ruminants (< 12 months)",
                min=0, max=100,
                value=30,
                step=1,
                post="%")
  }) #renderUI
  
  output$yspropsr <- renderUI({
    sliderInput("yspropsr","Small ruminants (< 12 months)",
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
        columnDefs = list(list(visible = FALSE, targets = c(0,1,3,4,5,7,8,9)))  # Adjust column visibility
      ),
      editable = list(target = 'cell', disable = list(columns = 2))
    )
  })
  
  # Populate the dropdown with countries dynamically
  observe({
    req(pcp_data())  # Ensure data is available
    updateSelectInput(
      session,
      inputId = "selected_country",
      choices = unique(pcp_data()$Country),
      selected = unique(pcp_data()$Country)[1]  # Default to the first country
    )
  })
  
  # Render the bar chart for the selected country
  output$pcps <- renderPlotly({
    req(input$selected_country, pcp_data())  # Ensure a country is selected and data exists
    
    # Filter data for the selected country
    filtered_data <- pcp_data() %>%
      filter(Country == input$selected_country)
    
    # Validate the filtered data
    validate(
      need(nrow(filtered_data) > 0, "No data available for the selected country.")
    )
    
    # Create the ggplot bar chart
    p <- ggplot(filtered_data, aes(x = Year, y = PCP_Stage_Numeric, fill = factor(PCP_Stage_Numeric))) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = c(
        "0" = '#fd030e',
        "0.5" = '#fd030e',
        "1" = '#ef8125',
        "1.5" = '#ef8125',
        "2" = '#fccc19',
        "2.5" = '#fccc19',
        "3" = '#4f8e32',
        "3.5" = '#4f8e32',
        "4" = '#15592b'
      )) +
      scale_y_continuous(
        breaks = seq(0, 4, by = 1),  # Show ticks at every 0.5 step
        limits = c(0, 4)              # Set fixed limits from 0 to 4
      ) +
      labs(
        title = paste("PCP-FMD Progression -", input$selected_country),
        x = "Year",
        y = "PCP-FMD Stage",
        fill = "PCP Stage"
      ) +
      theme_minimal() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 1)
      )
    
    # Convert ggplot to a Plotly object for interactivity
    ggplotly(p)
  })
  
  
  
  
  ########################STEP3###########################
  # Emergency vaccination km radius and outbreak info 
  # Server: Access the selected radius value using input$radius
  observe({
    radius_selected <- input$radius
   
  })
  
  
  
  
  
  
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
                         "Goats" = as.numeric(input$prophylactic_vc_sr) / 100,
                         "Sheep" = as.numeric(input$prophylactic_vc_sr) / 100,
                         "Swine / pigs" = as.numeric(input$prophylactic_vc_p) / 100,
                         NA)  # Default case
      print(paste("Coverage:",coverage))
      
      # Retrieve coverage percentages based on specie
      emergency_coverage <- switch(specie,
                         "Cattle" = as.numeric(input$outbreak_vc_lr) / 100,
                         "Buffalo" = as.numeric(input$outbreak_vc_lr) / 100,
                         "Camels" = as.numeric(input$outbreak_vc_lr) / 100,
                         "Goats" = as.numeric(input$outbreak_vc_sr) / 100,
                         "Sheep" = as.numeric(input$outbreak_vc_sr) / 100,
                         "Swine / pigs" = as.numeric(input$outbreak_vc_p) / 100,
                         NA)  # Default case
      print(paste("Coverage:",coverage))
      print(paste("Emergency Coverage:",emergency_coverage))

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
                                           Population_Value= format(round(forecast_value),
                                                                     big.mark = ",", scientific = FALSE, trim = TRUE),
                                           Prophylactic_Vaccination = format(round(total_vaccine_requirement), 
                                                                        big.mark = ",", scientific = FALSE, trim = TRUE),
                                           Youngstock_Coverage = format(round(youngstock_vaccine_requirement),
                                                                        big.mark = ",", scientific = FALSE, trim = TRUE),  
                                           # Actual youngstock to vaccinate
                                           Adult_Coverage = format(round(adultstock_vaccine_requirement),
                                                                   big.mark = ",", scientific = FALSE, trim = TRUE),
                                           # Actual adults to vaccinate
                                          Youngstock_Proportion = ys_prop,
                                          Adult_Proportion = adult_prop,
                                          Youngstock_Schedule = ys_vac_schedule,
                                          Adult_Schedule = adult_vac_schedule,
                                          Prophylactic_Coverage= (coverage *100),
                                          Emergency_Coverage = (emergency_coverage *100)
                                        ))
                                           
    }
    # Rename all columns at once
    colnames(results) <- c(
  "Country",
  "Year", 
  "Specie",
  "Population (head)",
  "Prophylactic Vaccination (doses)",
  "Youngstock Vaccination (doses)",
  "Adult Vaccination (doses)", 
  "Youngstock Proportion (%)",
  "Adult Proportion (%)",
  "Youngstock Schedule",
  "Adult Schedule",
  "Prophylactic Coverage (%)",
  "Emergency Coverage (%)"
)
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
        buttons = list(
          list(extend = 'csv', filename = "VADEMOS_Results"),
          list(extend = 'excel', filename = "VADEMOS_Results"),
          list(extend = 'pdf', filename = "VADEMOS_Results"),
          list(extend = 'print', filename = "VADEMOS_Results")
        ),
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
    
      req(input$radius)  # Ensure the radius input is available
      radius_selected <- input$radius
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
      
      # Get ISO3 codes for selected countries from stored data
      country_data <- filtered_countries_data()
      selected_country_data <- country_data[country_data$name_un %in% results$Country, ]
      iso_codes <- selected_country_data$ISO3CD
      
      # Query density data using GID_0 (ISO3 codes)
      density_query <- sprintf(
        "SELECT * FROM VADEMOS.density_2025 WHERE GID_0 IN (%s)",
        paste(shQuote(iso_codes), collapse = ", ")
      )
      density_data <- dbGetQuery(con, density_query)
      print(density_data)
      
      # Merge results with density data using ISO3 mapping
      # Use the stored country data instead of querying again
      results_with_iso <- merge(results, selected_country_data, by.x = "Country", by.y = "name_un")
      merged <- merge(results_with_iso, density_data, by.x = "ISO3CD", by.y = "GID_0")
    
    # Create expanded data for each species with proper density mapping
    expanded_data <- data.frame()
    
    for (i in seq_len(nrow(merged))) {
      row_data <- merged[i, ]
      specie <- row_data$Specie
      
      # Map species to density column names
      density_col <- case_when(
        specie == "Cattle" ~ row_data$cattle_density,
        specie == "Buffalo" ~ row_data$buffalo_density,
        specie == "Goats" ~ row_data$goats_density,
        specie == "Sheep" ~ row_data$sheep_density,
        specie == "Swine / pigs" ~ row_data$pigs_density,
        TRUE ~ 0
      )
      
      head_km2_col <- case_when(
        specie == "Cattle" ~ row_data$cattle_km2,
        specie == "Buffalo" ~ row_data$buffalo_km2,
        specie == "Goats" ~ row_data$goats_km2,
        specie == "Sheep" ~ row_data$sheep_km2,
        specie == "Swine / pigs" ~ row_data$pigs_km2,
        TRUE ~ 0
      )
      
      # Add the mapped values to the row
      row_data$Density <- density_col
      row_data$head_km2 <- head_km2_col
      
      expanded_data <- rbind(expanded_data, row_data)
    }
    
    # Calculate vaccine requirements using the expanded data
    expanded_data <- expanded_data %>% 
              mutate(
              Prophylactic_Vaccination = as.numeric(gsub(",", "", `Prophylactic Vaccination (doses)`)),
              Prophylactic_Vaccination = round((Density / 100) * Prophylactic_Vaccination, 0),
              Area_km2 = pi * (radius_selected^2),  # Area covered in km²
              Emergency_Youngstock = round(Area_km2 * head_km2 * `Youngstock Proportion (%)` / 100 * `Youngstock Schedule` * `Emergency Coverage (%)` / 100, 0),
              Emergency_Adult = round(Area_km2 * head_km2 * `Adult Proportion (%)` / 100 * `Adult Schedule` * `Emergency Coverage (%)` / 100, 0),
              Emergency_Vaccination = Emergency_Youngstock + Emergency_Adult)

    # Collapse by GID_1, Country, Specie (using the administrative ID and species from results)
    merged_summary <- expanded_data %>%
      group_by(Country, Specie, GID_1, NAME_1, Density, head_km2) %>%
      summarise(
        Prophylactic_Vaccination = paste(paste(Year, ':', format(Prophylactic_Vaccination, big.mark = ",", scientific = FALSE, trim = TRUE)), collapse = '<br>'),
        Emergency_Vaccination = format(sum(Emergency_Vaccination, na.rm = TRUE), big.mark = ",", scientific = FALSE, trim = TRUE),
        .groups = 'drop'
      )
    selected_countries <- unique(expanded_data$ISO3CD)
    
    ##########################code to fetch from GADM with DISK CACHE ONLY (LOW MEMORY)#########################################
    # Fetch administrative boundaries from GADM - read directly from disk to save memory
    tryCatch({
      # Check which countries are missing from disk cache
      missing_countries <- selected_countries[!sapply(selected_countries, function(code) {
        file.exists(file.path(cache_dir, paste0(code, "_adm1.rds")))
      })]
      
      # Download missing countries if needed
      if (length(missing_countries) > 0) {
        message(paste("Downloading GADM data for", length(missing_countries), "countries:", paste(missing_countries, collapse = ", ")))
        
        # Show a single notification for all downloads
        showNotification(
          paste("Downloading administrative boundaries for", length(missing_countries), "countries..."),
          type = "default",
          duration = NULL,
          closeButton = FALSE,
          id = "bulk_download"
        )
        
        # Download missing countries (will be saved to disk)
        lapply(missing_countries, function(country) {
          get_gadm_data(country, silent = TRUE)
        })
        
        # Remove the bulk download notification
        removeNotification(id = "bulk_download")
        
        # Show success notification
        showNotification(
          paste("Successfully loaded", length(missing_countries), "countries"),
          type = "default",
          duration = 3,
          closeButton = FALSE
        )
        
        # Force garbage collection after downloads
        gc()
      } else {
        message("All countries already in disk cache.")
      }
      
      # Read shapefiles directly from disk (no memory cache)
      message("Reading shapefiles from disk cache...")
      sf_data_list <- lapply(selected_countries, function(country) {
        cache_file <- file.path(cache_dir, paste0(country, "_adm1.rds"))
        readRDS(cache_file)
      })
      
      # Combine into one sf object
      sf_data <- do.call(rbind, sf_data_list)
      
      # Free the list immediately
      rm(sf_data_list)
      gc()
      
      # Simplify polygon geometry to speed up rendering
      message("Simplifying polygon geometry for faster rendering...")
      sf_data <- sf_data %>%
        sf::st_simplify(preserveTopology = TRUE, dTolerance = 0.01)
      message("Geometry simplified successfully")
      # Keep original GADM column names (GID_0, NAME_1, GID_1)
      # Filter the sf_data based on selected countries (using ISO3 codes)
      sf_data <- sf_data %>% filter(GID_0 %in% selected_countries)

      if (nrow(sf_data) == 0) {
        showNotification("No polygons available for the selected countries.", type = "warning")
        return()
      }
      
      # Merge sf_data (polygon data) with expanded_data (density and vaccine requirement) on GID_1
      merged_sf_data <- merge(sf_data, expanded_data, by = "GID_1", all.x = TRUE)
      
      # Print the merged data for debugging purposes
      print(merged_sf_data)
      
      # Render the leaflet map with density-based coloring
      output$worldmap <- renderLeaflet({
        
        # Create color palette with more granular breaks for lower density ranges
        breaks <- c(0, 2, 5, 10, 15, 20, 30, 50, 75, 100)
        palette <- colorBin("Greens", domain = merged_sf_data$Density, bins = breaks)
        
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
                      layerId = ~GID_1,
                      # Performance optimization options
                      options = pathOptions(pane = "overlayPane"),
                      smoothFactor = 2  # Simplify polygons during rendering

          ) %>%
            
          
          
          # Add a legend to represent density-based coloring
          addLegend(pal = palette, values = merged_sf_data$Density, opacity = 0.7, title = "Density", position = "bottomright")
      }) #map output
      
      # Reactive value to store the selected area
      selected_area <- reactiveVal(NULL)
      
      observeEvent(input$worldmap_shape_click, {
        click <- input$worldmap_shape_click
        req(click)
        
        # Get the clicked area ID
        area_id <- click$id
        
        # Update the reactive value with the new selection
        selected_area(area_id)
        
        # Filter data based on the selected area using GID_1 directly
        filtered_data <- merged_summary %>% filter(GID_1 == selected_area())
        print(filtered_data)
        
        
        output$detailstable <- renderUI({
          # Create a custom HTML table
          tableHTML <- lapply(1:nrow(filtered_data), function(i) {
            tags$table(
              tags$tr(tags$th("Country"), tags$td(filtered_data$Country[i])),
              tags$tr(tags$th("Specie"), tags$td(filtered_data$Specie[i])),
              tags$tr(tags$th("Administrative Area"), tags$td(filtered_data$NAME_1[i])),
              tags$tr(tags$th("GID_1"), tags$td(filtered_data$GID_1[i])),
              tags$tr(tags$th("Density"), tags$td(filtered_data$Density[i])),
              tags$tr(tags$th("Head_km2"), tags$td(filtered_data$head_km2[i])),
              tags$tr(tags$th("Prophylactic Vaccination (doses)"), tags$td(HTML(filtered_data$Prophylactic_Vaccination[i]))),
              tags$tr(tags$th("Emergency Vaccination (doses)"), tags$td(HTML(filtered_data$Emergency_Vaccination[i]))),
              tags$tr(tags$td(colspan = 2, style = "color: #888; font-size: 12px; padding-top: 8px;",
                "Emergency vaccination is calculated using the latest available density data and does not account for annual population predictions."))
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
        # Reshape expanded_data to wide format for years, using consistent column names
        library(tidyr)
        library(dplyr)
        table_data <- expanded_data %>%
          dplyr::select(Country, Specie, GID_1, NAME_1, Year, Prophylactic_Vaccination, Emergency_Vaccination) %>%
          tidyr::pivot_wider(
            id_cols = c(Country, Specie, GID_1, NAME_1),
            names_from = Year,
            values_from = c(Prophylactic_Vaccination, Emergency_Vaccination),
            names_glue = "{.value} ({Year})"
          )
        # Rename columns for consistency
        colnames(table_data) <- gsub("Prophylactic_Vaccination", "Prophylactic Vaccination (doses)", colnames(table_data))
        colnames(table_data) <- gsub("Emergency_Vaccination", "Emergency Vaccination (doses)", colnames(table_data))
        # Render the updated table
        output$fulltable <- renderDT({
          datatable(
            table_data,  # Use the wide-format dataframe
            extensions = 'Buttons',
            options = list(
              dom = 'Bfrtip',  # Add buttons to the top of the table
              buttons = list(
                list(extend = 'csv', filename = "VADEMOS_FullTable"),
                list(extend = 'excel', filename = "VADEMOS_FullTable"),
                list(extend = 'pdf', filename = "VADEMOS_FullTable"),
                list(extend = 'print', filename = "VADEMOS_FullTable")
              ),#export options
              paging = FALSE,  # No pagination
              ordering = TRUE   # Enable sorting (can be disabled if not needed)
            )
          )
        })
      })
    
    }, error = function(e) {
      showNotification("Error fetching administrative boundaries from GADM. Please try again in a few minutes.", type = "error")
      message("Error fetching polygon data from GADM: ", e$message)
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
