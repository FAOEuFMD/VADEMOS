tool <- function() {
    pageWithSidebar(  
    # App title ----,
        headerPanel(
         
          tags$title("")  # Explicitly set the page title
         
    ),#header panel
    
    # ----------------------------------
    # Sidebar panel for input parameters
    sidebarPanel( width= 3,
                  class = "sticky-sidebar",
      tabsetPanel(
        tabPanel("Help",
                 style="text-align: justify;",
                 uiOutput("help_content")
        ) #tabpanel
    )  #tabsetpanel
    ), #SidebarPanel
      # Main panel for displaying modeling outputs --
    mainPanel( width = 9, 
               h3('VADEMOS','Vaccine Demand Estimation tool- FMD', align= 'left'),
               p("At the bottom of each section there is a help button. Click on this to get additional support for the step of the tool you are using. This information will be displayed to the left of the screen."),
      # Output1: Tabs to show estimated future inputs: populations, outbreaks, pcp projections
      tabsetPanel(
        id="maintabset",
        # 1 - SELECT INPUT PARAMETERS
        tabPanel("Parameters",
                 # column(12,
                 # br(),
                 # ----------------------------------
                 # 1 - SELECT YEAR, SPECIES AND GEOGRAPHICAL AREA
                 
                 column(12,
                 wellPanel(
                   fluidRow(
                     column(10, h4("STEP 1: Select year, species of interest and country data to obtain a prediction of livestock"), br()),
                     
                     
                     column(5,
                            pickerInput(
                              inputId = "year_selected",
                              label = "Select Year",
                              choices = c(2025,2026,2027,2028,2029,2030,2031,2032,2033),
                              selected = 2025,
                              options = list(`actions-box` = TRUE,`style`="btn-custom"),
                              multiple = TRUE)
                            ),
                            
                     
                     column(5,
                            pickerInput(
                              inputId = "Species",
                              label = "Select Species",
                              choices = c('Cattle', 'Buffalo', 'Camels', 
                                          'Goats', 'Sheep', 'Swine / pigs'), #unique(forecast$Specie),
                              options = list(`actions-box` = TRUE,`style`="btn-custom"),
                              multiple = TRUE
                            )
                     ),
                     
                     
                     column(4,
                            pickerInput(
                              inputId = "Region",
                              label = "Select Continent",
                              choices = c('Africa','Americas', 'Asia', 'Europe', 'Oceania'),
                              #unique(data1$CONTINENT),
                              options = list(`actions-box` = TRUE,`style`="btn-custom"),
                              multiple = TRUE
                            )
                     ),
                     column(4,
                            pickerInput(
                              inputId = "Subregion",
                              label = "Select SubRegion",
                              choices = NULL,
                              options = list(`actions-box` = TRUE,`style`="btn-custom"),
                              multiple = TRUE
                            )
                     ),
                     
                     column(4,
                            pickerInput(
                              inputId = "Country",
                              label = "Select Country",
                              choices = NULL,
                              options = list(`actions-box` = TRUE, `style`="btn-custom"),
                              multiple = TRUE
                            )
                     ),
                     
                     column(10,actionButton("help1", "Help", class='btn-custom2'))
                     
                   ) # fluid row
                 ) # well Panel
                 ),  #column
                 
                 ############POP PREDICTION TABLE
                 column(12,
                        wellPanel(
                          fluidRow(
                            column(10, h4("Population prediction for the selected year, species and country")),
                            column(12, DTOutput("forecasttable"), style = "font-size:100%"), 
                            HTML("<br><br>"), 
                            column(12, align= 'right',actionButton("edit_values", "Edit Values",
                                                                   class='btn-custom2'), 
                                   actionButton("save_pops", "Save Values",
                                                                    class='btn-custom2')),
                            column(12, align = "right", 
                                   textOutput("dynamic_help_text")),
                            
                            HTML("<br><br>"), 
                            column(12, plotlyOutput("pops"))#plotOutput("pops")),
                            
                          ) #fluidRow
                        ) #well panel
                 ),#column
        
                 
                 # ----------------------------------
                 # 2 - DATATABLE TO TOGGEL INPUTS
                 shinyjs::useShinyjs(), # shinyjs and id of this part will be linked to the 'reset button'
                 id = "inputs1",
                 
                 column(12,
                 wellPanel(
                     fluidRow(
                     column(10, h4("STEP 2: Define vaccination schedule"), br()),
                     column(10, h4("Annual vaccination schedules")),
                     column(4, uiOutput("vschedule_lr_as"), align = "center"),
                     column(4, uiOutput("vschedule_sr_as"), align = "center"),
                     column(4, uiOutput("vschedule_p_as"), align = "center"),
                     column(10, br()),
                     column(4, uiOutput("vschedule_lr_ys", align = "center"), br()),
                     column(4, uiOutput("vschedule_sr_ys", align = "center"), br()),
                     column(4, uiOutput("vschedule_p_ys", align = "center"), br()),
                     column(10, br()),
                     
                     column(10, h4("Population proportion of youngstock")),
                     column(4, uiOutput("ysproplr", align = "center"), br()),
                     column(4, uiOutput("yspropsr", align = "center"), br()),
                     column(4, uiOutput("yspropp", align = "center"), br()),
                     column(10,actionButton("help2", "Help", class='btn-custom2'))
                     
                     
                     )#fluid Row
                   ) #well panel
                  ), #column
        
                   column(12,
                   wellPanel(
                     fluidRow(
                     column(10, h4("STEP 3: Define vaccine coverage according to PCP-FMD Stage")),
                            br(),
                     column(10, uiOutput("pcp_selected"), br()),
                     column(10, h4("Percentage of livestock covered by prophylactic vaccination")),
                     column(4, uiOutput("prophylactic_vc_lr", align = "center"), br()),
                     column(4, uiOutput("prophylactic_vc_sr", align = "center"), br()),
                     column(4, uiOutput("prophylactic_vc_p", align = "center"), br() ),
                     column(10, h4("Percentage of livestock covered by emergency vaccination")),
                     column(4, uiOutput("outbreak_vc_lr", align = "center"), br()),
                     column(4, uiOutput("outbreak_vc_sr", align = "center"), br()),
                     column(4, uiOutput("outbreak_vc_p", align = "center"), br()),
                     column(10,
                            actionButton("help3", "Help", class='btn-custom2'),
                            #actionButton("save_cv", "Use Modified Coverage", class='btn-custom2'), br(),
                            
                            # Text appears below different PCPs
                            conditionalPanel(
                              condition = "input.Country.includes('Türkiye') || input.Country.includes('Namibia')",
                              div(class = "help_texts",
                                  h5("There are more than 1 FMD-PCP stages within the selected countries.")
                              )
                            ),
                            conditionalPanel(
                              condition = "input.Region.includes('Europe') || input.Region.includes('Americas') || input.Region.includes('Oceania')",
                              div(class = "help_texts",
                                  h5("One or more regions in your selection do not have an FMD-PCP Stage. Please select coverage.")
                              )
                            )
                     ),
                     column(12, br()),
                     fluidRow(
                       column(
                         1,  # Add an empty column to create space on the left
                         ""
                       ),
                       column(
                         4,  # Adjust width for the title
                         h4("Select a country to see FMD-PCP progression", style = "display: inline;")
                       ),
                       column(
                         3,  # Adjust width for the dropdown
                         selectInput(
                           inputId = "selected_country",
                           label = NULL,  # Remove the label for compactness
                           choices = NULL,  # We'll populate this dynamically
                           selected = NULL,
                           width = "100%"  # Ensure full width within the column
                         )
                       ),
                       column(
                         4,  # Adjust width for the "Current PCP" text
                         h4("Current PCP", style = "display: inline;")
                       )
                     ),
                     
                     column(8, plotlyOutput("pcps")), 
                     column(4, DTOutput("pcp_table")),
                    
                     ) #fluidRow
                   )), #well panel
                    
                   br(),
                   
                   column(12,
                          wellPanel(
                            fluidRow(
                              column(10, h4("STEP 4: Emergency vaccination"), br()),
                              column(8, sliderInput("radius", "Select radious for emergency vaccination (km):", 
                                                    min = 0, max = 100, value = 10, step = 1, post = " km")),                                            
                              column(10, actionButton("help4", "Help", class='btn-custom2'))
                           
                              
                            ) #fluidRow
                          ) #well panel
                   ),#column
                 
                  
                 br(),
                
                 column(12, br()),
                 column(12, br()),
               
                 # ----------------------------------
                 # ACTION BUTTONS
                 fluidRow(
                   column(2),
                   # Submit button
                   column(5,
                          actionButton("resultsbutton", "VADEMOS!", class = "btn btn-primary",
                                       icon = icon("arrow-alt-circle-right"),
                                       style = "background-color: #073f23; border-color:transparent")),
                   
                   
                 ) # fluidRow 
                ), # tabPanel parameters
        
        
        
        # Tab 2. RESULT SECTION
        tabPanel("Results",
                 fluidRow( div(id= "results",
                     column(12, br()),
                     div('Result Table for Prophylactic vaccine dose estimation', 
                     style = "text-align: center; background-color: #FFFFFF; font-weight: bold; 
                     color:black; font-size:150%"),
                     column(12, dataTableOutput("resultstable")%>% withSpinner(type = 5), style = "font-size:102"),
                     # Button and conditional text layout
                     fluidRow(
                       column(12, br()),  # Space before the button
                       column(12, div(style = "text-align: right;",  # Align button to the right
                                      actionButton("mapbutton", "See/Update Map", class = "btn-custom2")
                       )),
                       column(12, br()),  # Space between the button and the text
                       column(12, conditionalPanel(
                         condition = "input.mapbutton > 0",  # Show text only after button is clicked
                         div('Select area to view density,prophylactic and emergency vaccination', 
                         style = "text-align: left; 
         background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%")
                       ))
                     ),
                     
                     
                       column(12, div(id = "loading",  # Spinner div
                                      style = "display: none; text-align: center;",
                                      icon("spinner", class = "fa-spin fa-3x"))  # Loading spinner
                       ),
                       
                 
                     column(7, 
                            leafletOutput("worldmap"),
                            div(' "The boundaries and names shown and the designations used on this map do not 
          imply the expression of any opinion whatsoever on the part of FAO concerning the 
          legal status of any country, territory, city or area or of its authorities, 
          or concerning the delimitation of its frontiers and boundaries" ', 
                                style = "text-align: center; background-color: #FFFFFF; font-weight: italic;                                              color:black; font-size:80%")
                     ),
                     column(5, uiOutput("detailstable")),
                     #column(4, dataTableOutput("detailstable")),
                     column(12, br()),  # Space before the button
                     column(12, div(style = "text-align: right;",  # Align button to the right
                                    actionButton("fulltablebutton", "See/Update full table", class = "btn-custom2")
                     )),
                     column(10, dataTableOutput("fulltable")),
                     column(12, br()),
                  
                   column(10),
                   #download tables and graphs button
                   # div(
                   #   id = "reportContent",
                   #   h2("VADEMOS Report"),
                   #   DTOutput("forecasttable"),
                   #   plotlyOutput("pops"),
                   #   DTOutput("pcp_table"),
                   #   DTOutput("outbreaktable"),
                   #   dataTableOutput("resultstable"),
                   #   leafletOutput("worldmap"),
                   #   dataTableOutput("fulltable")
                   # 
                   # ),
                    
                   #Generate report button  
                   #column(2, screenshotButton(id="forecasttable", label= "Generate Report", filename="Report",
                                              # selector = "#worldmap, #resultstable",
                                              # scale = 2, class="btn btn-primary", 
                                              # icon = icon("arrow-alt-circle-down"), 
                                              # style = "background-color: #073f23; border-color:transparent" )),
                   
                
                  
                  column(6, helpText("")),
                  column(12, br())
                  
                   )#div close
                             
                                 
                              
                            )#fluidrow
                  )#tabpanel Results
    
        
      ) # tabset panel
     )# mainpanel
   )# pageWithSidebar
}

#unused code for now. 

# column(10, plotlyOutput("outbreaks"))

# 
# column(12, selectizeInput("area", label = 'Select Area to view ', 
#                           choices = c(2010, 2011, 2012, 2013,2014,2015,2016,2017,2018,2019),                                               selected= 2019)),


# column(5, actionButton("resetbutton", "Reset inputs", icon = icon("refresh"))),
# column(2)
