tool <- function() {
    pageWithSidebar(  
    # App title ----,
        headerPanel(
        
          
          div(h3(" ", align= 'center'))
             # h3('Vaccine Demand Estimation tool- FMD', align= 'center'), br()),
         
    ),#header panel
    
    # ----------------------------------
    # Sidebar panel for input parameters
    sidebarPanel( width= 3,
      tabsetPanel(
        tabPanel("Help",
                 style="text-align: justify;",
                 includeHTML("./help.Rhtml")
        ) #tabpanel
    )  #tabsetpanel
    ), #SidebarPanel
    
    # Main panel for displaying modeling outputs --
    mainPanel( width = 9, 
               h3('VADEMOS'),
               h3('Vaccine Demand Estimation tool- FMD', align= 'left'), br(),
      # Output1: Tabs to show estimated future inputs: populations, outbreaks, pcp projections
      tabsetPanel(
        id="maintabset",
        # 1 - SELECT INPUT PARAMETERS
        tabPanel("Parameters",
                 column(12,
                 br(),
                 # ----------------------------------
                 # 1 - SELECT COUNTRY AND YEAR TO PREDICT
                 column(12,
                 wellPanel(
                   fluidRow(
                     column(10, h4("STEP 1: Select area and year"), br()),
                     column(4, selectizeInput("Region", label = NULL, choices = unique(data$Region))), 
                     column(4, selectizeInput('Subregion', label= NULL, choices = sort(unique(data$Subregion)))),
                     column(4, selectInput("country_selected", label = NULL, choices = sort(unique           (data$Country_name)), selected=NULL)),
                     # 
                     # A conditional panel to choose subnational regions if Turkey has been selected
                     conditionalPanel(condition = "input.country_selected == 'Turkey'",
                                      column(4, selectInput("subnational_selected", label = NULL, choices = unique(data$SubnationalRegion), selected="Region"))
                     ), # conditionalPanel
                     
                     column(4, selectInput("year_selected", label = NULL, choices = unique(data[data$Year > 2021, "Year"]))),
                   
                     # Text appears below dropdown menus
                     conditionalPanel(condition = "input.country_selected == 'Turkey'",
                                      column(10,
                                           div(class="help_texts", h5("There are more than 1 FMD-PCP stages within this country. Please select a region to proceed.")))
                   ) # conditionalPanel
                   ) # fluid row
                 ) # well Panel
                 )  #column
                 ),  #column
                 column(12,
                 # ----------------------------------
                 # 2 - DATATABLE TO TOGGEL INPUTS
                 shinyjs::useShinyjs(), # shinyjs and id of this part will be linked to the 'reset button'
                 id = "inputs1",
                 
                 column(12,
                 wellPanel(
                     fluidRow(
                     column(10, h4("STEP 2: Define prophylactic and Emergency vaccination plan"), br()),
                     column(10, h4("How many times a year preventive vaccines are scheduled?")),
                     column(4, uiOutput("vschedule_lr_as"), align = "center"),
                     column(4, uiOutput("vschedule_sr_as"), align = "center"),
                     column(4, uiOutput("vschedule_p_as"), align = "center"),
                     column(10, br()),
                     column(4, uiOutput("vschedule_lr_ys", align = "center"), br()),
                     column(4, uiOutput("vschedule_sr_ys", align = "center"), br()),
                     column(4, uiOutput("vschedule_p_ys", align = "center"), br()),
                     column(10, br()),
                     
                     column(10, h4("Population proportion of youngstocks")),
                     column(4, uiOutput("ysproplr", align = "center"), br()),
                     column(4, uiOutput("yspropsr", align = "center"), br()),
                     column(4, uiOutput("yspropp", align = "center"), br()),
                     column(10, br()),
                     
                     column(10, h4("Sector proportions of Large ruminants")),
                     column(4, uiOutput("sectorpropdairy", align = "center"), br()),
                     column(4, uiOutput("sectorpropbeef", align = "center"), br()),
                     column(4, uiOutput("sectorpropsmall", align = "center"), br()),
                     # Text appears below dropdown menus
                     conditionalPanel(condition = "input.sectorpropdairy+input.sectorpropbeef+input.sectorpropsmall!=100",
                                      column(10,
                                             div(class="help_texts", h5("These proportions combined must be 100%")))
                     ) # conditionalPanel
                     )#fluid Row
                   ) #well panel
                  ),
        
                   column(12),
                   column(12,
                   wellPanel(
                     fluidRow(
                     column(10, h4("STEP 3: Project PCP-FMD, Population, Outbreaks and Emergency vaccination plan")),
                            br(),
                     column(12, align = "center", DTOutput("inputtable"), style = "font-size:110%"),
                     column(12, align = "right", helpText("* Double click to edit values"))
                     ) #fluidRow
                   ), #well panel
                     column(4, plotOutput("pcps")),
                     column(4, plotOutput("pops")),
                     column(4, plotOutput("outbreaks"))
                   ),
                   br(),
                   column(12),
                   column(12,
                          wellPanel(
                            fluidRow(
                              column(10, h4("STEP 4: Define PCP-FMD-dependent vaccine coverage"), br()),
                              column(6, uiOutput("pcp_selected"), br()),
                              column(10, h4("% of livestocks most likely covered by Prophylactic vaccination")),
                              column(4, uiOutput("prophylactic_vc_lr", align = "center"), br()),
                              column(4, uiOutput("prophylactic_vc_sr", align = "center"), br()),
                              column(4, uiOutput("prophylactic_vc_p", align = "center"), br() ),
                              column(10, h4("% FMD outbreaks covered by emergency vaccination")),
                              column(4, uiOutput("outbreak_vc_lr", align = "center"), br()),
                              column(4, uiOutput("outbreak_vc_sr", align = "center"), br()),
                              column(4, uiOutput("outbreak_vc_p", align = "center"), br())
                            ) #fluidRow
                          ) #well panel
                   )
                   ), # column binding input id 'inputs'
                 br(),
                
                 column(12, br()),
                 column(12, br()),
               
                 # ----------------------------------
                 # ACTION BUTTONS
                 fluidRow(
                   column(2),
                   # Submit button
                   column(5,
                          actionButton("updatebutton", "Get result", class = "btn btn-primary",
                                       icon = icon("arrow-alt-circle-right"),
                                       style = "background-color: #073f23; border-color:transparent")),
                   
                   column(5, actionButton("resetbutton", "Reset inputs", icon = icon("refresh"))),
                   column(2)
                 ) # fluidRow 
                ), # tabPanel parameters
        
        
        
        # Tab 2. RESULT SECTION
        tabPanel("Result",
                 fluidRow( div(id= "rresults",
                   div('Result Table for selected Year', style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                   
                   column(12, dataTableOutput("resulttable"), style = "font-size:102"), 
                   column(12, br()),
                   
                   #Aggregate results button
                   column(10),
                   column(2,actionButton("aggregatebutton", "Aggregate results", class = "btn btn-primary",
                                         icon = icon("arrow-alt-circle-right"),
                                         style = "background-color: #073f23; border-color:transparent")),
                   
                   column(6, plotOutput('trajresult')),
                   column(6, plotOutput('histresult')),
                   column(12, br()), # Rob (16/02/22)
                   
                   column(6, plotOutput('histresultLR')), # # Rob (16/02/22)
                   column(6, plotOutput('histresultSR')), # # Rob (16/02/22)
                   column(12, br()), # # Rob (16/02/22)
                   
                   column(6, plotOutput('histresultP')), # # Rob (16/02/22)
                   column(6, plotOutput('histresultEV')), # # Rob (16/02/22)
                   column(12, br()), # # Rob (16/02/22)
                   
                  
                   
                   div('Results by Large Ruminants, Small Ruminants and Pigs', style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                   column(12, br()),
                   
                   column(12, dataTableOutput("resulttable2"), style = "font-size:102%"), # # Rob (16/02/22)
                   column(12, br()), # # Rob (16/02/22)
                   column(12, br()),
                   
                   div('Youngstock and Adult  proportion', style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                   column(12, br()),
                   
                   # h4('Youngstock proportion Output' ), 
                   column(12,highchartOutput("pieCHART")), #Pili 6/22 Youngstock and Adult proportion
                  
                   div('Sector  proportion', style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                   column(12, br()),
                   
                   # h4('Sector proportion Output' ), 
                   column(12,highchartOutput("barCHART")), #Pili 6/22 Youngstock proportion
                   column(12, br()))
                   
                   ),
                   #div and fluid row closure
                  
                   # ACTION BUTTONS
                   fluidRow(
                    column(2),
                   
                   #download tables and graphs button
                   
                   screenshotButton(id="rresults", label= "Generate Report", filename="Report", scale = 2, 
                                             class="btn btn-primary", icon = icon("arrow-alt-circle-down"), 
                                             style = "background-color: #073f23; border-color:transparent" ),
                   
                   # column(5,screenshotButton(id="r2", label= "Generate Report Part 2", filename="Report2", scale = 2, 
                   #                           class="btn btn-primary", icon = icon("arrow-alt-circle-down"), 
                   #                           style = "background-color: #073f23; border-color:transparent" )),
                   # 
                     
                          
                                
                   column(2),
                   
                   
                   column(6, helpText("")),
                   column(12, br())
      
                     
                    )#action button fluid row
                  #fluidrow
              ), #tabpanel results
                 
     # Tab 3. MAP and AGGregated data SECTION
        tabPanel("Map",
                fluidRow(
                  div(id="rmap",
                  # div('MAP', style = "text-align: center; 
                  # background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                            
                  # map from mapbox using leaflet package 
                  column(6,selectizeInput("mapviz", label = 'Visualization', choices =                                c('PCP Stages', 'Serotype Pool', 'Outbreaks', 'Results'))), 
                  column(6,selectInput("mapyear", label = 'Year', choices = c(2010, 2011, 2012, 2013,2014,2015,2016,2017,2018,2019, 2020))),
                 column(12,leafletOutput("worldmap")), 
                 
                 div(' "The boundaries and names shown and the designations used on this map do not 
                 imply the expression of any opinion whatsoever on the part of FAO concerning the 
                 legal status of any country, territory, city or area or of its authorities, 
                 or concerning the delimitation of its frontiers and boundaries" ', style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: italic; color:black; font-size:80%"),
                  
                 
                  column(12, br()),
                  
                  div('Aggregated Results Table ', id="rmap", style = "text-align: center; 
                  background-color: #FFFFFF; font-weight: bold; color:black; font-size:150%"),
                  
                  div(id="rmap", column(12, dataTableOutput("agresulttable"), style = "font-size:102"), 
                  column(12, br())), 
                 
                
                 
                  
                  #download data button
                 column(5, screenshotButton(id="rmap", label= "Generate Report", filename="Aggregated Results Report", scale = 2, 
                                           class="btn btn-primary", icon = icon("arrow-alt-circle-down"), 
                                           style = "background-color: #073f23; border-color:transparent" )),
                 
                 column(5, actionButton("removeRow", "Remove Row", icon = icon("refresh"))),
                 
                 
                 htmlOutput("serotype"),
                 
                 
                  
                  column(6, helpText("")),
                  column(12, br())
                  
                   )#div close
                             
                                 
                              
                            )#fluidrow
                  )#tabpanel map
    
        
      ) # tabset panel
     )# mainpanel
   )# pageWithSidebar
}

