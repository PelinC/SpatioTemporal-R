#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

install.packages('DT')
require(ggmap)
require(ggplot2)
require(scales)
require(shiny)
require(DT)
require(rgdal)

require(shapefiles)

require(foreign)
require(spacetime)
require(sp)
require(zoo)
require(xts)

shinyUI(pageWithSidebar(
  
  # Header:
  headerPanel("RAKUN SOFTWARE"),
  
  # Input in sidepanel:
  sidebarPanel(
    
    conditionalPanel( condition="input.conditionedPanels==1",
                      
                      helpText("Dataset Panel"),
    
    
    tags$style(type='text/css', ".well { max-width: 20em; }"),
    # Tags:
    tags$head(
      tags$style(type="text/css", "select[multiple] { width: 100%; height:10em}"),
      tags$style(type="text/css", "select { width: 100%}"),
      tags$style(type="text/css", "input { width: 19em; max-width:100%}")
    ),
    
    # Select filetype:
    selectInput("readFunction", "Function to read data:", c(
      # Base R:
      "read.table",
      "read.csv",
      "read.csv2",
      "read.delim",
      "read.delim2",
      
      # foreign functions:
      "read.spss",
      "read.arff",
      "read.dbf",
      "read.dta",
      "read.epiiinfo",
      "read.mtp",
      "read.octave",
      "read.ssd",
      "read.systat",
      "read.xport",
      
      # Advanced functions:
      "scan",
      "readLines"
    )),
    
    # Argument selecter:
    htmlOutput("ArgSelect"),
    
    # Argument field:
    htmlOutput("ArgText"),
    
    # Upload data:
    fileInput("file", "Upload data-file:"),
    fileInput('inputdata', 'Upload shapefile',accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj"), multiple=TRUE),

    
    uiOutput("choose_columns")
    ),
    
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText("Plot Variables"),
 
    textInput("filter", "X-axis:", value = "", width = NULL, placeholder = NULL),
    textInput("filter2", "Y-axis:", value = NULL, width = NULL, placeholder = NULL),
    helpText("Get Map"),
    actionButton("action", label = "Help"),
    uiOutput("HelpBox"),
    textInput("filter3", "LAT1:", value = "", width = NULL, placeholder = NULL),
    textInput("filter4", "LON1:", value = NULL, width = NULL, placeholder = NULL),
    textInput("filter5", "LAT2:", value = "", width = NULL, placeholder = NULL),
    textInput("filter6", "LON2:", value = NULL, width = NULL, placeholder = NULL),
    actionButton("action8", label = "Get Map by Location Name"),
    textInput("filter20", "Location Name:", value = NULL, width = NULL, placeholder = NULL)
    
    
 

    ),
    

  
  conditionalPanel(condition="input.conditionedPanels==3",
                   helpText("Plot Variables"),
                   actionButton("action6", label = "Filter Data"),
                   
                   textInput("filter7", "Search:", value = "", width = NULL, placeholder = NULL),
                   uiOutput("combobox15"),                   

                 
                   
                   uiOutput("combobox6"),
                   uiOutput("combobox7"),
                   uiOutput("combobox8"),
                   uiOutput("choose_columns4"),
                   

 
              
                   uiOutput("combobox2"),
                   uiOutput("combobox"),
                   uiOutput("slider")
                  

  ) ,     
  
  conditionalPanel(condition="input.conditionedPanels==4",
                   helpText("Plot Variables"),
                   
                   actionButton("action5", label = "Filter Data"),
                   
                   
                   textInput("filter11", "Search:", value = "", width = NULL, placeholder = NULL),
                 
                   uiOutput("combobox14"),
                   uiOutput("combobox12"),
                   
                   uiOutput("combobox13"),
                   
                   uiOutput("choose_columns3"),
                 
               
                   
                   
                   uiOutput("combobox3")
                
                 
                   
  ),
  
  conditionalPanel(condition="input.conditionedPanels==5",
                   helpText("Plot Variables"),
                   actionButton("action9", label = "Filter Data"),
                   
                   textInput("filter15", "Search:", value = "", width = NULL, placeholder = NULL),
                   textInput("filter16", "X-axis:", value = NULL, width = NULL, placeholder = NULL),
                   textInput("filter24", "Y-axis:", value = "", width = NULL, placeholder = NULL),
                   
                   
                   
                   
                   
                   uiOutput("combobox4")
                   
  ),
  
  conditionalPanel(condition="input.conditionedPanels==6",
                   helpText("Plot Variables"),
                   
                   actionButton("action7", label = "Filter Data"),
                   
                   
                   textInput("filter17", "Search:", value = "", width = NULL, placeholder = NULL),

                   uiOutput("combobox11"),
                   uiOutput("combobox9"),
                   
                   uiOutput("combobox10"),
                  
                    uiOutput("choose_columns2"),
                   
                   
                   uiOutput("combobox5"),
                   
                   uiOutput("slider2")
                  
                   
                   
  ),
  conditionalPanel(condition="input.conditionedPanels==7",
                   helpText("Plot Variables"),
                   
              
                   
                   
                   textInput("filter19", "Search:", value = "", width = NULL, placeholder = NULL),
                   
                   uiOutput("combobox16"),
                 
                   
           
                   uiOutput("choose_columns5")
                   
  )
  
  
  
  ),
  


  # Main:
  mainPanel(
    tabsetPanel(
    
      tabPanel("Dataset", DT:: dataTableOutput("data_table"),value = 1),
      tabPanel("Location Plot", plotOutput("data_point_plot"),value=2),
      tabPanel("Map Plot", plotOutput("map_by_year_selection"),value=3),
      tabPanel("St Plot", plotOutput("st_plot"),value=4),
      tabPanel("Plot", plotOutput("plot"),value=5),
      tabPanel("Moran Plot", plotOutput("moran_plot"),value=6),
      tabPanel("Correlation Plot", plotOutput("corr_plot"),value=7),
      
      id="conditionedPanels"

     

    
    )
  
   )
  
  
))