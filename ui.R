

library(leaflet)
library(plotly)
library(shiny)
library(DT)
library(plyr)
library(dplyr)
library(shinydashboard)
library(shinyBS)
library(shinythemes)
library(shinyWidgets)
library(formattable)
library(shinyjs)
library(shinyalert)
library(googlesheets)
library(RCurl)
library(gsheet)

dashboardPage(
  skin="black",
  
  dashboardHeader(title = tags$a(href='http://www.iita.org', tags$img(src='img/th.jpg',height='40')),titleWidth = 230),		
  dashboardSidebar(		
    tags$img(src='img/ACAI_logo2.png',height='150',align='middle'),
    tags$br(),
    tags$br(),
    sidebarMenu( id='menuTabs',
                 menuItem("Register", tabName = "ES", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity FR", tabName = "fr", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity IC", tabName = "ic", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity PP", tabName = "pp", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity SPHS_TZ", tabName = "sphs_tz", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity SPHS_NG", tabName = "sphs_ng", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity CIS", tabName = "CIS", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Activity PPxWM", tabName = "pptz", icon = icon("stats", lib = "glyphicon") ),
                 menuItem("Data issues", tabName = "DI", icon = icon("stats", lib = "glyphicon") )
                 
    ),
    
    conditionalPanel(
      condition = "input.menuTabs == 'ES' ",
      uiOutput(outputId="username"),
      uiOutput(outputId="tokenKey"),
      uiOutput(outputId="country"),
      uiOutput(outputId="usecase"),
      uiOutput(outputId="region"),
      uiOutput(outputId="partner"),
      uiOutput(outputId="eaid"),
      uiOutput(outputId="hhidea")
      
    ),
    conditionalPanel(
      condition = "input.menuTabs == 'fr' ",
      uiOutput(outputId="countryfr"),
      uiOutput(outputId="usecasefr"),
      uiOutput(outputId="regionfr"),
      uiOutput(outputId="partnerfr"),
      uiOutput(outputId="eaidfr"),
      uiOutput(outputId="seasonfr")
    ),
    
    conditionalPanel(
      condition = "input.menuTabs == 'ic' ",
      uiOutput(outputId="countryic"),
      uiOutput(outputId="usecaseic"),
      uiOutput(outputId="regionic"),
      uiOutput(outputId="partneric"),
      uiOutput(outputId="eaidic"),
      uiOutput(outputId="seasonic")
      
    ),
    
    conditionalPanel(
      condition = "input.menuTabs == 'pp' ",
      uiOutput(outputId="countrypp"),
      uiOutput(outputId="usecasepp"),
      uiOutput(outputId="regionpp"),
      uiOutput(outputId="partnerpp"),
      uiOutput(outputId="eaidpp"),
      uiOutput(outputId="seasonpp")
      
    ),
    
    
    conditionalPanel(
      condition = "input.menuTabs == 'pptz' ",
      uiOutput(outputId="countrypptz"),
      uiOutput(outputId="usecasepptz"),
      uiOutput(outputId="regionpptz"),
      uiOutput(outputId="partnerpptz"),
      uiOutput(outputId="eaidpptz"),
      uiOutput(outputId="seasonpptz")
      
    ),
    
    conditionalPanel(
      condition = "input.menuTabs == 'sphs_tz' ",
      uiOutput(outputId="countrysphs_tz"),
      uiOutput(outputId="usecasesphs_tz"),
      uiOutput(outputId="regionsphs_tz"),
      uiOutput(outputId="partnersphs_tz"),
      uiOutput(outputId="eaidsphs_tz"),
      uiOutput(outputId="seasonsphs_tz")
      
    ),
    
    
    conditionalPanel(
      condition = "input.menuTabs == 'sphs_ng' ",
      uiOutput(outputId="countrysphs_ng"),
      uiOutput(outputId="usecasesphs_ng"),
      uiOutput(outputId="regionsphs_ng"),
      uiOutput(outputId="partnersphs_ng"),
      uiOutput(outputId="eaidsphs_ng"),
      uiOutput(outputId="seasonsphs_ng")
      
    ),
    
    
    conditionalPanel(
      condition = "input.menuTabs == 'CIS' ",
      uiOutput(outputId="countryCIS"),
      uiOutput(outputId="usecaseCIS"),
      uiOutput(outputId="regionCIS"),
      uiOutput(outputId="partnerCIS"),
      uiOutput(outputId="eaidCIS"),
      uiOutput(outputId="seasonCIS")
      
    ),
    
    conditionalPanel(
      condition = "input.menuTabs == 'DI' ",
      uiOutput(outputId="countrydi"),
      uiOutput(outputId="partnerdi"),
      uiOutput(outputId="dataissue"),
      uiOutput(outputId="eaiddi"),
      uiOutput(outputId="hhiddi")
      
    )
    
    
  ),
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
    
    tags$head(tags$style(".table .alignRight {color: blue; text-align:center;}")),			
    
    tabItems( 
      
      tabItem(tabName = "ES",
              downloadButton('downloadr','Download Register data'),
              br(),
              br(),
              dataTableOutput("EASummary"),
              br(),
              fluidRow(
                column(width = 6, offset=2, leafletOutput("mapEA"))
              )
      ),
      
      tabItem(tabName = "fr",
              tabsetPanel(
                tabPanel("FR validation",
                         fluidPage(
                           fluidRow(
                             plotOutput("EAplot", height = 500),
                             br(),
                             br(),
                             plotOutput("FRwkheatplot", height = 500)))),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotFR','Download Plot'),
                         br(),
                         uiOutput("tpointout"),
                         
                         DT::dataTableOutput("FRtable"),
                         DT::dataTableOutput("FRptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomFR','Download Recommendations'),
                         DT::dataTableOutput("FRecom"))
              )
      ), 
      
      
      
      tabItem(tabName = "ic",
              tabsetPanel(
                tabPanel("IC validation",
                         plotOutput("EAICplot", height = 500),
                         br(),
                         br(),
                         plotOutput("ICwkheatplot", height = 500)),
                tabPanel("EA Points",
                         downloadButton('downloadPlotIC','Download Plot'),
                         br(),
                         uiOutput("ICpointout"),
                         br(),
                         br(),
                         DT::dataTableOutput("ICtable"),
                         DT::dataTableOutput("ICptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomIC','Download Recommendations'),
                         DT::dataTableOutput("ICrecom"))
                
              )
              
      ),
      
      tabItem(tabName = "pp",
              tabsetPanel(
                tabPanel("PP validation",
                         plotOutput("EAPPplot", height = 500),
                         br(),
                         br(),
                         plotOutput("PPwkheatplot")),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotPP','Download Plot'),
                         uiOutput("PPpointout"),
                         
                         DT::dataTableOutput("PPtable"),
                         DT::dataTableOutput("PPptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomPP','Download Recommendations'),
                         DT::dataTableOutput("PPrecom"))
              )   
              
      ),
      
      
      tabItem(tabName = "pptz",
              tabsetPanel(
                tabPanel("PPxWM validation",
                         plotOutput("EAPPTZplot", height = 500),
                         br(),
                         br(),
                         plotOutput("PPTZwkheatplot")),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotPPTZ','Download Plot'),
                         uiOutput("PPTZpointout"),
                         
                         DT::dataTableOutput("PPTZtable"),
                         DT::dataTableOutput("PPTZptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomPPTZ','Download Recommendations'),
                         DT::dataTableOutput("PPTZrecom"))
              )   
              
      ),
      
      tabItem(tabName = "sphs_tz",
              tabsetPanel(
                tabPanel("SP_TZ validation",
                         plotOutput("EASPHSplot", height = 500),
                         br(),
                         br(),
                         plotOutput("SPHSwkheatplot")),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotSPHS','Download Plot'),
                         uiOutput("SPHSpointout"),
                         
                         DT::dataTableOutput("SPHStable"),
                         DT::dataTableOutput("SPHSptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomSPHS','Download Recommendations'),
                         DT::dataTableOutput("SPHSrecom"))
              )   
              
      ),
      
      tabItem(tabName = "sphs_ng",
              tabsetPanel(
                tabPanel("SP_NG validation",
                         plotOutput("EASPNGplot", height = 500),
                         br(),
                         br(),
                         plotOutput("SPNGwkheatplot")),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotSPNG','Download Plot'),
                         uiOutput("SPNGpointout"),
                         
                         DT::dataTableOutput("SPNGtable"),
                         DT::dataTableOutput("SPNGptsummary"))
                
              )   
              
      ),
      
      tabItem(tabName = "CIS",
              tabsetPanel(
                tabPanel("CIS validation",
                         plotOutput("EACISplot", height = 500),
                         br(),
                         br(),
                         plotOutput("CISwkheatplot")),
                
                tabPanel("EA Points",
                         downloadButton('downloadPlotCIS','Download Plot'),
                         uiOutput("CISpointout"),
                         
                         DT::dataTableOutput("CIStable"),
                         DT::dataTableOutput("CISptsummary")),
                tabPanel("Recommendation",
                         downloadButton('downloadrecomCIS','Download Recommendations'),
                         DT::dataTableOutput("CISrecom"))
              )   
              
      ),
      
      tabItem(tabName = "DI",
              tabsetPanel(
                tabPanel("FR data",
                         DT::dataTableOutput("frditable"),
                         actionBttn(inputId = "discard1", label = "Discard", color= "danger", icon=icon("trash")),
                         actionBttn(inputId = "Restore1", label = "Keep", color= "royal", icon=icon("glyphicon glyphicon-asterisk"))),
                tabPanel("PP data ",
                         DT::dataTableOutput("ppditable"),
                         actionBttn(inputId = "discard2", label = "Discard", color= "danger", icon=icon("trash")),
                         actionBttn(inputId = "Restore2", label = "Keep", color= "royal", icon=icon("glyphicon glyphicon-asterisk"))),
                tabPanel("IC data",
                         DT::dataTableOutput("icditable"),
                         actionBttn(inputId = "discard3", label = "Discard", color= "danger", icon=icon("trash")),
                         actionBttn(inputId = "Restore3", label = "Keep", color= "royal", icon=icon("glyphicon glyphicon-asterisk")))
                
              )  
      )
    )
  )
)

