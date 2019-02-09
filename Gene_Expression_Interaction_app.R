library(shiny)
library(tidyverse)
library(shinydashboard)

skin <- "blue"

header <- dashboardHeader(
  
)
  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",
             tabname = "dashboard",
             icon = icon("tachometer-alt")),
    
    menuItem("Gene Data",
             tabname = "gene_info",
             icon = icon("dna")),
    
    menuItem("Expression Graphs",
             tabname = "expression_graphs",
             icon = icon("chart-bar")),
   
    menuItem("PubMed Webscrape Results",
             tabname = "webscrape",
             icon = icon("search")),
    
    menuItem("Data Table",
             tabname = "raw_data",
             icon = icon("table")),
    
    menuItem("Upload Data",
             tabname = "upload",
             icon = icon("sliders-h"),
             selected = TRUE),
    
    menuItem("Github Link",
             href = "https://github.com/kingstonPLaw/GeneExpression-Interactions",
             newtab = TRUE,
             icon = icon("code"))
  )
)
  
  
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard"
      
    ),
    
    tabItem(tabName = "gene_info"
            
    ),
    
    tabItem(tabName = "expression_graphs"
            
    ),
    
    tabItem(tabName = "webscrape"
            
    ),
    
    tabItem(tabName = "raw_data"
            
    ),
    
    tabItem(tabName = "upload"
            
    )
  )
)
  


# Define UI for application -- arguments are defined above
ui <- dashboardPage(skin = skin,
                    header = header,
                    sidebar = sidebar,
                    body = body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}
                    
  
# Run the application 
shinyApp(ui = ui, server = server)

