library(shiny)
library(tidyverse)
library(shinydashboard)

options(shiny.maxRequestSize = 30*1024^2)

skin <- "blue"

header <- dashboardHeader(title = "Automated Analysis")
  

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard",
             tabName = "dashboard",
             icon = icon("tachometer-alt")),
    
    menuItem("Gene Data",
             tabName = "gene_info",
             icon = icon("dna")),
    
    menuItem("Expression Graphs",
             tabName = "expression_graphs",
             icon = icon("chart-bar")),
   
    menuItem("PubMed Webscrape Results",
             tabName = "webscrape",
             icon = icon("search")),
    
    menuItem("Data Table",
             tabName = "raw_data",
             icon = icon("table")),
    
    menuItem("Upload Data",
             tabName = "upload",
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
    
    # Tab where users will upload files to run the WebApp 
    tabItem(tabName = "upload",
      titlePanel(tags$b("Upload Experimental Data")),
      fluidRow(
        box("Type the name of the experiment you would like to represent each",
          "experiment in graphs and figures.",
          tags$br(),
          "Then upload the experimental data csv file.")),
      
      tags$hr(),
      
      # Multilevel Data Upload
      fluidRow(
        box(title = tags$b("Multilevel Experiment"),
            solidHeader = TRUE,
            status = "primary",
            textInput(inputId = "multilevel_name",
                      label = "Multilevel Experiment Name:"),
            fileInput(inputId = "multilevel_file",
                      label = "Choose Multilevel CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"))),
      
      tags$hr(),
      
      #Experiment 1 Data Upload
      fluidRow(
        box(title = tags$b("Experiment 1"),
            solidHeader = TRUE,
            status = "primary",
            textInput(inputId = "experiment1_name",
                      label = "Experiment 1 Name:"),
            fileInput(inputId = "experiment1_file",
                      label = "Choose Experiment 1 CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected"))),
      
      tags$hr(),  
      
      # Experiment 2 Data Upload
      fluidRow(
        box(title = tags$b("Experiment 2"),
            solidHeader = TRUE,
            status = "primary",
            textInput(inputId = "experiment2_name",
                      label = "Experiment 2 Name:"),
            fileInput(inputId = "experiment2_file",
                      label = "Choose Experiment 2 CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"),
                      buttonLabel = "Browse...",
                      placeholder = "No file selected")))
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

