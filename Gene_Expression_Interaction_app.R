library(shiny)
library(tidyverse)
library(shinydashboard)

options(shiny.maxRequestSize = 30*1024^2)

header <- dashboardHeader(title = "Automated Analysis")
  

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
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
             icon = icon("code")),
    
    menuItemOutput("filters")
  )
)
  
  
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
      tags$hr(),
      
      #genes Identified from filter
      fluidRow(
        valueBoxOutput("total_genes"),
        valueBoxOutput("genes_experiment1"),
        valueBoxOutput("genes_experiment2")),
      
      tags$hr(),
      
      # String Network
      fluidRow(),
      
      tags$hr(),
      
      fluidRow()
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
                      placeholder = "No file selected"))),
      
      tags$hr(),
      
      # Action Button to Proceed to Dashboard
      fluidRow(
        infoBox(title = tags$b("Proceed to Dashboard"),
                color = "light-blue",
                icon = icon("tachometer-alt"),
                actionButton(inputId = "jumptodash",
                             label = "Go")))
    )
  )
)
  


# Define UI for application -- arguments are defined above
ui <- dashboardPage(skin = "blue",
                    header = header,
                    sidebar = sidebar,
                    body = body)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  scrape <- reactive({
    paste0("(((", input$scrape1, "+AND+", input$scrape2, ")+AND+",
              input$scrape3, ")+AND+", input$scrape4, ")")})
  
  # data to be used for EXPERIMENT 1 ANALYSIS:
  experiment1_data <- eventReactive(input$filter_button,{
    if (is.null(input$experiment1_file)) {
      return(NULL)
    } else {
      exp1 <- as_tibble(read_csv(input$experiment1_file$datapath), na.rm = TRUE)
      
      # Define filter parameters
      search <- paste(input$search1, input$search2,
                      input$search3, input$search4, sep = "|")
      
      # Filter based on Inputs
      experiment1_filt <- exp1 %>%
        filter(grepl(search, exp1[,ncol(exp1)]))
      experiment1_filt <- experiment1_filt %>%
        filter(experiment1_filt[,2] > input$fc_val | experiment1_filt[,2] < -(input$fc_val))
      experiment1_filtered <- experiment1_filt %>%
        filter(experiment1_filt[,3] > input$ppde_val)
      
      return(experiment1_filtered)
    }})
  
  # Data to be used for EXPERIMENT 2 ANALYSIS
  experiment2_data <- eventReactive(input$filter_button,{
    if (is.null(input$experiment2_file)) {
      return(NULL)
    } else {
      exp2 <- as_tibble(read_csv(input$experiment2_file$datapath), na.rm = TRUE)
      
      # Define filter parameters
      search <- paste(input$search1, input$search2,
                      input$search3, input$search4, sep = "|")
      
      # Filter based on Inputs
      experiment2_filt <- exp2 %>%
        filter(grepl(search, exp2[,ncol(exp2)]))
      experiment2_filt <- experiment2_filt %>%
        filter(experiment2_filt[,2] > input$fc_val | experiment2_filt[,2] < -(input$fc_val))
      experiment2_filtered <- experiment2_filt %>%
        filter(experiment2_filt[,3] > input$ppde_val)
      
      return(experiment2_filtered)
    }})
  
  # GENE counts for Dash:
  gene1_count <- reactive({nrow(experiment1_data())})
  gene2_count <- reactive({nrow(experiment2_data())})
  total_gene_count <- reactive({gene1_count() + gene2_count()})
  
  # GENE lists to filter Mulitlevel data
  gene1_list <- reactive({experiment1_data()[,1]})
  gene2_list <- reactive({experiment2_data()[,1]})
  
  #Filter Mulitlevel
  multilevel_data <- reactive({
    if (is.null(input$multilevel_file)) {
      return(NULL)
    } else {
      multi <- as_tibble(read_csv(input$multilevel_file$datapath), na.rm = TRUE)
      
      gene_list <- unique(rbind(gene1_list, gene2_list))
      multilvl <- multi %>%
        filter(X1 %in% gene_list$X1)
      
      return(multilvl)}
  })
        
  
  # When action button in upload tab selected, jump to dash
  observeEvent(input$jumptodash, {
    updateTabItems(session,
                   inputId = "tabs",
                   selected = "dashboard")})
  
  # Make Sliders for filtering data
  output$filters <- renderMenu({
    menuItem("FILTERS",
             tabName = "filters",
             icon = icon("filter"),
             sliderInput(inputId = "ppde_val",
                         label = "PPDE VALUE",
                         min = 0,
                         max = 1,
                         value = 0.75,
                         step = 0.01),
             sliderInput(inputId = "fc_val",
                         label = "FC ABSOLUTE VALUE",
                         min = 0,
                         max = 5,
                         value = 1.5,
                         step = 0.01),
             textInput(inputId = "search1",
                       label = "KEYWORD FILTER 1",
                       value = ""),
             textInput(inputId = "search2",
                       label = "KEYWORD FILTER 2",
                       value = ""),
             textInput(inputId = "search3",
                       label = "KEYWORD FILTER 3",
                       value = ""),
             textInput(inputId = "search4",
                       label = "KEYWORD FILTER 4",
                       value = ""),
             actionButton(inputId = "filter_button",
                          label = "FILTER"))})
  
  output$total_genes <- renderValueBox({
    # Total genes identified from filtered data of experiments 1 and 2 
    valueBox(value = total_gene_count(),
             subtitle = tags$b("Total Genes Identified"),
             icon = icon("dna"),
             color = "light-blue")})
  
  output$genes_experiment1 <- renderValueBox({
    # Genes identified from filtered data experiment 1
    valueBox(value = gene1_count(),
             subtitle = tags$b(paste0("Genes identified from experiment 1 (", input$experiment1_name, ")")),
             color = "light-blue")})
  
  output$genes_experiment2 <- renderValueBox({
    # Genes identified from filtered data experiment 2
    valueBox(value = gene2_count(),
             subtitle = tags$b(paste0("Genes identified from experiment 2 (", input$experiment2_name, ")")),
             color = "light-blue")})
}
                    
  
# Run the application 
shinyApp(ui = ui, server = server)
