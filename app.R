library(DT)
library(data.table)
library(shiny)
library(openxlsx)
library(ggplot2)

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    titlePanel("First Version"),
    sidebarLayout(position = "left", 
        sidebarPanel(
            fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"),
        ),
        mainPanel(
            verbatimTextOutput("textfield"),
            dataTableOutput("datatable")
        )
    )
)

server <- function(input, output, session) {
    inputdata <- reactive({
        req(infile <- input$upload)
        dat <- read.xlsx(infile$datapath, "grand_table", rowNames = T, colNames = T)
        meta_data <- read.xlsx(infile$datapath, "meta_data", colNames = T)
        vec <- rownames(dat) == meta_data$animal 
        #The following command should return true
        all(vec) == TRUE
        #2. The lables table contains the description of the behavioural variables (columns in the data_table). Check if the colnames are 
        #All present in the labels table
        labels <- read.xlsx(infile$datapath, "labels", colNames = T)
        vec <- colnames(dat) == labels$colnames
        #The following command should return true
        all(vec) == TRUE
        # returns the data if all tests are passed
        return(dat)
    })
    output$textfield <- renderText(colnames(inputdata()))
    
    output$datatable <- renderDataTable({
        inputdata() # rownames = T, colnames = T
    })
    # output$plot <- renderPlot({
    #    plot(dataset())
    # }, res = 96)
    
}

shinyApp(ui, server)