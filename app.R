library(corrplot)
library(DT)
library(data.table)
library(Hmisc)
library(ggplot2)
library(openxlsx)
library(shiny)

if(interactive()){

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    # titlePanel("First Version"),
    tabsetPanel(id = "wizard", type = "hidden",
        tabPanel("page_1", type = "hidden",
            fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"),
            actionButton("page_12", "next")
        ), 
        tabPanel("page_2", 
            verbatimTextOutput("textfield"),
            # checkboxGroupInput("corr_variables", "Which variables do you want to plot?", "all"),
            uiOutput("selectlabels"),
            actionButton("plot_selected", "plot!"),
            plotOutput("plot", height=800),        
            actionButton("page_21", "prev"),
            actionButton("page_23", "next")
        ),
        tabPanel("page_3", 
           
            dataTableOutput("datatable"),  
            actionButton("page_32", "prev")
        )
    )
    # sidebarLayout(position = "left", 
        #sidebarPanel(
    
    #       ),
    #    mainPanel(

 
#        )
#    )
)

server <- function(input, output, session) {
    labels <- reactive({
        req(infile <- input$upload)
        read.xlsx(infile$datapath, "labels", colNames = T)
    })
    inputdata <- reactive({
        req(infile <- input$upload)
        dat <- read.xlsx(infile$datapath, "grand_table", rowNames = T, colNames = T)
        meta_data <- read.xlsx(infile$datapath, "meta_data", colNames = T)
        vec <- rownames(dat) == meta_data$animal 
        #The following command should return true
        all(vec) == TRUE
        #2. The lables table contains the description of the behavioural variables (columns in the data_table). Check if the colnames are 
        #All present in the labels table
        vec <- colnames(dat) == labels()$colnames
        #The following command should return true
        all(vec) == TRUE
        # returns the data if all tests are passed
        return(dat)
    })
    switch_page <- function(i){
        updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
    }
    
    observeEvent(input$page_12, switch_page(2))
    
    output$selectlabels <- renderUI({
        checkboxGroupInput("selection", "Which data do you want to plot?", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
        # output$textfield <- renderText(input$selection)    
        
    })
    output$datatable <- renderDataTable({
        select <- req(input$selection)
        inputdata()[, c(paste0(select))] # rownames = T, colnames = T
    })
    observeEvent(input$plot_selected, 
    output$plot <- renderPlot({
        select <- req(input$selection)
        # cormat <- rcorr(as.matrix(inputdata()[,1:ncol(inputdata())]), type = "pearson")
        cormat <- rcorr(as.matrix(inputdata()[, c(paste0(select))]), type = "pearson")
        cormat_r <- cormat$r
        cormat_r[cormat$P > 0.05] <- 0
        corrplot(cormat_r, tl.col = "black", tl.srt = 60, tl.cex = 0.6, mar = c(0,0,2,2))
    }, res =96)
    )
      observeEvent(input$page_21, switch_page(1))
    observeEvent(input$page_32, switch_page(2))
    
    observeEvent(input$page_23, switch_page(3))
  
    # output$plot <- renderPlot({
    #    plot(dataset())
    # }, res = 96)
    
}

shinyApp(ui, server)
}