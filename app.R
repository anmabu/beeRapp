library(corrplot)
library(DT)
library(data.table)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(officer)
library(openxlsx)
# library(patchwork)
library(shiny)
library(shinydashboard)

# if(interactive()){
ui <- fluidPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"),
    # titlePanel("First Version"),
    dashboardPage(
        skin = "purple",
        dashboardHeader(title = "Basic Dashboard"), 
        dashboardSidebar(
            sidebarMenu(
                menuItem("Import Data", tabName = "importdata"), 
                menuItem("Correlation Matrices", tabName = "correlationmatrices")
            )
        ), 
        dashboardBody(
            tabItems(
                tabItem(tabName = "importdata", 
                        fluidRow(
                            column(12,
                                fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"), 
                                dataTableOutput("datatable")
                            )
                        )),
                tabItem(tabName = "correlationmatrices", 
                        fluidRow(
                            column(12, 
                                h4("Select Variables"),
                                div(style = "height:300px;overflow-y: scroll", uiOutput("selectlabels")), 
                            )
                        ),
                        fluidRow(
                            column(12, 
                                h4("Correlation Matrix"), 
                                actionButton("plot_selected", "Plot"),
                                downloadButton("downloadcorr", "Download"),
                                plotOutput("cormat", height=800)
                            )
                        )
                        )
                )
           # tabsetPanel(id = "wizard", type = "hidden",
        # tabPanel("page_1",
            #fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"),
            # dataTableOutput("datatable"), 
            # actionButton("page_12", "next")
        # ), 
        # tabPanel("page_2", 
            # verbatimTextOutput("textfield"),
            # checkboxGroupInput("corr_variables", "Which variables do you want to plot?", "all"),
            # uiOutput("selectlabels"),
            # actionButton("plot_selected", "Plot"),
            # plotOutput("cormat", height=800),
            # downloadButton("downloadcorr", "Download plot"),
            # actionButton("page_21", "prev"),
            # actionButton("page_23", "next")
        # ),
        # tabPanel("page_3", 
           
            
            # actionButton("page_32", "prev"),
            # actionButton("page_34", "next")
        # ), 
        # tabPanel("page_4",
            # plotOutput("paircorr"),
            # actionButton("page_43", "prev"))
    # )
        # )
    # )
    # sidebarLayout(position = "left", 
        #sidebarPanel(
    
    #       ),
    #    mainPanel(

        
        )
    )
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
    # switch_page <- function(i){
    #     updateTabsetPanel(inputId = "wizard", selected = paste0("page_", i))
    # }
    
    # observeEvent(input$page_12, switch_page(2))
    # observeEvent(input$page_21, switch_page(1))
    # observeEvent(input$page_32, switch_page(2))
    # observeEvent(input$page_23, switch_page(3))
    # observeEvent(input$page_34, switch_page(4))
    # observeEvent(input$page_43, switch_page(3))
    
    
    output$selectlabels <- renderUI({
        checkboxGroupInput("selection", "Which data do you want to plot?", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
        })
    output$datatable <- renderDataTable({
        inputdata()[, 1:ncol(inputdata())] # rownames = T, colnames = T
        })
    plot_cormat <- function(selectedLabels){ 
        # elect <- req(input$selection)
        # cormat <- rcorr(as.matrix(inputdata()[,1:ncol(inputdata())]), type = "pearson")
        cormat <- rcorr(as.matrix(inputdata()[, c(paste0(selectedLabels))]), type = "pearson")
        cormat_r <- cormat$r
        cormat_r[cormat$P > 0.05] <- 0
        cormat_plot <- corrplot(cormat_r, tl.col = "black", tl.srt = 60, tl.cex = 0.6, mar = c(0,0,2,2))
        return(cormat_plot)
        }
    
    observeEvent(input$plot_selected,
        {plotvalues <- input$selection
        output$cormat <- renderPlot({plot_cormat(plotvalues)}, res = 96)}
        )
    
    output$downloadcorr <- downloadHandler( 
        # opens pdf in new window on my machine and when exited, doesn't return to app
        # works good, when run directly in a window. possibly something up with my settings
        filename <- ("cormat.pdf"), 
        content <- function(file) { 
            # this might not work when running on shiny server
            pdf(file, height = 10, width = 11, paper = "a4")
            plot_cormat(input$selection)
            dev.off()
            }
        )
    output$paircorr <- renderPlot({
      
        threshold = 0.0001
        
        lplots <- list()
        index <- 1
        # doc <- read_pptx()
        for (i in 1:(ncol(inputdata())-1)){
            for (j in (i+1):ncol(inputdata())){
                #Calculate correlations 
                p.val <- cor.test(inputdata()[,i], inputdata()[,j])$p.value
                r <- round(cor.test(inputdata()[,i], inputdata()[,j])$estimate,2)

                #Create a plot if the correlation is significant
                if(p.val<=threshold){
                    
                    x_lab <- paste(labels()$label1[i], 
                                   labels()$label2[i], sep ="\n")
                    
                    y_lab <- paste(labels()$label1[j], 
                                   labels()$label2[j], sep ="\n")
                    
                    p <- ggplot(inputdata(), 
                                aes(x = inputdata()[,i], 
                                    y = inputdata()[,j])) +
                        geom_point(size=2.5) +
                        # stat_smooth(formula = "y~x", method = "lm", se = F, color="black") +
                        labs(col="", x = x_lab, y = y_lab) +
                        theme_test(base_size=14) + theme(axis.text=element_text(size=12, color="black")) 
                    
                    ymax = layer_scales(p)$y$range$range[2]
                    ymin = layer_scales(p)$y$range$range[1]
                    
                    p <- p + ylim(ymin, 1.4*ymax)
                    xcoord <- layer_scales(p)$x$range$range[2] - (layer_scales(p)$x$range$range[2] - layer_scales(p)$x$range$range[1])/2
                    # writeLines(paste0(xcoord))
                    if(p.val<0.001){
                        lab1 <- paste0("r=",  r, ", p<0.001")
                    }else{
                        lab1 <- paste0("r=",  r, ", p=" ,round(p.val,3))
                    }
                    
                    p <- p+ annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                                     label=lab1, fontface=3, size=4.5)
                     #if (p){
                    p <- list(p)
                    lplots <- append(lplots, p)
                    # lplots[[index]] <- new[p
                    # index <- index + 1
                    # }
                    # return(p)
                    writeLines(paste0(length(lplots)))  
                    writeLines(paste0(typeof(lplots[[index]])))
                }
            }
        }
    grid.arrange(grobs=lapply(lplots), ncol=3, top="Pairwise Correlation")
    # return(lplots)
    # wrap_plots(lplots)
    
    })

    # output$plot <- renderPlot({
    #    plot(dataset())
    # }, res = 96)
    
    }

shinyApp(ui, server)
# }