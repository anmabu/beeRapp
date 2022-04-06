library(corrplot)
library(DT)
library(data.table)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(officer)
library(openxlsx)
# library(patchwork)
library(reporter)
library(shiny)
library(shinydashboard)

# install.packages("reporter", dependencies = T)
# library(BiocManager)
# BiocManager::install('mixOmics')
# if(interactive()){

ui <- fluidPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"),
    # titlePanel("First Version"),
  dashboardPage(
    skin = "purple",
    dashboardHeader(title = "Behavior Analysis"), 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Import Data", tabName = "importdata"), 
        menuItem("Analysis", tabName = "analysis", 
                 menuSubItem("Correlation Matrices", tabName = "correlationmatrices"), 
                 menuSubItem("Pairwise Correlations", tabName = "pairwisecorrelations")
        )       
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
                 div(style = "display:inline-block", actionButton("plot_selected_script", "Plot")), 
                 div(style = "display:inline-block", downloadButton("downloadcorr", "Download")),
                 # div(style= "display:inline-block", radioButtons("dfcorrmatrix", "", c(".pdf", ".pptx"), inline = TRUE)),
                 plotOutput("cormat", height=800)
            )
          ), 
        ), 
        tabItem(tabName = "pairwisecorrelations", 
          fluidRow(
            column(12, 
              h4("Pairwise Correlations"),
              selectInput("corrtype", "Correlation Type", choices = c("pearson")),# , "spearman", "kendall")),
              downloadButton("downpaircorr", "Download"),
              div(style= "display:inline-block", radioButtons("dfpairmatrix", "", c(".pdf", ".pptx"), inline = TRUE))
              # verbatimTextOutput("selected_format"),
              # progress bar 
            )
          )
        )
      )
    )
  )
)
                
         

        

server <- function(input, output, session) {
    source("prelim_script.R")
  
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

    output$selected_format <- renderPrint(input$dfcorrmatrix)    
    
    output$selectlabels <- renderUI({
        checkboxGroupInput("selection", "Which data do you want to plot?", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
        })
    output$datatable <- renderDataTable({
        inputdata()[, 1:ncol(inputdata())] # rownames = T, colnames = T
        })
    
    observeEvent(input$plot_selected_script, 
        {plotvalues <- c(paste0(input$selection))
        source("prelim_script.R", local = TRUE)
        output$cormat <- renderPlot(correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL))
         })

    output$downloadcorr <- downloadHandler( 
        filename <- ("cormat.pdf"), 
        content <- function(file) { 
            plotvalues <- c(paste0(input$selection))
            pdf(file, height = 10, width = 11, paper = "a4")
            corrmatrix <- correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL)
            dev.off()
            } 
        )
  
    output$downpaircorr <- downloadHandler( 
      filename <- function () {
        # writeLines(input$dfpairmatrix)
        if (input$dfpairmatrix == ".pptx"){
          name <- "pair_corr_plots.pptx" 
          # writeLines(name)
          return(name)
        } else {
          name <- "pair_corr_plots.pdf"
          return(name)
        }
      },
      content <- function(file) { 
        format <- input$dfpairmatrix
        data_table <- inputdata()
        labels <- labels()
        type <- input$corrtype
        # writeLines(type)
        threshold <- 0.05 
        grouping <- NULL 
        color_groups <- NULL 
        # subset = NULL
        #Determine the output format selected by the user 
        if(format == ".pdf"){
          pdf(file, height=4.5, width = 5.5)
        }
        # writeLines(file)
        if(format == ".pptx"){
          doc <- officer::read_pptx()
        }
        
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Making plots", value = 0)
        #Calculate the correlations and create the plots
        for (i in 1:(ncol(data_table)-1)){
        # for(i in 1:5){
          progress$inc(1/(ncol(data_table)-1))
          for (j in (i+1):ncol(data_table)){
          # for (j in (i+1):5){  
            #Calculate correlations 
            p.val = stats::cor.test(data_table[,i], data_table[,j], method = type)$p.value
            r = round(stats::cor.test(data_table[,i], data_table[,j], method = type)$estimate,2)
            
            #Create a plot if the correlation is significant
            if(p.val<=threshold){
              
              x_lab = paste(labels$label1[i], 
                            labels$label2[i], sep ="\n")
              
              y_lab = paste(labels$label1[j], 
                            labels$label2[j], sep ="\n")
              
              p = ggplot2::ggplot(data_table, 
                                  aes(x = data_table[,i], 
                                      y = data_table[,j])) +
                geom_point(size=2.5, col = "black") +
                stat_smooth(formula = "y~x", method = "lm", se = F, color="black") +
                labs(col="", x = x_lab, y = y_lab) +
                theme_classic(base_size=14) + theme(axis.text=element_text(size=12, color="black")) 
              
              
              #Include grouping color if included
              if(!is.null(grouping)){
                grouping = factor(grouping)
                p = p + geom_point(size  = 2.5, aes(col = grouping)) 
                
                if(!is.null(color_groups) & length(color_groups) >= length(unique(grouping))){
                  p <- p + scale_color_manual(values = color_groups)
                }
              }
              
              #Get y-limits of the plotting area
              ymax = ggplot2::layer_scales(p)$y$range$range[2]
              ymin = ggplot2::layer_scales(p)$y$range$range[1]
              
              #Increase the limits of the plot to include the r and p-value
              p = p + ylim(ymin, 1.4*ymax)
              xcoord = layer_scales(p)$x$range$range[2] - (layer_scales(p)$x$range$range[2] - layer_scales(p)$x$range$range[1])/2
              
              if(p.val<0.001){
                lab1 = paste0("r=",  r, ", p<0.001")
              }else{
                lab1 = paste0("r=",  r, ", p=" ,round(p.val,3))
              }
              
              p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                                       label=lab1, fontface=3, size=4.5)
              
              if(format == ".pptx"){
                doc <- officer::add_slide(doc)
                doc <- officer::ph_with(x = doc, value = p, location =ph_location(type="body",width=6, height=4.5), res=600)
              } else {print(p)}
              
            }
          }
        }
        if(format == ".pptx"){
          print(doc, target = file) 
        } else {dev.off()}
      } 
     
      )
    

    
    }

shinyApp(ui, server)
# }