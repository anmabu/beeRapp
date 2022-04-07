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
# library(uuid)
library(promises)
library(future)
# install.packages("future", dependencies = T)
# if(interactive()){
plan(multisession)
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
                 menuSubItem("Correlation Matrix", tabName = "correlationmatrices"), 
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
              # Make more choices of Correlations available 
              selectInput("corrtype", "Select Correlation Type", choices = c("pearson")),# , "spearman", "kendall")),
              numericInput("thresholdvalue", "Choose Threshold", min = 0.001, max = 0.5, value = 0.05, step = 0.001),
              downloadButton("downpaircorr", "Download"),
              div(style= "display:inline-block", radioButtons("dfpairmatrix", "", c(".pdf", ".pptx", ".zip"), inline = TRUE))
              # verbatimTextOutput("selected_format"),
              # make options to deselect Values
              # output as Zip
            )
          ), 
          fluidRow(
            column(6, 
              h4("Example Plot"), 
              plotOutput("paircorrexample", height = 400)
              # change colors of plot
              # example plot for colors? 
              ),
            column(6, 
              h4("Plot Settings"),
              #column(2, 
              #radioButtons("colorpaircorr", "Colors", c("black", "red", "blue", "green"))
              #), 
              column(2,
              radioButtons("mouseID", "Mouse ID", c("no", "yes"))
              )
              # make column to select colors of plot
            )
          )
        )
      )
    )
  )
)
                
         

        

server <- function(input, output, session) {
    source("prelim_script.R")
    # loads labels corresponding to 'grand_table'
    labels <- reactive({
        req(infile <- input$upload)
        read.xlsx(infile$datapath, "labels", colNames = T)
    })
    # Load 'grand_table' Data and Evaluate completenes. 
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
    # This is not used rn, but displays selected Labels
    output$selected_format <- renderPrint(input$dfcorrmatrix)    
    # Display Data when loaded correctly
    output$datatable <- renderDataTable({
        inputdata()[, 1:ncol(inputdata())] # rownames = T, colnames = T
        })
    # Select labels for Correlation Matrix
    output$selectlabels <- renderUI({
      checkboxGroupInput("selection", "Which data do you want to plot?", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
    })
    # Render Correlation Matrix with selected values
    observeEvent(input$plot_selected_script, 
        {plotvalues <- c(paste0(input$selection))
        source("prelim_script.R", local = TRUE)
        output$cormat <- renderPlot(correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL))
         })

    # Download for Correlation Matrix with selected values
    output$downloadcorr <- downloadHandler( 
        filename <- ("cormat.pdf"), 
        content <- function(file) { 
            plotvalues <- c(paste0(input$selection))
            pdf(file, height = 10, width = 11, paper = "a4")
            corrmatrix <- correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL)
            dev.off()
            } 
        )
    output$paircorrexample <- renderPlot({
      data_table <- inputdata()
      labels <- labels()
      type <- input$corrtype
      threshold <- input$thresholdvalue
      id <- input$mouseID
      # runs until the first pair is true and displays it
      for (i in 1:(ncol(data_table)-1)){
        for (j in (i+1):ncol(data_table)){
          #Calculate correlations 
          p.val = stats::cor.test(data_table[,i], data_table[,j], method = type)$p.value
          r = round(stats::cor.test(data_table[,i], data_table[,j], method = type)$estimate,2)
          #Create a plot if the correlation is significant
          if(p.val<=threshold){
            # writeLines("true")
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
            #if(id == "yes"){
            #  lab2 = paste(rownames(data_table))[i]
              # writeLines(lab2)
            #  p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
            #                           label=paste0(lab1,", id=", lab2), fontface=3, size=4.5)
            # } else {
            p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                                     label=lab1, fontface=3, size=4.5)
            # }
           
            p = p + geom_point()
            return (p)
            # this doesn't work!
            #Include grouping color if included
            if(!is.null(grouping)){
              grouping = factor(grouping)
              p = p + geom_point(size  = 2.5, aes(col = grouping)) 
              if(!is.null(color_groups) & length(color_groups) >= length(unique(grouping))){
                p <- p + scale_color_manual(values = color_groups)
              } 
            }
          }
        }
      }
    })
    
    rundownpaircorr <- function(file, format, data_table, labels, type, threshold, grouping = NULL, color_groups = NULL, subset=NULL){
      #Subset the input data frame and the labels data frame if a subset argument is provided
      if(!is.null(subset)){
        data_table <- data_table[,subset]
        labels <- labels[subset,]
      }
      #Determine the output format selected by the user 
      if(format == ".pdf"){
        pdf(file, height=4.5, width = 5.5)
      }
      if(format == ".pptx"){
        doc <- officer::read_pptx()
      }
      if(format == ".zip"){   
        fs <- c()
      }
      # add progress bar
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating plots", value = 0)
      #Calculate the correlations and create the plots
      # for (i in 1:(ncol(data_table)-1)){
      for (i in 1:5){  
      # move progress bar
        # progress$inc(1/(ncol(data_table)-1))
        progress$inc(1/5)
        # for (j in (i+1):ncol(data_table)){
        for (j in (i+1):5){
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
            # if(id == "yes"){
            #  lab2 = paste(rownames(data_table))[i]
            # writeLines(lab2)
            #  p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
            #                           label=paste0(lab1,", id=", lab2), fontface=3, size=4.5)
            # } else {
            p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                                     label=lab1, fontface=3, size=4.5)
            # }
            # p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
            #                         label=lab1, fontface=3, size=4.5)
            if (format == ".zip"){
              fs <- c(fs, paste0(labels$colnames[i], labels$colnames[j], ".pdf"))
              pdf(paste0(labels$colnames[i], labels$colnames[j], ".pdf"), height = 4.5, width = 5.5)
              print(p)
              dev.off()
            }
            if(format == ".pptx"){
              doc <- officer::add_slide(doc)
              doc <- officer::ph_with(x = doc, value = p, location =ph_location(type="body",width=6, height=4.5), res=600)
            } else {print(p)}
          }
        }
      }
      if (format == ".zip"){
        zip(zipfile = file, files = fs)
      }
      if(format == ".pptx"){
        print(doc, target = file) 
      } else {
        dev.off()}
      # return(file)
    }
    outputfiles <- reactiveVal()
    # format_react <- reactive({input$dfpairmatrix})
    # Download for Pairwise Correlation Matrices
    output$downpaircorr <- downloadHandler( 
      filename <- function () {
        if (input$dfpairmatrix == ".pptx"){
          name <- "pair_corr_plots.pptx" 
          return(name)
        } else if (input$dfpairmatrix == ".pdf"){
          name <- "pair_corr_plots.pdf"
          return(name)
        } else if(input$dfpairmatrix == ".zip"){
          name <- "pair_corr_plots.zip"
          return(name)
        } 
      },
      content <- function(file) { 
        fileoutput <- reactive({ 
        format <- input$dfpairmatrix
        writeLines(format)
        data_table <- inputdata()
        labels <- labels()
        type <- input$corrtype
        threshold <- input$thresholdvalue
        # id <- input$mouseID
        grouping <- NULL 
        color_groups <- NULL 
        subset <- NULL
        # https://rstudio.github.io/promises/articles/casestudy.html
        # future_promise({
        # rundownpaircorr(file, format, data_table, labels, type, threshold)
        # }) %...>%
         # outputfiles()
        pairwiseCorrelations(file, data_table, labels, format, type, threshold)
        # rundownpaircorr(file, format, data_table, labels, type, threshold)
        # fileoutput <- callr::r_bg(func = rundownpaircorr, args=list(file, format, data_table, labels, type, threshold), supervise = TRUE)
        # fileoutput
        }
       )
        fileoutput()
      }
    )
}
# ?parallelly::supportsMulticore
shinyApp(ui, server)