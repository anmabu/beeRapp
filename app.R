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
library(sets)
# install.packages("future", dependencies = T)
# if(interactive()){
plan(multisession)
# UI ####
ui <- fluidPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"),
    # titlePanel("First Version"),
  dashboardPage(
    # skin = "purple",
    dashboardHeader(title = "Behavior Analysis"), 
    dashboardSidebar(
      sidebarMenu(
        # menuItem("Welcome!", tabName = "welcome"),
        menuItem("Import Data", tabName = "importdata"), 
        menuItem("Analysis", tabName = "analysis", 
                 menuSubItem("Correlation Matrix", tabName = "correlationmatrices"), 
                 menuSubItem("Pairwise Correlations", tabName = "pairwisecorrelations"), 
                 menuSubItem("Boxplots", tabName = "boxplots"), 
                 menuSubItem("Heatmap", tabName = "heatmap"), 
                 menuSubItem("Clustering", tabName = "clustering"), 
                 menuSubItem("PCA", tabName = "pca")
        )       
      )
    ),
    dashboardBody(
      tabItems(
        ## Import Data ####
        tabItem(tabName = "importdata", 
                fluidRow(
                  column(12,
                   fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"), 
                   dataTableOutput("datatable")
                  )
                )), 
        ## Correlation Matrix ####
        tabItem(tabName = "correlationmatrices", 
                # h1("Correlation Matrix"),
          fluidRow(
            column(12, 
                box(
                  div(style = "display:inline-block", actionButton("corrselectall", "Select All")), 
                  div(style = "display:inline-block", actionButton("corrselectnone", "Select None")),
                  div(style = "height:300px;overflow-y: scroll;width:100%", uiOutput("selectlabels")), 
                  # div(style = "height:300px;overflow-y: scroll; width:100%", checkboxGroupInput("selection", NULL, choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames)), 
                  
                  title = "Select Variables", width = 12, collapsible = T, 
                      solidHeader = T, status = "primary"
                )
            )
          ),
          fluidRow(
            column(12, 
              box(
                 # h4("Correlation Matrix"), 
                 # div(style = "display:inline-block", actionButton("plot_selected_script", "Plot")), 
                 div(style = "display:inline-block", downloadButton("downloadcorr", "Download")),
                 div(style= "display:inline-block", radioButtons("dfcorrmatrix", "", c(".pdf"))),
                 plotOutput("cormat", height=800), 
                 title = "Correlation Matrix", width = 12, collapsible = F, 
                 solidHeader = T, status = "primary"
              )
            )
          ), 
        ), 
        ## Pairwise Correlation ####
        tabItem(tabName = "pairwisecorrelations", 
          fluidRow(
            column(12, 
                box(
              # h4("Pairwise Correlations"),
                  selectInput("corrtype", "Select Correlation Type", choices = c("pearson", "spearman", "kendall"), width = "400px"),# , "spearman", "kendall")),
                  numericInput("thresholdvalue", "Choose cutoff p-value", min = 0.001, max = 0.5, value = 0.05, step = 0.001, width="400px"),
                  downloadButton("downpaircorr", "Download"),
                  div(style= "display:inline-block", radioButtons("dfpairmatrix", "", inline = TRUE, 
                              choiceNames = c(".pdf (All plots)", ".pptx (All plots)", ".zip (Each plot seperately as .pdf)"), 
                              choiceValues = c(".pdf", ".pptx", ".zip"))), 
                  title  = "Pairwise Correlations", width = 12, collapsible = F, solidHeader = T, status = "primary"
                )
              # verbatimTextOutput("selected_format"),
              # make options to selece/deselect all Values
              # make options to deselect Values
            )
          ), 
          fluidRow(
            column(6,
                box(
              # h4("Example Plot"), 
                  plotOutput("paircorrexample", width=500, height = 400), 
              # change colors of plot
              # example plot for colors?
                  title = "Example Plot", width = 12, collapsible = F, solidHeader = T, status = "primary", align = "center"
                ), 
                # div(style = "display:inline-block;vertical-align:top", box(
                  
                  # title = "test", width = 2, solidHeader = T
                # ))
              ),
            column(4, 
              # h4("Plot Settings"),
              box(
                column(6,
                  radioButtons("pairID", "Animal ID", choiceNames = c("yes", "no"), choiceValues = c(T, F), selected = F)
                ), 
                column(6, 
                  uiOutput("colorgroups")
                ),
              title = "Plot Settings", width = 12, solidHeader = T, collapsible = F, status = "primary", align = "left"
              )
              # make column to select colors of plot
            )
          )
        ), 
        ## Boxplots ####
        tabItem(tabName = "boxplots", 
          fluidRow(
            column(12,
              box(
                # h4("Boxplots"), 
                selectInput("comptype", "Select Comparison Type", choices = c("wilcoxon", "t-test"), width = "400px"),
                numericInput("compthresholdvalue", "Choose cutoff p-value", min = 0.001, max = 0.5, value = 0.05, step = 0.001, width = "400px"),
                downloadButton("downboxplots", "Download"),
                div(style= "display:inline-block", radioButtons("downboxformat", "", inline = TRUE, 
                                                                choiceNames = c(".pdf (All plots)", ".pptx (All plots)"), 
                                                                choiceValues = c("pdf", "pptx"))), 
              title = "Boxplots", width = 12, solidHeader = T, collapsible = F, status = "primary" 
              )
            )
          ), 
          fluidRow(
            column(6,
              box(
              # h4("Example Plot"), 
                plotOutput("boxplotexample", height = 600, width = 450), 
                title = "Example Plot", width = 12, collapsible = F, solidHeader = T, status = "primary", align = "center"
              )
            ), 
            column(4, 
              # h4("Plot Settings"), 
              box(
                column(4, 
                  uiOutput("colorboxplot")
                ), 
                column(4,
                  radioButtons("removeoutliers", "Remove Outliers?", choiceNames = c("yes", "no"), choiceValues = c(TRUE, FALSE), selected = F)
                ),
                column(4,
                  radioButtons("boxID", "Animal ID", choiceNames = c("yes", "no"), choiceValues = c(T, F), selected = F)
                ), 
                title = "Plot Settings", width = 12, solidHeader = T, collapsible = F, status = "primary", align = "left"
              ), 
              box(
                column(6, 
                  radioButtons("boxcolor.one", "Choose First Color", choices = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B"), selected = "#D55E00")), 
                column(6, 
                  radioButtons("boxcolor.two", "Choose Second Color", choices = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B"), selected = "#5D3A9B")),
                title = "Choose Colors", width = 12, solidHeader = T, collapsible = T, status = "primary"               
              )
            ) 
          )
        ), 
        ## Heatmap ####
        tabItem(tabName = "heatmap", 
          fluidRow(
            column(12,
                box(
              # h4("Heatmap"), 
                  selectInput("heatmapcolor", "Select Color Palette", choices = c("PiYG", "PRGn", "PuOr", "RdBu", "Blue-Yellow",
                                                                             "Teal", "Sunset", "Viridis"), selected = "RdBu", width = "400px"),
                  column(2, radioButtons("cluster_cols.button", "Clustering of Columns?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("cluster_rows.button", "Clustering of Rows?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = T)),
                  column(2, radioButtons("dendrogram_cols.button", "Dendrogram of Columns?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("dendrogram_rows.button", "Dendrogram of Rows?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = T)),
                  fluidRow(
                    column(12, 
                           div(style = "display:inline-block", downloadButton("downheatmap", "Download")),
                           div(style = "display:inline-block", radioButtons("visualbutton", "", choices = ("pdf"))))
                  ),
                  title = "Heatmap", width = 12, solidHeader = T, status = "primary"      
              )
            )
          ), 
          
          fluidRow(
            column(12, 
              box(
                plotOutput("exampleheatmap", height = 600, width = 1000), 
                title = "Example Heatmap", width = 12, solidHeader = T, status = "primary", align = "center"
              )
            )
          )
        )
      ## Clustering ####
      ## PCA ####
      )
    )
  )
)
                
         

        
# Server ####
server <- function(input, output, session) {
    source("prelim_script.R")
    ## Global Reactives ####
    # loads metadata corresponding to 'grand_table'
    metadata <- reactive({
        req(infile <- input$upload)
        read.xlsx(infile$datapath, "meta_data", rowNames = T, colNames=T)
    })
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
      datatable(
        inputdata()[, 1:ncol(inputdata())], # rownames = T, colnames = T
        extensions = c('Scroller'),
        options = list(scrollY = 400,
                       deferRender = TRUE,
                       scroller = TRUE,
                       paging = TRUE,
                       dom = 'lBfrtip',
                       fixedColumns = TRUE),
      )
        })
    
    ## Correlation Matrix ####
    ### Select Labels Correlation Matrix ####
    output$selectlabels <- renderUI({
      checkboxGroupInput("selection", NULL, choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
    })

    observeEvent(input$corrselectall, {
      updateCheckboxGroupInput(session, "selection", selected = labels()$colnames)
    })
    observeEvent(input$corrselectnone, {
      updateCheckboxGroupInput(session, "selection", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames)
    })    
    ### Example Correlation Matrix ####
    output$cormat <- renderPlot({
      plotvalues <- c(paste0(req(input$selection)))
      correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL)
      })
    

    ### Download Correlation Matrix ####
    output$downloadcorr <- downloadHandler( 
        filename <- ("cormat.pdf"), 
        content <- function(file) { 
            plotvalues <- c(paste0(input$selection))
            pdf(file, height = 10, width = 11, paper = "a4")
            corrmatrix <- correlationMatrix(inputdata(), subset = plotvalues, outDir = NULL, filename = NULL)
            dev.off()
            } 
        )
    ## Pairwise Correlation ####
    ### Example Plot Pairwise Correlation ####
    output$paircorrexample <- renderPlot({
      data_table <- inputdata()
      labels <- labels()
      type <- input$corrtype
      threshold <- input$thresholdvalue
      grouping <- NULL
      color_groups <- NULL
      animal_label <- input$pairID
      if (req(input$select_colorgroups) != "None"){
        grouping <- pairgroups()
        color_groups <- paircolor()
      }
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
            #Include animal label if argument is set to TRUE
            if(animal_label == TRUE){
              lab = rownames(data_table)
              p = p + ggrepel::geom_text_repel(aes(label = lab), max.overlaps = Inf, size=3,
                                               min.segment.length = 0, show.legend = F)
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
           
            #Include grouping color if included
            # cat(grouping)
            if(!is.null(grouping)){
              grouping = factor(grouping)
              p = p + geom_point(size  = 2.5, aes(col = grouping)) 
              if(!is.null(color_groups) & length(color_groups) >= length(unique(grouping))){
                p <- p + scale_color_manual(values = color_groups)
              } 
            }
            return (p)
          }
        }
      }
    })
    ### Color Pairwise Correlation ####
    output$colorgroups <- renderUI({
      choices <- c("None")
      data_choices <- req(colnames(metadata()))
      choices <- c(choices, data_choices)
      radioButtons("select_colorgroups", "Color by group", choices, selected="None") 
    })
    paircolor <- reactive({
      cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      # colors <- list("blue", "red", "green")
      return(cbbPalette)
    })
    
    ### Groups Pairwise Correlation####
    pairgroups <- reactive({
      # cat(input$select_colorgroups)
      if (input$select_colorgroups == "None"){
        return(NULL)
      } else {
        group <- metadata()[,c(input$select_colorgroups)]
        # cat(group)
        return(group)
      }
      })
    
    ### Download Pairwise Correlation #### 
    output$downpaircorr <- downloadHandler( 
      filename <- function () {
        # add correlation type to file name?
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
        data_table <- inputdata()
        labels <- labels()
        type <- input$corrtype
        threshold <- input$thresholdvalue
        # id <- input$mouseID
        # cat(unlist(paircolor()))
        grouping <- pairgroups()
        color_groups <- unlist(paircolor())
        subset <- NULL
        animal_label <- input$pairID
        # https://rstudio.github.io/promises/articles/casestudy.html
        # future_promise({
        # pairwiseCorrelations(file, data_table, labels, format, type, threshold)
        # }) %...>%
         # outputfiles()
        pairwiseCorrelations(file, data_table, labels, format, type, threshold, grouping = grouping, color_groups = color_groups, animal_label = animal_label)
        # fileoutput <- callr::r_bg(func = rundownpaircorr, args=list(file, format, data_table, labels, type, threshold), supervise = TRUE)
        # fileoutput
        }
       )
        fileoutput()
      }
    )
    
    ## Boxplots ####
    ### Example Plot Boxplots ####
    output$boxplotexample <- renderPlot({
      data_table <- inputdata()
      labels <- labels()
      test <- boxtesttype()
      threshold <- input$compthresholdvalue
      grouping <- boxplotgroups()
      color_groups <- unlist(boxplotcolor())
      remove_outliers <- input$removeoutliers
      subset <- NULL
      animal_label <- input$boxID
        
      #Perform the analysis only for a subset of variables 
      if(!is.null(subset)){
        data_table = data_table[,subset]
        labels = labels[subset,]
      }
        
      #Change the grouping variable to a factor
      grouping = factor(grouping)
        
      for (i in 1:ncol(data_table)){
        
        #Screen for outliers and remove them if remove_outliers = TRUE
        if(remove_outliers == TRUE){
            
          y_var = data_table[,i]
          names(y_var) = rownames(data_table)
          group1 = y_var[grouping == base::levels(grouping)[1]]
          group2 = y_var[grouping == base::levels(grouping)[2]]
            
          #Transform values to z-scores
          group1_z = (group1 - base::mean(stats::na.omit(group1)))/stats::sd(stats::na.omit(group1))
          group2_z = (group2 - base::mean(stats::na.omit(group2)))/stats::sd(stats::na.omit(group2))
            
          #get potential outliers
          outliers1 = base::names(group1_z)[base::abs(group1_z)>3.29]
          outliers2 = base::names(group2_z)[base::abs(group2_z)>3.29]
          outliers = c(outliers1, outliers2)
            
          #Set outliers to NA in the data_table
          data_table[rownames(data_table) %in% outliers, i] = NA
            
        }
          
        #Perform the statistical test, per default, a wilcox test, a t.test can be performed alternatively
        if (test == "wilcox"){
          p.val = base::suppressWarnings(stats::wilcox.test(data_table[,i]~grouping, var.equal=T)$p.value)
        }
          
        #Alternatively, a t-test is performed
        if (test == "t.test"){
          #First check for variance equality
          
          p.var = stats::var.test(data_table[,i]~grouping)$p.value
            
          #Perform  a standard or Welch's t-test depending on variance equality
          if (p.var < 0.05){
            p.val = stats::t.test(data_table[,i]~grouping)$p.value
          }else{
            p.val = stats::t.test(data_table[,i]~grouping, var.equal=T)$p.value
          }
        }
          
        #Create the plot if the p-value is lower than the threshold
        if(p.val <=threshold){
            
          y_lab = paste(labels$label1[i], 
                        labels$label2[i], sep ="\n")
            
          p = ggplot2::ggplot(data_table,
                              aes(x = grouping, y = data_table[,i], 
                                  col = grouping)) +
            geom_boxplot(outlier.shape = NA, show.legend =F) +
            geom_point(show.legend = F, size=3,
                       position = position_jitter(seed = 1)) + 
            theme_test(base_size = 14)+
            theme(axis.text = element_text(size=12, color="black")) +
            labs(x="", col="", y = y_lab) 
            
            
          #Include grouping color if included
          if(!is.null(color_groups) & length(color_groups) >= length(unique(grouping))){
            p = p + scale_color_manual(values = color_groups)
          }
          
          #Include animal label if argument is set to TRUE
          if(animal_label == TRUE){
            lab = rownames(data_table)
            
            p = p + ggrepel::geom_text_repel(aes(label = lab), max.overlaps = Inf, size=3,
                                             min.segment.length = 0, show.legend = F,
                                             col = "black", position = position_jitter(seed = 1))
          }
            
          #Get y-limits of the plotting area
          ymax = ggplot2::layer_scales(p)$y$range$range[2]
          ymin = ggplot2::layer_scales(p)$y$range$range[1]
          
          #Increase the limits of the plot to include the r and p-value
          p = p + ylim(ymin, 1.2*ymax)
            
          if(p.val<0.001){
            lab1 = "p<0.001"
          }else{
            lab1 = paste0("p=" ,round(p.val,3))
          }
            
            
          p = p + ggplot2::annotate(geom = "text", label = lab1, 
                                    x = 1.5, 
                                    y = 1.1*ymax, size = 4.5, color ="black", fontface="italic")
          return(p)
        }  
      }
    })
    ### Reactives Boxplots ####
    boxtesttype <- reactive({
      comp <- req(input$comptype)
      if (comp == "wilcoxon"){ comp = "wilcox"
      } else { comp = "t.test"}
      return(comp)
    })
    
    ### Color Boxplots ####
    output$colorboxplot <- renderUI({
      data_choices <- req(colnames(metadata()))
      # only make meaningful choices possible for boxplots
      possible_choices <- c()
      for (col in data_choices){
        group <- metadata()[, c(col)]
        max_group <- max(as.numeric(factor(group)))
        if (max_group == 2){
          possible_choices <- c(possible_choices, col)
        }
      }
      choices <- c(possible_choices)
      radioButtons("select_boxgroups", "Color by group", choices) 
    })
    boxplotcolor <- reactive({
      cbbPalette <- c(input$boxcolor.one, input$boxcolor.two)
      return(cbbPalette)
    })
    
    ### Groups Boxplots ####
    boxplotgroups <- reactive({
      req(input$select_boxgroups)
      group <- metadata()[,c(input$select_boxgroups)]
      # max_group <- max(as.numeric(factor(group)))
      # cat(max(as.numeric(factor(group))))
      # if (max_group == 2) { 
      return(group)
      # }
    })
    
    ### Download Boxplots ####
    output$downboxplots <- downloadHandler(
      filename <- function() {
        if (input$downboxformat == "pptx"){
          name <- "boxplots.pptx" 
          return(name)
        } else if (input$downboxformat == "pdf"){
          name <- "boxplots.pdf"
          return(name)
        }
      },
      content <- function(file){
        data_table <- inputdata()
        labels <- labels()
        format <- input$downboxformat
        test <- boxtesttype()
        threshold <- input$compthresholdvalue
        grouping <- boxplotgroups()
        color_groups <- unlist(boxplotcolor())
        remove_outliers <- input$removeoutliers
        subset <- NULL
        animal_label <- input$boxID
        pairwiseComparisons(data_table, labels, file, format, test, threshold, grouping, color_groups, remove_outliers, subset, animal_label)
      }
      
    )
    
    ## Heatmap####
    # Reactives
    cols_cluster <- reactive({
      if (eval(parse(text = input$dendrogram_cols.button)) | eval(parse(text = input$cluster_cols.button))) {
        updateRadioButtons(session, "cluster_cols.button", selected = T)
        return(T)
      } else {return(F)}
    })
    
    rows_cluster <- reactive({
      if (eval(parse(text=input$dendrogram_rows.button)) | eval(parse(text=input$cluster_rows.button))) {
        updateRadioButtons(session, "cluster_rows.button", selected = T)
        return (T)
      } else {return(F)}
    })
    
    
    ### Example Heatmap ####
    output$exampleheatmap <- renderPlot({
      # had to remove 'my' from 'my.breaks' in lines 423 + 424
      # createHeatmap(inputdata(), file = NULL)
      data_table <- inputdata()
      scale <- TRUE
      cluster_cols <- cols_cluster()
      cluster_rows <- rows_cluster()
      dendrogram_cols <- eval(parse(text=input$dendrogram_cols.button))
      dendrogram_rows <- eval(parse(text=input$dendrogram_rows.button))
      grouping <- NULL
      color_groups <- NULL
      subset <- NULL
      palette <- input$heatmapcolor
      #Check if the user wants to subset the matrix
      if(!is.null(subset)){
        data_table = data_table[,subset]
      }
      
      #Scale the data to z_scores to overcome different measuring scale. set to TRUE per default
      if(scale==TRUE){
        data_table = base::as.data.frame(base::scale(data_table))
        
        #Create color code centered around 0
        max.val = max(abs(data_table))
        
        cols = grDevices::hcl.colors(n = 20, palette = palette)
        
        if (palette %in% c("PiYG", "PRGn", "PuOr", "RdBu", "Blue-Yellow",
                           "Teal", "Sunset", "Viridis")){
          cols = base::rev(cols)
        }
        
        breaks = c(seq(-max.val, -0.001, length.out = 50), 0, seq(0.001, max.val, length.out = 50))
        # cat(breaks)
        colors = c(grDevices::colorRampPalette(colors = c(cols[c(1,2,4,6,10)]))(length(breaks[breaks<0])), "white",
                   grDevices::colorRampPalette(colors = c(cols[c(11,15,17,19,20)]))(length(breaks[breaks>0])))
        
      }else{
        
        cols = grDevices::hcl.colors(n = 20, palette = palette)
        
        if (palette %in% c("PiYG", "PRGn", "PuOr", "RdBu", "Blue-Yellow",
                           "Teal", "Sunset", "Viridis")){
          cols = base::rev(cols)
          colors = cols
          breaks = NA
        }
      }
      
      
      #Include annotation for groups if grouping variables is included
      if(!is.null(grouping)){
        grouping = factor(grouping)
        
        annotation = data.frame(group = grouping)
        rownames(annotation) = rownames(data_table)
        annot_colors = NA
        
        if(!is.null(color_groups)){
          annot_colors <- color_groups
          names(annot_colors) = levels(grouping)
          annot_colors <- list(group = annot_colors)
        }
        
      }else{
        annotation= NA
        annot_colors = NA}
      
      #Determine the height of the dendrograms and size of the heatmap
      if(dendrogram_cols == FALSE){
        treeheight_col = 0
      }else{treeheight_col = 30}
      
      if(dendrogram_rows == FALSE){
        treeheight_row = 0
      }else{treeheight_row = 30}
      
      
      #ECreate the heatmap
      if(ncol(data_table) <= 10){
        height = 7
        width = 5
        font_size = 10
      }
      
      if(ncol(data_table) > 10 & ncol(data_table) < 50){
        height = 6
        width = 8
        font_size = 9
      }
      
      if(ncol(data_table)>=50 & ncol(data_table)<90){
        height = 8
        width = 10
        font_size=8
      }
      
      if(ncol(data_table)>=90 ){
        height = 10
        width = 14
        font_size=7
      }
      pheatmap::pheatmap(data_table, cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                         treeheight_row = treeheight_row, treeheight_col = treeheight_col,
                         fontsize= font_size, angle_col = 315, na_col = "gray45",
                         color = colors, breaks = breaks,
                         annotation_row = annotation, annotation_names_row = FALSE,
                         annotation_colors = annot_colors)
    })
    ### Download Heatmap ####
    output$downheatmap <- downloadHandler(
      filename <- ("heatmap.pdf"),
      content <- function(file){
        data_table <- inputdata()
        file <- file
        scale <- TRUE
        cluster_cols <- cols_cluster()
        cluster_rows <- rows_cluster()
        dendrogram_cols <- eval(parse(text=input$dendrogram_cols.button))
        dendrogram_rows <- eval(parse(text=input$dendrogram_rows.button))
        grouping <- NULL
        color_groups <- NULL
        subset <- NULL
        palette <- input$heatmapcolor
        createHeatmap(data_table, file, scale = scale, cluster_cols = cluster_cols,
                      cluster_rows = cluster_rows, dendrogram_cols = dendrogram_cols,
                      dendrogram_rows = dendrogram_rows, grouping = grouping, color_groups = color_groups, 
                      subset = subset, palette = palette)
      }
    )
}
# ?parallelly::supportsMulticore
shinyApp(ui, server)