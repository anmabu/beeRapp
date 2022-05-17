suppressPackageStartupMessages({
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
})
# install.packages("future", dependencies = T)
# if(interactive()){
plan(multisession)
# Global Variables ####
color_values <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B")

# UI ####
ui <- fluidPage(
    # theme = bslib::bs_theme(bootswatch = "flatly"),
    # titlePanel("First Version"),
  dashboardPage(
    dashboardHeader(title = "behavioranalyzeR"), 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "welcome", icon = icon("compass", lib = "font-awesome")),
        menuItem("Import Data", tabName = "importdata", icon = icon("file-medical")), 
        menuItem("Analysis", tabName = "analysis", icon = icon("biohazard"), 
                 menuSubItem("Correlation Matrix", tabName = "correlationmatrices"), # icon = icon("handshake")), 
                 menuSubItem("Pairwise Correlations", tabName = "pairwisecorrelations"), # , icon = icon("dice-two")), 
                 menuSubItem("Boxplots", tabName = "boxplots"), #, icon = icon("box")), 
                 menuSubItem("Heatmap", tabName = "heatmap"), #, icon = icon("fire")), 
                 menuSubItem("Clustering", tabName = "clustering"), #, icon=icon("circle")), 
                 menuSubItem("PCA", tabName = "pca")
        )       
      )
    ),
    dashboardBody(
      tabItems(
        ## Welcome ####
        tabItem(tabName = "welcome", 
                fluidRow(
                  column(12, 
                         # box(
                           h3("Welcome to behavioranalyzeR!")
                         #   title = "", solidHeader = T, status = "primary", collapsible = F, width = 12
                         # ))
                ))),
        ## Import Data ####
        tabItem(tabName = "importdata", 
                fluidRow(
                  column(12,
                   fileInput("upload", "Select file to input", accept=".xlsx", placeholder = ".xlsx input file"), 
                   dataTableOutput("datatable")
                  )
                )), 
        
        ## Clustering ####
        tabItem(tabName = "clustering", 
                fluidRow(column(12,
                                box(
                                  div(style = "display:inline-block", actionButton("clusteringselectall", "Select All")), 
                                  div(style = "display:inline-block", actionButton("clusteringselectnone", "Select None")),
                                  div(style = "height:300px;overflow-y:scroll;width:100%", uiOutput("selectlabelsclustering")), 
                                  title = "Select Variables", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = F)
                )
                ),
                fluidRow(column(12,
                                box(
                                  selectInput("algorithmclustering", "Clustering Algorithm", choices = c("Gaussian Mixture Model" = "GMM", "k-means" = "kmeans"), selected = "GMM", width = "400px"),
                                  numericInput("numclusters", "Clusters", value = 2, min = 1, width = "400px"), 
                                  radioButtons("idclustering", "Animal ID", choices = c("Yes" = T, "No" = F), selected = F),
                                  div(style = "display: inline-block", downloadButton("down_clustering")),
                                  div(style = "display: inline-block", radioButtons("clustering.type", NULL, choices = c("pdf"))),
                                  div(style = "display: block", downloadButton("clustering.save", "Save Clustering")),
                                  helpText("Note: Saves clustering to metadata table and downloads the updated xlsx file."),
                                  title = "Clustering Settings", width = 12, solidHeader = T, status = "primary")
                )
                ),
                fluidRow(column(12, 
                                box(plotOutput("exampleclustering"), 
                                    title = "Clustering", width = 12, solidHeader = T, status = "primary")    
                )
                )
        ),
        
        ## Correlation Matrix ####
        tabItem(tabName = "correlationmatrices", 
                # h1("Correlation Matrix"),
          fluidRow(
            column(12, 
                box(
                  div(style = "display:inline-block", actionButton("corrselectall", "Select All")), 
                  div(style = "display:inline-block", actionButton("corrselectnone", "Select None")),
                  div(style = "height:300px;overflow-y:scroll;width:100%", uiOutput("selectlabels")), 
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
                  title  = "Pairwise Correlations Settings", width = 12, collapsible = F, solidHeader = T, status = "primary"
                )
              # verbatimTextOutput("selected_format"),
              # make options to select/deselect all Values
              # make options to deselect Values
            )
          ), 
          fluidRow(
            column(6,
                box(
                  plotOutput("paircorrexample", width=500, height = 400), 
              # change colors of plot
                  title = "Example Plot", width = 12, collapsible = F, solidHeader = T, status = "primary", align = "center"
                ), 
              ),
            column(4, 
              box(
                column(6,
                  radioButtons("pairID", "Animal ID", choiceNames = c("yes", "no"), choiceValues = c(T, F), selected = F)
                ), 
                column(6, 
                  uiOutput("colorgroups")
                ),
           
              title = "Plot Settings", width = 12, solidHeader = T, collapsible = F, status = "primary", align = "left"
              ), 
              # box(
              #   column(12, 
              uiOutput("pair_color_picker"),
              # title = "Choose Colors", width = 12, solidHeader = T, collapsible = T, status = "primary" 
              # ),
            
              # box(
              #  column(6, 
              #         radioButtons("paircolor.one", "Choose First Color", choices = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B"), selected = "#D55E00")), 
              #  column(6, 
              #         radioButtons("paircolor.two", "Choose Second Color", choices = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B"), selected = "#5D3A9B")),
              #  title = "Choose Colors", width = 12, solidHeader = T, collapsible = T, status = "primary"               
              # )
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
                                                                choiceNames = c(".pdf (All plots)", ".pptx (All plots)", ".zip (Each plot seperatly as .pdf)"), 
                                                                choiceValues = c("pdf", "pptx", "zip"))), 
              title = "Boxplots Settings", width = 12, solidHeader = T, collapsible = F, status = "primary" 
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
                  radioButtons("boxcolor.one", "Choose First Color", choices = color_values, selected = "#D55E00")), 
                column(6, 
                  radioButtons("boxcolor.two", "Choose Second Color", choices = color_values, selected = "#5D3A9B")),
                column(12, 
                  textInput("choose_add_boxcolor", "Add Custom Color", placeholder = "Color in Hex Code"), 
                  actionButton("add_boxcolor.button", "Add Color to Palette")),
                title = "Choose Colors", width = 12, solidHeader = T, collapsible = T, status = "primary"               
              )
            ) 
          )
        ), 
        ## Heatmap ####
        tabItem(tabName = "heatmap", 
          fluidRow(column(12, 
              box(
                div(style = "display:inline-block", actionButton("heatmapselectall", "Select All")), 
                div(style = "display:inline-block", actionButton("heatmapselectnone", "Select None")),
                div(style = "height:300px;overflow-y: scroll;width:100%",uiOutput("selectlabelsheatmap")),
              title = "Select Variables", width = 12, solidHeader = T, collapsible = T, status = "primary")
            )
          ), 
          fluidRow(
            column(12,
                box(
              # h4("Heatmap"), 
                  selectInput("heatmapcolor", "Select Color Palette", choices = c("PiYG", "PRGn", "PuOr", "RdBu", "Blue-Yellow",
                                                                             "Teal", "Sunset", "Viridis"), selected = "RdBu", width = "400px"),
                  column(2, radioButtons("cluster_cols.button", "Clustering of Columns?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("cluster_rows.button", "Clustering of Rows?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("dendrogram_cols.button", "Dendrogram of Columns?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("dendrogram_rows.button", "Dendrogram of Rows?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = F)),
                  column(2, radioButtons("scaled.button", "Scale Values?", choiceNames = c("Yes", "No"), choiceValues = c(T, F), selected = T)),
                  column(2, radioButtons("grouping.button", "Grouping?", choices = c("Yes" = T, "No" = F), selected = F)),
                  fluidRow(
                    column(12, 
                           div(style = "display:inline-block", downloadButton("downheatmap", "Download")),
                           div(style = "display:inline-block", radioButtons("visualbutton", "", choices = ("pdf"))))
                  ),
                  title = "Heatmap Settings", width = 12, solidHeader = T, status = "primary"      
              )
            )
          ), 
          fluidRow(
            column(12, 
                uiOutput("groups_heatmap")
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
        ), 
    
      ## PCA ####
      
      tabItem(tabName = "pca", 
          fluidRow(column(12,
                    box(
                      column(2, radioButtons("pca_animal_id", "Animal ID", choices = c("Yes" = T, "No" = F), selected = F)),
                      column(2, uiOutput("colorgroups_pca")),
                      column(8, uiOutput("pca_color_picker")),
                      column(12, 
                             div(style = "display:inline-block", downloadButton("down_pca")),
                             div(style = "display:inline-block", radioButtons("pca_down.type", NULL, choices = c(".pdf"))),
                      style = "margin-left: -15px"),
                      title = "PCA Settings", width = 12, solidHeader = T, status = "primary"
                    )
                )
          ),
          fluidRow(column(12, 
                     box(plotOutput("examplepca"), 
                         title = "PCA", width = 12, solidHeader = T, status = "primary")
                     )
          )
        )
      )
    )
  )
)
                
         

        
# Server ####
server <- function(input, output, session) {
    source("prelim_script.R")
  
    # validate order of labels and meta_data
    # substitute " " in col names with "_"
  
    # observeEvent(input$upload, validate(
      # need({
        # infile <- input$upload
        # cat(infile$datapath)
        # infile$datapath != ""
      # }, "select another file")
    # )
    # )
    ## Global Reactives ####
    # loads metadata corresponding to 'grand_table'
    metadata <- reactiveVal()
    # this way "metadata" can be updated when clustering took place
    observeEvent(input$upload, {
      value <- read.xlsx(input$upload$datapath, "meta_data", rowNames = T, colNames=T, sep.names = "_") 
      metadata(value)
    })
    # loads labels corresponding to 'grand_table'
    labels <- reactive({
        req(infile <- input$upload)
        read.xlsx(infile$datapath, "labels", colNames = T, sep.names = "_")
    })
    # Load 'grand_table' Data and Evaluate completeness. 
    inputdata <- reactive({
        req(infile <- input$upload)
        dat <- openxlsx::read.xlsx(infile$datapath, "grand_table", rowNames = T, colNames = T, sep.names = "_")
        # cat(colnames(dat))
        meta_data <- read.xlsx(infile$datapath, "meta_data", colNames = T, sep.names = "_")
        labels <- read.xlsx(infile$datapath, "labels", colNames = T, sep.names = "_")
        #SANITY CHECKS
        
        #1. The meta_data table should contain all animal id's as in the data_table. If not the case, an error message should be produced
        
        if(!all(meta_data$animal %in% rownames(dat)) | !all(rownames(dat) %in% meta_data$animal)){
          showModal(modalDialog(
            title = "Input Error", 
             "Not all animals IDs are present in the data table or meta data table.", 
            easyClose = T
          ))
          #Produce error message "Not all animals IDs are present in the data table or meta data table"
        }
        
        #2. The data_table contains the animal id as row names. Check if they are in the same order as in the meta_data table
        #If this is not the case, then reorder the meta_data table to match the data_table 
        #This is however only performed if the dimensions of the tables match and all animal IDs are available in both tables
        
        vec <- rownames(dat) == meta_data$animal 
        
        
        if(all(vec) == FALSE  & nrow(data_table) == nrow(meta_data) & all(meta_data$animal %in% rownames(data_table))){
          meta_data = meta_data[base::match(rownames(data_table), meta_data$animal),]
        }
        
        if(all(vec) == FALSE & (nrow(data_table) != nrow(meta_data) | !all(meta_data$animal %in% rownames(data_table)))){
          #Produce an error message: "Please make sure that the animal IDs match in the data and meta data tables")
          showModal(modalDialog(
            title = "Input Error", 
            "Please make sure that the animal IDs match in the data and meta data tables.", 
            easyClose = T
          ))
        }
        
        #3. The labels table contains the description of the behavioral variables (columns in the data_table). Check if the column names are 
        #all present in the labels table. If they are not, produce an error message
        
        if(!all(labels$colnames %in% colnames(dat)) | !all(colnames(dat) %in% labels$colnames)){
          showModal(modalDialog(
            title = "Input Error", 
            "Not all animals IDs are present in the meta data table.", 
            easyClose = T
          ))
          #Produce error message "Not all animals IDs are present in the meta data table"
        }
        
        
        #4. Check if the order of the columns in data_table matches the order of the column names in the labels table and reorder if not
        #Only done if dimensions between the tables match and all column names are contained in both tables and only the order is wrong
        
        vec <- colnames(dat) == labels$colnames
        
        if(all(vec) == FALSE & ncol(data_table) == nrow(labels) & all(labels$colnames %in% colnames(data_table))){
          labels = labels[base::match(colnames(data_table), labels$colnames),]
        }
        if(all(vec) == FALSE & (ncol(data_table) != nrow(labels) | !all(labels$colnames %in% colnames(data_table)))){
          #Produce an error message: "Please make sure that the column names in the data table match the labels in the labels table")
          showModal(modalDialog(
            title = "Input Error", 
            "Please make sure that the column names in the data table match the labels in the labels table.", 
            easyClose = T
          ))
        }
        # validate that all values are numeric
        for (i in dat) {
          if (class(i) != "numeric"){
            showModal(modalDialog(
              title = "Value Error", 
              "Not all data in 'grand_table' seems to be numeric. 
              Please make sure, that all values in 'grand_table' are a number.", 
              easyClose = T
            ))
          }
        }
        # cat(unlist(meta_data))
        # returns the data if all tests are passed
        return(dat)
    })
    
    
    # This is not used rn, but displays selected Labels
    # output$selected_format <- renderPrint(input$dfcorrmatrix)    
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
      req(paircolor())
      if (req(input$select_colorgroups) != "None"){
        grouping <- pairgroups()
        color_groups <- paircolor()
      }
      # cat(color_groups)
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
      data_choices <- req(colnames(metadata()))  # only add when 2 or more different values?
      choices <- c(choices, data_choices)
      radioButtons("select_colorgroups", "Color by group", choices, selected="None") 
    })
    
    observeEvent(input$add_paircolor.button, {
      req(input$choose_add_paircolor)
      req(length_pair_color())
      newVal <- input$choose_add_paircolor
      updatedValues <- c(color_values, newVal)
      color_values <<- updatedValues  #super-assign operator
      for (i in 1:length_pair_color()){
        updateRadioButtons(session, paste0("pair_color.", i), choices = updatedValues)
      }
      # updateRadioButtons(session, "boxcolor.one", choices = updatedValues)
      # updateRadioButtons(session, "boxcolor.two", choices = updatedValues)
    })
    paircolor <- reactive({
      # cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
      # colors <- list("blue", "red", "green")
      cbbPalette <- c()
      req(length_pair_color())
      req(input$select_colorgroups)
      if (length_pair_color() == 1 | input$select_colorgroups == "None"){
        cbbPalette <- c("#000000")
        return(cbbPalette)
      } 
      if (length_pair_color() > 1){ 
        # for (i in 1:length_pair_color()){
        new_list <- lapply(1:length_pair_color(), function(x){
          input_value <- input[[paste0('pair_color.', x)]]  # use this input type for dynamic input
          # the following variable name of cbbPalette is a bit weird but it works
          cbbPalette <- c(cbbPalette, input_value)
          return(cbbPalette)
        })
      cbbPalette <- c(new_list)
      }
      return(cbbPalette)
    })
    
    # how many different variables are in a group
    length_pair_color <- eventReactive(input$select_colorgroups, {
      req(input$select_colorgroups)
      if (!input$select_colorgroups == "None"){
        return (length(levels(factor(pairgroups()))))
      } else (return(1))
    })
    
    output$pair_color_picker <- renderUI({
      req(length_pair_color())
      column_width = as.integer(12 / length_pair_color())
      if (length_pair_color() > 1){
          box(
            lapply(1:length_pair_color(), function(x){
              # dynamically render radio buttons
            column(column_width, 
              radioButtons(paste0("pair_color.", x), NULL, choices = color_values))}), 
            textInput("choose_add_paircolor", "Add Custom Color", placeholder = "Color in Hex Code"), 
            actionButton("add_paircolor.button", "Add Color to Palette"),
            title = "Choose Colors", solidHeader = T, collapsible = T, status = "primary", width = 12
          )
        }
    })
    ### Groups Pairwise Correlation####
    pairgroups <- reactive({
      if (input$select_colorgroups == "None"){
        return(NULL)
      } else {
        group <- metadata()[,c(input$select_colorgroups)]
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
    
    observeEvent(input$add_boxcolor.button, {
      req(input$choose_add_boxcolor)
      newVal <- input$choose_add_boxcolor
      updatedValues <- c(color_values, newVal)
      color_values <<- updatedValues  #super-assign operator
      updateRadioButtons(session, "boxcolor.one", choices = updatedValues)
      updateRadioButtons(session, "boxcolor.two", choices = updatedValues)
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
      return(group)
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
        } else if (input$downboxformat == "zip"){
          name <- "boxplots.zip"
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
    ### Color Groups Heatmap ####
    length_heatmap_color <- reactive({
      req(color_groups.heatmap())
      return (length_color <- length(levels(factor(color_groups.heatmap()))))
    })
    
    output$heatmap_color_picker <- renderUI({
      req(length_heatmap_color())
      # print(length_heatmap_color())
      # column_width = as.integer(12 / length_pair_color())
      fluidRow(column(12, h5("Select Colors")))
      if (length_heatmap_color() > 0){
          lapply(1:length_heatmap_color(), function(x){
            fluidRow(
                   radioButtons(paste0("group_color_heatmap.", x), paste0("Select color ", x), choices = color_values, inline= T))})
      }
    })
    
    # make box for UI (for some reason can't do this directly in renderUI)
    makeUIgroups <- function(input, output, session, coll, choices) {
      box(
        column(2, radioButtons("select_colorgroups.heatmap", "Color by group", choices)), 
        column(10, uiOutput("heatmap_color_picker")),
        title = "Heatmap Grouping Settings", collapsible = T, status = "primary", solidHeader = T, collapsed = coll, width = 12
      )}
    
    # observeEvent(input$select_colorgroups.heatmap, {
    #   print(heat_groups_color())
      # print(color_groups.heatmap())
    # })
    
    # renders box above
    output$groups_heatmap <- renderUI({
      data_choices <- req(colnames(metadata()))  # only add when 2 or more different values?
      choices <- c(data_choices)
      if (req(input$grouping.button)) {
        makeUIgroups(input, output, session, F, choices)
      } else {
        makeUIgroups(input, output, session, T, choices)
      }
    })
    
    # outputs the colors for the groups in a list
    heat_groups_color <- reactive({
      req(color_groups.heatmap())
      req(length_heatmap_color())
      if (length_heatmap_color() == 1){
        cbbPalette <- c(input[[paste0('group_color_heatmap.1')]])
        return(cbbPalette)
      } else 
      if (length_heatmap_color() > 1){ 
        empty_list <- c()
        new_list <- lapply(1:length_heatmap_color(), function(x){
          input_value <- input[[paste0('group_color_heatmap.', x)]]  # use this input type for dynamic input
          empty_list <- c(empty_list, input_value)
        })
        return(new_list)
      }
   
    })
    
    # returns the groups to color for the heatmap
    color_groups.heatmap <- reactive({
      group <- metadata()[,c(input$select_colorgroups.heatmap)]
      return(group)
    })

    
    ### Select Labels Heatmap ####
    output$selectlabelsheatmap <- renderUI({
      checkboxGroupInput("selectionheatmap", NULL, choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
    })
    
    observeEvent(input$heatmapselectall, {
      updateCheckboxGroupInput(session, "selectionheatmap", selected = labels()$colnames)
    })
    observeEvent(input$heatmapselectnone, {
      updateCheckboxGroupInput(session, "selectionheatmap", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames)
    })   
    
    ### Example Heatmap ####
    output$exampleheatmap <- renderPlot({
      # had to remove 'my' from 'my.breaks' in lines 423 + 424
      # createHeatmap(inputdata(), file = NULL)
   
      # req(color_groups.heatmap())
      # req(heat_groups_color())
      data_table <- inputdata()
      scale <- eval(parse(text = input$scaled.button))
      cluster_cols <- cols_cluster()
      cluster_rows <- rows_cluster()
      dendrogram_cols <- eval(parse(text=input$dendrogram_cols.button))
      dendrogram_rows <- eval(parse(text=input$dendrogram_rows.button))
      grouping <- NULL
      color_groups <- NULL
      subset <- input$selectionheatmap
      palette <- input$heatmapcolor
      # for some reason the function is called twice and during the first round not all colors are picked, resulting in a warning
      if (req(input$grouping.button)){
        grouping <- color_groups.heatmap()
        color_groups <- unlist(heat_groups_color())
      }
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
        scale <- eval(parse(text=input$scaled.button))
        cluster_cols <- cols_cluster()
        cluster_rows <- rows_cluster()
        dendrogram_cols <- eval(parse(text=input$dendrogram_cols.button))
        dendrogram_rows <- eval(parse(text=input$dendrogram_rows.button))
        grouping <- color_groups.heatmap()
        color_groups <- unlist(heat_groups_color())
        subset <- input$selectionheatmap
        palette <- input$heatmapcolor
        createHeatmap(data_table, file, scale = scale, cluster_cols = cluster_cols,
                      cluster_rows = cluster_rows, dendrogram_cols = dendrogram_cols,
                      dendrogram_rows = dendrogram_rows, grouping = grouping, color_groups = color_groups, 
                      subset = subset, palette = palette)
      }
    )
    ## Clustering ####
    ### Select Labels Clustering ####
    output$selectlabelsclustering <- renderUI({
      checkboxGroupInput("selectionclustering", NULL, choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames, selected=labels()$colnames) 
    })
    
    observeEvent(input$clusteringselectall, {
      updateCheckboxGroupInput(session, "selectionclustering", selected = labels()$colnames)
    })
    observeEvent(input$clusteringselectnone, {
      updateCheckboxGroupInput(session, "selectionclustering", choiceNames = paste(labels()$label1, labels()$label2), choiceValues=labels()$colnames)
    })   
    
    ### Example Clustering ####
    output$exampleclustering <- renderPlot({
      req(inputdata())
      req(input$selectionclustering)
      data_table <- inputdata()[input$selectionclustering]
      file <- NULL
      algorithm <- input$algorithmclustering
      n_clusters <- input$numclusters
      color_groups = NULL 
      animal_label <- input$idclustering
      meta_data <- metadata()
      print(metadata())
      #Convert to a data frame if only one variable is selected
      data_table = data.frame(data_table)
      animal = meta_data$animal
      #Perform Gaussian mixture model clustering per default
      
      if(algorithm == "GMM"){
        
        clustering = mclust::Mclust(data_table, G = n_clusters)
        
        clusters = clustering$classification
      }else{
        
        clustering = stats::kmeans(x = data_table, centers = n_clusters)
        
        clusters = clustering$cluster
      }
      
      
      #Create plots with the clustering results. 
      
      #Option 1 - only one variable was provided for clustering
      
      if(ncol(data_table) == 1){
        
        colnames(data_table) = "InputVar"
        
        p =  ggplot2::ggplot(data_table, aes(y = InputVar, x  =factor(1), color = factor(clusters))) + 
          ggplot2::geom_point(size = 2.5) +
          ggplot2::theme_test(base_size = 14) +
          ggplot2::theme(axis.text.y = ggplot2::element_text(color  ="black"),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank()) +
          ggplot2::labs(x = "", color = "cluster")
        
        
        #Add custom colors if provided by the user
        
        if(length(color_groups)>= length(unique(clusters))){
          
          p = p + ggplot2::scale_color_manual(values  =color_groups)
        }
        
        
        #Add animal labels if requested
        if(animal_label == TRUE){
          p =  p + ggrepel::geom_text_repel(aes(label= animal), min.segment.length = 0, max.overlaps = Inf, size=2)
        }
        
        #Define height and width for the plot
        h = 3
        w = 4
        
      }
      
      #Option 2 - if two variables are provided for clustering, create a scatter plot with them
      
      if(ncol(data_table) == 2){
        
        p =  ggplot2::ggplot(data_table, aes(x = data_table[,1] , y  = data_table[,2], 
                                             color = factor(clusters))) + 
          ggplot2::geom_point(size = 2.5) +
          ggplot2::theme_test(base_size = 14) +
          ggplot2::theme(axis.text = ggplot2::element_text(color  ="black")) +
          ggplot2::labs(color = "cluster", x  ="InputVar 1", y  = "InputVar 2") +
          ggplot2::stat_ellipse()
        
        
        #Add custom colors if provided by the user
        
        if(length(color_groups)>= length(unique(clusters))){
          
          p = p + ggplot2::scale_color_manual(values  =color_groups)
        }
        
        #Add animal labels if requested
        if(animal_label == TRUE){
          p =  p + ggrepel::geom_text_repel(aes(label= animal), min.segment.length = 0, max.overlaps = Inf, size=2)
        }
        
        
        #Define height and width for the plot
        h = 4
        w = 5
        
      }
      
      
      #Option 3 - if 3 or more variables are provided for clustering, a PCA using the input is shown
      
      if(ncol(data_table)>2){
        
        pca_res <- mixOmics::pca(data_table, ncomp = 2, scale = TRUE)
        
        pca_data = data.frame(pca_res$variates$X)
        pca_data$animal = rownames(data_table)
        
        var1 = round(pca_res$cum.var[1]*100,2)
        var2 =  round((pca_res$cum.var[2] - pca_res$cum.var[1])*100,2)
        
        p = ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, col = factor(clusters))) +
          ggplot2::geom_point(size=2.5) +
          ggplot2::theme_test(base_size = 14) +
          ggplot2::theme(axis.text = ggplot2::element_text(colour  = "black")) +
          ggplot2::labs(x = paste0("PC1 (", var1, "%)"), y = paste0("PC2 (", var2, "%)"),
                        col  ="cluster") +
          ggplot2::stat_ellipse()
        
        #Add custom colors if the user specified them
        
        if(length(color_groups)>= length(unique(clusters))){
          p = p + ggplot2::scale_color_manual(values = color_groups) 
        }
        
        
        #Add animal labels if true
        
        if(animal_label == TRUE){
          p =  p + ggrepel::geom_text_repel(aes(label= animal), min.segment.length = 0, 
                                            max.overlaps = Inf, size=2)
        }
        
        h = 4
        w = 5
        
      }
      return(p)
    })
    
    ### Download Clustering ####
    output$down_clustering <- downloadHandler(
      filename <- function() {
        req(input$algorithmclustering)
        (paste0(input$algorithmclustering, "_", input$numclusters, "_", "cluster.pdf"))
      }, 
      content <- function(file){
        data_table <- inputdata()[input$selectionclustering]
        # file <- NULL
        algorithm <- input$algorithmclustering
        n_clusters <- input$numclusters
        color_groups = NULL 
        animal_label <- input$idclustering
        meta_data <- metadata()
        
        clusteringAnalysis(data_table, file, algorithm, n_clusters,
                           color_groups, animal_label, meta_data)
      }  
    )
    
    
    ### Save Metadata Clustering ####
    # observeEvent(input$clustering.save, {
    output$clustering.save <- downloadHandler(
      filename <- function() {
        new_name <- sub(".xlsx$", "", basename(input$upload$name))
        return(paste0(new_name, "_clustering_", input$algorithmclustering, "_", input$numclusters, ".xlsx"))
        },
      content <- function(file){
        data_table <- inputdata()[input$selectionclustering]
        algorithm <- input$algorithmclustering
        n_clusters <- input$numclusters
        color_groups = NULL 
        animal_label <- input$idclustering
        meta_data <- metadata()
        meta_data$clustering = clusteringAnalysis(data_table, NULL, algorithm, n_clusters,
                                                  color_groups, animal_label, meta_data)
        metadata(meta_data)
        req(infile <- input$upload)
        print(file)
        
        sheet_names <- list("grand_table" = inputdata()[input$selectionclustering], 
                            "labels" = labels(), 
                            "metadata" = metadata())
        write.xlsx(sheet_names, file, rowNames = T, colNames = T)
      })
     
    
    
    ## PCA ####
    
    ### Color Groups PCA ####
   
    output$colorgroups_pca <- renderUI({
      data_choices <- req(colnames(metadata()))
      possible_choices <- c("None")
      for (col in data_choices){
        possible_choices <- c(possible_choices, col)
      }
      choices <- c(possible_choices)
      radioButtons("select_pcagroups", "Color by group", choices) 
    })
    
    # return selected group with data
    pca_groups <- reactive({
      if (input$select_pcagroups == "None"){
        return(NULL)
      } else {
        group <- metadata()[,c(input$select_pcagroups)]
        return(group)
      }
    })
    
    length_pca_color <- eventReactive(input$select_pcagroups, {
      req(input$select_pcagroups)
      if (!input$select_pcagroups == "None"){
        return (length(levels(factor(pca_groups()))))
      } else (return(0))
    })
    
    output$pca_color_picker <- renderUI({
      req(length_pca_color())
      fluidRow(column(12, h5("Select Colors")))
      if (length_pca_color() > 0){
        lapply(1:length_pca_color(), function(x){
          fluidRow(
            radioButtons(paste0("group_color_pca.", x), paste0("Select color ", x), choices = color_values, inline= T))})
      }
    })
    
    # outputs the colors for the groups in a list
    pca_groups_color <- reactive({
      req(pca_groups())
      req(length_pca_color())
      if (input$select_pcagroups == "None"){return(NULL)} 
      if (length_pca_color() == 1){
        cbbPalette <- c(input[[paste0('group_color_pca.1')]])
        return(cbbPalette)
      } else 
        if (length_pca_color() > 1){ 
          empty_list <- c()
          new_list <- lapply(1:length_pca_color(), function(x){
            input_value <- input[[paste0('group_color_pca.', x)]]  # use this input type for dynamic input
            empty_list <- c(empty_list, input_value)
          })
          return(new_list)
        }
    })
    ### Example PCA ####
    output$examplepca <- renderPlot({
      data_table <- inputdata()
      file <- NULL
      grouping <- NULL
      color_groups <- NULL
      animal_label <- input$pca_animal_id
      
      if (input$select_pcagroups != "None"){
        grouping <- pca_groups()
        color_groups <- unlist(pca_groups_color())
      }
      
      if(any(is.na(data_table)) == FALSE){
        
        pca_res = stats::prcomp(data_table, scale. = TRUE)
        
        var1 = base::round(pca_res$sdev[1]^2/base::sum(pca_res$sdev^2)*100,2)
        var2 = base::round(pca_res$sdev[2]^2/base::sum(pca_res$sdev^2)*100,2)
        
        
        pca_data = base::data.frame(pca_res$x[,1:2])
        pca_data$animal = rownames(data_table)
        
      }
      
      #Use the pca function from mixOmics if missing data are present
      #If more than 40% of the values in a variable are missing, omit the variable
      if(any(is.na(data_table)) == TRUE){
        
        #Get the percentage of missing data in each variable
        
        missing_data = base::vector()
        
        for (i in 1:ncol(data_table)){
          v = base::is.na(data_table[,i]) == TRUE
          
          missing_data = c(missing_data, length(v[v==TRUE]))
          
        }
        
        missing_data = missing_data/nrow(data_table)
        
        #Get indices of columns that contain more than 50% missing values
        ind = which(missing_data >= 0.4)
        
        #Drop the columns from the input
        
        data_table = data_table[,-ind]
        
        
        #Check if there are enough variables
        
        try(if(ncol(data_table)<3)
          stop("Not enough variables to perform PCA after removing missing values"))
        
        if(ncol(data_table)>=3){
          
          pca_res = mixOmics::pca(data_table, ncomp = 2, center = TRUE, scale = TRUE)
          
          pca_data = data.frame(pca_res$variates$X)
          pca_data$animal = rownames(data_table)
          
          var1 = round(pca_res$cum.var[1]*100,2)
          var2 =  round((pca_res$cum.var[2] - pca_res$cum.var[1])*100,2)
          
        }
        
      }
      
        #Plot the results
      
        if(ncol(pca_data) != 0){
        
        p = ggplot2::ggplot(pca_data, ggplot2::aes(x = PC1, y = PC2, col = grouping)) +
          ggplot2::geom_point(size=2.5) +
          ggplot2::theme_test(base_size = 14) +
          ggplot2::theme(axis.text = ggplot2::element_text(colour  = "black")) +
          ggplot2::labs(x = paste0("PC1 (", var1, "%)"), y = paste0("PC2 (", var2, "%)"),
                        col  ="") +
          ggplot2::stat_ellipse()
        
        #Add custom colors if the user specified them
        
        if(!is.null(grouping) & length(color_groups) >= length(unique(grouping))){
          p = p + ggplot2::scale_color_manual(values = color_groups) 
        }
        #Add animal labels if true
        if(animal_label == TRUE){
          p =  p + ggrepel::geom_text_repel(aes(label= animal), min.segment.length = 0, max.overlaps = Inf, size=2)
        }
        if(is.null(grouping)){
          h  = 3.5
          w = 4
        }else{
          h=4
          w = 5.5
        }
      }
      return (p)  
    })
    
    ### Download PCA ####
    output$down_pca <- downloadHandler(
      filename <- ("pca.pdf"), 
      content <- function(file){
        data_table <- inputdata()
        file <- file
        grouping <- NULL
        color_groups <- NULL
        animal_label <- input$pca_animal_id
        
        if (input$select_pcagroups != "None"){
          grouping <- pca_groups()
          color_groups <- unlist(pca_groups_color())
        }
        pcaAnalysis(data_table, file, grouping, color_groups, animal_label)
      }  
    )
}

shinyApp(ui, server)