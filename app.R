source("install.R", local = TRUE)

suppressPackageStartupMessages({
  library(corrplot)
  library(DT)
  library(data.table)
  library(Hmisc)
  library(ggplot2)
  library(gridExtra)
  library(officer)
  library(openxlsx)
  library(shiny)
  library(shinydashboard)
  # library(sets)
})


# Global Variables ####
color_values = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#5D3A9B")
names(color_values) = c("black", "orange", "soft blue", "lime green", "bright yellow",
                        "dark blue", "red", "pink", "violet")
# UI ####
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "beeRapp"), 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Welcome", tabName = "welcome", icon = icon("compass", lib = "font-awesome")),
        menuItem("Import Data", tabName = "importdata", icon = icon("file-medical")), 
        menuItem("Analysis", tabName = "analysis", icon = icon("biohazard"), 
                 menuSubItem("Clustering", tabName = "clustering", icon=icon("angles-right", lib="font-awesome")), 
                 menuSubItem("Boxplots", tabName = "boxplots", icon=icon("angles-right", lib="font-awesome")), 
                 menuSubItem("Heatmap", tabName = "heatmap",icon=icon("angles-right", lib="font-awesome")), 
                 menuSubItem("PCA", tabName = "pca",icon=icon("angles-right", lib="font-awesome")),
                 menuSubItem("Correlation Matrix", tabName = "correlationmatrices",icon=icon("angles-right", lib="font-awesome")),  
                 menuSubItem("Pairwise Correlations", tabName = "pairwisecorrelations",icon=icon("angles-right", lib="font-awesome")) 
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
                         shiny::h2("Welcome to beeRapp!"), 
                         shiny::h3("What is beeRapp?"),
                         shiny::p("The BEhavioral Explorative analysis R shiny APP (beeRapp) is aimed at animal behavioral researchers without 
                         programming and data analysis background. The user-friendly app is designed to provide an easy access to f
                         undamental analysis techniques such as clustering, boxplot visualization and pairwise comparisons, heatmaps, principal component analysis (PCA), 
                         correlation matrices and pairwise correlations. All results can be generated via the graphical user interface of the app and the user has the options 
                         to control settings such as color schemes or thresholds for statistical significance. Figures can be exported in .pdf or .pptx format."),
                         shiny::p("beeRapp is written in R. Visit us at", shiny::a('GitHub', href='https://github.com/anmabu/beeRapp'), "to see how to contribute."),
                         # br(),
                         shiny::h3("How to use beeRapp?"), 
                         shiny::p("To use beeRapp, your data must be stored in an .xslx file with the three following tabs (please mind that the tabs must have the names specified below):"), 
                         shiny::p(strong("grand_table"), " includes your collected data. The first column contains the IDs of the tested subjects, i.e. animals. The following columns contain the measured values on each behavioral variable.
                         Values need to be numerical. If you have missing values in your data set,  leave the corresponding cells empty, do not replace missing values with special characters, as this will produce an error!
                                  The column names for the behavioral measures must only occupy one cell in the input table and they must not contain special characters. A more detailed definition of each column name can be provided in the", strong("labels"), "tab."),
                         shiny::p(strong("labels"), " contains three columns: 'label1', 'label2' and 'colnames'. 'colnames' corresponds to the column 
                                  names of the variables in", strong("grand_table"), " . The 'label1' and 'label2' columns provided a more detailed description of the measured variables
                                  that are also used in data processing and axis labeling on the generated plots. All three columns must be present in the table and column names must 
                                  match between the", strong("labels"), " and ",  strong("grand_table"), " tables. If the description of the measured variable is not long enough, leave out the cell in 'label1' or 'label2' empty."),
                         shiny::p(strong("meta_data"), " contains further information on the the tested subjects. The first column contains the animal IDs which must match the IDs in ",
                                  strong("grand_table"), " . The remaining columns contain information such as group assignment, genotype, treatment, etc. Such grouping factors can be used for statistical comparisons or group annotation on the resulting plots."),
                         # br(),
                         
                         # br(),
                         shiny::p("An example of how the input table needs to be formatted is provided in the" , strong("example_data"), " folder on our ",
                                   shiny::a('GitHub', href='https://github.com/anmabu/beeRapp'), "page."),
                         # br(),
                         shiny::p("With the .xlsx file setup as described above, you are ready to go!
                           Select 'Import Data' on the left and upload your file. Once uploaded, you can analyse the data with the tools provided under 'Analysis'."),
 
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
                                  div(style = "display: block", downloadButton("clustering.save", "Save Clustering")),
                                  helpText("Note: Saves clustering to metadata table and downloads the updated xlsx file."),
                                  title = "Clustering Settings", width = 12, solidHeader = T, status = "primary")
                )
                ),
                fluidRow(column(6, 
                                box(
                                  div(style = "display: inline-block", downloadButton("down_clustering")),
                                  div(style = "display: inline-block", radioButtons("clustering.type", NULL, choices = c("pdf"))),
                                  plotOutput("exampleclustering"), 
                                    title = "Clustering", width = 12, solidHeader = T, status = "primary")    
                        ), 
                        column(4, 
                               box(
                                 column(4,
                                   radioButtons("idclustering", "Animal ID", choices = c("Yes" = T, "No" = F), selected = F)
                                 ),
                                 column(6, 
                                   radioButtons("colorselect.clustering", "Select custom colors", choices = c("Yes" = F, "No" = T), selected = T)
                                  ),
                                 title = "Plot Settings", width = 12, solidHeader = T, status = "primary"),
                        uiOutput("colorpicker.clustering")
                        ),
                )
        ),
        ## Boxplots ####
        tabItem(tabName = "boxplots", 
                fluidRow(
                  column(12,
                         box(
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
                           plotOutput("boxplotexample", height = 600, width = 450), 
                           title = "Example Plot", width = 12, collapsible = F, solidHeader = T, status = "primary", align = "center"
                         )
                  ), 
                  column(4, 
                         box(
                           column(4,
                                  radioButtons("boxID", "Animal ID", choiceNames = c("yes", "no"), choiceValues = c(T, F), selected = F)
                           ),
                           column(4,
                                  radioButtons("removeoutliers", "Remove Outliers?", choiceNames = c("yes", "no"), choiceValues = c(TRUE, FALSE), selected = F)
                           ),
                           column(4, 
                                  uiOutput("colorboxplot")
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
                                    title = "PCA", width = 6, solidHeader = T, status = "primary")
                         )
                )
        ),
        ## Correlation Matrix ####
        tabItem(tabName = "correlationmatrices", 
          fluidRow(
            column(12, 
                box(
                  div(style = "display:inline-block", actionButton("corrselectall", "Select All")), 
                  div(style = "display:inline-block", actionButton("corrselectnone", "Select None")),
                  div(style = "height:300px;overflow-y:scroll;width:100%", uiOutput("selectlabels")), 
                  title = "Select Variables", width = 12, collapsible = T, 
                      solidHeader = T, status = "primary"
                )
            )
          ),
          fluidRow(
            column(12, 
              box(
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
                  selectInput("corrtype", "Select Correlation Type", choices = c("pearson", "spearman", "kendall"), width = "400px"),# , "spearman", "kendall")),
                  numericInput("thresholdvalue", "Choose cutoff p-value", min = 0.001, max = 0.5, value = 0.05, step = 0.001, width="400px"),
                  downloadButton("downpaircorr", "Download"),
                  div(style= "display:inline-block", radioButtons("dfpairmatrix", "", inline = TRUE, 
                              choiceNames = c(".pdf (All plots)", ".pptx (All plots)", ".zip (Each plot seperately as .pdf)"), 
                              choiceValues = c("pdf", "pptx", "zip"))), 
                  title  = "Pairwise Correlations Settings", width = 12, collapsible = F, solidHeader = T, status = "primary"
                )
            )
          ), 
          fluidRow(
            column(6,
                box(
                  plotOutput("paircorrexample", width=500, height = 400), 
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
              uiOutput("pair_color_picker"),
            )
          )
        ) 
      )
    )
  )
)
                
         

        
# Server ####
server <- function(input, output, session) {
    source("src/script.R")
  
    ## Global Reactives ####
    # loads metadata corresponding to 'grand_table'
    metadata <- reactiveVal()
    # this way "metadata" can be updated when clustering took place
    # observeEvent(input$upload, {
    #   # substitute " " in col names with "_"
    #   value <- read.xlsx(input$upload$datapath, "meta_data", rowNames = T, colNames=T, sep.names = "_")
    # 
    #   metadata(value)
    # })
    # loads labels corresponding to 'grand_table'
    labels <- reactive({
        req(infile <- input$upload)
        dat <- openxlsx::read.xlsx(infile$datapath, "grand_table", rowNames = T, colNames = TRUE, sep.names = "_")
        value <- openxlsx::read.xlsx(infile$datapath, "labels", colNames = TRUE, sep.names = "_")
        #Substitute NA values in labels with a string
        value$label1 <- ifelse(is.na(value$label1), " ", value$label1)
        value$label2 <- ifelse(is.na(value$label2), " ", value$label2)
        
        # value["colnames"] <- gsub(" ", "_", value["colnames"])
        for (i in 1:nrow(value)){ # substitute whitespace in colnames to match colnames in grand_table
          value[i, "colnames"] <- gsub(" ", "_", value[i, "colnames"])
          # print(labels[i, "colnames"])
        }
        
        #Check if the order of the columns in data_table matches the order of the column names in the labels table and reorder if not
        #Only done if dimensions between the tables match and all column names are contained in both tables and only the order is wrong
        
        vec <- colnames(dat) == value$colnames
        
        if(all(vec) == FALSE & ncol(dat) == nrow(value) & all(value$colnames %in% colnames(dat))){
            value = value[base::match(colnames(dat), value$colnames),]
            
        }
        if(all(vec) == FALSE & (ncol(dat) != nrow(value) | !all(value$colnames %in% colnames(dat)))){
            #Produce an error message: "Please make sure that the column names in the data table match the value in the value table")
            showModal(modalDialog(
                title = "Input Error", 
                "Please make sure that the column names in the data table match the labels in the labels table.", 
                easyClose = T
            ))
        }
        
        return(value)
    })
    
    # Load 'grand_table' Data and Evaluate completeness. 
    inputdata <- reactive({
        req(infile <- input$upload)
        dat <- openxlsx::read.xlsx(infile$datapath, "grand_table", rowNames = T, colNames = TRUE, sep.names = "_")
        meta_data <- openxlsx::read.xlsx(infile$datapath, "meta_data", colNames = TRUE, sep.names = "_", rowNames = T)
 
        # validate order of labels and meta_data
        #SANITY CHECKS
        
        #1. The meta_data table should contain all animal id's as in the data_table. If not the case, an error message should be produced
        
        if(!all(rownames(meta_data) %in% rownames(dat)) | !all(rownames(dat) %in% rownames(meta_data))){
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
        
        vec <- rownames(dat) == rownames(meta_data) 
        
        
        if(all(vec) == FALSE  & nrow(dat) == nrow(meta_data) & all(rownames(meta_data) %in% rownames(dat))){
          if(ncol(meta_data)>1){
              meta_data = meta_data[base::match(rownames(dat), rownames(meta_data)),]
              metadata(meta_data)
          }else{
              varname = colnames(meta_data)[1]
              meta_data = meta_data[base::match(rownames(dat), rownames(meta_data)),]
              meta_data = data.frame(meta_data)
              colnames(meta_data) = varname
              rownames(meta_data) = rownames(dat)
              metadata(meta_data)
          }

        }
        
        if(all(vec) == FALSE & (nrow(dat) != nrow(meta_data) | !all(rownames(meta_data) %in% rownames(dat)))){
          #Produce an error message: "Please make sure that the animal IDs match in the data and meta data tables")
          showModal(modalDialog(
            title = "Input Error", 
            "Please make sure that the animal IDs match in the data and meta data tables.", 
            easyClose = T
          ))
        }
        


        # validate that all values are numeric
        for (i in dat) {
          if (class(i) != "numeric"){
            showModal(modalDialog(
              title = "Value Error", 
              "Not all data in 'grand_table' seem to be numeric. 
              Please make sure, that all values in 'grand_table' are numbers.", 
              easyClose = T
            ))
          }
        }
        # returns the data if all tests are passed
        return(dat)
    })
    
    # Display Data when loaded correctly
    output$datatable <- renderDataTable({
      datatable(
        inputdata()[, 1:ncol(inputdata())],
        extensions = c('Scroller'),
        options = list(scrollY = 400,
                       deferRender = TRUE,
                       scroller = TRUE,
                       paging = TRUE,
                       dom = 'lBfrtip',
                       fixedColumns = FALSE),
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
      pairwiseCorrelations.example(data_table, labels, format, type, threshold, grouping = grouping, color_groups = color_groups, animal_label = animal_label)
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
    })
    paircolor <- reactive({
      cbbPalette <- c()
      req(length_pair_color())
      req(input$select_colorgroups)
      if (length_pair_color() == 1 | input$select_colorgroups == "None"){
        cbbPalette <- c("#000000")
        return(cbbPalette)
      } 
      if (length_pair_color() > 1){ 
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
        return(paste0("pair_corr_plots.", input$dfpairmatrix))
      },
      content <- function(file) { 
        format <- input$dfpairmatrix
        data_table <- inputdata()
        labels <- labels()
        type <- input$corrtype
        threshold <- input$thresholdvalue
        grouping <- pairgroups()
        color_groups <- unlist(paircolor())
        subset <- NULL
        animal_label <- input$pairID
        pairwiseCorrelations(file, data_table, labels, format, type, threshold, grouping = grouping, color_groups = color_groups, animal_label = animal_label)
      }
    )
    
    ## Boxplots ####
    ### Example Plot Boxplots ####
    output$boxplotexample <- renderPlot({
      data_table <- inputdata()
      labels <- labels()
      file <- NULL
      test <- boxtesttype()
      threshold <- input$compthresholdvalue
      grouping <- boxplotgroups()
      color_groups <- unlist(boxplotcolor())
      remove_outliers <- input$removeoutliers
      subset <- NULL
      animal_label <- input$boxID
      
      pairwiseComparisons.example(data_table, labels, file, format, test, threshold, grouping, color_groups, remove_outliers, subset, animal_label)
    })
    
    ### Reactives Boxplots ####
    boxtesttype <- reactive({
      comp <- req(input$comptype)
      if (comp == "wilcoxon"){ 
        comp = "wilcox"
      } else { 
        comp = "t.test"
      }
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
        max_group <- max(as.numeric(factor(group)))  # converts grouping var into number
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
        return(paste0("boxplots.", input$downboxformat))
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
      if (as.logical(input$dendrogram_cols.button) | as.logical(input$cluster_cols.button)) {
        updateRadioButtons(session, "cluster_cols.button", selected = T)
        return(T)
      } else {return(F)}
    })
    
    rows_cluster <- reactive({
      if (as.logical(input$dendrogram_rows.button) | as.logical(input$cluster_rows.button)) {
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
      data_table <- inputdata()
      file <- NULL
      scale <- as.logical(input$scaled.button)
      cluster_cols <- cols_cluster()
      cluster_rows <- rows_cluster()
      dendrogram_cols <- as.logical(input$dendrogram_cols.button)
      dendrogram_rows <- as.logical(input$dendrogram_rows.button)
      grouping <- NULL
      color_groups <- NULL
      subset <- input$selectionheatmap
      palette <- input$heatmapcolor
      # for some reason the function is called twice and during the first round not all colors are picked, resulting in a warning
      if (as.logical(input$grouping.button)){
        grouping <- color_groups.heatmap()
        color_groups <- unlist(heat_groups_color())
      }
      createHeatmap.example(data_table, file, scale = scale, cluster_cols = cluster_cols,
                    cluster_rows = cluster_rows, dendrogram_cols = dendrogram_cols,
                    dendrogram_rows = dendrogram_rows, grouping = grouping, color_groups = color_groups, 
                    subset = subset, palette = palette)
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
    
    ### Color Clustering ####
    output$colorpicker.clustering <- renderUI({
      box(
        if (input$numclusters > 0){
          lapply(1:input$numclusters, function(x){
            column_width <- as.integer(12 / input$numclusters)
            column(column_width,
              radioButtons(paste0("group_color_clustering.", x), paste0("Select color ", x), choices = color_values))})
        },
        textInput("choose_add_clustercolor", "Add Custom Color", placeholder = "Color in Hex Code"), 
        actionButton("add_clustercolor.button", "Add Color to Palette"),
        title = "Choose Colors", width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = as.logical(input$colorselect.clustering)
      )
    })
    
    # outputs the colors for the groups in a list
    clustering_groups_color <- reactive({
      if (input$numclusters == 1){
        cbbPalette <- c(input[[paste0('group_color_clustering.1')]])
        return(cbbPalette)
      } else 
        if (input$numclusters > 1){ 
          empty_list <- c()
          new_list <- lapply(1:input$numclusters, function(x){
            input_value <- input[[paste0('group_color_clustering.', x)]]  # use this input type for dynamic input
            empty_list <- c(empty_list, input_value)
          })
          return(new_list)
        }
    })
    
    observeEvent(input$add_clustercolor.button, {
      req(input$choose_add_clustercolor)
      newVal <- input$choose_add_clustercolor
      updatedValues <- c(color_values, newVal)
      color_values <<- updatedValues  #super-assign operator
      for (i in 1:input$numclusters){
        updateRadioButtons(session, paste0("group_color_clustering.", i), choices = updatedValues)
      }
    })
    
    ### Example Clustering ####
    output$exampleclustering <- renderPlot({
      req(inputdata())
      req(input$selectionclustering)
      data_table <- inputdata()[input$selectionclustering]
      file <- NULL
      algorithm <- input$algorithmclustering
      n_clusters <- input$numclusters
      color_groups <- NULL 
      animal_label <- input$idclustering
      meta_data <- metadata()
      
      if (!as.logical(input$colorselect.clustering)) {
        color_groups <- unlist(clustering_groups_color())
      }
      
      clusteringAnalysis.example(data_table, file, algorithm, n_clusters,
                         color_groups, animal_label, meta_data)
    })
    
    ### Download Clustering ####
    output$down_clustering <- downloadHandler(
      filename <- function() {
        req(input$algorithmclustering)
        (paste0(input$algorithmclustering, "_", input$numclusters, "_", "cluster.pdf"))
      }, 
      content <- function(file){
        data_table <- inputdata()[input$selectionclustering]
        algorithm <- input$algorithmclustering
        n_clusters <- input$numclusters
        color_groups = NULL 
        animal_label <- input$idclustering
        meta_data <- metadata()
        
        if (!as.logical(input$colorselect.clustering)) {
          color_groups <- unlist(clustering_groups_color())
        }
        
        clusteringAnalysis(data_table, file, algorithm, n_clusters,
                           color_groups, animal_label, meta_data)
      }  
    )
    
    
    ### Save Metadata Clustering ####
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
        meta_data$clustering = as.character(clusteringAnalysis(data_table, NULL, algorithm, n_clusters,
                                                  color_groups, animal_label, meta_data))
        metadata(meta_data)
        req(infile <- input$upload)
        
        sheet_names <- list("grand_table" = data.frame("ID" = rownames(inputdata()),inputdata()), 
                            "labels" = labels(), 
                            "meta_data" = data.frame("ID" = rownames(metadata()),metadata()))
        write.xlsx(sheet_names, file, rowNames = F, colNames = T)
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
      pcaAnalysis.example(data_table, file, grouping, color_groups, animal_label)
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
