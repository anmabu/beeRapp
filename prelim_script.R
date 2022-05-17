#BEHAVIORAL ANALYSIS
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(pheatmap)
library(mclust)
library(openxlsx)
library(mice)
library(ggpubr)
library(nlme)
library(lme4)
library(emmeans)
library(car)
library(gam)
library(mixOmics)
library(clustvarsel)
library(PMCMRplus)
library(ggrepel)
library(officer)

###########################################################################
#                              IMPORT DATA
#--------------------------------------------------------------------------
#Either set directory to the folder containing the files or write the full directory to the files
#setwd("/Directory/To/Files)


data_table  <- read.xlsx("grand_table.xlsx", colNames = T, sheet =  "grand_table", rowNames = T)
labels <- read.xlsx("grand_table.xlsx", colNames = T, sheet =  "labels")
meta_data <- read.xlsx("grand_table.xlsx", colNames = T, sheet =  "meta_data") 

#################################################################################################################
#SANITY CHECKS ####

#1. The data_table contains the animal id as row names. Check if they are in the same order as in the meta_data table

vec <- rownames(data_table) == meta_data$animal 
#The following command should return true
all(vec) == TRUE

#2. The lables table contains the description of the behavioural variables (columns in the data_table). Check if the colnames are 
#All present in the labels table

vec <- colnames(data_table) == labels$colnames

#The following command should return true
all(vec) == TRUE

# Correlation Matrix all Variables ####
#Function to calculate a correlation matrix for the behavioural data matrix

#ARGUMENTS
#data_table - the input data frame 
#outDir - the output directory where the file will be saved
#filename - name of the output file
#type - type of the correlation, by default "pearson", can be changed to "spearman" for non-parametric correlation
#threshold - the cutoff p-value for considering a correlation to be statistically significant, by default 0.05
#subset - a vector containing column indices in case the user once to calculate the matrix for a subset of the data


correlationMatrix <- function(data_table, outDir, 
                              filename, type = "pearson", 
                              threshold  = 0.05, subset = NULL){
  
  if(!is.null(subset)){
    cormat = Hmisc::rcorr(as.matrix(data_table[,subset]), type = type) 
  }else{
    cormat = Hmisc::rcorr(as.matrix(data_table[,1:ncol(data_table)]), type = type)
  }
  
  #Extract the correlation coefficients
  cormat_r = cormat$r
  #Set non-significant coefficients to 0
  cormat_r[cormat$P > threshold] = 0
  
  #Export correlation matrix
  if(ncol(cormat_r) < 10){
    height = 5
    width = 6
    font_size = 1
  }
  
  if(ncol(cormat_r) > 10 & ncol(cormat_r) < 50){
    height = 7
    width = 8
    font_size = 0.8
  }
  
  if(ncol(cormat_r)>=50 & ncol(cormat_r)<90){
    height = 9
    width = 10
    font_size=0.7
  }
  
  if(ncol(cormat_r)>=90 ){
    height = 10
    width = 11
    font_size=0.6
  }
  
  # pdf(paste0(outDir, "/", filename, ".pdf"), height = height, width = width)
  corrplot::corrplot(cormat_r, tl.col = "black", tl.srt = 60, tl.cex = font_size, mar = c(0,0,2,2))
  # dev.off()
  
  
}



# Pairwise Correlation ####
#Function to calculate pairwise correlations (all or only significant, controlled by the threshold variable)
#ARGUMENTS
#file - name of the output file
#data_table - the input data frame 
#labels - data frame with labels
#format -  the file format for the output, per default a pdf with the correlations is created, optionally, plots can be saved in a power point file
#type - type of the correlation, by default "pearson", can be changed to "spearman" or "kendall" for non-parametric correlation
#threshold - the cutoff p-value for considering a correlation to be statistically significant, by default 0.05
#grouping - a grouping variable cab be included to color data points according to a group assignment
#color_groups - a vector with as many values as levels of the "grouping" argument
#subset - a vector containing column indices in case the user once to calculate the matrix for a subset of the data

pairwiseCorrelations <- function(file, data_table, labels, format = ".pdf", 
                                 type = "pearson", threshold = 0.05, 
                                 grouping = NULL, color_groups = NULL, 
                                 subset = NULL, 
                                 animal_label = FALSE){
  
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
  for (i in 1:(ncol(data_table)-1)){
  # for (i in 1:5){  
    # move progress bar
    progress$inc(1/(ncol(data_table)-1))
    # progress$inc(1/5)
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
          # cat(grouping)
          grouping = factor(grouping)
          p = p + geom_point(size  = 2.5, aes(col = grouping)) 
          if(!is.null(color_groups) & length(color_groups) >= length(unique(grouping))){
            p <- p + scale_color_manual(values = color_groups)
          }
        }
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
        p = p+ ggplot2::annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                                 label=lab1, fontface=3, size=4.5)
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
    # clean up pdf files in working directory after zipping together
    for (elem in fs){
      system(paste("rm -f", elem)) 
    }
  }
  if(format == ".pptx"){
    print(doc, target = file) 
  } else {
    dev.off()}
}



# Boxplot #### 
#Function to statistically compare two groups and show results as a boxplot (all or only significant, controlled by the threshold variable)
#ARGUMENTS
#data_table - the input data frame 
#labels - data frame with labels
#file - file name including directory
#format -  the file format for the output, per default a pdf with the correlations is created, optionally, plots can be saved in a power point file
#test - the pairwise statistical test, per default a Wilcoxon test ("wilcox") is used, alternatively a t.test can be performed ("t.test")
#threshold - the cutoff p-value for considering a result to be statistically significant, by default 0.05
#grouping - a grouping variable cab be included to color data points according to a group assignment
#color_groups - a vector with as many values as levels of the "grouping" argument
#remove_outliers - logical, per default set to FALSE, if set to true, outliers in both groups will be removed
#Outliers are evaluated based on z-scores with a two-sided p-value <0.001. Recommended to set to TRUE if test = "t.test"
#subset - a vector containing column indices in case the user once to calculate the matrix for a subset of the data
#animal_label - an argument set to FALSE, if TRUE, animal labels are shwon next to the data points

pairwiseComparisons <- function(data_table, labels, file, format = "pdf", 
                                test = "wilcox", threshold = 0.05, 
                                grouping, color_groups = NULL,
                                remove_outliers = FALSE,
                                subset = NULL,
                                animal_label = FALSE){
  
  #Perform the analysis only for a subset of variables 
  if(!is.null(subset)){
    data_table = data_table[,subset]
    labels = labels[subset,]
  }
  
  #Determine the output format selected by the user 
  if(format == "pdf"){
    pdf(file, height=4, width = 3)
  }
  
  if(format == "pptx"){
    doc = officer::read_pptx()
  }
  if(format == "zip"){   
    fs <- c()
  }
  
  #Change the grouping variable to a factor
  grouping = factor(grouping)
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Creating plots", value = 0)
  
  for (i in 1:ncol(data_table)){
    # move progress bar
    progress$inc(1/(ncol(data_table)-1))
    
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
      if (format == "zip"){
        # y_lab <- gsub("%", "percent", y_lab)
        # y_lab <- gsub("/", "per", y_lab)
        fs <- c(fs, paste0(labels$colnames[i], ".pdf"))
        pdf(paste0(labels$colnames[i], ".pdf"), height = 4, width = 3)
        print(p)
        dev.off()
      }
      
      if(format == "pptx"){
        doc = officer::add_slide(doc)
        doc = officer::ph_with(x = doc, p, ph_location(type="body",width=3, height=4), res=600)
      }else{print(p)}
      
    }
    
  }
  if (format == "zip"){
    zip(zipfile = file, files = fs)
    # clean up pdf files in working directory after zipping together
    for (elem in fs){
      system(paste("rm -f", elem)) 
    }
  }
  if(format == "pptx"){
    print(doc, target = file) 
  }else(dev.off())
}

# Heatmap ####

#FUNCTION TO CREATE A HEATMAP OF THE INPUT DSTS TABLE
#ARGUMENTS
#data_table - the input data frame 
#file - the output directory and file name to save the output
#scale = TRUE: an argument to specify if the values are centered and scaled (z-scores). Per default true to account for different scales of the input data
#cluster_cols = FALSE: specify whether a hierarchical clustering of columns should be applied
#cluster_rows = TRUE: specifiy whetehr a hierarchical clusterinf of rows should be applied
#dendrogram_cols = FALSE: specify whether the dendrogram of the columns should be shown
#dendrogram_rows = TRUE: specif whether the dendrogram with clustering of rows should be shown
#grouping = NULL - an optional argument to annotate groups that animals belong to
#color_groups = NULL - an optional argument to specify the colors for the annotation 
#subset - a vector containing column indices in case the user once to calculate the heatmap for a subset of the data
#palette - color palette for the heatmap. Recommended values include:
#Diverging colors: "RdBu", "Blue-Red", Cyan-Magenta, "PRGn", "Tropic", "PuOr", "PiYG"
#Sequential colors: "Blue-Yellow", "Teal", "Sunset", "Viridis"
createHeatmap <- function(data_table, file, scale = TRUE, cluster_cols = FALSE, 
                          cluster_rows  =TRUE, dendrogram_cols  =FALSE,
                          dendrogram_rows = TRUE, grouping = NULL,
                          color_groups = NULL, subset = NULL,
                          palette = "RdBu"){
  
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
  
  pdf(file, height = height, width = width)
  #Create the heatmap
  # if (is.null(file)){
    # return(
  pheatmap::pheatmap(data_table, cluster_rows = cluster_rows, cluster_cols = cluster_cols,
                     treeheight_row = treeheight_row, treeheight_col = treeheight_col,
                     fontsize= font_size, angle_col = 315, na_col = "gray45",
                     color = colors, breaks = breaks,
                     annotation_row = annotation, annotation_names_row = FALSE,
                     annotation_colors = annot_colors)
    # )}
  dev.off()
  
  
}

# PCA ####
#FUNCTION TO PERFORM Principal Component Analysis (PCA) with the input data

#ARGUMENTS
#data_table - the input data frame 
#file - the output directory and file name to save the output
#grouping = NULL - an optional argument to annotate groups that animals belong to
#color_groups = NULL - an optional argument to specify the colors for the annotation 
#animal_label - an argument set to FALSE, if TRUE, animal labels are shown next to the data points

pcaAnalysis <- function(data_table, file, grouping = NULL, color_groups = NULL, 
                        animal_label = FALSE){
  
  
  #Check if there are missing data. If not, use the standard prcomp function
  
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
    print(typeof(file))
    pdf(file, height = h, width= w)
    # p
    print(p)
    dev.off()
    
  }
  
  
}


# Clustering ####
#FUNCTION TO PERFORM CLUSTERING BASED ON SELECTED VARIABLES FROM THE INPUT DATA MATRIX

#ARGUMENTS
#data_table - the input data frame 
#file - the output directory and file name to save the output visualizing the clustering
#algorithm = "GMM" - per default, Gaussian mixture model clustering is performed, the user can also select "kmeans" 
#n_clusters = 2, per default, at least 2 clusters are produced, the user can specify a higher number of clusters desired
#color_groups = NULL - an optional argument to specify the colors for the produced clusters
#animal_label - an argument set to FALSE, if TRUE, animal labels are shown next to the data points
#meta_data - the table containing the meta data - used to extract the animal IDs

clusteringAnalysis <- function(data_table, file, algorithm  = "GMM", n_clusters=2,
                               color_groups = NULL, animal_label  =FALSE,
                               meta_data){
  
  #Convert to a data frame if only one varible is selected
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
  pdf(file, height = h, width = w)
  print(p)
  dev.off()
  
  return(clusters)
}


#Add the clustering variable to the meta_data
# meta_data$clustering = clusteringAnalysis(data_table, file, meta_data = meta_data)



