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
#SANITY CHECKS

#1. The data_table contains the animal id as row names. Check if they are in the same order as in the meta_data table

vec <- rownames(data_table) == meta_data$animal 
#The following command should return true
all(vec) == TRUE

#2. The lables table contains the description of the behavioural variables (columns in the data_table). Check if the colnames are 
#All present in the labels table

vec <- colnames(data_table) == labels$colnames

#The following command should return true
all(vec) == TRUE

#
################################################################################################################
#CORRELATION MATRIX ALL VALIABLES
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




##############################################################################
#Function to calculate pairwise correlations (all or only significant, controlled by the threshold variable)
#ARGUMENTS
#data_table - the input data frame 
#labels - data frame with labels
#outDir - the output directory where the file will be saved
#filename - name of the output file
#format -  the file format for the output, per default a pdf with the correlations is created, optionally, plots can be saved in a power point file
#type - type of the correlation, by default "pearson", can be changed to "spearman" or "kendall" for non-parametric correlation
#threshold - the cutoff p-value for considering a correlation to be statistically significant, by default 0.05
#grouping - a grouping variable cab be included to color data points according to a group assignment
#color_groups - a vector with as many values as levels of the "grouping" argument
#subset - a vector containing column indices in case the user once to calculate the matrix for a subset of the data

pairwiseCorrelations <- function(file, data_table, labels, format = ".pdf", 
                                 type = "pearson", threshold = 0.05, 
                                 grouping = NULL, color_groups = NULL, 
                                 subset = NULL){
  
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
  }
  if(format == ".pptx"){
    print(doc, target = file) 
  } else {
    dev.off()}
}

