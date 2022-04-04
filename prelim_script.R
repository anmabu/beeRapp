#BEHAVIORAL ANALYSIS
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(pheatmap)
library(mclust)
library(openxlsx)
library(mice)
# library(ggpubr)
library(nlme)
library(lme4)
library(emmeans)
library(car)
library(gam)
# library(mixOmics)
library(clustvarsel)
library(PMCMRplus)
library(ggrepel)
library(officer)
# install.packages("mixOmics", dependencies = T)
# install.packages("curl", dependencies = T)

###########################################################################
#                              IMPORT DATA
#--------------------------------------------------------------------------
#Either set directory to the folder containing the files or write the full directory to the files
#setwd("/Directory/To/Files)

setwd("/mnt/sda2/Dokumente/Bioinformatik/6.Semester/Behavior_GUI")
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

cormat <- rcorr(as.matrix(data_table[,1:ncol(data_table)]), type = "pearson")
cormat_r <- cormat$r
cormat_r[cormat$P > 0.05] <- 0

#Export correlation matrix
pdf("cormat.pdf",height = 10, width = 11)
corrplot(cormat_r, tl.col = "black", tl.srt = 60, tl.cex = 0.6, mar = c(0,0,2,2))
dev.off()


##############################################################################
#Calculate pairwise correlations (all or only significant, controlled by the threshold variable)
library(officer)
threshold = 0.0001

doc <- read_pptx()
for (i in 1:(ncol(data_table)-1)){
  for (j in (i+1):ncol(data_table)){
    
    #Calculate correlations 
    p.val <- cor.test(data_table[,i], data_table[,j])$p.value
    r <- round(cor.test(data_table[,i], data_table[,j])$estimate,2)
    
    #Create a plot if the correlation is significant
    if(p.val<=threshold){
      
      x_lab <- paste(labels$label1[i], 
                     labels$label2[i], sep ="\n")
      
      y_lab <- paste(labels$label1[j], 
                     labels$label2[j], sep ="\n")
      
      p <- ggplot(data_table, 
                  aes(x = data_table[,i], 
                      y = data_table[,j])) +
        geom_point(size=2.5) +
        stat_smooth(formula = "y~x", method = "lm", se = F, color="black") +
        labs(col="", x = x_lab, y = y_lab) +
        theme_test(base_size=14) + theme(axis.text=element_text(size=12, color="black")) 
      
      ymax = layer_scales(p)$y$range$range[2]
      ymin = layer_scales(p)$y$range$range[1]
     
       p <- p + ylim(ymin, 1.4*ymax)
      xcoord <- layer_scales(p)$x$range$range[2] - (layer_scales(p)$x$range$range[2] - layer_scales(p)$x$range$range[1])/2
      
      if(p.val<0.001){
        lab1 <- paste0("r=",  r, ", p<0.001")
      }else{
        lab1 <- paste0("r=",  r, ", p=" ,round(p.val,3))
      }
      
      p <- p+ annotate(geom = "text", x = xcoord, y = 1.2*ymax,
                       label=lab1, fontface=3, size=4.5)
      
      
      doc <- add_slide(doc)
      doc <- ph_with(x = doc, p, ph_location(type="body",width=6, height=4.5), res=600)
    }
    
  }
}

print(doc, target = "pairwise_cor_plots.pptx")

