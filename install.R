# source of this script:
#    Title: beeRapp
#    Author: anmabu
#    Date: 25 July 2022
#    Availability: https://github.com/anmabu/beeRapp


################################################################################
# Check that the currently-installed version of R
# is the correct version
################################################################################
#R_min_version = "4.1.3"
#R_version = paste0(R.Version()$major, ".", R.Version()$minor)
#if(compareVersion(R_version, R_min_version) == -1){
 #stop("You need to have at least version 4.1.3 of R to run the app.\n", 
  #    "Launch should fail.\n",
   #   "Go to http://cran.r-project.org/ and install version 4.1.3 of R or higher.")
#}
#
#Check if BiocManager is installed and install otherwise
availpacks = .packages(all.available = TRUE)
if (!("BiocManager" %in% availpacks)){
  install.packages("BiocManager")
  
}

################################################################################
# Install basic required packages if not available/installed.
################################################################################
install_missing_packages = function(pkg, version = NULL, verbose = TRUE){
  availpacks = .packages(all.available = TRUE)
  require("BiocManager")
  missingPackage = FALSE
  if(!any(pkg %in% availpacks)){
    if(verbose){
      message("The following package is missing.\n",
              pkg, "\n",
              "Installation will be attempted...")
    }
    missingPackage <- TRUE
  }
  if(!is.null(version) & !missingPackage){
    # version provided and package not missing, so compare.
    if( compareVersion(a = as.character(packageVersion(pkg)),
                       b = version) < 0 ){
      if(verbose){
        message("Current version of package\n", 
                pkg, "\t", 
                packageVersion(pkg), "\n",
                "is less than required.
                Update will be attempted.")
      }
      missingPackage <- TRUE
    }
  }
  if(missingPackage){
      BiocManager::install(pkg, update=FALSE)
  }
}
################################################################################
# Define list of package names and required versions.
################################################################################
deppkgs = c(corrplot = "0.92",
            DT = "0.23",
            data.table = "1.14.2", 
            Hmisc = "4.7-0", 
            ggplot2 = "3.3.6", 
            gridExtra = "2.3", 
            officer = "0.4.2", 
            openxlsx = "4.2.5", 
            shiny = "1.7.1", 
            shinydashboard = "0.7.2",
            sets = "1.0-21",
            ggpubr = "0.4.0", 
            pheatmap = "1.0.12",
            mclust = "5.4.10", 
            mixOmics = "6.18.1", 
            ggrepel = "0.9.1",
            stringr = "1.4.0",
            coin = "1.4.2",
            NbClust = "3.0.1",
            grDevices = "4.2.1",
            mice = "3.14.0",
            rmarkdown = "2.16"
            )

# Loop on package check, install, update
pkg1 = mapply(install_missing_packages,
              pkg = names(deppkgs), 
              version = deppkgs,
              MoreArgs = list(verbose = TRUE), 
              SIMPLIFY = FALSE,
              USE.NAMES = TRUE)
################################################################################
# Load packages 
################################################################################
for(i in names(deppkgs)){
  library(i, character.only = TRUE)
  message(i, " package version:\n", packageVersion(i))
}
################################################################################
