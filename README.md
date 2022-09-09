# beeRapp
## Welcome to beeRapp! 
### What is beeRapp?
The BEhavioral Explorative analysis R shiny APP (beeRapp) is aimed at animal behavioral researchers without programming and data analysis background. The user-friendly app is designed to provide an easy access to fundamental analysis techniques such as clustering, boxplot visualization and pairwise comparisons, heatmaps, principal component analysis (PCA), correlation matrices and pairwise correlations. All results can be generated via the graphical user interface of the app and the user has the options to control settings such as color schemes or thresholds for statistical significance. Figures can be exported in .pdf or .pptx format.

beeRapp is written in R. Feel free to contribute if it seems that something is missing!
 
### How to use beeRapp 
To use beeRapp, your data must be stored in an .xslx file with the three following tabs (please mind that the tabs must have the names specified below):  
- __grand_table__ includes your collected data. The first column contains the IDs of the tested subjects, i.e. animals. 
The following columns contain the measured values on each behavioral variable. Values need to be numerical. If you have missing values in your data set, leave the corresponding cells empty, do not replace missing values with special characters, as this will produce an error!
The column names for the behavioral measures must only occupy one cell in the input table and they must not contain special characters. A more detailed definition of each column name can be provided in the _labels_ tab.

- __labels__ contains three columns: 'label1', 'label2' and 'colnames'. 
'colnames' corresponds to the column names of the variables in _grand_table_. The 'label1' and 'label2' columns provided a more detailed description of the measured variables that are also used in data processing and axis labeling on the generated plots. All three columns must be present in the table and column names must match between the _labels_ and _grand_table_ tables. The value in the 'label1' column corresponds to the first line of text that will be displayed on the axis labels of boxplots and correlation plots, 'label2' corresponds to the second line. Either of these values can be left empty if the variable description is not long and would fit on a single line on the plot axis.

- __meta_data__ contains further information on the the tested subjects. The first column contains the animal IDs which must match the IDs in _grand_table_. The remaining columns contain information such as group assignment, genotype, treatment, etc. Such grouping factors can be used for statistical comparisons or group annotation on the resulting plots. 
                         
An example of how the input table needs to be formatted is provided in the _example_data_ folder. These behavioral data were obtained from the following study:

```
Dopfel, D., Perez, P.D., Verbitsky, A. et al. Individual variability in behavior and
functional networks predicts vulnerability using an animal model of PTSD. Nat Commun 10,
2372 (2019). https://doi.org/10.1038/s41467-019-09926-z
```

With the .xlsx file setup as described above, you are ready to go! Select 'Import Data' on the left and upload your file. Once uploaded, you can analyse the data with the tools provided under 'Analysis'.

### Setup

Install R (we recommed at least version 4.1.3 or higher). 

Start R in your terminal or RStudio and run

```
	install.packages("shiny")
	shiny::runGitHub("beeRapp", "anmabu")
```

The first time you run these commands, all required packages will be installed if they are missing in your R environment. 
ATTENTION: running the app for the first time might take a long time (up to 30 min) untill all missing packages are installed!

After the successful setup, the app will open in your browser.
Following the initial setup, you can start the app again (this should take a couple of seconds) using the command:

```
	shiny::runGitHub("beeRapp", "anmabu")
```


### Send feedback and report issues

You can send your feedback or report issues directly to hristo.todorov@uni-mainz.de.
