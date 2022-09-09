# beeRapp
## Welcome to beeRapp! 
### What is beeRapp?
The BEhavioral Explorative analysis R shiny APP (beeRapp) is aimed at animal behavioral researchers without programming and data analysis background. The user-friendly app is designed to provide an easy access to fundamental analysis techniques such as clustering, boxplot visualization and pairwise comparisons, heatmaps, principal component analysis (PCA), correlation matrices and pairwise correlations. All results can be generated via the graphical user interface of the app and the user has the options to control settings such as color schemes or thresholds for statistical significance. Figures can be exported in .pdf or .pptx format.

beeRapp is written in R. Feel free to contribute if it seems that something is missing!


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
 
### How to use beeRapp 
To demonstrate the functionality of beeRapp, we will use behavioral data obtained from the following study:

```
Dopfel, D., Perez, P.D., Verbitsky, A. et al. Individual variability in behavior and
functional networks predicts vulnerability using an animal model of PTSD. Nat Commun 10,
2372 (2019). https://doi.org/10.1038/s41467-019-09926-z
```

Dopfel et al. investigzated freezing responses of rats exposed to a predator scent (fox urine) compared to a control group, as a model for post-traumatic stress disorder. The data set can be found in the _example data_ folder. 

The input data must be stored in an .xslx file with the three following tabs (please mind that the tabs must have the names specified below): 

- __grand_table__ includes your collected data. The first column contains the IDs of the tested subjects, i.e. animals. 
The following columns contain the measured values on each behavioral variable. Values need to be numerical. If you have missing values in your data set, leave the corresponding cells empty, do not replace missing values with special characters, as this will produce an error!
The column names for the behavioral measures must only occupy one cell in the input table and they must not contain special characters. A more detailed definition of each column name can be provided in the _labels_ tab. 

The grand table for the Dopfel et al. data set can be seen below.
<img width="1026" alt="image" src="https://user-images.githubusercontent.com/73937893/189454890-7aefdce4-d9c4-4265-b65a-fdfa9d464481.png">

- __labels__ contains three columns: 'label1', 'label2' and 'colnames'. 
'colnames' corresponds to the column names of the variables in _grand_table_. The 'label1' and 'label2' columns provided a more detailed description of the measured variables that are also used in data processing and axis labeling on the generated plots. All three columns must be present in the table and column names must match between the _labels_ and _grand_table_ tables. The value in the 'label1' column corresponds to the first line of text that will be displayed on the axis labels of boxplots and correlation plots, 'label2' corresponds to the second line. Either of these values can be left empty if the variable description is not long and would fit on a single line on the plot axis.

<img width="1026" alt="image" src="https://user-images.githubusercontent.com/73937893/189455448-2c3ce4e2-1ed8-458e-a190-474a2fa9172a.png">

- __meta_data__ contains further information on the the tested subjects. The first column contains the animal IDs which must match the IDs in _grand_table_. The remaining columns contain information such as group assignment, genotype, treatment, etc. Such grouping factors can be used for statistical comparisons or group annotation on the resulting plots. 

<img width="612" alt="image" src="https://user-images.githubusercontent.com/73937893/189455564-c1d0ec78-6f0a-4ca2-9c6c-aa035e1ffc96.png">                         


### Send feedback and report issues

You can send your feedback or report issues directly to hristo.todorov@uni-mainz.de.
