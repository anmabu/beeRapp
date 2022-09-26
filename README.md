# beeRapp
## Welcome to beeRapp! 
### What is beeRapp?
The BEhavioral Explorative analysis R shiny APP (beeRapp) is aimed at animal behavioral researchers without programming and data analysis background. The user-friendly app is designed to provide an easy access to fundamental analysis techniques such as clustering, boxplot visualization and pairwise comparisons, heatmaps, principal component analysis (PCA), correlation matrices and pairwise correlations. All results can be generated via the graphical user interface of the app and the user has the options to control settings such as color schemes or thresholds for statistical significance. Figures can be exported in .pdf or .pptx format.

beeRapp is written in R. Feel free to contribute if it seems that something is missing!


## Setup

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
 
## How to use beeRapp 
To demonstrate the functionality of beeRapp, we will use behavioral data obtained from the following study:

```
Dopfel, D., Perez, P.D., Verbitsky, A. et al. Individual variability in behavior and
functional networks predicts vulnerability using an animal model of PTSD. Nat Commun 10,
2372 (2019). https://doi.org/10.1038/s41467-019-09926-z
```

Dopfel et al. investigzated freezing responses of rats exposed to a predator scent (fox urine) compared to a control group, as a model for post-traumatic stress disorder. The data set can be found in the _example data_ folder. 

### Formatting the input data

The input data must be stored in an .xslx file with the three following tabs (please mind that the tabs must have the names specified below): 

- __grand_table__ includes your collected data. The first column contains the IDs of the tested subjects, i.e. animals. 
The following columns contain the measured values on each behavioral variable. Values need to be numerical. If you have missing values in your data set, leave the corresponding cells empty, do not replace missing values with special characters, as this will produce an error!
The column names for the behavioral measures must only occupy one cell in the input table and they must not contain special characters. A more detailed definition of each column name can be provided in the _labels_ tab. 

The grand table for the Dopfel et al. data set can be seen below.
<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189454890-7aefdce4-d9c4-4265-b65a-fdfa9d464481.png">

- __labels__ contains three columns: 'label1', 'label2' and 'colnames'. 
'colnames' corresponds to the column names of the variables in _grand_table_. The 'label1' and 'label2' columns provided a more detailed description of the measured variables that are also used in data processing and axis labeling on the generated plots. All three columns must be present in the table and column names must match between the _labels_ and _grand_table_ tables. The value in the 'label1' column corresponds to the first line of text that will be displayed on the axis labels of boxplots and correlation plots, 'label2' corresponds to the second line. Either of these values can be left empty if the variable description is not long and would fit on a single line on the plot axis.

<img width="700" alt="image" src="https://user-images.githubusercontent.com/73937893/189455448-2c3ce4e2-1ed8-458e-a190-474a2fa9172a.png">

- __meta_data__ contains further information on the the tested subjects. The first column contains the animal IDs which must match the IDs in _grand_table_. The remaining columns contain information such as group assignment, genotype, treatment, etc. Such grouping factors can be used for statistical comparisons or group annotation on the resulting plots. 

Our example data set only contains one meta data category which is the group assignment of animals to either control or fox urine treatment. We can use this group variable later for statistical comparisons.

<img width="400" alt="image" src="https://user-images.githubusercontent.com/73937893/189455564-c1d0ec78-6f0a-4ca2-9c6c-aa035e1ffc96.png">                         

### Starting the app and importing the data

Once we have prepared our data set in the right format, we can run beeRapp which will be opened in your browser.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189502383-61b81757-e358-47b8-8737-a5095b00a5fa.png">

We can then navigate to the _Import Data_ tab and use the _Browse_ button to find and import out file containing the data set. beeRapp automatically performs some sanity checks. For example, if the grand table contains non-numeric values, an error message will be produced. In that case, go back to the excel file and remove any non-numeric values from the table. If you have missing values, then leave the corresponding cells empty.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189502460-f59f7b3c-7831-443a-a73b-a7154ddf01b5.png">

If the data is formatted correctly, the table will be imported and the grand table will be displayed in the app. We can now move on to some analyses.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189502498-eb26be81-0bab-463b-a99d-a7a29c49e2a3.png">

### Clustering analysis

Analysis modules can be exectuted independently of each other and in no predifined order. We will start with a clustering analysis. In animal behavior research, we are often interested in identifying subpopulations of animals with a particular behavioral phenotype. In the Dopfel et al. data set, we will employ beeRapp's clustering module to identify low and high freezing animals. We allow the user to select any of the input measures to be used for clustering. Here, we will use two variables - freezing time and the distance to the cotton containing the fox urine. beeRapp offers three clustering options: Gaussian mixture model (GMM) clustering, k-means and hierarchical clustering. We have generally made the best experience with GMM and recommend this method but users can also experiment with the additional algorithms.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189503182-44de8764-2fb8-49cf-b40d-6eb96bfdfc09.png">

Once we have selected the variables, clustering algorithm and the number of clusters, the results are visualized in the app. In our example, we see two distinct clusters of animals. We can choose to show animal IDs on the plot to directly evalues which animal is assigned to which cluster. Furthermore, we can decide to change the default colors. There is a number of predifined colors, however users can add any color as long as it is provided as a hex code (see below for an example).

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189503666-4dda8701-80ff-4bda-bc1f-a6bca0741a36.png">

We can also save our clustering results with the _Save Clustering_ button. This will create a new excel file which contains clustering assignment as an additional column in the meta data table. We we want to use clustering as a grouping variable in subsequent analysis, we will have to go back an import the new table in beeRapp.

<img width="400" alt="image" src="https://user-images.githubusercontent.com/73937893/189504032-4c8b61c7-8ae0-4852-9b19-e89a7ef2a539.png">

### Pairwise comparisons



## Send feedback and report issues

You can send your feedback or report issues directly to hristo.todorov@uni-mainz.de.
