# beeRapp
## Welcome to beeRapp! 

- [What is beeRapp?](#what-us-beeRapp)
- [Setup](#setup)
- [How to use beeRapp](#how-to-use-beerapp)
  * [Formatting the input data](#formatting-the-input-data)
  * [Starting the app and importing the data](#starting-the-app-and-importing-the-data)
  * [Clustering analysis](#clustering-analysis)
  * [Pairwise comparisons](#pairwise-comparisons)
  * [Explorative analysis - heatmap](#explorative-analysis---heatmap)
  * [Explorative analysis - PCA](#explorative-analysis---pca)
  * [Correlational analysis](#correlational-analysis)
    + [Pairwise correlations](#pairwise-correlations)
  * [Conclusion](#conclusion)
- [Send feedback and report issues](#send-feedback-and-report-issues)


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
ATTENTION: running the app for the first time might take a long time (up to 30 min) until all missing packages are installed!

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

Dopfel et al. investigated freezing responses of rats exposed to a predator scent (fox urine) compared to a control group, as a model for post-traumatic stress disorder. The data set can be found in the _example data_ folder. 

### Formatting the input data

The input data must be stored in an .xslx file with the three following tabs (please mind that the tabs must have the names specified below): 

- __grand_table__ includes your collected data. The first column contains the IDs of the tested subjects, i.e. animals. 
The following columns contain the measured values on each behavioral variable. Values need to be numerical. If you have missing values in your data set, leave the corresponding cells empty, do not replace missing values with special characters, as this will produce an error!
The column names for the behavioral measures must only occupy one cell in the input table and they must not contain special characters. A more detailed descritpion of each column name can be provided in the _labels_ tab. 

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

We can then navigate to the _Import Data_ tab and use the _Browse_ button to find and import our file containing the data set. beeRapp automatically performs some sanity checks. For example, if the grand table contains non-numeric values, an error message will be produced. In that case, go back to the excel file and remove any non-numeric values from the table. If you have missing values, then leave the corresponding cells empty.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189502460-f59f7b3c-7831-443a-a73b-a7154ddf01b5.png">

If the data is formatted correctly, the table will be imported and the grand table will be displayed in the app. We can now move on to some analyses.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/189502498-eb26be81-0bab-463b-a99d-a7a29c49e2a3.png">

### Clustering analysis

Analysis modules can be exectuted independently of each other and in no predifined order. We will start with a clustering analysis. In animal behavior research, we are often interested in identifying subpopulations of animals with a particular behavioral phenotype. In the Dopfel et al. data set, we will employ beeRapp's clustering module to identify low and high freezing animals. We allow the user to select any of the input measures to be used for clustering. Here, we will use two variables - freezing time and the distance to the cotton containing the fox urine. beeRapp offers three clustering options: Gaussian mixture model (GMM) clustering, k-means and hierarchical clustering. We have generally made the best experience with GMM and recommend this method but users can also experiment with the additional algorithms.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/194920955-2dc25a28-2309-4976-b242-3f5ce65c182c.png">

Once we have selected the variables, clustering algorithm and the number of clusters, the results are visualized in the app. In our example, we see two distinct clusters of animals. We can choose to show animal IDs on the plot to directly evaluate which animal is assigned to which cluster. Furthermore, we can decide to change the default colors. There is a number of predifined colors, however users can add any color as long as it is provided as a hex code (see below for an example).

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/194921746-923045f1-d08d-43b7-9d42-f5553d09a121.png">

We can also save our clustering results with the _Save Clustering_ button. This will create a new excel file which contains clustering assignment as an additional column in the meta data table. If we want to use clustering as a grouping variable in subsequent analysis, we will have to go back an import the new table in beeRapp.

<img width="400" alt="image" src="https://user-images.githubusercontent.com/73937893/189504032-4c8b61c7-8ae0-4852-9b19-e89a7ef2a539.png">

If we do not hava a predefined number of clusters, we can also let beeRapp determine this automatically by setting the _Determine number of clusters automatically_ option to _yes_. For GMM, this is facilitated using the _clustCombiOptim_ function from the _mclust_ package. For k-means and hierarchical clustering, the NbClust package is used to determine the optimal number of clusters.

If your variables for clustering contain missing values, beeRapp will automatically impute them using multiple imputation with predictive mean matching implemented in the _Hmisc_ package. However, we recommend to exclude variables from the clustering with missing values as imputation might bias clustering results.

As reproducibility of the analysis is important, you can generate an html analysis report with all settings used for the analysis using the _Download analysis report_ button.

<img width="600" alt="image" src="https://user-images.githubusercontent.com/73937893/194924022-a4cc5acd-234a-4d17-a998-93805ff11727.png">

### Pairwise comparisons

We can use beeRapp to perform statistical comparisons between two groups using the _Boxplots_ module. Using our example data set, we can compare the control and fox urine groups. Additionally, we can import the table with the clustering results and compare cluster 1 versus cluster 2. 
beeRapp offers three different statistical tests: the non-parametric Wilcoxon rank sum test (recommended if data are not normally distributed), a t-test and a permutation based test (recommended if you do not want to make assumptions about the distribution of your data). 

Behavioural data are mostly normally distributed, therefore in most cases, a t-test is the best option. beeRapp will automatically check if variance between both groups is equal and if not, a Welch's t-test will be performed instead. You also have the option to test for outliers and remove them from the analysis (we recommend to do this if you use a t-test because outliers can influence the results of parametric tests). Outliers are detected by transforming original values to z-scores and removing values with an absolute z-score exceeding 3.29 (p<0.001) which is a rather conservetive approach to outlier detection (meaning we are generally cautious to throw out any values).

We also need to define a significance threshold for the statistical comparisons. Per default this value is set to 0.05 meaning that results will be generated only when the p-values from the pairwise comparison exceeds this threshold (if you want to produce all pairwise comparisons in the data set, then set the treshold to 1).

As usual, you have the option to define the colors for both groups. Results are visualized as boxplots and can be exported either as a PowerPoint file containing each plot on an individual slide, a pdf file or a collection of pdf files exported as a zip file.

Once you've selected all analysis settings, click on _Download_ to generate the results. A progress bar at the lower right corner of the app will indicate the progress of the analysis.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/194926366-402074c3-03cf-42b7-9a33-36382f1be53c.png">

In our comparison of the control versus the fox urine group, 13 variables differ significantly. The html analysis report contains the relevant information including which type of test was selected, what was the significance threshold, and which values were detected as outliers and removed. For the t-test option, an overview table also indicates whether an ordinary or a Welch's t-test was performed for each variable.

<img width="488" alt="image" src="https://user-images.githubusercontent.com/73937893/194927023-a964c463-b682-48a4-9aa3-4503e9242d03.png">

### Explorative analysis - heatmap

beeRapp offers a few analysis strategies to investigate the multivariate nature of behavioral data. To begin with, we can visualize the data matrix as a heatmap using the _pheatmap_ package. We can use hiearchical clustering to see which measurements or animals are more similar to each other. The radio buttons in the _Heatmap Settings_ panel control options for clustering rows and columns and showing the respective dendrograms. An important argument is the _scaling_ option. Per default, each variable is scaled to 0 mean and a standard deviation of 1. This is recommended when the data set consists of a mixture of variables measured on different scales. If we do not scale such data, then patterns in the results might be dominated by variables measured on larger scales. However, if all variables are measured on the same scale (e.g., freezing time), the argument can be set to _no_. In our example, we prefer to scale the data as we have different measurement scales.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/195075358-b58a76c0-5223-4491-b8fe-2d2cb4d10f66.png">

We can also annotate the heatmap with a group assignment for each sample - in our case we have the control and fox urine groups but also the clustering results we obtained previously. Indeed, if we use the clustering assignment we obtained using a GMM, we will notice that both clusters are also separated on the heatmap indicating that GMM and hierarchical clustering produce similar results in this case.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/195076184-657a4ae4-9080-4f75-8359-b355f70a51ee.png">

### Explorative analysis - PCA

Dimensionality reduction techniques are a relevant analysis approach in the context of multivariate data as they allow representation of high-dimensional sets in 2D space while simultaneously retaining important features of the data (e.g., directions of maximal vairance or Euclidean distance between input data points). The most common dimensionality reduction technique which beeRapp also implements, is principal components analysis. We can use it to investigate if the data set contains multivariate outliers (which would appear as points far removed from the remaining data) or if different groups cluster together. In our example, control and fox urine groups overlap in their overall multivariate pattern, however the clusters obtainend with GMM corresponding to high and low freezers are clearly separated from each other.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/195077671-5921252e-a05a-48dc-b218-be2dd89526b6.png">

### Correlational analysis

An important aspect of behavioral testing batteries is that they are often redundant, meaning that blocks of variables are often highly correlated with each other (e.g., freezing and distance moved). Investigating the correlational structure of the data is used as an internal validity criterion of the testing battery. In this context, correlations between different behavioral domains (e.g., freezing and performance in the elevated plus maze (EPM) test) are of particular interest. beeRapp allows you to generate a correlation matrix including all variables or a subset of them (the matrix is updated interactively as you add or remove variables). You can also choose the type of correlation (Pearson or Spearman) and the statistical threshold (non-significant correlations are not shown on the plot).


<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/195079315-285b4b8b-9f3c-4c1b-9c0b-dbdc29ade5f7.png">

#### Pairwise correlations

Beyond looking at the overall correlation structire, you might also be interested in more specific information about pairwise correlations between individual variables. beeRapp creates bivariate scatter plots for all combinations of variables in the data set for which the correlation coeffcient r is statistically significant (based on the user-defined threshold). There are three types of correlations that can be calculated - Pearson, Spearman and Kendall. The plots also show the trend line from a linear regression fit. Similarly to boxplots, plots can be exported as pdf or PowerPoint files. A progress bar indicates the progress of the analysis once you've clicked on _Download_. ATTENTION: there are n(n-1)/2 possible pairwise combinations, so for a data set with 100 variables, there are already 4950 possible pariwise correlations. Therefore, the analysis might take some time to finish! Our example data set contains 106 pairwise correlations with a p-value <0.05.

<img width="800" alt="image" src="https://user-images.githubusercontent.com/73937893/195081624-3e0ee0df-cb24-419b-a9df-a18714e25963.png">

### Conclusion
This tutorial summarizes the basic functionalities of beeRapp. Going through the individual modules should provide users with a solid understanding of the main structures and patterns in their behavioral data as well as enable them to elucidate subpopulations and test hypothesis for experimental groups or clusters generated by the app.

## Send feedback and report issues

You can send your feedback or report issues directly to hristo.todorov@uni-mainz.de.
