# beeRapp
## Welcome to beeRapp! 
###What is beeRapp
Explain the acronym here. 
With beeRapp fundamental analysis techniques become easily applicable to your own data. 
These include clustering, boxplots, heatmaps, PCA, correlation matrices and pairwise correlations.
All results can be saved in pdf files


beeRapp is written in R. Feel free to contribute if it seems that something is missing!
 
### How to use beeRapp 
To use beeRapp your data must be stored in an .xslx file with the three following tabs:  
- __grand_table__ includes your collected data. The first column includes the IDs of the tested subject, e.g. animals. 
The following columns contain the obtained values with the column names being specified further in the _labels_ tab.
- __labels__ contains three columns which are 'label1', 'label2' and 'colnames'. 
With 'colnames' being the colnames from _grand_table_ which are abbreviations, the labels 1 and 2 the fully written names 
of the columns used in data processing and plot representation.
- __meta_data__ contains further information on the the tested subject. With the first column containing IDs, the further columns contain information such as group affiliations.
                         
With the .xlsx file setup as described above, you are ready to go! Select 'Import Data' on the left and upload your file. Once uploaded, you can analyse the data with the tools provided under 'Analysis'.

