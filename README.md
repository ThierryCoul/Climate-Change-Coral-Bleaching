# The geogrpahical differenciated impact of climate change on coral reef bleaching

The materials in this repository allow users to reproduce the statistical analyses and the graphs in the main text and extended data of the manuscript.

If you find meaningful errors in the code or have questions or suggestions, please contact Thierry Yerema Coulibaly at yerema.coul@gmail.com


## Organization of repository

* **Analysis/code**: folder for scripts for downloading, processing and replication of the data, as well as for the statistical analysis.
* **Analysis/Input**: folder for data inputs for analysis.
* **Analysis/Temporary**: folder for data manipulation during the processing of the data.
* **Analysis/Output**: folder for data after processing


* **Scripts**:

    Analyses_with_stata.do shows statistical regressions in Stata

    ArcGISpro_calculations.ipynb describes the geo-processing analyses with Jupyter Notebook

    Plots.R describes the plot code with R


## R packages required

* **sp**
* **raster**
* **rgdal**
* **sf**
* **terra**
* **R.utils**

* **dplyr**
* **tidyr**
* **tidyverse**
* **sf**
* **raster**
* **ggplot2**
* **rnaturalearth**
* **foreign**
* **prismatic**
* **ggpubr**

Scripts were written with R 3.6.3

## python packages required

* **arcpy**

Scripts were written with python 3.6

Scripts were written with Stata 16.1 for MAC
