# StreamFlow

## Data_Acqusition
  gauge_information.txt: 671 catchments huc ID in American (We call them as site following)
  
  Region_name.txt: Region name each site belongs to
  
  Siteinfo.Rdata: site information obtained
  
  Streamflow: Daily Streamflow data of 671 sites during 1980-2019 obtained.
  
  Note: This part is only the process we read the data at the first time. 
  
  All data cleaned are stored in Stream.Rdata. We will load Stream.Rdata directly instead of them.
  
  Due to the limitation of Github, Stream.Rdata can not be uploaded. Therefore I share it in OneDrive as this link.
  
  [Stream.Rdata](https://gla-my.sharepoint.com/:u:/g/personal/2592713l_student_gla_ac_uk/EUr9-JfRzzZCqoaT3R8Cl88BSifU7cAC5mSXVMxYyaCa9A?e=xCsIlf)
  
  It will be updated if any change happens.

## Plot

boxplot.png: boxplot of population

Boxplot_Region.png: boxplot of each region

Map_Statistics_Region.png: map of statistics

### Acf

Acf_Spring/Summer/Autumn/Winter.png: acf plots of each season.

Acf_Spring/Autumn/Winter_diff.png: acf plots of each season after 1-diff.

### MeanExceedance

MeanExceedance_0.05/0.1/0.2/0.70/0.90/0.95/0.975/0.99.png: mean exceedance of each quantiles.

### MeanExceedance_Year

Folers: MeanExceedance_Year_0.05/0.1/0.2/0.70/0.90/0.95/0.975/0.99: every plot of animation

MeanExceedance_Year_0.05/0.1/0.2/0.70/0.90/0.95/0.975/0.99.gif: mean exceedance of each quantiles over 40 years

### Trend

Trend_Month.png: trend of monthly streamflow

Trend_Season.png: trend of season streamflow

Trend_Season_Region.png: trend of season streamflow in each region

## Report

EDA of Streamflow.pdf: first EDA

EDA.Rmd: code of EDA of Streamflow

Map.html: web map file

Map.Rmd: code of Map.html

Reply to question.Rmd

Reply to questions.pdf

## code

Stream.R: main code

Trend_Season.R: code of Reply to question

Web_Map.R: code of Map.html

Hydrologic_Boundary_Map.R: R code for drawing the Hydrologic Boundary Map

## wbdhu2_a_us_september2020.gdb

[wbdhu2_a_us_september2020.gdb](https://nrcs.app.box.com/v/huc/folder/18546994164): Hydrologic Boundary shapefile

