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
  
## Stream.R  
  R code of this project
## EDA of Streamflow.pdf  
  It is generated by Rmarkdown, including Temporal EDA, Spatial EDA and Spatial & Temporal EDA.
  
  EDA.rmd is the code file. 
## StreamFlow\plot
  All plots in this project
  ### Temporal EDA
  Boxplot.png: Boxplot of streamflow over 40 years.
  
  Boxplot_Region: Boxplot, whose facet is region
  
  Trend_Month.png: The trend of each month
  
  Grouped_Trend.png： The trend of each group. The first group contais of 3, 4, 5, 8, 9, 10.
  
  \StreamFlow\plot\Acf: Acf plot of each month to check their stationarity.
  ### Spatial EDA
  \StreamFlow\plot\MeanExceedance: Mean Exceedance of each quantile, 0.05, 0.1, 0.2, 0.7, 0.9, 0.95, 0.975, 0.99
  ### Spatial & Temporal EDA
  \StreamFlow\plot\MeanExceedance_Year: 
  
  MeanExceedance_Year_q.gif: Mean Exceedance of q quantile over 1980-2019.
  
  \StreamFlow\plot\MeanExceedance_Year\MeanExceedance_Year_q: plots of each year during 1980-2019.
## Hydrologic Boundary Plot
  [wbdhu2_a_us_september2020.gdb](https://nrcs.app.box.com/v/huc/folder/18546994164): Hydrologic Boundary shapefile
  Hydrologic_Boundary_Map.R: R code for drawing the Hydrologic Boundary Map
