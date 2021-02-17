# MigratoryMismatchShiny

This repository contains code to create the interactive Shiny app associated with the following publication:

Casey Youngflesh, Jacob Socolar, Bruna R. Amaral, Ali Arab, Robert P. Guralnick, Allen H. Hurlbert, Raphael LaFrance, Stephen J. Mayor, David A. W. Miller, and Morgan W. Tingley. 2021. Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up.

This online app can be found at the following link at the time of the paper's publication:

https://migratory-sensitivity.shinyapps.io/MigSen-app

Code and data to produce this app are archive on Zenodo:

XXXX


Background
----------

Shiny is an R package used to create interactive applications (more info here: https://shiny.rstudio.com/). While this application is hosted on the web at the above URL (at the time of the paper's publication), it can also be reproduced locally. 

To produce the app locally, download the code and data from the above Zenodo link. This Github repository contains only the necessary code and not the data to create this app. A README to produce the app is included with the Zenodo link.


File Descriptions
-----------------
app.R - R script to run Shiny application
read_me_arrival.txt - read me file for species arrival data
read_me_greenup.txt - read me file for vegetation green up data
read_me_interan.txt - read me file for inter-annual variation data
read_me_sensi.txt - read me file for species sensitivity data
read_me_trait.txt - read me file for species traits data
Data (ignored) - folder with data to run the shiny app
   *data_arr.rds - species arrival data.frame
   *data_sensi.rds - species spatial sensitivity data.frame
   *fit_df_tab5.rds - species sensitivity to latitude data.frame
   *fit_df_tab6.rds - fitted migratory traits data.frame
   *for_green-up_dl.rds - vegetation green-up data data.frame
   *master_cell_grid.rds - hexagon grid coordinates
   *mrg2_xi_PC.rds - species migratory traits data.frame
   *species_Grid.RData - hexagon grid for each species for their distribution range
images - folder with plots of vegetation green up for all years, and plots of bird arrival for all combinations of species, year, model type and range.
Create_Maps - code to create maps displayed in app
	*Generate_mapPNGgreen.R - script to create plots for vegetation green up in different years (output is in ‘MigSen_app/images’)
	*Generate_mapPNGranges.R - script to create plots for bird arrival for different species, years, models and range (output is in ‘MigSen_app>Images’)


Computing Environment
---------------------

This app has been built and tested with the following computing environment (obtained from sessionInfo() in R):

R version 4.0.3 (2020-10-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Big Sur 10.16

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] zip_2.1.1            shinyBS_0.61         geosphere_1.5-10     forcats_0.5.0       
 [5] stringr_1.4.0        purrr_0.3.4          readr_1.4.0          tidyr_1.1.2         
 [9] tibble_3.0.5         tidyverse_1.3.0      robustHD_0.6.1       perry_0.2.0         
[13] robustbase_0.93-7    mapproj_1.2.7        maps_3.3.0           viridis_0.5.1       
[17] viridisLite_0.3.0    plotly_4.9.3         ggplot2_3.3.3        dplyr_1.0.3         
[21] shinydashboard_0.7.1 shinythemes_1.1.2    shiny_1.5.0  
