README
======

Thank you for your interest in this data product. Any use of this data should be cited as:

Casey Youngflesh, Jacob Socolar, Bruna R. Amaral, Ali Arab, Robert P. Guralnick, Allen H. Hurlbert, Raphael LaFrance, Stephen J. Mayor, David A. W. Miller, and Morgan W. Tingley. 2021. Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up.


Greenup data were derived from the MCD12Q2 (https://lpdaac.usgs.gov/products/mcd12q2v006/) and MCD12Q1 (https://lpdaac.usgs.gov/products/mcd12q1v006/) data products and should be cited as:

Friedl, M., Gray, J., Sulla-Menashe, D. 2019. MCD12Q2 MODIS/Terra+Aqua Land Cover Dynamics Yearly L3 Global 500m SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC.

Friedl, M., Gray, J., Sulla-Menashe, D. 2019. MCD12Q1 MODIS/Terra+Aqua Land Cover Type Yearly L3 Global 500m SIN Grid V006 [Data set]. NASA EOSDIS Land Processes DAAC.


Field Descriptions - data_interan.csv
-------------------------------------
species - species common name
sci_name - species scientific name
year - year
cell - cell number
cell_lat - latitude of cell centroid
cell_lng - longitude of cell centroid
arr_IAR_mean - posterior mean of IAR-derived arrival estimate
arr_IAR_sd - posterior mean of IAR-derived arrival estimate
gr_mn - mean 'midgreen-up' date for all pixels classified as 'good' or 'best' that fall within cell that were classified as forest - green-up dates derived from the MCD12Q2 data product - land cover information derived from MCD12Q1 data product
