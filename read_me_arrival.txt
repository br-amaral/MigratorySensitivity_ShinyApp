README
======

Thank you for your interest in this data product. Any use of this data should be cited as:

Casey Youngflesh, Jacob Socolar, Bruna R. Amaral, Ali Arab, Robert P. Guralnick, Allen H. Hurlbert, Raphael LaFrance, Stephen J. Mayor, David A. W. Miller, and Morgan W. Tingley. 2021. Migratory strategy drives species-level variation in bird sensitivity to vegetation green-up.


Field Descriptions - data_arrival.csv
-------------------------------------
species - species common name
sci_name - species scientific name
year - year
cell - cell number
cell_lat - latitude of cell centroid
cell_lng - longitude of cell centroid
mod - model type that estimates were derived from (IAR - spatially smoothed; GAM - local estimation)
posterior_mean - posterior mean of arrival estimate
posterior_sd - posterior standard deviation arrival estimate


NOTES
-----
It is recommended that only IAR-derived estimates for species-cells-years with associated GAM data be used for downstream analyses. For those cases where IAR-derived estimates are available but GAM-derived estimates are not available, values have been interpolated beyond the spatial extent (for that species-cell) of data used to fit the model and should therefore be used with caution.
