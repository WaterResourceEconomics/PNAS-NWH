# Project flow for Water Quality Hedonic
The project used the following dataset at contigious US scale to find out what is the property value implications of water quality:
1. Housing data: Zillow's [ZTRAX](https://www.zillow.com/research/ztrax/) data processed through [PLACES](https://placeslab.org/).
2. Water quality data: **LA**ke multi-scaled **G**e**OS**patial and temporal database[(LAGOS-NE)](https://lagoslakes.org/lagos-ne/) and USEPA's [Water Quality Portal](https://www.waterqualitydata.us//) for water quality measures.

## Get Housing data

1. ***collect_data.R***: This script collect fips level housing data that is processed throguh PLACES. It only contains housing data for houses located within 2km of all the lakes greater than 4ha. Latest data from PLACES is from May 19, 2021.

## Get Water Quality Data
 1. ***EPA_data.py***: Get EPA-WQP water quality data and spatially match with NHD to get NHD ID. Data collected on March 10, 2021.
 2. ***LAGOS-NE.r***: Get LAGOS-NE Data including NHD ID (merge epi_nutr and lakes.geo dataframes). Data collected on March 10, 2021.
 
 ## Combine Housing and Water Quality data
 1. ***Fuzzy_date_match.R***: This algorithm matches "***exactly***" between housing data and water quality data using NHDID and "***fuzzily***" between housing data and water quality data using housing transaction year and water quality sample year. If it cannot find a match still keeps the data. The outputs do not contain all housing attributes as it will slow down the matching algorithm. It saves two csv files for chla and secchi. Similarly other water quality parameters can also be matched.
 2. ***regression_data.R***: Based on the sale_id identified in the *Fuzzy_date_match.R*, this algorithm matches housing data with each water quality data. It also creates several variables that we need to use in the regression. The output file *sale_water_quality_data.pqt* is basis of regression analyses.
 
## Analyses
1. ***DID_NWH_slim_cluster_obs_robust.R***: This is baseline analysis with multiple variations. The running model names are:
	1. baseline: run the model as it is.
	2. limited observations: In the baseline model, we used the data if there is at least one property transactions within 100m of the lake. Here we restrict model to see if there are any impacts using multiple models. lim_obse = [10, 30, 100]
	3. adjust_cpi: run the model with dep_var='price_updated'
	4. adjust_hpi_nsa: run the model with dep_var='price_updated_hpi_nsa'
	5. Alternative definition of lakeshore and lakefront property: in the main model we defined lakeshore as 100m buffer and lakefront as 100-300m buffer from the lake. Her we test alternative definitions with multiple models.
	6. river_15000: control for river distances, baseline delete if distance from lake is greater than distance from river. other distances = [50, 100, 200, 300]
	7. water quality frequency: In the baseline model we used the data if there is only one water quality data (secchi_freq) available. Here we test by restricting the model requiring 2, 5, 7, 10, median secchi frequencies.
	8. saoe_sample_difference: In the baseline, we allow property sale year and water quality sample year to have a difference of 5 years. Here we test alternative parameters value to see how the results vary based on this assumption.
	14. change the fixed effects: baseline = fe_year_by_tract; others = [fe_year_state, fe_year_by_state, fe_year_tract, fe_year_fips, fe_year_by_fips, fe_year_bg, fe_year_by_bg]
2. ***DID_NWH_slim_cluster_obs_chla.R***: provides Chlorophyll-a results.
3. ***DID_NWH_repeat.R***: provides repeat sales analysis.
4. ***DID_NWH_slim_cluster_obs_state.R***: variation by states.
5. ***lake_maps.py***: visualization of key coefficient results for models, visualization of lakes, visualization of water quality by census tract (Chlorophyll-a and secchi depth)