#Installing required package if not already installed
req_packages = c("data.table", "arrow", "lfe", "memisc", "stringr")
for (package in req_packages) {
  if (package %in% rownames(installed.packages()) == FALSE) {install.packages(package, dependencies = TRUE)}
}
#loading the required packages
for (package in req_packages){
  library(package,character.only=TRUE,quietly=TRUE,verbose=FALSE)
}
#setting working directory to the location of the *.R file (script)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setting working directory manually if script file and data files are in difference directory
working_dir= "G:/My Drive/NRRI/Zillow Project/Water Quality Hedonic/Water_Data"
setwd(working_dir)


############################################################################################################################################
# Importing sales data
sale_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'Lake_quality_data_2021_05_19', 
                                                   'Combined','sale.pqt')))
sale_pids <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'Lake_quality_data_2021_05_19', 
                                                   'Combined','sale_pids.pqt')))
cpi_june <- read.csv("cpi_June.csv")
state_zone <- read.csv("state_zone.csv")
nhd_lake_sale <- read.csv("nhd_lake_sale.csv")
###################################################################################################################
# Importing water quality data
parameters <- c('secchi', 'chla')

for (par in parameters){
  par_data <- read.csv(file=file.path(working_dir, 'Fuzzy_match', paste0('sale_match_', par,'_med_seasonal.csv')))
  colnames(par_data)[colnames(par_data) == "sampleyear"] <- paste0(par,'_year')
  sale_lakes <- merge(sale_lakes, par_data[c('sid', par, paste0(par,'_year'))],
                      by='sid', all.x=TRUE)
}

sale_lakes$secchi_year <- as.Date(sale_lakes$secchi_year, format="%Y-%m-%d")
sale_lakes$secchi_year[is.na(sale_lakes$secchi_year)] <- "2025-01-01"

sale_lakes$chla_year <- as.Date(sale_lakes$chla_year, format="%Y-%m-%d")
sale_lakes$chla_year[is.na(sale_lakes$chla_year)] <- "2025-01-01"

sale_lakes$sample_min_year <- apply(sale_lakes[c('secchi_year', 'chla_year')], 1, min)

###############################################################################################################
# merging with nhd_lake_sale data to get lake size
colnames(nhd_lake_sale)[colnames(nhd_lake_sale) == "AREASQKM"] <- "lake_size_sqkm"

sale_lakes <- merge(sale_lakes, nhd_lake_sale[c('PERMANENT_IDENTIFIER', 'lake_size_sqkm')],
                    by.x = 'lake_nb_nhd_id', by.y = 'PERMANENT_IDENTIFIER', all.x = TRUE)
###############################################################################################################
# merging with cpi_data
colnames(cpi_june) <- c("Year", "Period", "Label", "cpi", "hpi_nsa", "hpi_sa")
sale_lakes$year <- as.integer(sale_lakes$year)

sale_lakes <- merge(sale_lakes, cpi_june[c('Year', 'cpi', "hpi_nsa", "hpi_sa")], by.x = 'year', by.y = 'Year',
                    all.x = TRUE)
#############################################################################################
# create variable to indicate multiple property and keep pid in the sale data
dup_property <- as.data.frame(table(sale_pids$sid))
colnames(dup_property) <- c("sid", "property_number")
sale_pids <- merge(sale_pids, dup_property, by = 'sid', all.x = TRUE)

#use the below code if you want to delete if there is any multiple property
sale_pids <- sale_pids[sale_pids$property_number==1,]
sale_pids$property_number <- NULL

# merge with sale_lakes data
sale_lakes <- merge(sale_lakes, sale_pids[c('sid', 'pid')], by='sid')
#####################################################################################################
# get state fips from sid
sale_lakes$state_fips = str_sub(sale_lakes$sid,1,2) 

#get air condition, heating yes, no
sale_lakes$bld_air <- ifelse(sale_lakes$bld_air == "", 0, ifelse(sale_lakes$bld_air=="NO", 0, 1))
sale_lakes$bld_heat <- ifelse(sale_lakes$bld_heat == "", 0,1)
sale_lakes$bld_fire <- ifelse(sale_lakes$bld_fire == "", 0,1)

# get area of the building
sale_lakes$bld_gross_area <- ifelse((sale_lakes$bld_sqft == 0 | is.na(sale_lakes$bld_sqft) == TRUE),
                                    ifelse((sale_lakes$bld_sqft_g==0 | is.na(sale_lakes$bld_sqft_g) == TRUE),
                                           sale_lakes$m2_bld_fp*10.76391, sale_lakes$bld_sqft_g),
                                    sale_lakes$bld_sqft)

# get age of the building
sale_lakes$bld_yr_combined <- ifelse((is.na(sale_lakes$bld_yr_eff) == TRUE | sale_lakes$bld_yr_eff ==0),
                                     sale_lakes$year-as.integer(sale_lakes$bld_yr),
                                     sale_lakes$year-as.integer(sale_lakes$bld_yr_eff))

sale_lakes$bld_yr_combined <- replace(sale_lakes$bld_yr_combined, which(sale_lakes$bld_yr_combined < 0), NA)

# get two digit building code and development
sale_lakes$bld_code_two <- substr(sale_lakes$bld_code, 1, 2)
sale_lakes$develop <- ifelse(sale_lakes$bld_gross_area > 0, 1,0)

# get household median income
sale_lakes$`hh_inc_med_bg_2006-2010` <- ifelse(is.na(sale_lakes$`hh_inc_med_bg_2006-2010`)== TRUE,
                                               ifelse(is.na(sale_lakes$`hh_inc_avg_bg_2006-2010`)==TRUE,
                                                      ifelse(is.na(sale_lakes$`hh_inc_med_bg_2012-2016`)==TRUE,
                                                             sale_lakes$`hh_inc_avg_bg_2012-2016`, sale_lakes$`hh_inc_med_bg_2012-2016`),
                                                      sale_lakes$`hh_inc_avg_bg_2006-2010`),
                                               sale_lakes$`hh_inc_med_bg_2006-2010`)

sale_lakes$`hh_inc_med_bg_2012-2016` <- ifelse(is.na(sale_lakes$`hh_inc_med_bg_2012-2016`)== TRUE,
                                               ifelse(is.na(sale_lakes$`hh_inc_avg_bg_2012-2016`)==TRUE,
                                                      ifelse(is.na(sale_lakes$`hh_inc_med_bg_2006-2010`)==TRUE,
                                                             sale_lakes$`hh_inc_avg_bg_2006-2010`, sale_lakes$`hh_inc_med_bg_2006-2010`),
                                                      sale_lakes$`hh_inc_avg_bg_2012-2016`),
                                               sale_lakes$`hh_inc_med_bg_2012-2016`)

sale_lakes$hh_med_income <- ifelse(sale_lakes$year <=2011,
                                   sale_lakes$`hh_inc_med_bg_2006-2010`,
                                   sale_lakes$`hh_inc_med_bg_2012-2016`)

# get population density
sale_lakes$`pop_dens_bg_2006-2010` <- ifelse(is.na(sale_lakes$`pop_dens_bg_2006-2010`)== TRUE,
                                             sale_lakes$`pop_dens_bg_2012-2016`,
                                             sale_lakes$`pop_dens_bg_2006-2010`)

sale_lakes$`pop_dens_bg_2012-2016` <- ifelse(is.na(sale_lakes$`pop_dens_bg_2012-2016`)== TRUE,
                                             sale_lakes$`pop_dens_bg_2006-2010`,
                                             sale_lakes$`pop_dens_bg_2012-2016`)


sale_lakes$hh_pop_dens <- ifelse(sale_lakes$year <=2011,
                                 sale_lakes$`pop_dens_bg_2006-2010`,
                                 sale_lakes$`pop_dens_bg_2012-2016`)
# inflation corrected price
sale_lakes$price_updated <- log(sale_lakes$price*256.143/sale_lakes$cpi) #2019 June cpi
sale_lakes$price_updated_hpi_nsa <- log(sale_lakes$price*277.47/sale_lakes$hpi_nsa) #2019 June hpi_nsa (not seasonal adjusted)
sale_lakes$price_updated_hpi_sa <- log(sale_lakes$price*272.71/sale_lakes$hpi_sa) #2019 June hpi_nsa (seasonal adjusted)
# sale_lakes$prc_ha_log <- log(sale_lakes$price/sale_lakes$ha)
# sale_lakes$price_updated <- as.numeric(sale_lakes$prc_ha_log)*sale_lakes$cpi/256.143 #2019 June cpi

####################################################################################################
# Clean some variable based on summary statistics
sale_lakes$travel_weiss <- replace(sale_lakes$travel_weiss, which(sale_lakes$travel_weiss <= 0), NA)
sale_lakes$rd_dist_hwy <- replace(sale_lakes$rd_dist_hwy, which(sale_lakes$rd_dist_hwy <= 0), NA)
sale_lakes$p_bld_fp_5000 <- replace(sale_lakes$p_bld_fp_5000, which(sale_lakes$p_bld_fp_5000 <= 0), NA)

sale_lakes$hh_med_income <- replace(sale_lakes$hh_med_income, which(sale_lakes$hh_med_income <= 0), NA)
sale_lakes$hh_pop_dens <- replace(sale_lakes$hh_pop_dens, which(sale_lakes$hh_pop_dens <= 0), NA)
################################################################################
sale_lakes$bld_age <- sale_lakes$bld_yr_combined + 1
sale_lakes$lot_area_sqm <- 10000*sale_lakes$ha
# Transform some variables
sale_lakes$log_secchi <- log(sale_lakes$secchi)
sale_lakes$log_chla <- log(sale_lakes$chla)

sale_lakes$log_bld_age <- log(sale_lakes$bld_age)
sale_lakes$log_bld_area <- log(sale_lakes$bld_gross_area)

sale_lakes$log_lot_area <- log(sale_lakes$lot_area_sqm)

sale_lakes$log_travel_to_city <- log(sale_lakes$travel_weiss)
sale_lakes$log_rd_dist_hwy <- log(sale_lakes$rd_dist_hwy)
sale_lakes$log_p_bld_fp_5000 <- log(sale_lakes$p_bld_fp_5000)

sale_lakes$log_med_inc_bg <- log(sale_lakes$hh_med_income)
sale_lakes$log_pop_dens_bg <- log(sale_lakes$hh_pop_dens)

######################################################################################
data_output <- file.path(working_dir, 'sale_water_quality_data')
dir.create(file.path(data_output))

write_parquet(sale_lakes, sink = file.path(data_output, "sale_water_quality_data.pqt"))

write.csv(sale_lakes, file= file.path(data_output, "sale_water_quality_data.csv"))