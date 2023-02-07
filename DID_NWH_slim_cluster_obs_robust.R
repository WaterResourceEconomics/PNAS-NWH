#Installing required package if not already installed
req_packages = c("data.table", "arrow", "lfe", "memisc", "stringr", "pastecs", "corrplot", "car")
lib_pkg <- '/panfs/roc/groups/5/polasky/salmamun/r_library'
for (package in req_packages) {
  if (package %in% rownames(installed.packages(lib_pkg)) == FALSE) {install.packages(package, lib=lib_pkg, repos="http://cran.rstudio.com/", dependencies = TRUE)}
}
#loading the required packages
for (package in req_packages){
  library(package,character.only=TRUE,quietly=TRUE,verbose=FALSE)
}
#setting working directory manually if script file and data files are in difference directory
working_dir= '/panfs/roc/groups/5/polasky/salmamun/WH/Workspace'
setwd(working_dir)
#################################################################################################
# MODEL SELECTION
args <- commandArgs(trailingOnly=TRUE)
model <- args[1]

# 'baseline', 'baseline_upper_filter', 'baseline_nla_2012'

# 'lim_obs_10', 'lim_obs_30', 'lim_obs_100'

# 'adjust_cpi', adjust_hpi_nsa'

# 'treat_shore_only_50',	'treat_shore_only_100'	'treat_shore_only_200', 'treat_waterfront',
# 'treat_shore_50_front_200',	'treat_shore_50_front_300',
# 'treat_shore_100_front_200',	'treat_shore_200_front_500',	'treat_shore_300_front_1000'

# 'river_15000',	'river_50',	'river_100',	'river_200',	'river_300'

# 'sale_sample_diff_0', 'sale_sample_diff_1',	'sale_sample_diff_3',	'sale_sample_diff_10',	'sale_sample_diff_not_considered'

# 'secchi_freq_2', 'secchi_freq_5', 'secchi_freq_7', 'secchi_freq_median', 'secchi_freq_10', 'secchi_freq_not_consider'

# 'lim_obs_1_sample_1', 'lim_obs_10_sample_3'   <- we dont have to run this separately.

# 'fe_year_state', 'fe_year_by_state', 'fe_year_tract', 'fe_year_fips', 'fe_year_by_fips',
# 'fe_year_bg', 'fe_year_by_bg', 'fe_year', 'fe_tract'


# 'het_region', 'het_quality', 'het_quality_10', 'het_state', 'het_state_no_drop'
# 'semi_log', 'reg_by_year', 'reg_with_repeat_sales'
############################################################################################################################################
# Importing sales data
sale_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'sale_water_quality_data',
                                                   'sale_water_quality_data.pqt')))

#########################################################################################################
# SUBSET THE DATA
# delete all great lakes (max lake size Lake Ontario: 18845.9359 sqkm, Great Salt Lake: 3389.502116)
sale_subset <- sale_lakes[which(sale_lakes$lake_size_sqkm <= 10000),]

# delete if date (date of sale) is before 2000
sale_subset <- sale_subset[which(sale_subset$year>=2000),]

# delete if parameters sampledate is not within 5 years of sale date
# 'sale_sample_diff_1',	'sale_sample_diff_3',	'sale_sample_diff_10',	'sale_sample_diff_not_considered'

if(model == 'sale_sample_diff_not_considered'){
  print("sale_sample_diff_not_considered")
} else if(model == 'sale_sample_diff_0'){
  sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
  sale_subset <- sale_subset[which(sale_subset$year_diff ==0),]
} else if(model == 'sale_sample_diff_1'){
  sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
  sale_subset <- sale_subset[which(sale_subset$year_diff <=1),]
} else if(model == 'sale_sample_diff_3'){
  sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
  sale_subset <- sale_subset[which(sale_subset$year_diff <=3),]
} else if(model == 'sale_sample_diff_10'){
  sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
  sale_subset <- sale_subset[which(sale_subset$year_diff <=10),]
} else{
  sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
  sale_subset <- sale_subset[which(sale_subset$year_diff <=5),]
}

#delete if source is other than 'zt'
sale_subset <- sale_subset[which(sale_subset$source == 'zt'),]

# delete the data if sale year is before max(bld_yr, bld_yr_eff)
my_row_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
sale_subset$row_max_bld_yr <- apply(sale_subset[c('bld_yr', 'bld_yr_eff')], 1, 
                                       my_row_max)

sale_subset <- sale_subset[which(sale_subset$year >= sale_subset$row_max_bld_yr | 
                                         is.na(sale_subset$row_max_bld_yr)),]

# delete if river is closer than the lake
# create a high value (15000) for river distance if it is missing
sale_subset$river_dist <- ifelse(is.na(sale_subset$river_dist), 15000, sale_subset$river_dist)
# 'river_15000',	'river_50',	'river_100',	'river_200',	'river_300'
if(model == 'river_15000'){
  print('river_15000')
} else if(model == 'river_50'){
  sale_subset <- sale_subset[-which(sale_subset$river_dist < 50),]
} else if(model == 'river_100'){
  sale_subset <- sale_subset[-which(sale_subset$river_dist < 100),]
} else if(model == 'river_200'){
  sale_subset <- sale_subset[-which(sale_subset$river_dist < 200),]
} else if(model == 'river_300'){
  sale_subset <- sale_subset[-which(sale_subset$river_dist < 300),]
}else{
  sale_subset <- sale_subset[-which(sale_subset$river_dist < sale_subset$lake_dist),]
}

# delete data with less than 10,000
sale_subset <- sale_subset[-which(sale_subset$price < 10000),]

# delete if price_per_ha is more than 90th percentile and lowest 10 percentile
# 'baseline_upper_filter'
sale_subset$price_per_ha <- sale_subset$price/sale_subset$ha
if(model != 'baseline_upper_filter'){
  sale_subset <- sale_subset[-which(sale_subset$price_per_ha >= quantile(sale_subset$price_per_ha, p=0.99)),]
}
# 'baseline_nla_2012'
# keep only nla2012 lakes
if(model == 'baseline_nla_2012'){
  nla_2012_lakes = read.csv(file.path(working_dir, 'lake_maps', 'baseline_nla_lakes.csv'))
  sale_subset <- sale_subset[which(sale_subset$lake_nb_nhd_id %in% c(as.character(nla_2012_lakes$PERMANENT_))),]
}

##########################################################################################################
# Create Treatment and control variables

# 'treat_shore_only_50',	'treat_shore_only_100'	'treat_shore_only_200', 'treat_waterfront',
# 'treat_shore_50_front_200',	'treat_shore_50_front_300',
# 'treat_shore_100_front_200',	'treat_shore_200_front_500',	'treat_shore_300_front_1000'

if(model == 'treat_shore_only_50'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 50, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 50), 1,0)
} else if(model == 'treat_shore_only_100'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 100, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 100), 1,0) 
} else if(model == 'treat_shore_only_200'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 200, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 200), 1,0) 
} else if(model == 'treat_waterfront'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_nb_nhd_id == sale_subset$lake_nhd_id & 
                                     sale_subset$lake_frontage>0, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 100), 1,0) 
} else if(model == 'treat_shore_50_front_200'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 50, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 200 & sale_subset$lake_dist > 50), 1,0)
} else if(model == 'treat_shore_50_front_300'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 50, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 50), 1,0) 
} else if(model == 'treat_shore_100_front_200'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 100, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 200 & sale_subset$lake_dist > 100), 1,0)
} else if(model == 'treat_shore_200_front_500'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 200, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 500 & sale_subset$lake_dist > 200), 1,0) 
} else if(model == 'treat_shore_300_front_1000'){
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 300, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 1000 & sale_subset$lake_dist > 300), 1,0) 
} else{
  sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 100, 1,0)
  sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 100), 1,0)
}


# sale_subset <- sale_subset[which(sale_subset$lake_dist <= 1000),]

sale_subset <- sale_subset[!is.na(sale_subset$bg_id),]
sale_subset$tract_id <- paste0(sale_subset$state_fips, sale_subset$fips, sale_subset$tract_id)

###########################################################################################################
# 'adjust_cpi', adjust_hpi_nsa'

if(model == 'adjust_cpi'){
  dep_var <- 'price_updated ~' # price_updated_hpi_sa # price_updated_hpi_nsa, # price_updated
} else if(model == 'adjust_hpi_nsa'){
  dep_var <- 'price_updated_hpi_nsa ~'
}else{
  dep_var <- 'price_updated_hpi_sa ~'
}

if(model %in% c('treat_shore_only_50',	'treat_shore_only_100',	'treat_shore_only_200', 'treat_waterfront')){
  lake_shore_front <- c('lake_shore')
} else{
  lake_shore_front <- c('lake_shore', 'lake_front')
}


if (model == 'semi_log'){
  wq_secchi <- 'secchi'
} else {
  wq_secchi <- 'log_secchi'
}

resid_control <- c("log_bld_age",
                   "log_bld_area")
common_control <- c("log_lot_area", 'slope', 'elev', 'log_rd_dist_hwy')

non_fe_control <- 'lake_size_sqkm'

neighbor_control <- c('0') # 'log_med_inc_bg'


# fe_year_state, fe_year_by_state, fe_year_tract, fe_year_fips, fe_year_by_fips, fe_year_bg, fe_year_by_bg
if(model == 'fe_year_state'){
  fixed_effect <- c('year', 'state_fips')
} else if(model == 'fe_year_by_state'){
  fixed_effect <- c('year:state_fips')
} else if(model == 'fe_year_tract'){
  fixed_effect <- c('year', 'tract_id')
} else if(model == 'fe_year_fips'){
  fixed_effect <- c('year', 'fips')
} else if(model %in% c('fe_year_by_fips', 'baseline_nla_2012')){
  fixed_effect <- c('year:fips')
}else if(model == 'fe_year_bg'){
  fixed_effect <- c('year', 'bg_id')
} else if(model == 'fe_year_by_bg'){
  fixed_effect <- c('year:bg_id')
} else if(model == 'fe_year'){
  fixed_effect <- c('year')
} else if(model %in% c('fe_tract', 'reg_by_year')){
  fixed_effect <- c('tract_id')
} else{
  fixed_effect <- c('year:tract_id')
}

cluster_error <- c('tract_id') # lake_nb_nhd_id

# fixed effect models
fe_eqn_secchi_int <- as.formula(paste0(c(paste0(dep_var,
                                                   paste0(c(lake_shore_front, non_fe_control, wq_secchi,
                                                            paste0(CJ(lake_shore_front, wq_secchi)$lake_shore_front, ':',
                                                                   CJ(lake_shore_front, wq_secchi)$wq_secchi),
                                                            common_control, resid_control, neighbor_control),
                                                          collapse = '+')),
                                            paste0(fixed_effect, collapse = '+'),
                                            "0",cluster_error),collapse = '|'))


##############################################################################################################
resid_dev_sale <- sale_subset[(sale_subset$bld_code_two == 'RR' & sale_subset$develop == 1),]

# handling complete cases and limited data by lake
# 'lim_obs_10', 'lim_obs_30', 'lim_obs_100'
if(model %in% c('lim_obs_10', 'lim_obs_10_sample_3')){
  min_number_transaction <- 10
} else if(model == 'lim_obs_30'){
  min_number_transaction <- 30
} else if(model == 'lim_obs_100'){
  min_number_transaction <- 100
} else{
  min_number_transaction <- 1
}

water_quality_sale <- resid_dev_sale[which(complete.cases(resid_dev_sale[names(get_all_vars(
  fe_eqn_secchi_int, resid_dev_sale))])),]

limited_lake_front <- as.data.frame(table(water_quality_sale$lake_nb_nhd_id[water_quality_sale$lake_shore==1]))
limited_lake_front_lake <- as.character(unique(limited_lake_front$Var1[limited_lake_front$Freq >= min_number_transaction]))

limited_not_lake_front <- as.data.frame(table(water_quality_sale$lake_nb_nhd_id[water_quality_sale$lake_shore==0]))
limited_not_lake_front_lake <- as.character(unique(limited_not_lake_front$Var1[limited_not_lake_front$Freq >= min_number_transaction]))

limited_impair_lake <- intersect(limited_lake_front_lake, limited_not_lake_front_lake)

water_quality_sale <- water_quality_sale[water_quality_sale$lake_nb_nhd_id %in% c(limited_impair_lake),]

if(model == 'baseline'){
  water_quality_sale$price_adjusted_2019 <- water_quality_sale$price*272.71/water_quality_sale$hpi_sa    # hpi_sa for 2019 is 272.71
  # get summary table
  summary_vars <- c('lake_shore', 'lake_front', 'secchi', 'chla', 'lot_area_sqm', 'slope', 'elev', 'travel_weiss', 'rd_dist_hwy',
                    'p_bld_fp_5000', 'bld_n_beds', 'bld_n_baths', 'bld_age', 'bld_gross_area',
                    'hh_med_income', 'hh_pop_dens', 'lake_size_sqkm','year', 'price_per_ha', 'price', 'price_adjusted_2019')
  data_summary <- t(stat.desc(water_quality_sale[summary_vars]))
  lakeshore_data_summary <- t(stat.desc(water_quality_sale[summary_vars][water_quality_sale$lake_shore==1,]))
  lakefront_data_summary <- t(stat.desc(water_quality_sale[summary_vars][water_quality_sale$lake_front==1,]))
}
###################################################################################################
if(model == 'baseline'){
  # get latest wq data
  water_quality_sale <- water_quality_sale[rev(order(water_quality_sale$secchi_year)), ]
  secchi_latest_lake <- water_quality_sale[!duplicated(water_quality_sale$lake_nb_nhd_id),]
  water_quality_sale$secchi_year <- as.character(water_quality_sale$secchi_year)
  water_quality_sale$secchi <- as.character(water_quality_sale$secchi)
  secchi_lastest <- aggregate(.~tract_id+lake_nb_nhd_id,
                           water_quality_sale[c('tract_id', 'lake_nb_nhd_id', 'secchi_year', 'secchi')], first)
  secchi_lastest$secchi <- as.numeric(as.character(secchi_lastest$secchi))
  secchi_lastest <- aggregate(.~tract_id,
                              secchi_lastest[c('tract_id', 'secchi')], mean)

  water_quality_sale <- water_quality_sale[rev(order(water_quality_sale$chla_year)), ]
  chla_latest_lake <- water_quality_sale[!duplicated(water_quality_sale$lake_nb_nhd_id),]
  water_quality_sale$chla_year <- as.character(water_quality_sale$chla_year)
  water_quality_sale$chla <- as.character(water_quality_sale$chla)
  chla_lastest <- aggregate(.~tract_id+lake_nb_nhd_id,
                              water_quality_sale[c('tract_id', 'lake_nb_nhd_id', 'chla_year', 'chla')], first)
  chla_lastest$chla <- as.numeric(as.character(chla_lastest$chla))
  chla_lastest <- aggregate(.~tract_id,
                              chla_lastest[c('tract_id', 'chla')], mean)
  
  # income by distance at blockgroup level
  bg_income_by_distance <- function(water_quality_sale, distance_group){
    if(distance_group=='All property within 2000m'){
      wq_subset <- water_quality_sale
    } else if(distance_group=='Property within 100m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_shore==1,]
    }else if(distance_group=='Property within 100m-300m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_front==1,]
    }else if(distance_group=='Property within 300m-2000m buffer'){
      wq_subset <- water_quality_sale[!(water_quality_sale$lake_front==1 | water_quality_sale$lake_shore==1),]
    }
    
    median_by_bg <- mean(wq_subset$hh_med_income)
    return(c(distance_group, median_by_bg))
  }
  
  income_all <- bg_income_by_distance(water_quality_sale, distance_group='All property within 2000m')
  income_100 <- bg_income_by_distance(water_quality_sale, distance_group='Property within 100m buffer')
  income_100_300 <- bg_income_by_distance(water_quality_sale, distance_group='Property within 100m-300m buffer')
  income_300_2000 <- bg_income_by_distance(water_quality_sale, distance_group='Property within 300m-2000m buffer')
  
  income_by_bg <- as.data.frame(rbind(income_all, income_100, income_100_300, income_300_2000))
  colnames(income_by_bg) <- c('Distance bins', 'Average Block Group Median Household Income')
  
  # plot time trends
  make_agg_price <- function(water_quality_sale, group){
    if(group=='All property within 2000m'){
      wq_subset <- water_quality_sale
    } else if(group=='Property within 100m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_shore==1,]
    }else if(group=='Property within 100m-300m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_front==1,]
    }else if(group=='Property within 300m-2000m buffer'){
      wq_subset <- water_quality_sale[!(water_quality_sale$lake_front==1 | water_quality_sale$lake_shore==1),]
    }
    
    price_data <- aggregate(.~year,
                            wq_subset[c('year', 'price')], mean)
    # colnames(price_data)[colnames(price_data) == 'price'] <- 'average'
    price_sd <- aggregate(.~year,
                          wq_subset[c('year', 'price')], sd)
    colnames(price_sd)[colnames(price_sd) == 'price'] <- 'sd'
    
    price_data <- merge(price_data, price_sd, by="year")
    
    price_data$series <- group
    
    return(price_data)
  }
  
  all_property <- make_agg_price(water_quality_sale, group='All property within 2000m')
  lakeshore_property <- make_agg_price(water_quality_sale, group='Property within 100m buffer')
  lakefront_property <- make_agg_price(water_quality_sale, group='Property within 100m-300m buffer')
  non_lakefront_property <- make_agg_price(water_quality_sale, group='Property within 300m-2000m buffer')
  
  property_price <- rbind(all_property, lakeshore_property, lakefront_property, non_lakefront_property)
  property_price$year <- as.numeric(as.character(property_price$year))

  # ggplot(data = property_price, aes(x=year, y=price)) + geom_line(aes(colour=series))
  
  my_legend = theme(
    legend.text = element_text(size=8),
    legend.title.align =0,
    legend.position = "top", 
    legend.box = "horizontal", 
    legend.title = element_text(size=8, vjust=0.5, hjust = 0.3))
  
  price_trend <- ggplot(property_price,aes(year,price))+
    # geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=0.2)+
    geom_line(aes(colour=series))+geom_point(aes(colour=series)) +
    xlab("Year of sale") +
    ylab("Price of house") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous("Year of sale", labels = unique(property_price$year), breaks = unique(property_price$year))+
    ggtitle(" (A) Price trend by distance to lake")+ theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1))+
    my_legend
  
  # plot water quality time trends
  water_quality_sale$secchi <- as.numeric(water_quality_sale$secchi)
  water_quality_sale$chla <- as.numeric(water_quality_sale$chla)
  secchi_quality_df <- aggregate(.~lake_nb_nhd_id+secchi_year,
                           water_quality_sale[c('lake_nb_nhd_id', 'secchi_year', 'secchi')], mean)
  secchi_quality_df <- secchi_quality_df[secchi_quality_df$secchi_year >= 2000,]
  chla_quality_df <- aggregate(.~lake_nb_nhd_id+chla_year,
                           water_quality_sale[c('lake_nb_nhd_id', 'chla_year', 'chla')], mean)
  chla_quality_df <- chla_quality_df[chla_quality_df$chla_year >= 2000,]
  
  het_data <- read.csv(file.path(working_dir, 'lake_maps', 'lake_epa_ecozone.csv'))
  water_trend_secchi <- merge(secchi_quality_df, het_data, by="lake_nb_nhd_id")
  water_trend_chla <- merge(chla_quality_df, het_data, by="lake_nb_nhd_id")
  water_trend_secchi <- aggregate(.~eco_zone+secchi_year,
                                  water_trend_secchi[c('eco_zone', 'secchi_year', 'secchi')], mean)
  
  water_trend_chla <- aggregate(.~eco_zone+chla_year,
                                  water_trend_chla[c('eco_zone', 'chla_year', 'chla')], mean)
  
  water_trend_secchi$secchi_year <- as.numeric(format(as.Date(water_trend_secchi$secchi_year, format="%Y-%m-%d"),"%Y"))
  secchi_trend <- ggplot(water_trend_secchi,aes(secchi_year,secchi))+
    # geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=0.2)+
    geom_line(aes(colour=eco_zone))+geom_point(aes(colour=eco_zone)) +
    xlab("Year of sample") +
    ylab("Secchi depth in m") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous("Year of sample", labels = unique(water_trend_secchi$secchi_year), 
                       breaks = unique(water_trend_secchi$secchi_year))+
    ggtitle("(C) Secchi depth trend by ecological zone")+ theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1))+
    my_legend
  
  water_trend_chla$chla_year <- as.numeric(format(as.Date(water_trend_chla$chla_year, format="%Y-%m-%d"),"%Y"))
  chla_trend <- ggplot(water_trend_chla,aes(chla_year,chla))+
    # geom_errorbar(aes(ymin=average-sd,ymax=average+sd),width=0.2)+
    geom_line(aes(colour=eco_zone))+geom_point(aes(colour=eco_zone)) +
    xlab("Year of sample") +
    ylab("Chlorophyl A concentration in ug/l") +
    theme_classic()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous("Year of sample", labels = unique(water_trend_chla$chla_year), 
                       breaks = unique(water_trend_chla$chla_year))+
    ggtitle(" (D) Chl-A trend by ecological zone")+ theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1))+
    my_legend
  
  secchi_year_df <- aggregate(.~secchi_year,
                              secchi_quality_df[c('secchi_year', 'secchi')], mean)
  chla_year_df <- aggregate(.~chla_year,
                            chla_quality_df[c('chla_year', 'chla')], mean)
  water_trend_data <- merge(secchi_year_df, chla_year_df, by.x="secchi_year", by.y = "chla_year")
  water_trend_data$secchi_year <- as.numeric(format(as.Date(water_trend_data$secchi_year, format="%Y-%m-%d"),"%Y"))
  
  coef <- 8
  
  quality_trend <- ggplot(water_trend_data, aes(x=secchi_year)) +
    
    geom_line(aes(y=secchi)) + 
    geom_line( aes(y=chla/coef), color="#FF0000") +
    
    geom_point(aes(y=secchi)) + 
    geom_point( aes(y=chla/coef), color="#FF0000") +
    
    scale_y_continuous(
      
      # Features of the first axis
      name = "Secchi depth in m",
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*coef, name="Chlorophyl A concentration in ug/l")
    ) + 
    theme_classic()+
    theme(axis.title.y.right = element_text(color = "#FF0000"), 
          axis.line.y.right=element_line(color="#FF0000"),
          axis.text.y.right = element_text(color="#FF0000"))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_continuous("Year of sample", labels = unique(water_trend_data$secchi_year), 
                       breaks = unique(water_trend_data$secchi_year))+
    ggtitle("(B) Secchi depth and Chl-A Concentration trend")+ theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1))
  
  trend_plot <- arrangeGrob(price_trend, quality_trend, secchi_trend, chla_trend,
               layout_matrix = rbind(c(1,1),c(2,2), c(3,4)))

}
########################################################################################################
# model with repeat sales only
if(model == 'reg_with_repeat_sales'){
  water_quality_sale <- water_quality_sale[
    which(water_quality_sale$pid %in% names(table(water_quality_sale$pid))[
      which(table(water_quality_sale$pid)>1)]),]
}
########################################################################################################
# factorizing variable
factorize_variables <- function(dataset){
  dataset$bg_id <- factor(dataset$bg_id)
  dataset$tract_id <- factor(dataset$tract_id)
  dataset$fips <- factor(dataset$fips)
  dataset$state_fips <- factor(dataset$state_fips)
  dataset$year <- factor(dataset$year)
  return(dataset)
}
water_quality_sale <- factorize_variables(water_quality_sale)
##########################################################################################
getSummary.felm <- function (obj, alpha = 0.05, ...) {
  smry <- summary(obj, robust=TRUE)
  coef <- smry$coef
  # numdf <- unname(smry$fstatistic[2])
  # dendf <- unname(smry$fstatistic[3])
  
  numdf <- unname(smry$F.fstat[2])
  dendf <- unname(smry$F.fstat[3])
  
  lower <- coef[, 1] + coef[, 2] * qt(p = alpha/2, df = dendf)
  upper <- coef[, 1] + coef[, 2] * qt(p = 1 - alpha/2, df = dendf)
  coef <- cbind(coef, lower, upper)
  dn <- list(rownames(coef), c("est", "se", "stat", 
                               "p", "lwr", "upr"), all.vars(obj$formula)[1])
  dim(coef) <- c(dim(coef)[1], dim(coef)[2], 1)
  dimnames(coef) <- dn
  sigma <- smry$sigma
  r.squared <- smry$r.squared
  adj.r.squared <- smry$adj.r.squared
  F <- unname(smry$F.fstat[1])
  p <- pf(F, numdf, dendf, lower.tail = FALSE)
  N <- length(smry$residuals)
  ll <- logLik(obj)
  deviance <- deviance(obj)
  AIC <- AIC(obj)
  BIC <- AIC(obj, k = log(N))
  sumstat <- c(sigma = sigma, r.squared = r.squared, adj.r.squared = adj.r.squared, 
               F = F, numdf = numdf, dendf = dendf, p = p, logLik = ll, 
               deviance = deviance, AIC = AIC, BIC = BIC, N = N)
  list(coef = coef, sumstat = sumstat, contrasts = obj$contrasts, 
       xlevels = obj$xlevels, call = obj$call)
}
setSummaryTemplate("felm" = c(
  "R-squared" = "($r.squared:f#)",
  "adj. R-squared" = "($adj.r.squared:f#)",
  F = "($F:f#)",
  AIC = "($AIC:f#)",
  BIC = "($BIC:f#)",
  "Log-likelihood"="($logLik:f#)",
  p = "($p:f#)",
  N = "($N:d)"
))
#########################################################################################
# create heterogeneity variables if applicable
if(model == 'het_region'){
  het_data <- read.csv(file.path(working_dir, 'lake_maps', 'lake_epa_ecozone.csv'))
  
  water_quality_sale <- merge(water_quality_sale, het_data, by='lake_nb_nhd_id')
  water_quality_sale$eco_zone <- as.factor(water_quality_sale$eco_zone)
  water_quality_sale <- dummy_cols(water_quality_sale, select_columns = c("eco_zone"))
  
  eco_var <- names(water_quality_sale)[grepl("eco_zone_", names(water_quality_sale))]
  eco_var <- eco_var[! eco_var %in% c('eco_zone_UMW')]
  for(ev in eco_var){
    water_quality_sale[ev] <- water_quality_sale[ev]*water_quality_sale$log_secchi
  }
  het_var <- paste0(CJ(eco_var, lake_shore_front)$eco_var, ':',
                    CJ(eco_var, lake_shore_front)$lake_shore_front)
} else if(model %in% c('het_quality', 'het_quality_10')){
    secchi_lake <- aggregate(.~lake_nb_nhd_id+secchi_year,
                             water_quality_sale[c('lake_nb_nhd_id', 'secchi_year', 'secchi')], mean)
    secchi_lake <- aggregate(.~lake_nb_nhd_id,
                             secchi_lake[c('lake_nb_nhd_id', 'secchi')], mean)
    if(model == 'het_quality_10'){
      secchi_lake$quality_category <- cut(secchi_lake$secchi,
                                          quantile(secchi_lake$secchi, probs = seq(0, 1, by = .1)),
                                          include.lowest=TRUE,labels=FALSE)
    } else{
      secchi_lake$quality_category <- cut(secchi_lake$secchi,
                                          quantile(secchi_lake$secchi),
                                          include.lowest=TRUE,labels=FALSE)
    }
    
    
    water_quality_sale <- merge(water_quality_sale, secchi_lake[c('lake_nb_nhd_id', 'quality_category')], by='lake_nb_nhd_id')
    water_quality_sale$quality_category <- as.factor(water_quality_sale$quality_category)
    water_quality_sale <- dummy_cols(water_quality_sale, select_columns = c("quality_category"))
    
    quality_var <- names(water_quality_sale)[grepl("quality_category_", names(water_quality_sale))]
    if(model== 'het_quality_10'){
      quality_var <- quality_var[! quality_var %in% c('quality_category_1')]
    } else{
      quality_var <- quality_var[! quality_var %in% c('quality_category_4')]
    }
    for(ev in quality_var){
      if(model== 'het_quality_10'){
        water_quality_sale[ev] <- water_quality_sale[ev]
      } else{
        water_quality_sale[ev] <- water_quality_sale[ev]*water_quality_sale$log_secchi
      }
    }
    if(model== 'het_quality_10'){
      wq_secchi <- quality_var
      het_var <- NULL
    } else{
      het_var <- paste0(CJ(quality_var, lake_shore_front)$quality_var, ':',
                        CJ(quality_var, lake_shore_front)$lake_shore_front)
    }
  
} else if(model == 'het_state' | model == 'het_state_no_drop'){
    water_state_keep <- c('01', '04', '05', '06', '08', '09', '10', '12',
                          '13', '17', '18', '19', '21', '22', '23', '24', '25','26', '27', '28', '29', '30', '31',
                          '32', '33', '34', '36', '37', '39', '40', '41', '42', '44', '45', '46', '47', '48', '49', '50',
                          '51', '53', '54', '55')
    
    water_state_abr <- c('AL', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 
                         'GA', 'IL', 'IN', 'IA', 'KY', 'MD', 'LA', 'ME', 'MI', 'MA', 'MN', 'MS', 'MO', 'MT', 'NE',
                         'NV', 'NH', 'NJ', 'NY', 'NC', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 
                         'VA', 'WA', 'WV', 'WI')
    state_df <- data.frame(water_state_keep, water_state_abr)
    colnames(state_df) <- c('state_fips', 'state_name')
    
    water_quality_sale <- merge(water_quality_sale, state_df, by='state_fips')
    water_quality_sale <- dummy_cols(water_quality_sale, select_columns = c("state_name"))
    
    state_var <- names(water_quality_sale)[grepl('state_name_', names(water_quality_sale))]
    if(model == 'het_state'){
      state_var <- state_var[! state_var %in% c('state_name_MN')]
    }
    for(sv in state_var){
      water_quality_sale[sv] <- water_quality_sale[sv]*water_quality_sale$log_secchi
    }
    
    het_var <- paste0(CJ(state_var, lake_shore_front)$state_var, ':',
                      CJ(state_var, lake_shore_front)$lake_shore_front)
} else if(model == 'reg_by_year'){
  year_list <- unique(water_quality_sale$year)
  year_list <- year_list[-length(year_list)]
  
  water_quality_sale <- dummy_cols(water_quality_sale, select_columns = c("year"))
  
  year_var <- names(water_quality_sale)[grepl('year_', names(water_quality_sale))]
  year_var <- year_var[! year_var %in% c('year_diff', 'year_2019')]
  
  for(yr in year_var){
    water_quality_sale[yr] <- water_quality_sale[yr]*water_quality_sale$log_secchi
  }
  
  het_var <- paste0(CJ(year_var, lake_shore_front)$year_var, ':',
                    CJ(year_var, lake_shore_front)$lake_shore_front)
  
} else{
  het_var = NULL
}
##################################################################################################
# Cluster std error
cluster_error_list <- c('0', 'bg_id', 'tract_id', 'fips', 'state_fips')
did_cluster_fe <- list()
for(ce in 1: length(cluster_error_list)){
  
  fe_eqn_wq_int_cluster <- as.formula(paste0(c(paste0(dep_var,
                                                      paste0(c(lake_shore_front, non_fe_control, wq_secchi, 
                                                               paste0(CJ(lake_shore_front, wq_secchi)$lake_shore_front, ':',
                                                                      CJ(lake_shore_front, wq_secchi)$wq_secchi),
                                                               common_control, resid_control, neighbor_control, het_var),
                                                             collapse = '+')),
                                               paste0(fixed_effect, collapse = '+'),
                                               "0",cluster_error_list[ce]),collapse = '|'))
  
  if(model== 'het_state_no_drop' ){
    fe_eqn_wq_int_cluster <- as.formula(paste0(c(paste0(dep_var,
                                                      paste0(c(lake_shore_front, non_fe_control, wq_secchi, 
                                                               common_control, resid_control, neighbor_control, het_var),
                                                             collapse = '+')),
                                               paste0(fixed_effect, collapse = '+'),
                                               "0",cluster_error_list[ce]),collapse = '|'))
  }
  
  
  did_cluster_fe[[ce]] <- felm(fe_eqn_wq_int_cluster, data = water_quality_sale)
}

names(did_cluster_fe) <- cluster_error_list

cluster_table <- mtable(# 'fe_fips_cluster_fips' = did_cluster_fe2[['fips']],
  'without_cluster' = did_cluster_fe[['0']],
  
  'fe_bg' = did_cluster_fe[['bg_id']], 'fe_tract' = did_cluster_fe[['tract_id']],
  'fe_fips' = did_cluster_fe[['fips']], 'fe_state' = did_cluster_fe[['state_fips']],
  
  summary.stats = c("N", "adj. R-squared", "R-squared", "Log-likelihood", "BIC", "AIC"),
  signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
  digits = 4)

############################################################################################
# Get residual plot for 'het_quality_10' model
if (model == 'het_quality_10'){
  res <- did_cluster_fe$tract_id$coefficients[,1]
  het_10_se <- did_cluster_fe$tract_id$STATS$price_updated_hpi_sa$se
  secchi_quality <- water_quality_sale$quality_category
  res_secchi <- as.data.frame(cbind(res, het_10_se))
  res_secchi$water_quality <- rownames(res_secchi)
  res_secchi$water_quality_category <- sub(".*:quality_", "", res_secchi$water_quality)
  res_secchi$Proximity <- sub(":.*", "", res_secchi$water_quality)
  res_secchi$Proximity <- ifelse(res_secchi$Proximity == "lake_shore", "100m buffer", "100-300m buffer")
  
  res_secchi <- tail(res_secchi, 18)
  res_secchi$serial <- as.numeric(sub(".*_", "", res_secchi$water_quality_category))
  
  my_legend = theme(
    legend.text = element_text(size=12),
    legend.title.align =0,
    legend.position = "top", 
    legend.box = "horizontal", 
    # legend.title = element_text(size=12, vjust=0.5, hjust = 0.3),
    legend.title=element_blank())
  
  ggplot(res_secchi, aes(x=reorder(water_quality_category, serial), y=res, color=Proximity)) + 
    geom_errorbar(aes(ymin=res-1.959*het_10_se, ymax=res+1.959*het_10_se), width=.1) +
    #geom_line() +
    geom_point() +
    geom_hline(yintercept=0, linetype='dashed', color=c('red')) +
    scale_color_manual(values = c("100m buffer" = "red", "100-300m buffer" = "blue")) +
    
    xlab("Water Quality Category") +
    ylab("Residualized log of housing prices") +
    
    theme_classic()+
    theme(axis.text = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    my_legend
  
  # income by water quality at blockgroup level
  
  
  bg_income_by_quality <- function(water_quality_sale, distance_group){
    if(distance_group=='All property within 2000m'){
      wq_subset <- water_quality_sale
    } else if(distance_group=='Property within 100m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_shore==1,]
    }else if(distance_group=='Property within 100m-300m buffer'){
      wq_subset <- water_quality_sale[water_quality_sale$lake_front==1,]
    }else if(distance_group=='Property within 300m-2000m buffer'){
      wq_subset <- water_quality_sale[!(water_quality_sale$lake_front==1 | water_quality_sale$lake_shore==1),]
    }
    
    lake_quality <- aggregate(.~lake_nb_nhd_id+quality_category, 
                              wq_subset[c('lake_nb_nhd_id', 'quality_category', 'hh_med_income')], mean)
    bg_quality <- aggregate(.~quality_category, 
                            lake_quality[c('quality_category', 'hh_med_income')], 
                            function(x) c(mean = mean(x), sd = sd(x)))
    bg_quality$mean_income <- bg_quality$hh_med_income[,1]
    bg_quality$sd_income <- bg_quality$hh_med_income[,2]
    bg_quality$hh_med_income <- NULL
    bg_quality$distance_group <- distance_group
    colnames(bg_quality) <- c('quality_category', 'mean_income', 'sd_income', 'Distance')
    return(bg_quality)
  }
  
  wq_income_all <- bg_income_by_quality(water_quality_sale, distance_group='All property within 2000m')
  wq_income_100 <- bg_income_by_quality(water_quality_sale, distance_group='Property within 100m buffer')
  wq_income_100_300 <- bg_income_by_quality(water_quality_sale, distance_group='Property within 100m-300m buffer')
  wq_income_300_2000 <- bg_income_by_quality(water_quality_sale, distance_group='Property within 300m-2000m buffer')
  
  wq_income_by_bg <- as.data.frame(rbind(wq_income_all, wq_income_100, wq_income_100_300, wq_income_300_2000))
  wq_income_by_bg$serial <- as.numeric(sub(".*_", "", wq_income_by_bg$quality_category))
  
  ggplot(wq_income_by_bg, aes(x=reorder(quality_category, serial), y=mean_income, group = Distance)) + 
    # geom_errorbar(aes(ymin=mean_income-sd_income, ymax=mean_income+sd_income), width=.1) +
    geom_line(aes(colour=Distance))+geom_point(aes(colour=Distance)) +
    # geom_hline(yintercept=0, linetype='dashed', color=c('red')) +
    # scale_color_manual(values = c("100m buffer" = "red", "100-300m buffer" = "blue")) +
    scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K", accuracy = 0.1)) +
    
    xlab("Water Quality Category") +
    ylab("Average household median income by block group") +
    
    theme_classic()+
    theme(axis.text = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5)) +
    guides(color = guide_legend(nrow = 2))+
    my_legend
}

if (model == 'reg_by_year'){
  res <- did_cluster_fe$tract_id$coefficients[,1]
  
  het_10_se <- did_cluster_fe$tract_id$STATS$price_updated_hpi_sa$se
  
  res_secchi <- as.data.frame(cbind(res, het_10_se))
  res_secchi$water_quality <- rownames(res_secchi)
  res_secchi$water_quality_category <- sub(".*:year_", "", res_secchi$water_quality)
  
  res_secchi$Proximity <- sub(":.*", "", res_secchi$water_quality)
  res_secchi$Proximity <- ifelse(res_secchi$Proximity == "lake_shore", "100m buffer", "100-300m buffer")
  
  res_secchi$water_quality_category <- sub("lake_front:log_secchi", 2019, res_secchi$water_quality_category)
  res_secchi$water_quality_category <- sub("lake_shore:log_secchi", 2019, res_secchi$water_quality_category)
  
  res_secchi <- tail(res_secchi, 40)
  res_secchi$Significant <- ifelse(abs(res_secchi$res)/res_secchi$het_10_se>=1.65, 'Significantly different from 2019',
                                   'Not Significant')
  res_secchi$value <- ifelse(res_secchi$water_quality_category !=2019, 
                             ifelse(res_secchi$Proximity == '100-300m buffer', 
                                    res_secchi$res + res_secchi$res[res_secchi$water_quality_category==2019 & 
                                                                      res_secchi$Proximity == '100-300m buffer'],
                                    res_secchi$res + res_secchi$res[res_secchi$water_quality_category==2019 & 
                                                                      res_secchi$Proximity == '100m buffer']),
                             res_secchi$res)
  
  
  res_secchi$serial <- as.numeric(sub(".*_", "", res_secchi$water_quality_category))
  
  my_legend = theme(
    legend.text = element_text(size=12),
    legend.title.align =0,
    legend.position = "top", 
    legend.box = "horizontal", 
    legend.title = element_text(size=12, vjust=0.5, hjust = 0.3),
    legend.title=element_blank())
  
  reg_by_year_plot <- ggplot(res_secchi, aes(x=water_quality_category, y=value, 
                         color=Proximity, shape = Significant)) + 
    #geom_errorbar(aes(ymin=res-1.959*het_10_se, ymax=res+1.959*het_10_se), width=.1) +
    #geom_line() +
    geom_point() +
    geom_hline(yintercept=0, linetype='dashed', color=c('red')) +
    scale_color_manual(values = c("100m buffer" = "red", "100-300m buffer" = "blue")) +
    
    xlab("Year of sale") +
    ylab("Log of housing prices") +
    
    theme_classic()+
    theme(axis.text = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1)) +
    my_legend
}
##################################################################################################
# Get results by year with fixed effect tract
if(model == 'reg_by_year'){
  did_reg_by_year <- list()
  
  reg_by_year_seg_lf <- c()
  reg_by_year_seg_ls <- c()
  reg_by_year_seg_lf_se <- c()
  reg_by_year_seg_ls_se <- c()
  
  for (yr in 1:length(year_list)){
    year_list <- unique(water_quality_sale$year)
    year_sale <- water_quality_sale[water_quality_sale$year == year_list[[yr]],]
    year_sale <- factorize_variables(year_sale)
    did_reg_by_year[[yr]] <- felm(fe_eqn_secchi_int, data = year_sale)
    
    reg_by_year_seg_ls[[yr]] <- did_reg_by_year[[yr]]$coefficients[,1]['lake_shore:log_secchi']
    reg_by_year_seg_lf[[yr]] <- did_reg_by_year[[yr]]$coefficients[,1]['lake_front:log_secchi']
    reg_by_year_seg_ls_se[[yr]] <- did_reg_by_year[[yr]]$STATS$price_updated_hpi_sa$se['lake_shore:log_secchi']
    reg_by_year_seg_lf_se[[yr]] <- did_reg_by_year[[yr]]$STATS$price_updated_hpi_sa$se['lake_front:log_secchi']
  }
  names(did_reg_by_year) <- year_list
  
  reg_by_year_table <- mtable('yr_2000' = did_reg_by_year[['2000']], 'yr_2001' = did_reg_by_year[['2001']],
                              'yr_2002' = did_reg_by_year[['2002']], 'yr_2003' = did_reg_by_year[['2003']],
                              'yr_2004' = did_reg_by_year[['2004']], 'yr_2005' = did_reg_by_year[['2005']],
                              'yr_2006' = did_reg_by_year[['2006']], 'yr_2007' = did_reg_by_year[['2007']],
                              'yr_2008' = did_reg_by_year[['2008']], 'yr_2009' = did_reg_by_year[['2009']],
                              
                              'yr_2010' = did_reg_by_year[['2010']], 'yr_2011' = did_reg_by_year[['2011']],
                              'yr_2012' = did_reg_by_year[['2012']], 'yr_2013' = did_reg_by_year[['2013']],
                              'yr_2014' = did_reg_by_year[['2014']], 'yr_2015' = did_reg_by_year[['2015']],
                              'yr_2016' = did_reg_by_year[['2016']], 'yr_2017' = did_reg_by_year[['2017']],
                              'yr_2018' = did_reg_by_year[['2018']], 'yr_2019' = did_reg_by_year[['2019']],
                              
                              summary.stats = c("N", "adj. R-squared", "R-squared", "Log-likelihood", "BIC", "AIC"),
                              signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
                              digits = 4)
  
  
  reg_by_year_seg_ls_df <- data.frame("year" = as.character(year_list),"effect" = reg_by_year_seg_ls, 'se' = reg_by_year_seg_ls_se)
  reg_by_year_seg_lf_df <- data.frame("year" = as.character(year_list),"effect" = reg_by_year_seg_lf, 'se' = reg_by_year_seg_lf_se)
  
  reg_by_year_seg_ls_df$Proximity <- "100m buffer"
  reg_by_year_seg_lf_df$Proximity <- "100-300m buffer"
  
  reg_by_year_seg_data <- rbind(reg_by_year_seg_ls_df, reg_by_year_seg_lf_df)
  reg_by_year_seg_data$Significant <- ifelse(abs(reg_by_year_seg_data$effect)/reg_by_year_seg_data$se>=1.65,
                                             'Significant','Not Significant')
  
  ls_baseline <- 0.1673
  lf_baseline <- 0.0405
  
  reg_by_year_seg_plot <- ggplot(reg_by_year_seg_data, aes(x=year, y=effect, 
                                                 color=Proximity, shape = Significant)) + 
    #geom_errorbar(aes(ymin=res-1.959*het_10_se, ymax=res+1.959*het_10_se), width=.1) +
    #geom_line() +
    geom_point() +
    geom_hline(yintercept=ls_baseline, linetype='dashed', color=c('red')) +
    geom_text(aes(median(as.numeric(year)),ls_baseline, label = ls_baseline, vjust = - 1), col = "red") +
    geom_hline(yintercept=lf_baseline, linetype='dashed', color=c('blue')) +
    geom_text(aes(median(as.numeric(year)),lf_baseline, label = lf_baseline, vjust = - 1), col = "blue") +
    scale_color_manual(values = c("100m buffer" = "red", "100-300m buffer" = "blue")) +
    
    xlab("Year of sale") +
    ylab("Log of housing prices") +
    
    theme_classic()+
    theme(axis.text = element_text(size = 12)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    # theme(plot.background = element_rect(color = "#000000", fill = NA, size = 1)) +
    my_legend
}
########################################################################################
# only keep lakes that has a significant number of secchi observations
water_quality_sale$year_sample <- year(water_quality_sale$sample_min_year)
secchi_year <- aggregate(.~lake_nb_nhd_id+year_sample, 
                         water_quality_sale[c('lake_nb_nhd_id', 'year_sample', 'log_secchi')], first)
secchi_frequency <- as.data.frame(table(secchi_year$lake_nb_nhd_id))

# 'secchi_freq_2', 'secchi_freq_5', 'secchi_freq_7', 'secchi_freq_median', 'secchi_freq_10', 'secchi_freq_not_consider'

if(model == 'secchi_freq_not_consider'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 1])
} else if(model == 'secchi_freq_2'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 2])
} else if(model == 'secchi_freq_5'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 5])
} else if(model == 'secchi_freq_7'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 7])
} else if(model == 'secchi_freq_median'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= median(secchi_frequency$Freq)])
} else if(model == 'secchi_freq_10'){
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 10])
}else{
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 3])
}

secchi_sale <- water_quality_sale[water_quality_sale$lake_nb_nhd_id %in% lake_secchi,]

secchi_size <- aggregate(.~lake_nb_nhd_id, 
                         water_quality_sale[c('lake_nb_nhd_id', 'lake_size_sqkm')], first)
selected_lakes <- water_quality_sale[water_quality_sale$lake_nb_nhd_id %in% c('87961872', '112049879'),]

# Notes: 87961872 - size - 42.61 sqkm, sample - 1, total sales = 1847, lake_shore_sale = 230
# Notes: 112049879 - size - 5.16 sqkm, sample - 11, total sales = 694, lake_shore_sale = 187

#########################################################################################
cluster_error_list <- c('0', 'bg_id', 'tract_id', 'fips', 'state_fips')
did_secchi_fe <- list()
for(ce in 1: length(cluster_error_list)){
  
  fe_eqn_wq_int_cluster <- as.formula(paste0(c(paste0(dep_var,
                                                      paste0(c(lake_shore_front, non_fe_control, wq_secchi, 
                                                               paste0(CJ(lake_shore_front, wq_secchi)$lake_shore_front, ':',
                                                                      CJ(lake_shore_front, wq_secchi)$wq_secchi),
                                                               common_control, resid_control, neighbor_control),
                                                             collapse = '+')),
                                               paste0(fixed_effect, collapse = '+'),
                                               "0",cluster_error_list[ce]),collapse = '|'))
  
  
  did_secchi_fe[[ce]] <- felm(fe_eqn_wq_int_cluster, data = secchi_sale)
}

names(did_secchi_fe) <- cluster_error_list

secchi_table <- mtable('without_cluster' = did_secchi_fe[['0']],
                        
                        'fe_bg' = did_secchi_fe[['bg_id']], 'fe_tract' = did_secchi_fe[['tract_id']],
                        'fe_fips' = did_secchi_fe[['fips']], 'fe_state' = did_secchi_fe[['state_fips']],
                        
                        summary.stats = c("N", "adj. R-squared", "R-squared", "Log-likelihood", "BIC", "AIC"),
                        signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
                        digits = 4)
########################################################################################

sum_type <- list('slim_cluster_lim_obs_100' = water_quality_sale,
                 'slim_cluster_lim_obs_100_sample_5' = secchi_sale)

df_summary <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c('name',"lake", "bg", "tract","fips", "state"))
for(dt in 1: length(sum_type)){
  item <- sum_type[[dt]]
  sumstat_output <- data.frame('name'= names(sum_type)[[dt]],
                               'lake' = length(unique(item$lake_nb_nhd_id)),
                               'bg' = length(unique(item$bg_id)),
                               'tract' = length(unique(item$tract_id)),
                               'fips' = length(unique(item$fips)),
                               'state' = length(unique(item$state_fips))
  )
  df_summary <- rbind(df_summary, sumstat_output)
}
#########################################################################################################
# get F/Chi-sq test for joint significant
if(model %in% c('treat_shore_only_50',	'treat_shore_only_100',	'treat_shore_only_200', 'treat_waterfront')){
  nullhyp <- list(c("log_secchi", "lake_shore:log_secchi"))
  lake_shore_chi_sq <- linearHypothesis(did_cluster_fe[['tract_id']], unlist(nullhyp[1]))
  
  test_data_type <- list('lake_shore' = lake_shore_chi_sq)
} else{
  if(model == 'semi_log'){
    nullhyp <- list(c("secchi", "lake_shore:secchi"), c("secchi", "lake_front:secchi"))
  } else {
    nullhyp <- list(c("log_secchi", "lake_shore:log_secchi"), c("log_secchi", "lake_front:log_secchi"))
  }
  lake_shore_chi_sq <- linearHypothesis(did_cluster_fe[['tract_id']], unlist(nullhyp[1]))
  lake_front_chi_sq <- linearHypothesis(did_cluster_fe[['tract_id']], unlist(nullhyp[2]))
  
  test_data_type <- list('lake_shore' = lake_shore_chi_sq,
                         'lake_front' = lake_front_chi_sq)
}

chi_test <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), 
                     c('name',"Res_df_full", "Res_df_restricted", "df","Chi_sq", "p_value"))
for(t in 1: length(test_data_type)){
  item <- test_data_type[[t]]
  test_output <- data.frame('name'= names(test_data_type)[[t]],
                               'Res_df_full' = item$Res.Df[1],
                               'Res_df_restricted' = item$Res.Df[2],
                               'df' = item$Df[2],
                               'Chi_sq' = item$Chisq[2],
                               'p_value' = item$`Pr(>Chisq)`[2]
  )
  chi_test <- rbind(chi_test, test_output)
}
########################################################################################
result_folder <- file.path(working_dir, 'Results_individual', '2022_04_09')
dir.create(result_folder, showWarnings = FALSE)

if(model %in% c('lim_obs_1_sample_1', 'lim_obs_10_sample_3', 'lim_obs_100', 'baseline')){
  lake_output <- read.csv(file.path(working_dir, 'lake_maps', 'lake_ids.csv'))
  if(model %in% c('lim_obs_100', 'baseline')){
    lake_output[model] <- c(unique(water_quality_sale$lake_nb_nhd_id), 
                            rep(NA, nrow(lake_output)-length(unique(water_quality_sale$lake_nb_nhd_id))))
  }else{
    lake_output[model] <- c(unique(secchi_sale$lake_nb_nhd_id), 
                            rep(NA, nrow(lake_output)-length(unique(secchi_sale$lake_nb_nhd_id))))
  }
    
  write.csv(lake_output, file = file.path(working_dir, 'lake_maps', 'lake_ids.csv'), row.names = FALSE)
}


write.mtable(cluster_table,
             file=file.path(result_folder,
                            paste0(model,'_2022_04_09.txt')))
if(model != 'baseline_nla_2012'){
  write.mtable(secchi_table,
             file=file.path(result_folder,
                            paste0(model,'_sample_3_table_2022_04_09.txt')))
}
if(model == 'secchi_freq_median'){
  secchi_median <- median(secchi_frequency$Freq)
  write.table(secchi_median, paste0(model,'_sample_number_2022_04_09.txt'), row.names = FALSE)
}

write.csv(df_summary, file=file.path(result_folder, paste0(model, '_summary_2022_04_09.csv')))
write.csv(chi_test, file=file.path(result_folder, paste0(model, '_chi_test_2022_04_09.csv')))

if(model == 'baseline'){
  write.csv(data_summary, file=file.path(result_folder, paste0(model, '_data_summary_2022_04_09.csv')))
  write.csv(lakeshore_data_summary, file=file.path(result_folder, paste0(model, '_lakeshore_data_summary_2022_04_09.csv')))
  write.csv(lakefront_data_summary, file=file.path(result_folder, paste0(model, '_lakefront_data_summary_2022_04_09.csv')))
  
  write_parquet(secchi_lastest, sink = file.path(working_dir, 'lake_maps', paste0('tract_secchi_2022_04_09.pqt')))
  write_parquet(chla_lastest, sink = file.path(working_dir, 'lake_maps', paste0('tract_chla_2022_04_09.pqt')))

  write.csv(secchi_lastest, file=file.path(working_dir, 'lake_maps', paste0('tract_secchi_2022_04_09.csv')))
  write.csv(chla_lastest, file=file.path(working_dir, 'lake_maps', paste0('tract_chla_2022_04_09.csv')))

  write.csv(secchi_latest_lake, file=file.path(working_dir, 'lake_maps', paste0('lake_secchi_2022_04_09.csv')))
  write.csv(chla_latest_lake, file=file.path(working_dir, 'lake_maps', paste0('lake_chla_2022_04_09.csv')))
  
  write.csv(income_by_bg, file=file.path(result_folder, paste0('income_by_bg_2022_04_09.csv')), row.names = FALSE)
}
if(model == 'het_quality_10'){
  write.csv(wq_income_by_bg, file=file.path(result_folder, paste0('wq_income_by_bg_2022_04_09.csv')), row.names = FALSE)
}
if(model == 'baseline'){
  trend_file <- file.path(result_folder, paste0(model, '_trend_plot_2022_04_09.pdf'))
  ggsave(trend_file, trend_plot, width=13, height=10)
}

if(model == 'reg_by_year'){
  write.mtable(reg_by_year_table,
               file=file.path(result_folder,
                              paste0(model,'_segregate_2022_04_09.txt')))
  
  plot_file <- file.path(result_folder, paste0(model, '_plot_2022_04_09.pdf'))
  ggsave(plot_file, reg_by_year_plot, width=10, height=10)
  
  seg_plot_file <- file.path(result_folder, paste0(model, '_seg_plot_2022_04_09.pdf'))
  ggsave(seg_plot_file, reg_by_year_seg_plot, width=10, height=10)
}


