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

# 'treat_shore_only_50',	'treat_shore_only_100'	'treat_shore_only_200', 
# 'treat_shore_50_front_200',	'treat_shore_50_front_300',
# 'treat_shore_100_front_200',	'treat_shore_200_front_500',	'treat_shore_300_front_1000'

# 'river_15000',	'river_50',	'river_100',	'river_200',	'river_300'

# 'sale_sample_diff_1',	'sale_sample_diff_3',	'sale_sample_diff_10',	'sale_sample_diff_not_considered'

# 'secchi_freq_2', 'secchi_freq_5', 'secchi_freq_7', 'secchi_freq_median', 'secchi_freq_10', 'secchi_freq_not_consider'

# 'lim_obs_1_sample_1', 'lim_obs_10_sample_3'   <- we dont have to run this separately.

# fe_year_state, fe_year_by_state, fe_year_tract, fe_year_fips, fe_year_by_fips, fe_year_bg, fe_year_by_bg
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

# 'treat_shore_only_50',	'treat_shore_only_100'	'treat_shore_only_200', 
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

if(model %in% c('treat_shore_only_50',	'treat_shore_only_100',	'treat_shore_only_200')){
  lake_shore_front <- c('lake_shore')
} else{
  lake_shore_front <- c('lake_shore', 'lake_front')
}


wq_secchi <- 'log_secchi'

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
}else if(model == 'fe_year_tract'){
  fixed_effect <- c('year', 'tract_id')
} else if(model == 'fe_year_fips'){
  fixed_effect <- c('year', 'fips')
} else if(model %in% c('fe_year_by_fips', 'baseline_nla_2012')){
  fixed_effect <- c('year:fips')
}else if(model == 'fe_year_bg'){
  fixed_effect <- c('year', 'bg_id')
} else if(model == 'fe_year_by_bg'){
  fixed_effect <- c('year:bg_id')
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
}

########################################################################################################
# factorizing variable
water_quality_sale$bg_id <- factor(water_quality_sale$bg_id)
water_quality_sale$tract_id <- factor(water_quality_sale$tract_id)
water_quality_sale$fips <- factor(water_quality_sale$fips)
water_quality_sale$state_fips <- factor(water_quality_sale$state_fips)
water_quality_sale$year <- factor(water_quality_sale$year)
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
##################################################################################################
# Cluster std error
cluster_error_list <- c('0', 'bg_id', 'tract_id', 'fips', 'state_fips')
did_cluster_fe <- list()
for(ce in 1: length(cluster_error_list)){
  
  fe_eqn_wq_int_cluster <- as.formula(paste0(c(paste0(dep_var,
                                                      paste0(c(lake_shore_front, non_fe_control, wq_secchi, 
                                                               paste0(CJ(lake_shore_front, wq_secchi)$lake_shore_front, ':',
                                                                      CJ(lake_shore_front, wq_secchi)$wq_secchi),
                                                               common_control, resid_control, neighbor_control),
                                                             collapse = '+')),
                                               paste0(fixed_effect, collapse = '+'),
                                               "0",cluster_error_list[ce]),collapse = '|'))
  
  
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
if(model %in% c('treat_shore_only_50',	'treat_shore_only_100',	'treat_shore_only_200')){
  nullhyp <- list(c("log_secchi", "lake_shore:log_secchi"))
  lake_shore_chi_sq <- linearHypothesis(did_cluster_fe[['tract_id']], unlist(nullhyp[1]))
  
  test_data_type <- list('lake_shore' = lake_shore_chi_sq)
} else{
  nullhyp <- list(c("log_secchi", "lake_shore:log_secchi"), c("log_secchi", "lake_front:log_secchi"))
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
}

