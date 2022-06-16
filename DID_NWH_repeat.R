#Installing required package if not already installed
req_packages = c("data.table", "arrow", "lfe", "memisc", "stringr", "pastecs", "corrplot")
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

# 'repeat_analysis','repeat_year' # repeat_year_multi
# 'repeat_analysis_diff','repeat_year_diff' # repeat_year_multi_diff
# 'repeat_analysis_var','repeat_year_var' # repeat_year_multi_var <- we do not have to do this.
# 'all_repeat_sample_3' parameters will be sufficient.

# 'repeat_no_fe'

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
sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)

if(model %in% c('repeat_analysis_diff', 'repeat_year_diff', 'repeat_year_multi_diff')){
  sale_subset <- sale_subset[which(sale_subset$year_diff <=0),] #baseline 5
} else{
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
sale_subset <- sale_subset[-which(sale_subset$river_dist < sale_subset$lake_dist),]
# sale_subset <- sale_subset[-which(sale_subset$river_dist <= 300),]

# delete data with less than 10,000
sale_subset <- sale_subset[-which(sale_subset$price < 10000),]

# delete if price_per_ha is more than 99th percentile
sale_subset$price_per_ha <- sale_subset$price/sale_subset$ha
sale_subset <- sale_subset[-which(sale_subset$price_per_ha >= quantile(sale_subset$price_per_ha, p=0.99)),]

##########################################################################################################
# Create Treatment and control variables
sale_subset$lake_shore <- ifelse(sale_subset$lake_dist <= 100, 1,0)

sale_subset$lake_front <- ifelse((sale_subset$lake_dist <= 300 & sale_subset$lake_dist > 100), 1,0)

# sale_subset <- sale_subset[which(sale_subset$lake_dist <= 200 | sale_subset$lake_dist >=500),]

sale_subset <- sale_subset[!is.na(sale_subset$bg_id),]
sale_subset$tract_id <- paste0(sale_subset$state_fips, sale_subset$fips, sale_subset$tract_id)

###########################################################################################################
dep_var <- 'price_updated_hpi_sa ~' # price_updated, price_updated_hpi_nsa, price_updated_hpi_sa
# lake_shore_front <- c('lake_shore')
lake_shore_front <- c('lake_shore', 'lake_front')
wq_secchi <- 'log_secchi'

resid_control <- c("log_bld_age",
                   "log_bld_area")
common_control <- c("log_lot_area", 'slope', 'elev', 'log_rd_dist_hwy')

non_fe_control <- 'lake_size_sqkm'

neighbor_control <- c('0') # 'log_med_inc_bg'

# 'repeat_analysis','repeat_year' # repeat_year_multi
# 'repeat_analysis_diff','repeat_year_diff' # repeat_year_multi_diff
# 'repeat_analysis_var','repeat_year_var' # repeat_year_multi_var
# 'repeat_analysis_var_diff','repeat_year_var_diff' # repeat_year_multi_var_diff

# 'repeat_no_fe'

if(model == 'repeat_no_fe'){
  fixed_effect <- c('0')
}else if(model %in% c('repeat_year', 'repeat_year_diff', 'repeat_year_var', 'repeat_year_var_diff')){
  fixed_effect <- c('pid', 'year')
} else if(model %in% c('repeat_year_multi', 'repeat_year_multi_diff', 'repeat_year_multi_var', 'repeat_year_multi_var_diff')){
  fixed_effect <- c('pid:year')
} else{
  fixed_effect <- c('pid') # 'year' as linear, 'year' as multi
}

cluster_error <- c('tract_id') # lake_nb_nhd_id


# fixed effect models
fe_repeat_eqn <- as.formula(paste0(c(paste0(dep_var,
                                                paste0(c(wq_secchi),
                                                       collapse = '+')),
                                         paste0(fixed_effect, collapse = '+'),
                                         "0",cluster_error),collapse = '|'))

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

# handling complete cases
water_quality_sale <- resid_dev_sale[which(complete.cases(resid_dev_sale[names(get_all_vars(
  fe_repeat_eqn, resid_dev_sale))])),]
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
#############################################################################################
#REGRESSIONS
# complying that it only include duplicate sales
water_quality_sale <- water_quality_sale[
  which(water_quality_sale$pid %in% names(table(water_quality_sale$pid))[
    which(table(water_quality_sale$pid)>1)]),]

# complying that it has variation of water quality by pid
# 'repeat_analysis_var','repeat_year_var' # repeat_year_multi_var
# 'repeat_analysis_var_diff','repeat_year_var_diff' # repeat_year_multi_var_diff
if(model %in% c('repeat_analysis_var','repeat_year_var', 'repeat_year_multi_var',
  'repeat_analysis_var_diff','repeat_year_var_diff', 'repeat_year_multi_var_diff')){
  
  wq_min <- aggregate(.~pid, water_quality_sale[c('pid', 'log_secchi')], min)
  wq_max <- aggregate(.~pid, water_quality_sale[c('pid', 'log_secchi')], max)
  wq_agg <- merge(wq_min, wq_max, by='pid')
  wq_agg$wq_variation <- ifelse(wq_agg$log_secchi.x == wq_agg$log_secchi.y, 0, 1)
  
  water_quality_sale <- merge(water_quality_sale, wq_agg[c('pid', 'wq_variation')],
                              by = 'pid')
  water_quality_sale <- water_quality_sale[water_quality_sale$wq_variation == 1,]
}

# factorizing variable
factor_dataset <- function(dataset){
  dataset$year <- factor(as.integer(dataset$year))
  dataset$state_fips <- factor(as.character(dataset$state_fips))
  dataset$tract_id <- factor(as.character(dataset$tract_id))
  dataset$pid <- factor(as.character(dataset$pid))
  return(dataset)
}
water_quality_sale <- factor_dataset(water_quality_sale)

did_repeat <- felm(fe_repeat_eqn, data = water_quality_sale)
############################################################################################
# Only using lake_shore_front sales
lake_front_shore_repeat_sale <- water_quality_sale[(water_quality_sale$lake_shore==1 | water_quality_sale$lake_shore==1),]

# complying that it only include duplicate sales
lake_front_shore_repeat_sale <- lake_front_shore_repeat_sale[
  which(lake_front_shore_repeat_sale$pid %in% names(table(lake_front_shore_repeat_sale$pid))[
    which(table(lake_front_shore_repeat_sale$pid)>1)]),]

lake_front_shore_repeat_sale <- factor_dataset(lake_front_shore_repeat_sale)

did_lake_front_shore <- felm(fe_repeat_eqn, data = lake_front_shore_repeat_sale)
############################################################################################
#using all repeat sales and filtering minimum water quality sample
# only keep lakes that has a significant number of secchi observations
secchi_filtering <- function(sale_data, minimum_sample=5){
  sale_data$year_sample <- year(sale_data$sample_min_year)
  secchi_year <- aggregate(.~lake_nb_nhd_id+year_sample, 
                           sale_data[c('lake_nb_nhd_id', 'year_sample', 'log_secchi')], first)
  secchi_frequency <- as.data.frame(table(secchi_year$lake_nb_nhd_id))
  
  # lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= median(secchi_frequency$Freq)])
  
  lake_secchi <- as.character(secchi_frequency$Var1[secchi_frequency$Freq >= 5])
  
  secchi_sale <- sale_data[sale_data$lake_nb_nhd_id %in% lake_secchi,]
  
  return(secchi_sale)
}

secchi_sale <- secchi_filtering(sale_data = water_quality_sale, minimum_sample = 3)

# complying that it only include duplicate sales
secchi_sale <- secchi_sale[
  which(secchi_sale$pid %in% names(table(secchi_sale$pid))[
    which(table(secchi_sale$pid)>1)]),]

secchi_sale <- factor_dataset(secchi_sale)

did_repeat_sample <- felm(fe_repeat_eqn, data = secchi_sale)
############################################################################################
# using lake_shore_front_sale after filtering minimum water quality sample
lake_front_shore_repeat_sample_5_sale <- secchi_filtering(sale_data = lake_front_shore_repeat_sale, minimum_sample = 3)

# complying that it only include duplicate sales
lake_front_shore_repeat_sample_5_sale <- lake_front_shore_repeat_sample_5_sale[
  which(lake_front_shore_repeat_sample_5_sale$pid %in% names(table(lake_front_shore_repeat_sample_5_sale$pid))[
    which(table(lake_front_shore_repeat_sample_5_sale$pid)>1)]),]

lake_front_shore_repeat_sample_5_sale <- factor_dataset(lake_front_shore_repeat_sample_5_sale)

did_lake_front_shore_sample <- felm(fe_repeat_eqn, data = lake_front_shore_repeat_sample_5_sale)
###########################################################################################
repeat_table <- mtable('all_repeat' = did_repeat,
                       
                       'repeat_within_300m' = did_lake_front_shore,
                       'all_repeat_sample_3' = did_repeat_sample,
                       'repeat_within_300m_sample_3' = did_lake_front_shore_sample,
                       
                       summary.stats = c("N", "adj. R-squared", "R-squared", "Log-likelihood", "BIC", "AIC"),
                       signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
                       digits = 4)
########################################################################################

sum_type <- list('all_repeat' = water_quality_sale,
                 'repeat_within_300m' = lake_front_shore_repeat_sale,
                 'all_repeat_sample_3' = secchi_sale,
                 'repeat_within_300m_sample_3' = lake_front_shore_repeat_sample_5_sale)

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

########################################################################################
result_folder <- file.path(working_dir, 'Results_individual', '2022_04_09')
dir.create(result_folder, showWarnings = FALSE)

write.mtable(repeat_table,
             file=file.path(result_folder,
                            paste0(model,'_2022_04_09.txt')))

write.csv(df_summary, file=file.path(result_folder, paste0(model, '_summary_2022_04_09.csv')))



