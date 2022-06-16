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
############################################################################################################################################
# Importing sales data
sale_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'sale_water_quality_data',
                                                   'sale_water_quality_data.pqt')))

#########################################################################################################
# SUBSET THE DATA
# Only keep lake states (MI-26, MN-27, WI-55)
# state_keep <- c(26, 27, 55)

# sale_subset <- sale_lakes[which(sale_lakes$state_fips %in% state_keep),]

# delete all great lakes (max lake size Lake Ontario: 18845.9359 sqkm, Great Salt Lake: 3389.502116)
sale_subset <- sale_lakes[which(sale_lakes$lake_size_sqkm <= 10000),]

# delete if date (date of sale) is before 2000
sale_subset <- sale_subset[which(sale_subset$year>=2000),]

# delete if parameters sampledate is not within 5 years of sale date
sale_subset$year_diff <- abs(year(sale_subset$sample_min_year) - sale_subset$year)
sale_subset <- sale_subset[which(sale_subset$year_diff <=5),]

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
sale_subset <- sale_subset[-which(sale_subset$price_per_ha >= quantile(sale_subset$price_per_ha, p=.99)),]

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
wq_secchi <- c('log_secchi')
wq_chla <- c('log_chla')
wq_both <- c(wq_secchi, wq_chla)

resid_control <- c("log_bld_age",
                   "log_bld_area")
common_control <- c("log_lot_area", 'slope', 'elev', 'log_rd_dist_hwy')

non_fe_control <- 'lake_size_sqkm'

neighbor_control <- c('0') # 'log_med_inc_bg'

fixed_effect <- c('year:tract_id')
cluster_error <- c('tract_id') # lake_nb_nhd_id

# fixed effect models
fe_eqn_wq_int <- as.formula(paste0(c(paste0(dep_var,
                                                   paste0(c(lake_shore_front, non_fe_control, wq_both,
                                                            paste0(CJ(lake_shore_front, wq_both)$lake_shore_front, ':',
                                                                   CJ(lake_shore_front, wq_both)$wq_both),
                                                            common_control, resid_control, neighbor_control),
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

fe_eqn_chla_int <- as.formula(paste0(c(paste0(dep_var,
                                            paste0(c(lake_shore_front, non_fe_control, wq_chla,
                                                     paste0(CJ(lake_shore_front, wq_chla)$lake_shore_front, ':',
                                                            CJ(lake_shore_front, wq_chla)$wq_chla),
                                                     common_control, resid_control, neighbor_control),
                                                   collapse = '+')),
                                     paste0(fixed_effect, collapse = '+'),
                                     "0",cluster_error),collapse = '|'))


##############################################################################################################
resid_dev_sale <- sale_subset[(sale_subset$bld_code_two == 'RR' & sale_subset$develop == 1),]

# handling complete cases and limited data by lake

water_quality_sale <- resid_dev_sale[which(complete.cases(resid_dev_sale[names(get_all_vars(
  fe_eqn_wq_int, resid_dev_sale))])),]

min_number_transaction <- 1

limited_lakefront_data <- function(data=water_quality_sale, min_number_transaction=1){
  limited_lake_front <- as.data.frame(table(data$lake_nb_nhd_id[data$lake_shore==1]))
  limited_lake_front_lake <- as.character(unique(limited_lake_front$Var1[limited_lake_front$Freq >= min_number_transaction]))
  
  limited_not_lake_front <- as.data.frame(table(data$lake_nb_nhd_id[data$lake_shore==0]))
  limited_not_lake_front_lake <- as.character(unique(limited_not_lake_front$Var1[limited_not_lake_front$Freq >= min_number_transaction]))
  
  limited_impair_lake <- intersect(limited_lake_front_lake, limited_not_lake_front_lake)
  
  data <- data[data$lake_nb_nhd_id %in% c(limited_impair_lake),]
  
  return(data)
}

water_quality_sale <- limited_lakefront_data(data=water_quality_sale, min_number_transaction=min_number_transaction)
########################################################################################################
# BOTH WATER QUALITY
# factorizing variable
water_quality_sale$tract_id <- factor(water_quality_sale$tract_id)
water_quality_sale$year <- factor(water_quality_sale$year)

# regresion
both_water_quality <- felm(fe_eqn_wq_int, data = water_quality_sale)
secchi_only_both_data_available <- felm(fe_eqn_secchi_int, data = water_quality_sale)
chla_only_both_data_available <- felm(fe_eqn_chla_int, data = water_quality_sale)

# secchi only where secchi data available
secchi_only_sale <- resid_dev_sale[which(complete.cases(resid_dev_sale[names(get_all_vars(
  fe_eqn_secchi_int, resid_dev_sale))])),]
secchi_only_sale <- limited_lakefront_data(data=secchi_only_sale, min_number_transaction=min_number_transaction)
# factorizing variable
secchi_only_sale$tract_id <- factor(secchi_only_sale$tract_id)
secchi_only_sale$year <- factor(secchi_only_sale$year)

secchi_only <- felm(fe_eqn_secchi_int, data = secchi_only_sale)

# chla only where secchi data available
chla_only_sale <- resid_dev_sale[which(complete.cases(resid_dev_sale[names(get_all_vars(
  fe_eqn_chla_int, resid_dev_sale))])),]
chla_only_sale <- limited_lakefront_data(data=chla_only_sale, min_number_transaction=min_number_transaction)

# factorizing variable
chla_only_sale$tract_id <- factor(chla_only_sale$tract_id)
chla_only_sale$year <- factor(chla_only_sale$year)

chla_only <- felm(fe_eqn_chla_int, data = chla_only_sale)

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
chla_table <- mtable('both_secchi_chla' = both_water_quality,
                        
                        'secchi_with_both_data_available' = secchi_only_both_data_available,
                     'chla_with_both_data_available' = chla_only_both_data_available,
                        'secchi_only' = secchi_only,
                     'chla_only' = chla_only,
                        
                        summary.stats = c("N", "adj. R-squared", "R-squared", "Log-likelihood", "BIC", "AIC"),
                        signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
                        digits = 4)


########################################################################################

sum_type <- list('both_secchi_chla' = water_quality_sale,
                 'secchi_only' = secchi_only_sale,
                 'chla_only' = chla_only_sale)

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
#########################################################################################
# get F/Chi-sq test for joint significant
nullhyp <- list(c("log_secchi", "lake_shore:log_secchi"), c("log_secchi", "lake_front:log_secchi"),
                c("log_chla", "lake_shore:log_chla"), c("log_chla", "lake_front:log_chla"))
lake_shore_chi_sq <- linearHypothesis(chla_only, unlist(nullhyp[3]))
lake_front_chi_sq <- linearHypothesis(chla_only, unlist(nullhyp[4]))

test_data_type <- list('lake_shore_chla' = lake_shore_chi_sq,
                       'lake_front_chla' = lake_front_chi_sq)

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

write.mtable(chla_table,
             file=file.path(result_folder,
                            paste0('chla','_2022_04_09.txt')))

write.csv(df_summary, file=file.path(result_folder, paste0('chla', '_summary_2022_04_09.csv')))
write.csv(chi_test, file=file.path(result_folder, paste0('chla', '_chi_test_2022_04_09.csv')))



