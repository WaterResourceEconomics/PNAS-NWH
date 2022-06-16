#Installing required package if not already installed
req_packages = c("LAGOSNE","dplyr", "readxl", "data.table", "doParallel", "arrow", "bayesbio")
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

#Load environment/data file

if(!exists("saleh_lagosne_extract.RData") )
{
  load("saleh_lagosne_extract.RData")
}

# data_lagosne <- lagosne_load()
data_lagosne <- readRDS(file.path(working_dir, 'LAGOSNE_data', 'data_1.087.3.rds'))

all_epi <- merge(data_lagosne$epi_nutr, 
                data_lagosne$lakes.geo[c('state_zoneid', 'lakes_nhdid', 'lagoslakeid')],
                by = 'lagoslakeid', all.x = TRUE)


######################################################################################################################
state_id = 'State_14'

mn_epi <- all_epi[all_epi['state_zoneid'] == state_id,]

# us_epa <- read.csv("us_epa.csv")
# 
# nrow(us_epa[us_epa$StateCode == 42,])

#load MN data from EPA
mn_epa <- read.csv("mn_epa.csv")

mn_epa$ActivityStartDate <- as.Date(mn_epa$ActivityStartDate, format('%Y-%m-%d'))
mn_epa$PERMANENT_IDENTIFIER <- as.character(mn_epa$PERMANENT_IDENTIFIER)

# mn_epa_epi <- merge(mn_epa[c('ActivityStartDate', 'MonitoringLocationIdentifier', 'PERMANENT_IDENTIFIER', 'secchi_result')],
#                      mn_epi, by.x = c('ActivityStartDate', 'PERMANENT_IDENTIFIER'), 
#                      by.y = c('sampledate', 'lakes_nhdid'), all.x = TRUE)
# 
# mn_epa_epi2 <- merge(mn_epa[c('ActivityStartDate', 'MonitoringLocationIdentifier','PERMANENT_IDENTIFIER', 'secchi_result')],
#                     mn_epi, by.x = c('ActivityStartDate', 'PERMANENT_IDENTIFIER'), 
#                     by.y = c('sampledate', 'lakes_nhdid'), all.y = TRUE)

# this is same as mn_matched below
mn_merged_both <- merge(mn_epa[c('ActivityStartDate', 'MonitoringLocationIdentifier','PERMANENT_IDENTIFIER', 
                                 'secchi_result', 'chla_result',
                                 "LatitudeMeasure", "LongitudeMeasure")],
                                       mn_epi, by.x = c('ActivityStartDate', 'PERMANENT_IDENTIFIER'), 
                                       by.y = c('sampledate', 'lakes_nhdid'), all = TRUE)

dupe = mn_epa[,c('ActivityStartDate','PERMANENT_IDENTIFIER')]
mn_epa[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]





# write.csv(mn_epa_epi, "mn_epa_epi.csv")
# write.csv(mn_epa_epi2, "mn_epa_epi2.csv")
# write.csv(mn_merged_both, "mn_epa_epi_merged.csv")


##################################################################################################
#loan MN delist data
mn_delist <- read.csv("MN_delist.csv")
mn_matched <- merge(mn_epa[c('ActivityStartDate', 'MonitoringLocationIdentifier', 'PERMANENT_IDENTIFIER', 
                             "LatitudeMeasure", "LongitudeMeasure",
                             'secchi_result', 'chla_result')],
                    mn_epi, by.x = c('ActivityStartDate', 'PERMANENT_IDENTIFIER'), 
                    by.y = c('sampledate', 'lakes_nhdid'), all = TRUE)


mn_matched$secchi <- ifelse(is.na(mn_matched$state_zoneid), mn_matched$secchi_result, mn_matched$secchi)

# for(i in 1:nrow(mn_matched)){
#   if(is.na(mn_matched$state_zoneid[i])==TRUE){
#     mn_matched$secchi[i] = mn_matched$secchi_result[i]
#     mn_matched$chla[i] = mn_matched$chla_result[i]
#   }
# }


mn_matched[mn_matched$PERMANENT_IDENTIFIER %in% mn_delist$REACHCODE,]

mn_matched[mn_matched$PERMANENT_IDENTIFIER == '{E9D0097E-8C2C-32F4-E043-0100007F64F3}',]


########################################################################
sale_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'sale', 
                                                  'sale_MN_lakes_1km.pqt')))
pc_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'sale', 
                                                   'pc_MN_lakes_1km.pqt')))
sale_pid_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'sale', 
                                                 'sale_pids_MN_lakes_1km.pqt')))


length(unique(pc_lakes$lake_nhd_id))

# remove multiple property sales by deleting both duplicated sids
multiple_pids <- as.data.frame(table(sale_pid_lakes$sid))
names(multiple_pids) <- c('sid', 'count_sid')
multiple_pids <- multiple_pids[multiple_pids$count_sid==1,]

# merge multiple_pids with sale_pid_lakes
sale_pid_uq <- merge(multiple_pids, sale_pid_lakes[c('sid', 'pid')], by='sid', all.x = TRUE)
# merge sale_pid_uq with pc_lakes to get lakes nhdid and nb_lakes nhdid
sale_pid_nhid <- merge(sale_pid_uq, pc_lakes, by = 'pid', all.x = TRUE)

# merge to get complete sales data
sale_lakes_zt <- sale_lakes[sale_lakes$source=='zt',]
sale_all <- merge(sale_pid_nhid, sale_lakes_zt, by = 'sid', all.x=TRUE)

###########################################################################################
mn_epa_impaired_delisted <- read.csv('mn_epa_impaired_delisted_2020.csv')

mn_epa_impaired_delisted <- mn_epa_impaired_delisted[c('PERMANENT_IDENTIFIER',
                                                       'Delist.year',
                                                       'Year.TMDL.plan.approved',
                                                       'Year.added.to.List')]
mn_epa_impaired_delisted <- mn_epa_impaired_delisted[!duplicated(mn_epa_impaired_delisted$PERMANENT_IDENTIFIER),]

mn_matched_impaired <- merge(mn_matched, mn_epa_impaired_delisted, by='PERMANENT_IDENTIFIER', all.x = TRUE)


# merge water quality data with sale_all
mn_matched_impaired$ActivityStartDate <- as.Date(mn_matched_impaired$ActivityStartDate, format='%Y-%m-%d')

colnames(mn_matched_impaired)[colnames(mn_matched_impaired) == "PERMANENT_IDENTIFIER"] <- "lake_nb_nhd_id"

mn_matched_impaired$lake_nb_nhd_id <- as.character(mn_matched_impaired$lake_nb_nhd_id)

#have to delete sale that does not have any water_quality data before matching
sale_all <- sale_all[sale_all$lake_nb_nhd_id %in% 
                       unlist(list(unique(mn_matched_impaired$lake_nb_nhd_id))),]
svsv <- merge(sale_all, mn_epa_impaired_delisted, by.x = 'lake_nb_nhd_id',
              by.y = 'PERMANENT_IDENTIFIER')

#do it by year, it is very slow for each date.
mn_matched_impaired$sampleyear <- year(mn_matched_impaired$ActivityStartDate)

#mn_matched_impaired$sampleyear <- as.Date(ISOdate(mn_matched_impaired$sampleyear, 6, 30))

mn_matched_subset <- mn_matched_impaired[c('lake_nb_nhd_id', 'sampleyear', 'secchi', 'chla')]

#replace NA with 0
mn_matched_subset$secchi[is.na(mn_matched_subset$secchi)] <- 0
mn_matched_subset$chla[is.na(mn_matched_subset$chla)] <- 0

mn_matched_subset_agg <- aggregate(.~lake_nb_nhd_id+sampleyear, mn_matched_subset, max)

#replace 0 with NA for secchi and chla
mn_matched_subset_agg$secchi[mn_matched_subset_agg$secchi == 0] <- NA
mn_matched_subset_agg$chla[mn_matched_subset_agg$chla==0] <- NA

# merge only subset as it is taking a long time [THIS IS DOING EXACT MATCHING.
# I WOULD LIKE TO USE NEARBYTIME MATCHING> SEE BELOW.]
sale_delisted <- sale_all[sale_all$lake_nb_nhd_id %in% 
                       unlist(list(unique(mn_matched_impaired$lake_nb_nhd_id[
                         !is.na(mn_matched_impaired$Delist.year)]))),]


sale_impaired <- sale_all[sale_all$lake_nb_nhd_id %in% 
                            unlist(list(unique(mn_matched_impaired$lake_nb_nhd_id[
                              !is.na(mn_matched_impaired$added.to.list)]))),]

##########################################################################################
agg <- mn_matched_subset_agg %>%
  group_by(lake_nb_nhd_id) %>%
  arrange(sampleyear) %>%
  filter(row_number()==1)


sale_all <- sale_all[which(!is.na(sale_all$year)),]
sale_all$year <- as.Date(paste0(sale_all$year,"-01-01"))

mn_matched_subset_agg$sampleyear <- as.Date(paste0(mn_matched_subset_agg$sampleyear,"-01-01"))

sale_all <- sale_all[c('sid', 'pid', 'count_sid', 'lake_nhd_id', 'lake_dist', 'lake_nb_nhd_id',
             'year')]



sale_water <- sale_water[which(!is.na(sale_water$sid)),]
sale_water[c('sid', 'pid', 'count_sid', 'lake_nhd_id', 'lake_dist', 'lake_nb_nhd_id',
             'year', 'sampleyear',
             'secchi', 'chla'),]


sale_water_exact <- merge(sale_all, mn_matched_subset_agg, 
                    by.x = c('lake_nb_nhd_id', 'year'),
                    by.y = c('lake_nb_nhd_id', 'sampleyear'),
                    all.x = TRUE)

sale_non_exact <- sale_all[which(is.na(sale_water_exact$secchi)),]
row.names(sale_non_exact) <- NULL

sale_water_non_exact <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(sale_water_non_exact) <- c("sid", "pid", "count_sid", "lake_nhd_id",
                                    "lake_dist", "lake_nb_nhd_id", "year", "secchi", "chla" )

sale_list <- list()

system.time({
sale_water_non_exact_p <- nearestTimeandID(sale_non_exact[1:7,], mn_matched_subset_agg,
                                           'year', 'sampleyear', 'lake_nb_nhd_id')
})

system.time({
for(i in 1:7){
  sale_water_non_exact_x <- nearestTimeandID(sale_non_exact[i,], mn_matched_subset_agg,
                                               'year', 'sampleyear', 'lake_nb_nhd_id')
  sale_water_non_exact <- rbind(sale_water_non_exact, sale_water_non_exact_x)
  # sale_list[[i]] <- sale_water_non_exact_x
}
})

big_data = do.call(rbind, sale_list)

#######################################################################################
fuzzy_match_time_id <- function(i, chunk=50, sale_non_exact, mn_matched_subset_agg){
  s <- i*chunk + 1
  e <- min((i+1)*chunk, nrow(sale_non_exact))
  sale_water_non_exact_x <- nearestTimeandID(sale_non_exact[s:e,], mn_matched_subset_agg,
                                             'year', 'sampleyear', 'lake_nb_nhd_id')
  return(sale_water_non_exact_x)
}

chunk_size <- 500
range_non_exact <- 1:12
  
  #ceiling(nrow(sale_non_exact)/chunk_size)

(cl = detectCores() %>% 
   makeCluster()) %>% 
  registerDoParallel()

# read and bind
#use the below block if you need to merge sale data. It will take time


system.time({
  sale_water_fuzzy = foreach(i = range_non_exact,
                     .packages = c("data.table", "bayesbio")) %dopar% {
                       fuzzy_match_time_id(i, chunk=chunk_size, sale_non_exact, mn_matched_subset_agg)
                     } %>%
    rbindlist(fill = T)
})
# end of parallel work
#stopImplicitCluster(cl)
stopCluster(cl)



nrow(head(sale_non_exact))
sale_water <- merge(sale_water, mn_epa_impaired_delisted, 
                    by.x = 'lake_nb_nhd_id',
                    by.y = 'PERMANENT_IDENTIFIER',
                    all.x = TRUE)

#########################################################################################
#Clean sale_water data
#merging with cpi_data
cpi_june <- read.csv("cpi_June.csv")
colnames(cpi_june) <- c("Year", "Period", "Label", "cpi")
sale_water$year <- as.integer(sale_water$year)

sale_clean <- merge(sale_water, cpi_june[c('Year', 'cpi')], by.x = 'year', by.y = 'Year',
                 all.x = TRUE)


#get two digit building code
sale_clean$bld_code_two <- substr(sale_clean$bld_code, 1, 2)

#get air condition, heating yes, no
sale_clean$bld_air <- ifelse(sale_clean$bld_air == "", 0, ifelse(sale_clean$bld_air=="NO", 0, 1))
sale_clean$bld_heat <- ifelse(sale_clean$bld_heat == "", 0,1)
sale_clean$bld_fire <- ifelse(sale_clean$bld_fire == "", 0,1)
sale_clean$lake <- ifelse(sale_clean$lake_frontage > 0, 1,0)

sale_clean$bld_gross_area <- ifelse((sale_clean$bld_sqft == 0 | is.na(sale_clean$bld_sqft) == TRUE),
                                 ifelse((sale_clean$bld_sqft_g==0 | is.na(sale_clean$bld_sqft_g) == TRUE),
                                        sale_clean$m2_bld_fp*10.76391, sale_clean$bld_sqft_g),
                                 sale_clean$bld_sqft)

#TODO: have to change to age at the time of sale
sale_clean$bld_yr_combined <- ifelse(is.na(sale_clean$bld_yr_eff) == TRUE,
                                  2019-as.integer(sale_clean$bld_yr), 2019-as.integer(sale_clean$bld_yr_eff))

sale_clean$develop <- ifelse(sale_clean$bld_gross_area > 0, 1,0)

#inflation corrected price
sale_clean$price_updated <- log(as.numeric(sale_clean$prc_ha))*sale_clean$cpi/256.143 #2019 June cpi

###########################################################################################
resid_dev_sale <- sale_clean[(sale_clean$bld_code_two == "RR" &
                                sale_clean$develop == 1),]

##########################################################################################
##############################################################################
dep_treat <- c('price_updated', 'after_before', 'polygon')
common_control <- c("ha", 'slope', 'elev',
                    'p_wet', 'p_prot_2010_1000',
                    'travel_weiss', 'rd_dist_hwy', 'rd_dist_pvd', 'p_bld_fp_5000', 'secchi')

resid_control <- c("bld_n_rooms", "bld_n_beds", "bld_n_baths", "bld_yr_combined",
                   "bld_gross_area",
                   "bld_air", "bld_heat", "bld_fire")
#delist analysis
resid_delist <- resid_dev_sale[!is.na(resid_dev_sale$Delist.year),]

resid_delist$after_before <- ifelse(resid_delist$year>=resid_delist$Delist.year, 1, 0)


delist_eqn <- as.formula(paste0('price_updated ~', paste0(c('after_before','lake','after_before:lake', 
                                                     common_control, resid_control), 
                         collapse = '+')))

delist_reg <- lm(delist_eqn, data = resid_delist)

summary(delist_reg)

#impair analysis
resid_impair <- resid_dev_sale[!is.na(resid_dev_sale$Year.added.to.List),]

resid_impair$after_before <- ifelse((resid_impair$year>=resid_impair$Year.added.to.List &
                                       is.na(resid_impair$Delist.year)),1, 0)


impair_eqn <- as.formula(paste0('price_updated ~', paste0(c('after_before','lake','after_before:lake', 
                                                            common_control, resid_control), 
                                                          collapse = '+')))

impair_reg <- lm(impair_eqn, data = resid_impair)

summary(impair_reg)

#lakefront analysis
resid_lakefront <- resid_dev_sale[resid_dev_sale$lake == 1,]

resid_lakefront$impaired <- ifelse(is.na(resid_lakefront$Year.added.to.List), 1, 0)


lakefront_eqn <- as.formula(paste0('price_updated ~', paste0(c('impaired', 
                                                            common_control, resid_control), 
                                                          collapse = '+')))

lakefront_reg <- lm(lakefront_eqn, data = resid_lakefront)

summary(lakefront_reg)

###############################################################################################
#Get the table in excel / latex
coeff_table <- mtable("delisted_lake" = delist_reg,
                           "impaired_lake" = impair_reg,
                           "lakefront_impaired" = lakefront_reg,
                           
                           summary.stats = c("N", "Log-likelihood", "BIC", "AIC"),
                           signif.symbols = c("***" = .01, "**" = .05, "*" = .1),
                           digits = 4)

#displaying table as latex code
toLatex(coeff_table)

write.mtable(coeff_table,file="coeff_table.txt")

#Save the project data
save.image(file="saleh_lagosne_extract.RData")
