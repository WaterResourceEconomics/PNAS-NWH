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
# working_dir = "C:/Users/salmamun/Documents/Zillow_project/Water Quality Hedonic/Water_Data"
setwd(working_dir)

#Load environment/data file

# if(!exists("fuzzy_date_match.RData") )
# {
#   load("fuzzy_date_match.RData")
# }

# data_lagosne <- lagosne_load()
data_lagosne <- readRDS(file.path(working_dir, 'LAGOSNE_data', 'data_1.087.3.rds'))

all_epi <- merge(data_lagosne$epi_nutr, 
                data_lagosne$lakes.geo[c('state_zoneid', 'lakes_nhdid', 'lagoslakeid')],
                by = 'lagoslakeid', all.x = TRUE)


######################################################################################################################
# state_id = 'State_14'
# 
# mn_epi <- all_epi[all_epi['state_zoneid'] == state_id,]

#load MN data from EPA
us_epa <- read.csv(file = file.path(working_dir, 'EPA_WQP', 'Mar 2021', "us_epa.csv"))

us_epa$ActivityStartDate <- as.Date(us_epa$ActivityStartDate, format('%Y-%m-%d'))
us_epa$PERMANENT_IDENTIFIER <- as.character(us_epa$PERMANENT_)

##################################################################################################
us_matched <- merge(us_epa[c('ActivityStartDate', 'MonitoringLocationIdentifier', 'PERMANENT_IDENTIFIER', 
                             "LatitudeMeasure", "LongitudeMeasure",
                             'secchi_result', 'chla_result')],
                    all_epi, by.x = c('ActivityStartDate', 'PERMANENT_IDENTIFIER'), 
                    by.y = c('sampledate', 'lakes_nhdid'), all = TRUE)


us_matched$secchi <- ifelse(is.na(us_matched$state_zoneid), us_matched$secchi_result, us_matched$secchi)
us_matched$chla <- ifelse(is.na(us_matched$state_zoneid), us_matched$chla_result, us_matched$chla)

########################################################################
# Importing sales data
sale_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'Lake_quality_data_2021_05_19', 
                                                  'Combined','sale.pqt')))

sale_pid_lakes <- as.data.frame(read_parquet(file.path(working_dir, 'Christoph_data', 'Lake_quality_data_2021_05_19', 
                                                       'Combined','sale_pids.pqt')))

#####################################################################################
# FILTERING SALES DATA ALONE
# remove multiple property sales by deleting both duplicated sids
multiple_pids <- as.data.frame(table(sale_pid_lakes$sid))
names(multiple_pids) <- c('sid', 'count_sid')
multiple_pids <- multiple_pids[multiple_pids$count_sid==1,]

# merge multiple_pids with sale_pid_lakes
sale_pid_uq <- merge(multiple_pids, sale_pid_lakes[c('sid', 'pid')], by='sid')

# merge to get complete sales data
sale_all <- merge(sale_pid_uq, sale_lakes, by = 'sid')

###########################################################################################
# merge water quality data with sale_all
us_matched$ActivityStartDate <- as.Date(us_matched$ActivityStartDate, format='%Y-%m-%d')

colnames(us_matched)[colnames(us_matched) == "PERMANENT_IDENTIFIER"] <- "lake_nb_nhd_id"

us_matched$lake_nb_nhd_id <- as.character(us_matched$lake_nb_nhd_id)

#do it by year, it is very slow for each date.
us_matched$sampleyear <- year(us_matched$ActivityStartDate)

#us_matched$sampleyear <- as.Date(ISOdate(us_matched$sampleyear, 6, 30))

us_matched_subset <- us_matched[c('lake_nb_nhd_id', 'sampleyear', 'ActivityStartDate', 'secchi', 'chla')]

##############################################################################################
parameters = c('secchi', 'chla')

for (par in parameters){
  
  #######################################################################################
  # handling the missing values and extreme values
  us_matched_par <- us_matched_subset[is.na(us_matched_subset[par])==FALSE,]
  
  us_matched_par <- us_matched_par[us_matched_par[par]>0,]
  us_matched_par <- us_matched_par[us_matched_par[par] != Inf,]
  
  us_matched_par <- us_matched_par[us_matched_par[par] <= quantile(us_matched_par[par],
                                                                   probs = 0.99, na.rm = TRUE),]
  
  
  # handling within year variation (use only data for Apr-Sep) and agregate by median
  us_matched_par$activityMonth <- month(us_matched_par$ActivityStartDate)
  us_matched_par <- us_matched_par[(us_matched_par$activityMonth >=4 & us_matched_par$activityMonth <=9),]
  
  us_matched_par <- us_matched_par[c('lake_nb_nhd_id', 'sampleyear', par)]
  
  us_matched_par_agg <- aggregate(.~lake_nb_nhd_id+sampleyear, us_matched_par, median)
  
  ##########################################################################################
  # have to delete sale that does not have any water_quality data before matching
  sale_all <- sale_all[sale_all$lake_nb_nhd_id %in% 
                         unlist(list(unique(us_matched_par_agg$lake_nb_nhd_id))),]
  
  sale_all <- sale_all[which(!is.na(sale_all$year)),]
  sale_all$year <- as.Date(paste0(sale_all$year,"-01-01"))
  
  us_matched_par_agg$sampleyear <- as.Date(paste0(us_matched_par_agg$sampleyear,"-01-01"))
  us_matched_par_agg$ind <- seq.int(nrow(us_matched_par_agg))
  
  ###################################################################################################
  # reduce memory by keeping only required columns
  sale_all <- sale_all[which(!is.na(sale_all$sid)),]
  sale_all <- sale_all[c('sid', 'lake_nhd_id', 'lake_dist', 'lake_nb_nhd_id',
               'year')]
  
  sale_lake <- sale_all[!duplicated(sale_all[c('lake_nb_nhd_id', 'year')]), ]
  
  # GET exact match
  
  sale_water_exact <- merge(sale_lake, us_matched_par_agg, 
                      by.x = c('lake_nb_nhd_id', 'year'),
                      by.y = c('lake_nb_nhd_id', 'sampleyear'),
                      all.x = TRUE)
  
  sale_non_exact <- sale_water_exact[which(is.na(sale_water_exact$ind)),]
  row.names(sale_non_exact) <- NULL
  sale_non_exact$ind <- NULL
  sale_non_exact[par] <- NULL
  us_matched_par_agg$ind <- NULL
  
  
  sale_water_exact$sampleyear <- sale_water_exact$year
  sale_water_exact <- sale_water_exact[which(!is.na(sale_water_exact$ind)),]
  sale_water_exact$ind <- NULL
  
  # get lake water quality that only in non_exact match
  us_matched_non_exact <- us_matched_par_agg[us_matched_par_agg$lake_nb_nhd_id %in% 
                         unlist(list(unique(sale_non_exact$lake_nb_nhd_id))),]
  #################################################################################################
  # GET NON-EXACT MATCH
  #################################################################################################
  # df1 <- sale_non_exact[201:701,]
  # df2 <- us_matched_non_exact
  # timeCol1 <- 'year'
  # timeCol2 <- 'sampleyear'
  # IDcol <- 'lake_nb_nhd_id'
  
  # fuzzyTimeandID <- function(df1, df2, timeCol1, timeCol2, IDcol){
  #   if(!timeCol1 %in% colnames(df1)) stop("timeCol1 must specify a column name in df1.")
  #   if(!timeCol2 %in% colnames(df2)) stop("timeCol2 must specify a column name in df2.")
  #   # dfMinTime = data.frame(matrix(ncol = (ncol(df2) - 1), nrow = nrow(df1)))
  #   dfMinTime = data.frame(matrix(ncol = ncol(df2), nrow = 0))
  #   # colnames(dfMinTime) = colnames(df2)[!colnames(df2) %in% timeCol2]
  #   colnames(dfMinTime) = colnames(df2)
  #   ties_count = 0
  #   for(i in 1:nrow(df1)){
  #     ID = df1[i, IDcol]
  #     min_rows = vector()
  #     for(j in 1:nrow(df2)){
  #       if(df2[j, IDcol] == ID){
  #         tmp = abs(as.numeric(difftime(df1[i, timeCol1], df2[j, timeCol2])))
  #         min_rows = c(min_rows, tmp)
  #       } else { min_rows = c(min_rows, NA)}
  #     }
  #     mins = (min_rows == min(min_rows))
  #     # dfMinTime[i, ] = df2[which.min(min_rows), !(colnames(df2) %in% timeCol2), drop = FALSE]
  #     selected <- df2[which.min(min_rows),]
  #     dfMinTime <- rbind(dfMinTime, selected)
  #     if(sum(mins, na.rm = TRUE) > 1) { ties_count = ties_count + 1 }
  #   }
  #   if(ties_count > 0){
  #     message("Warning: there were ", ties_count, " difftime ties, for which the first corresponding row of df2 was chosen for merging.")
  #   }
  #   dfAll = cbind(df1, dfMinTime)
  #   dfAll = dfAll[ , !duplicated(colnames(dfAll))]
  #   return(dfAll)
  # }
  
  
  fuzzyTimeandID <- function(df1, df2, timeCol1, timeCol2, IDcol){
    if(!timeCol1 %in% colnames(df1)) stop("timeCol1 must specify a column name in df1.")
    if(!timeCol2 %in% colnames(df2)) stop("timeCol2 must specify a column name in df2.")
    # dfMinTime = data.frame(matrix(ncol = (ncol(df2) - 1), nrow = nrow(df1)))
    dfMinTime = data.frame(matrix(ncol = ncol(df2), nrow = 0))
    # colnames(dfMinTime) = colnames(df2)[!colnames(df2) %in% timeCol2]
    colnames(dfMinTime) = colnames(df2)
    ties_count = 0
    for(i in 1:nrow(df1)){
      ID = df1[i, IDcol]

      df3 <- df2[df2[IDcol] == ID,]
      min_rows = vector()
      for(j in 1:nrow(df3)){
          tmp = abs(as.numeric(difftime(df1[i, timeCol1], df3[j, timeCol2])))
          min_rows = c(min_rows, tmp)
      }
      mins = (min_rows == min(min_rows))
      # dfMinTime[i, ] = df2[which.min(min_rows), !(colnames(df2) %in% timeCol2), drop = FALSE]
      selected <- df3[which.min(min_rows),]
      dfMinTime <- rbind(dfMinTime, selected)
      if(sum(mins, na.rm = TRUE) > 1) { ties_count = ties_count + 1 }
    }
    if(ties_count > 0){
      message("Warning: there were ", ties_count, " difftime ties, for which the first corresponding row of df2 was chosen for merging.")
    }
    dfAll = cbind(df1, dfMinTime)
    dfAll = dfAll[ , !duplicated(colnames(dfAll))]
    return(dfAll)
  }
  #################################################################################################
  # GET NON-EXACT MATCH
  # # Approach 1: DO ALL AT ONCE
  # system.time({
  #   sale_water_non_exact_s <- nearestTimeandID(sale_non_exact[1:7,], us_matched_non_exact,
  #                                              'year', 'sampleyear', 'lake_nb_nhd_id')
  # })
  # 
  # # Approach 2: DO ONE BY ONE
  # sale_list <- list()
  # system.time({
  # for(i in 1:7){
  #   sale_water_non_exact_x <- nearestTimeandID(sale_non_exact[i,], us_matched_non_exact,
  #                                                'year', 'sampleyear', 'lake_nb_nhd_id')
  #   sale_water_non_exact <- rbind(sale_water_non_exact, sale_water_non_exact_x)
  # }
  # })
  # 
  # sale_water_non_exact_p = do.call(rbind, sale_list)
  
  #######################################################################################
  # Approach 3: DO BY CHUNK
  fuzzy_match_time_id <- function(i, chunk_size=50, sale_non_exact, us_matched_non_exact){
    print(paste0('matching chunk_number ',i, ', chunk_size=',chunk_size))
    s <- i*chunk_size + 1
    e <- min((i+1)*chunk_size, nrow(sale_non_exact))
    sale_water_non_exact_x <- fuzzyTimeandID(sale_non_exact[s:e,], us_matched_non_exact,
                                               'year', 'sampleyear', 'lake_nb_nhd_id')
    return(sale_water_non_exact_x)
  }
  
  chunk_size <- 500
  range_non_exact <- 0:(ceiling(nrow(sale_non_exact)/chunk_size)-1)
  
  cores = detectCores()-2
  
  (cl = cores %>% 
     makeCluster()) %>% 
    registerDoParallel()
  
  # read and bind
  #use the below block if you need to merge sale data. It will take time
  
  
  system.time({
    sale_water_fuzzy = foreach(i = range_non_exact,
                       .packages = c("data.table", "bayesbio")) %dopar% {
                         fuzzy_match_time_id(i, chunk_size=chunk_size, sale_non_exact, us_matched_non_exact)
                       } %>%
      rbindlist(fill = T)
  })
  # end of parallel work
  #stopImplicitCluster(cl)
  stopCluster(cl)
  
  #################################################################################
  sale_water_exact$match_type = 'exact'
  sale_water_fuzzy$match_type = 'fuzzy'
  sale_match <- rbind(sale_water_exact, sale_water_fuzzy)
  sale_match <- sale_match[,-c('sid', 'lake_dist')]
  sale_match <- merge(sale_all[c("sid", "lake_nb_nhd_id", "year", 'lake_dist')], sale_match, 
                      by = c('lake_nb_nhd_id', 'year'),
                      all.x = TRUE)
  write.csv(sale_match, file=file.path(working_dir, 'Fuzzy_match', paste0('sale_match_', par,'_med_seasonal.csv')),
            row.names = FALSE)
}
##################################################################################

#Save the project data
#save.image(file="fuzzy_date_match.RData")
