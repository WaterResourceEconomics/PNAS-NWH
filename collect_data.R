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
working_dir= "G:/My Drive/NRRI/Zillow Project/Water Quality Hedonic/Water_Data/Christoph_data/Lake_quality_data_2021_05_19"
# working_dir = "C:/Users/salmamun/Documents/Zillow_project/Water Quality Hedonic/Water_Data"
setwd(working_dir)

######################################################################################################################
# check one data
# sale_lakes <- read_parquet(file = file.path(working_dir, 'unziped', 'AL_sale.pqt'),
#                            as_data_frame = TRUE)

######################################################################################################################
# Unzip all data
# You just need to do this once

file_list <- list.files(path = file.path(working_dir))
 
dir.create(file.path(working_dir, 'unziped'))
for (file in file_list){
   unzip(file, exdir = file.path(working_dir, 'unziped'))
}

##############################################################################################################

# Create list of files
all_sale_file <- list.files(path = file.path(working_dir, 'unziped'), 
                            full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = "sale.pqt$")


all_sale_pids_file <- list.files(path = file.path(working_dir, 'unziped'), 
                            full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = "sale_pids.pqt$")

all_sale_tids_file <- list.files(path = file.path(working_dir, 'unziped'), 
                                 full.names = TRUE, all.files = TRUE, recursive = TRUE, pattern = "sale_tids.pqt$")

#################################################################################################################
(cl = detectCores() %>% 
   makeCluster()) %>% 
  registerDoParallel()

# read and bind
#use the below block if you need to merge sale data. It will take time

system.time({
  sale_raw = foreach(i = all_sale_file,
                     .packages = c("data.table", "arrow")) %dopar% {
                         read_parquet(file = i,as_data_frame = TRUE)
                     } %>%
    rbindlist(fill = T)
})

system.time({
  sale_pids_raw = foreach(i = all_sale_pids_file,
                        .packages = c("data.table", "arrow")) %dopar% {
                          read_parquet(file = i,as_data_frame = TRUE)
                        } %>%
    rbindlist(fill = T)
})

system.time({
  sale_tids_raw = foreach(i = all_sale_tids_file,
                          .packages = c("data.table", "arrow")) %dopar% {
                            read_parquet(file = i,as_data_frame = TRUE)
                         } %>%
    rbindlist(fill = T)
})

# end of parallel work
#stopImplicitCluster(cl)
stopCluster(cl)

####################################################################################################################
dir.create(file.path(working_dir, 'Combined'))
data_output <- file.path(working_dir, 'Combined')

saveRDS(sale_raw, file = file.path(data_output, "sale.rds"))
saveRDS(sale_pids_raw, file = file.path(data_output, "sale_pids.rds"))
saveRDS(sale_tids_raw, file = file.path(data_output, "sale_tids.rds"))

write_parquet(sale_raw, sink = file.path(data_output, "sale.pqt"))
write_parquet(sale_pids_raw, sink = file.path(data_output, "sale_pids.pqt"))
write_parquet(sale_tids_raw, sink = file.path(data_output, "sale_tids.pqt"))


