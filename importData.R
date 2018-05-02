# Project: Skript to load/install packages automatically and to import multiple data files into a single data frame.
# Author: Ulrike Hiltner ulrike.hiltner@ufz.de
# Date: 02-05-2018
# Path: A relative path to the working directory is needed.

# Notes:Before the skript runs automatically, please specify the following:
# ENTER: package names used, chraracter
packagesUsed <- c("tidyverse")
# at field data: 
# ENTER: folder name of field data within your working directory
dataInput_filesObs <- "/dataInput/"
# ENTER: matching patterns of field data's file names
matchPat_filesObs <- "_corr.txt" #... inventory data Paracou, global wood density data base (by Chave et al.)
# at simulation results:
# ENTER: folder name of simulation results within your working directory
dataInput_filesSim <- "/dataInput/moopen_final/lowlandTropicalForest_8pft/simulation_results/at1stPaper/"
# ENTER: matching patterns of simulation results's file names
matchPat_filesSim <- "_paracouForest_" #090_paracouForest_T1loggPlots_8pft_T1
# at FORMIND parameter file:
# ENTER: folder name of parameter file within your working directory
dataInput_filesPar <- "/dataInput/moopen_final/lowlandTropicalForest_8pft/formind_parameters/"
# ENTER: matching patterns of simulation results's file names
matchPat_filesPar <- "paracouForest_controlPlots_8pft.par"
# ENTER: file name endings of simulation results to be considered
filterSc_agb <- "bt_th$" # Aboveground biomass
filterSc_ba <- "ba_th$" # Basal area
filterSc_sn <- "n_th$" # Stem numbers
filterSc_ha <- "ha$" # simulation results of AGB, SN, BA, SV per ha
filterSc_dia <- "dia$" # results per diameter classes
filterSc_prod <- "prod$" # gpp,npp,resp
filterSc_bv <- "bv_th$" # bole volume 
filterSc_lai <- "lai_mean$" # mean leaf area index
filterSc_log <- "log$" # simulation results of each logging event 
# ENTER: all years of logging events in the simulation results
logEvent <- c(500)
# ENTER file name of reference scenario (T0) and RIL (T1) 
referenceSc <- "paracouForest_controlPlots_8pft."
undistGrowthSc <- "000_paracouForest_T1loggPlots_8pft_T1."
loggingT1Sc <- "paracouForest_T1loggPlots_8pft_T1."


#########################################################
# BEWARE: Do not change anything in the code below here #
#########################################################

#  Load/install packages----
# function checks for missing packages and ... 
packages_needed <- function(x){
  for(i in x){
    # require() returns TRUE invisibly if it was able to load package
    if(!require( i , character.only = TRUE) ){
      #  If package was not able to be loaded then install
      install.packages(i, dependencies = TRUE)
      #  Load package after installing. 
      library(i, character.only = TRUE ) # library() will throw an exception if the install wasn't successful
    }
  }
}
# ... load/install them automatically.
packages_needed(packagesUsed) 

# Functions----

# Tidy Logging result files: Search and Replace Lines of variable 'Time' of Logging Result files by variable 'filename'
# before the function can be used, aggregated result files of multiple scenarios need to be imported into a single dataframe of type tibble and filenames need to be added in a column
# the function takes four arguments: 
# chr df (name of dataframe), 
# vector int logEvent (ALL years where a logging event was simulated), 
# int nolpy (number of lines per year written by Formind into the results file. e.g. supposing that 16 ha were simulated, then *.ba_th: 1 time step 1 line or *.ha: 1 time step 16 lines)
# chr vector undistGrowthSc (all scenarios without logging events)
tidy_logResults <- function(df, logEvent = 500, nolpy = 1, undistGrowthSc = "000_paracouForest_T1loggPlots_8pft_T1."){
  # filtern for logging scenarios and years after logging events
  cond1 <- df$Time == logEvent+1 & df$filename != undistGrowthSc
  # delete results for year after a logging event
  df <- df[!cond1,]
  # search for logging scenario names
  nameLogSc <- unique(df$filename) 
  nameLogSc <- nameLogSc[which(nameLogSc != undistGrowthSc)]
  # initialize couter
  h = 1
  # for each logging scenario do ...
  while(h <= length(nameLogSc)){ #in seq_along(nameLogSc)
    # search for row numbers of logging events
    rowNumLogEvent <- which(df$Time==logEvent & df$filename == nameLogSc[h])
    
    i = 1 # initialize counter
    # for each row number with duplicate years do ...
    while(i <= length(rowNumLogEvent)){
      # write logEvent + 1 into duplicated year
      df$Time[rowNumLogEvent[i+1]] = df$Time[rowNumLogEvent[i]]+1
      
      # counter two positions up
      i = i + (2*nolpy) 
    }
    h = h+1
  }
  return(df)
}



# Todo: Versionierung?

# Assign working directory----
# relative path to working direktory and field data 
wdPath <- getwd()
dataPathObs <- paste0(wdPath, dataInput_filesObs)
dataPathSim <- paste0(wdPath, dataInput_filesSim)
dataPathPar <- paste0(wdPath, dataInput_filesPar)
# find all file names with pattern as given below:
fileNamesObs <- list.files(dataPathObs, pattern = matchPat_filesObs) # field data
fileNamesSim <- list.files(dataPathSim, pattern = matchPat_filesSim) # simulation results
fileNamesPar <- list.files(dataPathPar, pattern = matchPat_filesPar) # parameter file
# Import data----
# field data:
# create a data frame holding the file names, then...
dataObs <- dplyr::data_frame(filename = fileNamesObs) %>% # import field data, dependacy: fileNamesObs
  # read nested files into a new data column
  mutate(file_contents = purrr::map(filename, ~ readr::read_tsv(file.path(dataPathObs, .), col_names = TRUE))) 
# turn these data into one useful for downstream analysis
dataObs <-  tidyr::unnest(dataObs)

# par-file:
dataPar <- read_lines(paste0(dataPathPar,fileNamesPar)) %>% as_tibble()

# simulation results:
# create a data frame holding the file names, then...
dataSim <- dplyr::data_frame(filename = fileNamesSim) %>% # import simulation results, dependacy: fileNamesSim
  # read nested files into a new data column
  mutate(file_contents = purrr::map(filename, ~ readr::read_tsv(file.path(dataPathSim, .), col_names = TRUE, skip = 2))) 
# turn these data into one useful for downstream analysis
dataSim <-  tidyr::unnest(dataSim)




# Tidy data----
# seperate field data into forest inventory (mature and juvenile) and wsg
fieldData <- dataObs %>%
  filter(filename %in% filterInv & Plot!= 17) %>%
  select(-woodDensity) %>%
  rename(Annee = campagne)
succData <- dataObs %>%
  filter(filename %in% filterInv & Plot == 17) %>%
  select(-woodDensity) %>%
  rename(Annee = campagne)
wsgData <- dataObs %>%
  filter(filename %in% filterWSG) %>%
  select(filename, Famille, Genre, Espece, woodDensity)
# seperate these data into simulation results you want to analyze
# Tidy simulation data
dataSim <- dataSim %>%
  # ... extract cutting threshold of minimum DBH for trees to be logged from filename, 
  # and transform into double 
  mutate(DBH_cutth = as.double(str_sub(filename, 1,3)),
         # transform unit DBH [cm] to DBH [m].
         DBH_cutth = DBH_cutth/100,
         # add column with type of Treatment: differentiate reference and reduced impact scenarios, and all others by cutting th. 
         Treatment = if_else(DBH_cutth == 0.0, "RSc",
                             if_else(DBH_cutth == 0.55, "RIL", paste("dbhCut_th", str_sub(filename, 1, 3), sep = "_"))),
         # for visualization: data of RSC shall occur on right side of figures
         DBH_cutth = if_else(DBH_cutth == 0.0, max(DBH_cutth)+0.1, DBH_cutth))
# ENTER: file names of inventory data
filterInv <- fileNamesObs[-1]
# ENTER: file name of global wood density data set
filterWSG <- fileNamesObs[-c(2,3)]
# aboveground biomass
simData_agb <- dataSim %>% 
  filter(str_detect(filename, filterSc_agb)) %>%
  select(filename:Time,TotalBiomass:MeanWoodDensity, DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(undistGrowthSc = paste0(undistGrowthSc, "bt_th")) #filterSc_agb
# basal area
simData_ba <- dataSim %>% 
  filter(str_detect(filename, filterSc_ba)) %>%
  select(filename:Time , TotalBasalArea:BasalAreaPerPFT_8,DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(undistGrowthSc = paste0(undistGrowthSc,"ba_th"))
# stem numbers
simData_sn <- dataSim %>% 
  filter(str_detect(filename, filterSc_sn)) %>%
  select(filename:Time , TotalNumber:NumberPerPFT_8,DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(undistGrowthSc = paste0(undistGrowthSc,"n_th"))
# simulation results of agb, ba, sn, bv, etc. per simulated ha
simData_ha <- dataSim %>% 
  filter(str_detect(filename, filterSc_ha)) %>%
  select(filename, Time, HectarNo:SN, DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(nolpy = 16, undistGrowthSc = paste0(undistGrowthSc,"ha"))
# simulation results per stem diameter class
simData_dia <- dataSim %>% 
  filter(str_detect(filename, filterSc_dia)) %>%
  select(filename, Time, DiameterClass:BasalAreaPFT_8, DBH_cutth, Treatment)
# simulation results per simulated productivity (gpp, npp, resp, mn)
simData_prod <- dataSim %>% 
  filter(str_detect(filename, filterSc_prod)) %>%
  select(filename, Time, gpp:mortality,DBH_cutth, Treatment)%>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(undistGrowthSc = paste0(undistGrowthSc,"prod"))
# leaf area index
simData_lai <- dataSim %>% 
  filter(str_detect(filename, filterSc_lai)) %>%
  select(filename, Time, LAI,DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults(undistGrowthSc = paste0(undistGrowthSc,"lai_mean"))
# bole volume
simData_bv <- dataSim %>% 
  filter(str_detect(filename, filterSc_bv)) %>%
  select(filename, Time, TotalBoleVolume:BoleVolumePerPFT_8,DBH_cutth, Treatment) %>%
  # remove duplicate second year in results of logging scenarios
  tidy_logResults( undistGrowthSc = paste0(undistGrowthSc,"bv_th"))
# logging results per event
simData_log <- dataSim %>% 
  filter(str_detect(filename, filterSc_log)) %>%
  select(filename, T:priorSV,DBH_cutth, Treatment)