# Set up data
# Added Fremont County in CO on 6/15/20, Calaveras County in CA 04/2020

library(usmap)
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)

# dir
root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data"


#### Create list of Counties included in the siting tool ####
# create list of VCA counties with FIPS codes 

#Selected the relevant columns and renamed them to match masterlist.csv
sites_all <- read.csv(paste0(root, "/Indicator_Inputs/Raw_County_List.csv"), stringsAsFactors = FALSE)
sites_all$FIPS <- sprintf("%05d", sites_all$GEOID)
sites_all$CountyName <- sprintf(sites_all$NAME)
sites_all <- sites_all[,c(14, 15, 3)]
head(sites_all)

# export list to use as source list of counties going forward
write.csv(sites_all, paste0(root, "/Indicator_Output/Siting_Counties_MasterList.csv"), row.names = FALSE)
