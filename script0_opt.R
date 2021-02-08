# load external sources
source("libraries.R")
source("globals.R")

## Create list of Counties included in the siting tool
#Selected the relevant columns and renamed them to match masterlist.csv
sites_all <- read.csv(paste0(root, "/Indicator_Inputs/Siting_Counties_MasterList_CO_test.csv"), stringsAsFactors = FALSE)
sites_all$FIPS <- sprintf("%05d", sites_all$GEOID)
sites_all$CountyName <- sprintf(sites_all$NAME)
sites_all <- sites_all[,c(14, 15, 3)]
head(sites_all)

# export list to use as source list of counties going forward
write.csv(sites_all, paste0(root, "/Indicator_Output/Siting_Counties_MasterList.csv"), row.names = FALSE)
