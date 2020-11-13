# Create the worker density data 
# from the origin-destination data from LODES

library(data.table)
library(spdep)
library(rgeos)
library(maptools)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(RColorBrewer)
library(dplyr)

# set file path
root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data" 

# Read in counties 
siteCounties <- read.csv(paste0(root, "/Indicator_Output/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS <- sprintf("%05d", siteCounties$FIPS)

head(siteCounties)

# 2017: Main (only CA residents)  **UPDATED TO 2017 from 2015 (Feb 2020) ****
# Downloaded LODES 7 OD Main
# https://lehd.ces.census.gov/data/#lodes

# w_geocode Char15 Workplace Census Block Code
# h_geocode Char15 Residence Census Block Code 

### load  files   
# list all files in the directory
lodesFiles <- list.files(path = "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/LODES/", full.names=TRUE) 
lodesFiles <- lodesFiles[lodesFiles %like% "2017"] # select only updated 2017 files

# review 
lodesFiles # yes those are the files we want--one csv per state for 2017

# read in the csvs # this takes a min --data is block level
lodes <- lapply(lodesFiles, function(i){
  fread(i, header=TRUE, stringsAsFactors = FALSE, colClasses = c("w_geocode"="character", "h_geocode" = "character"), data.table = FALSE)
}) 

head(lodes[[1]])


# create county id  to query
lodes <- lapply(lodes, function(x) within(x, w_geocode <- str_pad(x$w_geocode, 15, pad = "0")))
lodes <- lapply(lodes, function(x) within(x, h_geocode <- str_pad(x$h_geocode, 15, pad = "0")))
lodes <- lapply(lodes, function(x) within(x, h_FIPS <- substr(x$h_geocode, 1, 5)))
lodes <- lapply(lodes, function(x) within(x, w_FIPS <- substr(x$w_geocode, 1, 5)))

# grab only the home and work place within the same county
wrk <- lapply(lodes, function(x) x[x$h_FIPS %in% siteCounties$FIPS & x$w_FIPS %in% siteCounties$FIPS, ])
wrk <- lapply(wrk, function(x) x[x$h_FIPS == x$w_FIPS, ]) # home and work location has to be same county

head(wrk[[1]])


#### Aggregate on Block ID by county
# Now I know that every block group in this subset originates where it works, so I can just aggregate on work centers
# sum in-county job sum by block
wrkBlock <- lapply(wrk, function(x) x %>%
                     dplyr::group_by(w_geocode, w_FIPS) %>%
                     dplyr::summarize(inCounty_JobSum= sum(S000, na.rm=T)))

head(wrkBlock[[1]])

# get in-county job sum for the whole county
wrkCounty <- lapply(wrk, function(x) x %>%
                      group_by(w_FIPS) %>%
                      summarize(tot = sum(S000, na.rm=T)))

wrkCounty <- do.call("rbind", wrkCounty)
head(wrkCounty)


# join in county total and calculate the share of in-county jobs in each block
wrkBlock <- lapply(wrkBlock, function(x) x %>% 
                     left_join(wrkCounty) %>%
                     mutate(jobShare = inCounty_JobSum/tot))

head(wrkBlock[[2]])

# name list elements -- make sure this order is correct manually
names(wrkBlock) <- c("blocksAZ", "blocksCA", "blocksFL", "blocksGA", "blocksMI","blocksNC","blocksPA","blocksTX","blocksWI")


# Export finished block data files to output folder
for (i in seq_along(wrkBlock)) {
  filename = paste("JobShare_Block_", names(wrkBlock)[i], ".csv", sep = "")
  write.csv(wrkBlock[[i]], paste0(root, "/Indicator_Output/", filename), row.names = FALSE)
}


#### Merge up to tracts (for visualization only - we don't show blocks on the website) ####
head(wrkBlock[[4]])

# create tract id by subseting the first 11 characters of the block id string
wrkTract <- lapply(wrkBlock, function(x) within(x, GEOID_Tract <- substr(x$w_geocode, 1, 11)))
head(wrkTract$blocksAZ)
head(wrkTract$blocksTX)

# Aggregate on tract id
wrkTract <- lapply(wrkTract, function(x) x %>%
                     group_by(GEOID_Tract, w_FIPS) %>%
                     summarize(inCounty_JobSum = sum(inCounty_JobSum, na.rm = T),
                               tot = mean(tot, na.rm=T),
                               jobShareTract = inCounty_JobSum/tot,
                               jobsPer10KTract = inCounty_JobSum/(tot/10000)))  # I added this jobs per 10k jobs in county Feb 2020--might be a better way to think about it? 

head(wrkTract[[1]])

# Export finished block data files to output folder
for (i in seq_along(wrkTract)) {
  filename = paste("JobShare_Tract_", names(wrkTract)[i], ".csv", sep = "")
  write.csv(wrkTract[[i]], paste0(root, "/Indicator_Output/visualize/", filename), row.names = FALSE)
}

# done
