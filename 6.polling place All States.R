# Voter Data: Share of Polling Place Voters (total only)

library(data.table)
library(dplyr)
library(totalcensus)
options(scipen=999)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root = "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/"


# load Voter data
allCata <- fread(paste0(root, "/Indicator_Output/Reg_2016_ALLSTATES.csv"), data.table=FALSE)
head(allCata)

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "/Indicator_output/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE,
                         colClasses = c("FIPS"="character"))

# filter to the states that are ready from allCata - update this as needed
site_filtered = siteCounties[(siteCounties$State=="Arizona") |
                               (siteCounties$State=="Texas") |
                               (siteCounties$State=="Georgia") |
                               (siteCounties$State=="Michigan") |
                               (siteCounties$State=="Florida") |
                               (siteCounties$State=="North Carolina") |
                               (siteCounties$State=="Pennsylvania") |
                               (siteCounties$State=="Wisconsin"), ]

site_filtered$COUNTY = toupper(site_filtered$CountyName)

# verify the right states and number of counties are filtered per allCata
table(site_filtered$State)


## Assume that if someone isn't a mail voter, they voted in person 
allCata <- allCata %>% mutate(poll_voters = TOTAL_2016_VOTERS - VBM_2016_GENERAL) 

head(allCata)
dim(allCata) 

# need to join the precincts to my block/precinct conversion file, and summarize by block

###### Convert Precincts to Blocks #####
## Read in block-precinct conversion file 

# load conversion/crosswalk files - make sure to update this as more states are run (check states in allCata)
az_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/az_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
mi_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/mi_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
ga_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/ga_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
tx_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/tx_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID"="character", "BLOCKID10"="character"), data.table = FALSE)
fl_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/fl_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
nc_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/nc_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
wi_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/wi_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
pa_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/pa_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
head(ga_intr)
head(nc_intr)
head(wi_intr)
head(pa_intr)

## create a master dataframe of all the crosswalk files - add in new states as they are run
# grab: state fips, county fips, tractce, blockce, blockID, partfl, housing, pop, 
# area_m2, blockfull_area, prec_join, precfull_area, intrsct_area, prc_intrsect_area (in that order!)
az_intr <- az_intr[,c(1:10,28:31)]
colnames(az_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

ga_intr <- ga_intr[,c(1:10,25:28)]
colnames(ga_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

tx_intr <- tx_intr[,c(1:4,34,6:9,11,31,30,32,33)]
colnames(tx_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

mi_intr <- mi_intr[,c(1:10,22:25)]
colnames(mi_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

fl_intr <- fl_intr[,c(1:10,30:33)]
colnames(fl_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

nc_intr <- nc_intr[,c(1:10,65:68)]
colnames(nc_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

wi_intr <- wi_intr[,c(1:10,64:67)]
colnames(wi_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

pa_intr <- pa_intr[,c(1:10,29:32)]
colnames(pa_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")
# merge all dataframes together
all_intr <- rbind(az_intr, tx_intr, mi_intr, ga_intr, fl_intr, nc_intr, wi_intr, pa_intr)
View(all_intr)


### multiply proportion by registered voters
# first join crosswalk to poll voters voters
intrReg = merge(all_intr, allCata, by="PREC_JOIN", all=TRUE)

head(intrReg)
dim(intrReg); dim(all_intr); dim(allCata)


# check nas
subset(intrReg, is.na(BLOCKID))
subset(intrReg, is.na(TOTAL_2016_VOTERS))

# multiply to get proportional registration for all variables
intrReg <- 
  intrReg %>% 
  mutate(propTotVoters = prc_Intrsct_area * TOTAL_2016_VOTERS,
         propMailVoters = prc_Intrsct_area * VBM_2016_GENERAL)

head(intrReg)

# summarize by block
blockReg <- 
  intrReg %>%
  as.data.frame() %>% 
  dplyr::group_by(BLOCKID) %>%
  dplyr::summarize(totVotersBlock = sum(propTotVoters, na.rm=T),
                   mailVotersBlock = sum(propMailVoters, na.rm=T)) %>% 
    as.data.frame()

head(blockReg)
summary(blockReg)
table(blockReg$BLOCKID, useNA = "always")

# now calculate the polling place share
# block level polling place share
blockReg$pollVotersBlock <- blockReg$totVotersBlock - blockReg$mailVotersBlock

# create county FIPS code
blockReg$FIPS <- substr(blockReg$BLOCKID, 1, 5)

# get the county total of in person/polling place voters (denominator of polling place share)
countyPollVoters <-  
  data.frame(blockReg %>% 
               filter(!is.na(FIPS)) %>% 
               group_by(FIPS) %>% 
               summarize(sumTotVoters = sum(totVotersBlock, na.rm=T), 
                         sumMailVoters = sum(mailVotersBlock, na.rm=T), 
                         sumPollVoters = sum(pollVotersBlock, na.rm=T)))
#  mutate(diffPollVoters = sumTotVoters - sumMailVoters) # these two methods match, so I'll just stick with the sum of poll voters

# countyPollVoters # I checked these county total votes cast numbers against the SOS  table downloaded here:
# https://apps.azsos.gov/election/2016/General/ElectionInformation.htm
# the total ballots cast is pretty close.

# divide the number of in person voters in each block by the county total
pollShare <- 
  blockReg %>% 
  left_join(countyPollVoters) %>% 
  mutate(pollShare = pollVotersBlock/sumPollVoters ) %>% 
  dplyr::select(BLOCKID, FIPS, pollShare) %>% 
  filter(!is.na(BLOCKID)) %>% 
  rename("BLOCK_KEY" = BLOCKID) 


head(pollShare)
summary(pollShare)

# add back in field for state, to export by state
pollShare$STATE <- substr(pollShare$BLOCK_KEY, 1, 2)
pollShare$STATE <- convert_fips_to_names(pollShare$STATE)

## export
pollShare_export <- split(pollShare, pollShare$STATE, drop = FALSE)

# loop through export
for (i in seq_along(pollShare_export)) {
  filename = paste("PollShare_Blocks", names(pollShare_export)[i], ".csv", sep = "")
  write.csv(pollShare_export[[i]], paste0(root, "Indicator_Output/", filename), row.names = FALSE)}


####### START HERE ######
##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID = substr(blockReg$BLOCKID, 1, 11)  # extract the first digit of the block id

head(blockReg)

### Sum records by census tract
tractPoll  <- 
  data.frame(blockReg %>%
               dplyr::group_by(GEOID, FIPS) %>%   
               dplyr::summarize(totVotersTract = sum(totVotersBlock, na.rm=T),
                                pollVotersTract = sum(pollVotersBlock, na.rm=T)) %>% 
               
               # join county voter totals
               left_join(countyPollVoters) %>% 
               mutate(
                 pollShare = pollVotersTract/sumPollVoters) %>% 
               dplyr::select(GEOID, FIPS, pollShare) %>% 
               filter(!is.na(GEOID)))


head(tractPoll)
subset(tractPoll, is.na(GEOID))
summary(tractPoll)

## Export to tract for visuals
# add back in field for state, to export by state
tractPoll$STATE <- substr(tractPoll$FIPS, 1, 2)
tractPoll$STATE <- convert_fips_to_names(tractPoll$STATE)

## export
tractPoll_export <- split(tractPoll, tractPoll$STATE, drop = FALSE)

# loop through export
for (i in seq_along(tractPoll_export)) {
  filename = paste("PollShare_PrecinctsTracts", names(tractPoll_export)[i], ".csv", sep = "")
  write.csv(tractPoll_export[[i]], paste0(root, "Indicator_Output/visualize/", filename), row.names = FALSE)}

#done
