# AZ Maricopa County Voter Data: VBM Rates (total, youth, latino and asian)
library(data.table)
library(dplyr)
options(scipen=999)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root = "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"


# load AZ Voter data
azCata <- fread(paste0(root, "data/Catalist Data/AZ_mail.txt"), data.table=FALSE)

# fix spelling--Towne Meadows should have an E
azCata$geography[azCata$geography =="AZ MARICOPA TOWN MEADOWS"] <-  "AZ MARICOPA TOWNE MEADOWS"

head(azCata)

# denominator/ registered voters
regCata <- fread(paste0(root, "data/Catalist Data/AllStates_counts_reg_16_voters.txt"), data.table=FALSE)

# grab only AZ and precincts
regCata <- regCata[regCata$geography %like% "AZ",]

# fix spelling--Towne Meadows should have an E
regCata$geography[regCata$geography =="AZ MARICOPA TOWN MEADOWS"] <-  "AZ MARICOPA TOWNE MEADOWS"

head(regCata)


# join the mail voter to the registered voter file
join <- full_join(azCata, regCata) # both catalist, so the join should be good...

dim(azCata); dim(regCata); dim(join) # great p much a perfect join

# need to join the precicnts to my AZ block/precinct conversion file, and summarize by block
head(join)
subset(join, geography== "") # there is one row without a geo id. remove.

# remove NA row
join <- join[join$geography !="", ]
head(join)


#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties = read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS = sprintf("%05d", siteCounties$FIPS)
site_az = siteCounties[siteCounties$State=="Arizona",]
site_az$COUNTY = toupper(site_az$CountyName)
table(site_az$CountyName)
site_az

# MAKE name col that matches the block/precinct crosswalk
join$precName <- gsub("AZ MARICOPA ", "", join$geography)
head(join)
dim(join)


###### Convert Precincts to Blocks #####
## Read in block-precinct conversion file 

# load conversion/crosswalk file
intr <- fread(paste0(root, "Arizona Siting Tool/data/az_intersect_precinct_crosswalk.csv"), data.table = FALSE)
head(intr)

dim(intr); dim(join)

### multiply proportion by registered voters
# first join reg voters
intrReg = merge(intr, join, by="precName", all=TRUE)

head(intrReg)
dim(intrReg)

# Check for NAs
subset(intrReg, is.na(GEOID10))
subset(intrReg, is.na( TOTAL_REGISTERED_VOTERS))
# looks good!

# multiply to get proportional registration for all variables
intrReg <- 
intrReg %>% 
  mutate(propRegTot = prc_Intrsct_area * TOTAL_REGISTERED_VOTERS,
         propRegLat = prc_Intrsct_area * HISPANIC_REGISTERED_VOTERS, 
         propRegAsn = prc_Intrsct_area * ASIAN_REGISTERED_VOTERS,
         propRegYth = prc_Intrsct_area * YOUTH_REGISTERED_VOTERS,
         
         propMailTot = prc_Intrsct_area * AZ_TOTAL_2016_ABSENTEE_OR_EARLY_VOTERS,
         propMailLat = prc_Intrsct_area * AZ_HISPANIC_2016_ABSENTEE_OR_EARLY_VOTERS, 
         propMailAsn = prc_Intrsct_area * AZ_ASIAN_2016_ABSENTEE_OR_EARLY_VOTERS, 
         propMailYth = prc_Intrsct_area * AZ_YOUTH_2016_ABSENTEE_OR_EARLY_VOTERS)

# summarize by block
blockReg <- 
  intrReg %>%
  dplyr::group_by(GEOID10, NAME10, COUNTYFP10) %>%
  dplyr::summarize(regBlockTot = sum(propRegTot, na.rm=T),
                   regBlockLat = sum(propRegLat, na.rm=T), 
                   regBlockAsn = sum(propRegAsn, na.rm=T), 
                   regBlockYth = sum(propRegYth, na.rm=T), 
                   
                   mailBlockTot = sum(propMailTot, na.rm=T), 
                   mailBlockLat = sum(propMailLat, na.rm=T), 
                   mailBlockAsn = sum(propMailAsn, na.rm=T), 
                   mailBlockYth = sum(propMailYth, na.rm=T)) %>% as.data.frame()

head(blockReg)
summary(blockReg)

# now calculate the vbm rates by blocks
blockReg_Final <- 
  blockReg %>% 
  mutate(
    TotVBM = mailBlockTot/regBlockTot,
    LatVBM = mailBlockLat/regBlockLat,
    AsnVBM = mailBlockAsn/regBlockAsn,
    YouthVBM = mailBlockYth/regBlockYth,
    COUNTYFP10 = paste0("04", COUNTYFP10)) %>% 
  rename("BLOCK_KEY" = GEOID10, 
         "FIPS" = COUNTYFP10)

head(blockReg_Final)
summary(blockReg_Final)

blockReg_Final <- 
blockReg_Final %>% 
  # replace NA with 0 when there are 0 registered voters
  mutate(TotVBM = ifelse(regBlockTot == 0, 0, TotVBM),
         LatVBM = ifelse(regBlockLat == 0, 0, LatVBM),
         AsnVBM = ifelse(regBlockAsn == 0, 0, AsnVBM), 
         YouthVBM = ifelse(regBlockYth==0, 0, YouthVBM)) %>% 
  filter(!is.na(BLOCK_KEY)) %>% 

  # select only columns needed
  dplyr::select(BLOCK_KEY, FIPS, TotVBM, LatVBM, AsnVBM, YouthVBM) 
  

head(blockReg_Final)
summary(blockReg_Final)
subset(blockReg_Final, is.na(BLOCK_KEY))

## export
write.csv(blockReg_Final, paste0(root, "data/output/VBM_Use_Rate_BlocksAZ.csv"), row.names = FALSE)


##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID = substr(blockReg$GEOID10, 1, 11)  # extract the first digit of the block id

head(blockReg)

### Sum records by block group to get the voter totals per block group
tractVBM  <- 
  blockReg %>%
  dplyr::group_by(GEOID, COUNTYFP10) %>%   
  dplyr::summarize(regBlockTot = sum(regBlockTot, na.rm=T),
                   regBlockLat = sum(regBlockLat, na.rm=T), 
                   regBlockAsn = sum(regBlockAsn, na.rm=T), 
                   regBlockYth = sum(regBlockYth, na.rm=T), 
                   
                   mailBlockTot = sum(mailBlockTot, na.rm=T), 
                   mailBlockLat = sum(mailBlockLat, na.rm=T), 
                   mailBlockAsn = sum(mailBlockAsn, na.rm=T), 
                   mailBlockYth = sum(mailBlockYth, na.rm=T)) %>% 
  as.data.frame() %>% 
  mutate(
    TotVBM = mailBlockTot/regBlockTot,
    LatVBM = mailBlockLat/regBlockLat,
    AsnVBM = mailBlockAsn/regBlockAsn,
    YouthVBM = mailBlockYth/regBlockYth)

head(tractVBM)
summary(tractVBM)

# final clean up
tractVBM <- 
  tractVBM %>% 
  rename("FIPS"=COUNTYFP10) %>% 
  mutate(FIPS = paste0("04", FIPS)) %>% 
  dplyr::select(GEOID, FIPS, TotVBM, LatVBM, AsnVBM, YouthVBM) %>% 
  filter(!is.na(TotVBM) & !is.na(LatVBM))
  

summary(tractVBM)
head(tractVBM)
subset(tractVBM, is.na(GEOID))
subset(tractVBM, is.na(FIPS))

## Export to tract for visuals
write.csv(tractVBM, paste0(root, "data/output/visualize/VBM_Use_Rate_Tracts_PrecinctsAZ.csv"), row.names = FALSE)


