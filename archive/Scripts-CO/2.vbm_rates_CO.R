# Colorado Voter Data: VBM Rates (total, youth, latino and asian)
library(data.table)
library(dplyr)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"


# load CO Voter data
coCata <- fread(paste0(root, "data/Catalist Data/CO_mail.txt"), data.table=FALSE)
head(coCata)

# denominator/ registered voters
regCata <- fread(paste0(root, "data/Catalist Data/AllStates_counts_reg_16_voters.txt"), data.table=FALSE)

# grab only CO and precincts
regCata <- regCata[regCata$geography %like% "CO" & regCata$geography %like% "PR-",]
head(regCata)

# try to find the same precinct to see if the IDs line up
subset(coCata, geography %like% "ADAMS " & geography %like% "PR-247")
subset(regCata, geography %like% "ADAMS " & geography %like% "PR-247")

head(subset(regCata, geography %like% "EL PASO"))

# join the mail voter to the registered voter file
join <- full_join(coCata, regCata) 

dim(coCata); dim(regCata); dim(join) # great p much a perfect join

# need to join the precicnts to my CO block/precinct conversion file, and summarize by block
head(join)

head(subset(join, geography %like% "WELD"))
head(subset(join, geography %like% "FREMONT"))

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties = read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS = sprintf("%05d", siteCounties$FIPS)
site_co = siteCounties[siteCounties$State=="Colorado",]
site_co$COUNTY = toupper(site_co$CountyName)
table(site_co$CountyName)
head(site_co)

# limit catalist counties to only those included in the siting tool
library(stringr)

# extract county name
join$countyid <- word(join$geography, 1, 2, sep = " ") # get first two words
join$countyid <- gsub("CO ", "", join$countyid) # remove CO

head(join) # now just the county name
table(join$countyid)

# compare to list of siting tool counties
site_co$COUNTY # counties with second names like El Paso, La Plata, get cut off. Just add in manually

# fix el paso and la plata
join$countyid[join$countyid =="EL"] <- "EL PASO"
join$countyid[join$countyid =="LA"] <- "LA PLATA"

# MAKE name col that is title case
join$County <- str_to_title(join$countyid)
head(join)
table(join$County)

# limit to siting tool counties.
codat = join[join$countyid %in% site_co$COUNTY, ]
head(codat)
table(codat$countyid)
# all 16 counties are present

# grab the precinct number to match precNum
codat$precNum <- right(codat$geography, 3)

head(subset(codat, countyid=="PUEBLO"))
head(subset(codat, countyid=="EL PASO"))

###### Convert Precincts to Blocks #####
## Read in block-precinct conversion file 

# load conversion file
intr <- fread(paste0(root, "Colorado Siting Tool/data/intersect_precinct_crosswalk.csv"), data.table = FALSE)
head(intr)

head(subset(intr, FIPS=="08123"))

### multiply proportion by registered voters
# first join reg voters
intrReg <- merge(intr, codat, by=c("County", "precNum"), all=TRUE)

head(intrReg)
dim(intrReg); dim(intr); dim(codat)
checkna <- subset(intrReg, is.na(countyid))
table(checkna$County)

# multiply to get proportional registration for all variables
intrReg <- 
intrReg %>% 
  mutate(propRegTot = prc_Intrsct_area * TOTAL_REGISTERED_VOTERS,
         propRegLat = prc_Intrsct_area * HISPANIC_REGISTERED_VOTERS, 
         propRegAsn = prc_Intrsct_area * ASIAN_REGISTERED_VOTERS,
         propRegYth = prc_Intrsct_area * YOUTH_REGISTERED_VOTERS,
         
         propMailTot = prc_Intrsct_area * CO_TOTAL_2016_MAIL_VOTERS,
         propMailLat = prc_Intrsct_area * CO_HISPANIC_2016_MAIL_VOTERS, 
         propMailAsn = prc_Intrsct_area * CO_ASIAN_2016_MAIL_VOTERS, 
         propMailYth = prc_Intrsct_area * CO_YOUTH_2016_MAIL_VOTERS)

# summarize by block
blockReg <- 
  intrReg %>%
  dplyr::group_by(GEOID10, NAME10, County, FIPS) %>%
  dplyr::summarize(regBlockTot = sum(propRegTot, na.rm=T),
                   regBlockLat = sum(propRegLat, na.rm=T), 
                   regBlockAsn = sum(propRegAsn, na.rm=T), 
                   regBlockYth = sum(propRegYth, na.rm=T), 
                   
                   mailBlockTot = sum(propMailTot, na.rm=T), 
                   mailBlockLat = sum(propMailLat, na.rm=T), 
                   mailBlockAsn = sum(propMailAsn, na.rm=T), 
                   mailBlockYth = sum(propMailYth, na.rm=T)) %>% as.data.frame()

# review result
head(blockReg)
summary(blockReg)

# now calculate the vbm rates by blocks
blockReg_Final <- 
  blockReg %>% 
  mutate(
    TotVBM = mailBlockTot/regBlockTot,
    LatVBM = mailBlockLat/regBlockLat,
    AsnVBM = mailBlockAsn/regBlockAsn,
    YouthVBM = mailBlockYth/regBlockYth) %>% 
  rename("BLOCK_KEY" = GEOID10)

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


##### Export block vbm use rate #####
write.csv(blockReg_Final, paste0(root, "data/output/VBM_Use_Rate_BlocksCO.csv"), row.names = FALSE)


##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID <- substr(blockReg$GEOID10, 1, 11)  # extract the first digit of the block id

head(blockReg)

### Sum records by block group to get the voter totals per block group
tractVBM  <- 
  blockReg %>%
  dplyr::group_by(GEOID, FIPS, County) %>%   
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
  dplyr::select(GEOID, FIPS, TotVBM, LatVBM, AsnVBM, YouthVBM) %>% 
  filter(!is.na(TotVBM) & !is.na(LatVBM))
  

summary(tractVBM)
head(tractVBM)
subset(tractVBM, is.na(GEOID))


#### Export to tract for visuals #####
write.csv(tractVBM, paste0(root, "data/output/visualize/VBM_Use_Rate_Tracts_PrecinctsCO.csv"), row.names = FALSE)


