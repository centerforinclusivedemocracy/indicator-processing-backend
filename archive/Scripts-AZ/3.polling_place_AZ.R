# AZ Maricopa County Voter Data: Share of Polling Place Voters (total only)
library(data.table)
library(dplyr)
options(scipen=999)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root = "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"


# load CO Voter data
azCata <- fread(paste0(root, "data/Catalist Data/AZ_mail.txt"), data.table=FALSE)
head(azCata)

# denominator/ registered voters
regCata <- fread(paste0(root, "data/Catalist Data/AllStates_counts_reg_16_voters.txt"), data.table=FALSE)

# grab only az and precincts
regCata <- regCata[regCata$geography %like% "AZ",]
head(regCata)


# join the mail voter to the registered voter file
join <- full_join(azCata %>% 
                    dplyr::select(geography, AZ_TOTAL_2016_ABSENTEE_OR_EARLY_VOTERS), 
                  regCata %>% 
                    dplyr::select(geography, TOTAL_2016_VOTERS)) %>% 
  mutate(poll_voters = TOTAL_2016_VOTERS - AZ_TOTAL_2016_ABSENTEE_OR_EARLY_VOTERS) ## Assume that if someone isn't a mail voter, they voted in person 

head(join)
dim(azCata); dim(regCata); dim(join) # great p much a perfect join

# remove the one row with no geo id
join <- join[join$geography !="", ]

# change the spelling of Town Meados
join$geography[join$geography=="AZ MARICOPA TOWN MEADOWS"] <- "AZ MARICOPA TOWNE MEADOWS"
head(join)

# need to join the precicnts to my AZ block/precinct conversion file, and summarize by block


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

# load conversion file
intr <- fread(paste0(root, "Arizona Siting Tool/data/az_intersect_precinct_crosswalk.csv"), data.table = FALSE)
head(intr)


### multiply proportion by registered voters
# first join crosswalk to poll voters voters
intrReg = merge(intr, join, by="precName", all=TRUE)

head(intrReg)
dim(intrReg); dim(intr); dim(join)


# check nas
subset(intrReg, is.na(GEOID10))
subset(intrReg, is.na( TOTAL_2016_VOTERS))
# seems good!

# multiply to get proportional registration for all variables
intrReg <- 
intrReg %>% 
  mutate(propTotVoters = prc_Intrsct_area * TOTAL_2016_VOTERS,
         propMailVoters = prc_Intrsct_area * AZ_TOTAL_2016_ABSENTEE_OR_EARLY_VOTERS)

head(intrReg)

# summarize by block
blockReg <- 
  intrReg %>%
  as.data.frame() %>% 
  dplyr::group_by(GEOID10, NAME10, COUNTYFP10) %>%
  dplyr::summarize(totVotersBlock = sum(propTotVoters, na.rm=T),
                   mailVotersBlock = sum(propMailVoters, na.rm=T)) %>% 
  as.data.frame() %>% 
  rename("FIPS"=COUNTYFP10) %>% 
  mutate(FIPS = paste0("04", FIPS))

head(blockReg)
summary(blockReg)
table(blockReg$FIPS, useNA = "always")

# now calculate the polling place share
# block level polling place share
blockReg$pollVotersBlock <- blockReg$totVotersBlock - blockReg$mailVotersBlock

# get the county total of in person/polling place voters (denominator of polling place share)
countyPollVoters <- 
blockReg %>% 
  filter(!is.na(FIPS)) %>% 
  group_by(FIPS) %>% 
  summarize(sumTotVoters = sum(totVotersBlock, na.rm=T), 
            sumMailVoters = sum(mailVotersBlock, na.rm=T), 
            sumPollVoters = sum(pollVotersBlock, na.rm=T)) 
#  mutate(diffPollVoters = sumTotVoters - sumMailVoters) # these two methods match, so I'll just stick with the sum of poll voters
  
countyPollVoters # I checked these county total votes cast numbers against the SOS  table downloaded here:
# https://apps.azsos.gov/election/2016/General/ElectionInformation.htm
# the total ballots cast is pretty close.

# divide the number of in person voters in each block by the county total
pollShare <- 
blockReg %>% 
  left_join(countyPollVoters) %>% 
  mutate(pollShare = pollVotersBlock/sumPollVoters ) %>% 
  dplyr::select(GEOID10, FIPS, pollShare) %>% 
  filter(!is.na(GEOID10)) %>% 
    rename("BLOCK_KEY" = GEOID10) 


head(pollShare)
summary(pollShare)

## export
write.csv(pollShare, paste0(root, "data/output/PollShare_BlocksAZ.csv"), row.names = FALSE)


####### START HERE ######
##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID = substr(blockReg$GEOID10, 1, 11)  # extract the first digit of the block id

head(blockReg)

### Sum records by cenus tract
tractPoll  <- 
  blockReg %>%
  dplyr::group_by(GEOID, FIPS) %>%   
  dplyr::summarize(totVotersTract = sum(totVotersBlock, na.rm=T),
                   pollVotersTract = sum(pollVotersBlock, na.rm=T)) %>% 
  
  # join county voter totals
  left_join(countyPollVoters) %>% 
  mutate(
    pollShare = pollVotersTract/sumPollVoters) %>% 
  dplyr::select(GEOID, FIPS, pollShare) %>% 
  filter(!is.na(GEOID))


head(tractPoll)
subset(tractPoll, is.na(GEOID))
summary(tractPoll)

## Export to tract for visuals
write.csv(tractPoll, paste0(root, "data/output/visualize/PollShare_PrecinctsTractsAZ.csv"), row.names = FALSE)


