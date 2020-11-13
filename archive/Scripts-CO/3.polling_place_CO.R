# Colorado Voter Data: Share of Polling Place Voters (total only)
library(data.table)
library(dplyr)
options(scipen=999)

# add function that subsets characters from string from right side
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"


# load CO Voter data
coCata <- fread(paste0(root, "data/Catalist Data/CO_mail.txt"), data.table=FALSE)
head(coCata)

#  registered voters
regCata <- fread(paste0(root, "data/Catalist Data/AllStates_counts_reg_16_voters.txt"), data.table=FALSE)

# grab only CO and precincts
regCata <- regCata[regCata$geography %like% "CO" & regCata$geography %like% "PR-",]
head(regCata)

# try to find the same precinct to see if the IDs line up
subset(coCata, geography %like% "ADAMS " & geography %like% "PR-247")
subset(regCata, geography %like% "ADAMS " & geography %like% "PR-247")


# join the mail voter to the registered voter file
join <- full_join(coCata %>% 
                    dplyr::select(geography, CO_TOTAL_2016_MAIL_VOTERS), 
                  regCata %>% 
                    dplyr::select(geography, TOTAL_2016_VOTERS)) %>% 
  mutate(poll_voters = TOTAL_2016_VOTERS - CO_TOTAL_2016_MAIL_VOTERS) ## Assume that if someone isn't a mail voter, they voted in person 

head(join)
dim(coCata); dim(regCata); dim(join) # great p much a perfect join


# need to join the precicnts to my CO block/precinct conversion file, and summarize by block


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

# limit to siting tool counties.
codat = join[join$countyid %in% site_co$COUNTY, ]
head(codat)
table(codat$countyid) # cool all the names look right


# grab the precinct number to match precNum
codat$precNum <- right(codat$geography, 3)

# review
head(subset(codat, countyid=="PUEBLO"))

###### Convert Precincts to Blocks #####
## Read in block-precinct conversion file 

# load conversion file
intr <- fread(paste0(root, "Colorado Siting Tool/data/intersect_precinct_crosswalk.csv"), data.table = FALSE)
head(intr)


### multiply proportion by registered voters
# first join reg voters
intrReg <- merge(intr, codat, by=c("County", "precNum"), all=TRUE)

head(intrReg)
dim(intrReg); dim(intr); dim(codat)
head(subset(intrReg, countyid =="PUEBLO"))
summary(intrReg$CO_TOTAL_2016_MAIL_VOTERS)

# check nas
checknas <- subset(intrReg, is.na(CO_TOTAL_2016_MAIL_VOTERS))
table(checknas$County) # overall a small percentage/consistent with other nas

# multiply to get proportional registration for all variables
intrReg <- 
intrReg %>% 
  mutate(propTotVoters = prc_Intrsct_area * TOTAL_2016_VOTERS,
         propMailVoters = prc_Intrsct_area * CO_TOTAL_2016_MAIL_VOTERS)

head(intrReg)

# summarize by block
blockReg <- 
  intrReg %>%
  as.data.frame() %>% 
  dplyr::group_by(GEOID10, NAME10, County, FIPS) %>%
  dplyr::summarize(totVotersBlock = sum(propTotVoters, na.rm=T),
                   mailVotersBlock = sum(propMailVoters, na.rm=T)) %>% 
  as.data.frame()

head(blockReg)
summary(blockReg)
table(blockReg$County)

# now calculate the polling place share
# block level polling place share
blockReg$pollVotersBlock <- blockReg$totVotersBlock - blockReg$mailVotersBlock

# get the county total of in person/polling place voters (denominator of polling place share)
countyPollVoters <- 
blockReg %>% 
  filter(!is.na(FIPS)) %>% 
  group_by(County, FIPS) %>% 
  summarize(sumTotVoters = sum(totVotersBlock, na.rm=T), 
            sumMailVoters = sum(mailVotersBlock, na.rm=T), 
            sumPollVoters = sum(pollVotersBlock, na.rm=T)) 
#  mutate(diffPollVoters = sumTotVoters - sumMailVoters) # these two methods match, so I'll just stick with the sum of poll voters
  
head(countyPollVoters) # I checked these county total votes cast numbers against the CO SOS excel table downloaded here:
# https://www.sos.state.co.us/pubs/elections/Results/archive2000.html
# look at column "Ballots Cast"

# divide the number of in-person voters in each block by the county total
pollShare <- 
blockReg %>% 
  left_join(countyPollVoters) %>% 
  mutate(pollShare = pollVotersBlock/sumPollVoters ) %>% 
  dplyr::select(GEOID10, FIPS, pollShare) %>% 
  filter(!is.na(GEOID10)) %>% 
    rename("BLOCK_KEY" = GEOID10) 


head(pollShare)
summary(pollShare)

#### export to blocks for model ####
write.csv(pollShare, paste0(root, "data/output/PollShare_BlocksCO.csv"), row.names = FALSE)



##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID = substr(blockReg$GEOID10, 1, 11)  # extract the first digit of the block id

head(blockReg)

### Sum records by cenus tract
tractPoll  <- 
  blockReg %>%
  dplyr::group_by(GEOID, FIPS, County) %>%   
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


#### Export to tract for visuals ####
write.csv(tractPoll, paste0(root, "data/output/visualize/PollShare_PrecinctsTractsCO.csv"), row.names = FALSE)


