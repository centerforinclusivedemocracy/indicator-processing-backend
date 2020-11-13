# Calculate Eligible Non-Registered Voter Rates, Arizona - Maricopa County

library(data.table)
library(dplyr)
library(rgeos)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(sf)
library(lwgeom)
library(foreign)

# add function similar to right() in excel
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# set file path
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
AZroot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/Arizona Siting Tool/data/"

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE,
                         colClasses = c("FIPS"="character"))
site_az <- siteCounties[siteCounties$State =="Arizona", ]

### Prepare Data ####

# load AZ reg data from Catalist
# this data has all states
regAZ <- fread(paste0(root, "data/Catalist Data/AllStates_counts_reg_16_voters.txt"), data.table=FALSE)
head(regAZ)

# limit to AZ
regAZ <- regAZ[regAZ$geography %like% "AZ ", ]

# Manually correct precinct name spelling
# catalist has "Town Meadows" instead of Towne Meadows. I decided to trust the AZ county clerk website over catalist for the spelling
regAZ$geography[regAZ$geography =="AZ MARICOPA TOWN MEADOWS"] <-  "AZ MARICOPA TOWNE MEADOWS"

# make a name id that might match the precinct id
regAZ$precName <- gsub("AZ MARICOPA ", "", regAZ$geography)

head(regAZ)

### load Maricopa precincts to look at id

# load 2012 precincts to see how many join -- I got these precincts from the AZ Maricopa county clerks office (see readme file in the folder)
prec12 <- read.dbf(paste0(root, "Arizona Siting Tool/data/voter/VotPct2012.dbf"))
prec12$precName <- as.character(prec12$BdName)

head(prec12)
dim(prec12); dim(regAZ) # cool the number of precincts matches

# try joining the 2016 reg data to the precincts
joinAZ_12 <-  full_join(prec12 %>% dplyr::select(BdVal, PctNum, precName), regAZ,  by="precName")
head(joinAZ_12); dim(joinAZ_12)
summary(joinAZ_12) # no NAs! 


### load CVAP data ###
cvap <- fread(paste0(root, "data/acs/source/CVAP_2013-2017_ACS_csv_files/Tract.csv"),
              data.table = FALSE, colClasses = c("geoid"="character"))
head(cvap)

# create county fips 
cvap$FIPS <- substr(cvap$geoid, 8, 12)
cvap$GEOID <- substr(cvap$geoid, 8, 18)

# limit to total CVAP estimate and only Maricopa Co
cvap <- cvap[(cvap$lntitle =="Total") & cvap$FIPS %in% site_az$FIPS, ]

head(cvap); dim(cvap)


## Read in the incarcerated population data from the 2010 census (TRACTS)
# manually downloaded for only Maricopa county. could use census api/tidycensus instead for more efficient bulk processing 
instPop <- read.csv(paste0(root, "Arizona Siting Tool/data/institutionalized_pop/DEC_10_SF1_P42_with_ann.csv"), header = TRUE, 
                    stringsAsFactors = FALSE, skip=1,colClasses = c(Id2='character'))

instPop <- instPop[,c(2,4,6)]
colnames(instPop) <- c("GEOID", "TotalGroupQuarters", "Incarc_Adults")

head(instPop)

### non institutionalized pop data
totNonInstPop <- read.csv(paste0(root, "Arizona Siting Tool/data/institutionalized_pop/ACS_17_5YR_S1810_with_ann.csv"), header = TRUE, stringsAsFactors = FALSE, skip=1, colClasses = c(Id2='character'))
totNonInstPop <- totNonInstPop[,c(2, 4:5)]
colnames(totNonInstPop) <- c("GEOID", "tract_TotNonInstPop", "tract_TotNonInstPop.MOE")
head(totNonInstPop)
dim(totNonInstPop)
# right now I'm moving forward wiht only 2016 registration data. Don't have catalist data for AZ for 2014. Review with GINFO and Mindy. 



##### CAlculate the proportion of each precinct that is in each block ####
## load precinct shapefile
# note this is what was posted online, but is not necessarily the same precinct as the 2016/2014 general elections

prec <- st_read(paste0(root, "Arizona Siting Tool/data/voter/VotPct2012.shp"))
prec$precName <- as.character(prec$BdName)
prec <- st_transform(prec, 2868) # selected AZ specific projection

#test plot 
# plot(prec$geometry)

# load census block geometry
blocksAZ <- st_read(paste0(root, "data/decennial/blocksAZ.shp"))
blocksAZ <- st_transform(blocksAZ, 2868)  # projection uses ft but I think thats ok for now, just getting proportions


# Calculate area -- 
prec$PrecFull_area <- st_area(prec)

# get area of blocs
blocksAZ$BlockFull_area = st_area(blocksAZ)
head(blocksAZ)

## Intersect two shps
intr <- st_intersection(blocksAZ, prec) 


# question: how much of the precinct is in each block? 
intr$Intrsct_area <- st_area(intr)
head(intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
intr$prc_Intrsct_area  <- as.numeric(intr$Intrsct_area)/as.numeric(intr$PrecFull_area)

head(intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
intr_df <- 
  intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(intr_df)

## export crosswalk to csv
write.csv(intr_df, paste0(root, "Arizona Siting Tool/data/az_intersect_precinct_crosswalk.csv"), row.names = FALSE)



#### OPTIONAL: DO THIS IF YOU DIDN'T RUN THE INTERSECT ABOVE (Because that takes a min) #####
# load conversion file
intr <- fread(paste0(root, "Arizona Siting Tool/data/az_intersect_precinct_crosswalk.csv"), data.table = FALSE)  

head(intr)


### multiply proportion by registered voters
head(regAZ)

# first join reg voters
intrReg <- merge(intr, regAZ, by=c("precName"), all=TRUE, duplicateGeoms=T)
head(intrReg)
dim(intrReg); dim(intr); dim(regAZ)


# multiply to get proportional registration
intrReg$propReg <- intrReg$prc_Intrsct_area * intrReg$TOTAL_REGISTERED_VOTERS

# summarize by block
blockReg  <- 
  intrReg %>%
  as.data.frame() %>% 
  dplyr::group_by(GEOID10, NAME10, COUNTYFP10) %>%
  dplyr::summarize(regBlockTot = sum(propReg, na.rm=T))

head(blockReg)


# export registration stats at the block level--used to build the clusters
blockRegdf <- 
  blockReg %>% 
  as.data.frame() %>% 
  dplyr::select(GEOID10, COUNTYFP10, regBlockTot) %>% 
  rename("FIPS" = COUNTYFP10) %>% 
  mutate(County = "Maricopa", 
         FIPS = paste0("04", FIPS))

head(blockRegdf)
str(blockRegdf)


## Export registration blocks for model clusters
# write csv
write.csv(blockRegdf, paste0(root, "data/output/az_Reg_2016.csv"), row.names = FALSE)


##### Convert to Tract #####
## Create Tract ID
blockRegdf$GEOID = substr(blockRegdf$GEOID10, 1, 11)  # extract the first digit of the block id

head(blockRegdf)

### Sum records by block group to get the voter totals per block group
tractReg  <- 
  blockRegdf %>%
  dplyr::group_by(GEOID) %>%   
  dplyr::summarize(reg = sum(regBlockTot, na.rm = T)) %>%
  as.data.frame()

head(tractReg); dim(tractReg)


##### CAlculate the Eligible non reg voter pop #####
### Merge CVAP with the averaged tract registration file in order to calculate the number of people who are eligible to vote but are not registered
regCVAP <- full_join(tractReg, cvap[ ,c(1, 7:10)])

head(as.data.frame(regCVAP))
dim(regCVAP)
summary(regCVAP)
subset(regCVAP, is.na(GEOID))

### calculate % of the eligible population that is eligible and non-registered 
regCVAP$Tot_EligNonReg_prc  <- (regCVAP$CVAP_EST - regCVAP$reg)/regCVAP$CVAP_EST  # CVAP Total - Total Registered (2016) divided by CVAP total

# where cvap is zero, change the infinity (from the divide by zero) to NA
regCVAP$Tot_EligNonReg_prc <- ifelse(regCVAP$CVAP_EST==0, NA, regCVAP$Tot_EligNonReg_prc)
head(regCVAP)

# negative values recode to NA
# but first flag as unreliable
regCVAP$TotElig_flag <- 0

# flag as unreliable when the estimate is negative
regCVAP$TotElig_flag[regCVAP$Tot_EligNonReg_prc < 0] <- 1 
head(regCVAP)

### Calculate sampling error for CVAP : CV calculation (coefficient of variation). We don't have MOE for the numerator, just calculate the standard CV
# CV= [(MOE/1.645)/ESTIMATE] * 100%
regCVAP$CV_Tot <- ((regCVAP$CVAP_MOE/1.645)/regCVAP$CVAP_EST)*100

# if the CV is over 40%, flag as unreliable
regCVAP$TotElig_flag[regCVAP$CV_Tot > 40] <- 1 
head(regCVAP)

### Join the incarcerated & noninstitutionalized population to the CVAP data
regCVAP <- full_join(regCVAP, instPop)
dim(regCVAP)

regCVAP <- full_join(regCVAP, totNonInstPop)

head(regCVAP)
dim(regCVAP)

#### Calculate the percent of the tract CVAP that is the incarcerated adult population 
regCVAP$incarcPop_prc <- regCVAP$Incarc_Adults/regCVAP$CVAP_EST


### Create a 'final' column, where the % eligible is used EXCEPT if the value is negative or the CV is over 40% and the incarcerated population is > 25%
regCVAP$Tot_EligNonReg_prc_FINAL <- ifelse(regCVAP$incarcPop_prc > 0.25, 
                                           NA, regCVAP$Tot_EligNonReg_prc)

summary(regCVAP$Tot_EligNonReg_prc_FINAL)

regCVAP$Tot_EligNonReg_prc_FINAL <- ifelse(regCVAP$Tot_EligNonReg_prc < 0, 0, regCVAP$Tot_EligNonReg_prc)

head(regCVAP)

# Now where the incarcerated population is greater than 25%, use the bg estimate for non institutionalized populations (as a replacement for cvap)
# we have no estimate for latino or asian, so those data will have to be reomved.
# if incarcerated pop is over 25%, calculate a new eligible non-registered percentage, otherwise use the same final prc
regCVAP$Tot_EligNonReg_prc_FINAL <- ifelse(regCVAP$incarcPop_prc > 0.25, (regCVAP$tract_TotNonInstPop - regCVAP$avgReg)/regCVAP$tract_TotNonInstPop,
                                           regCVAP$Tot_EligNonReg_prc_FINAL)

# flag the unreliable estimate here
regCVAP$TotElig_flag[regCVAP$Tot_EligNonReg_prc_FINAL < 0] <- 1
regCVAP$Tot_EligNonReg_prc_FINAL[regCVAP$Tot_EligNonReg_prc_FINAL < 0] <- 0

# if the estimate is NA or the CV is NA or INF, flag as unreliable
regCVAP$TotElig_flag[is.na(regCVAP$Tot_EligNonReg_prc_FINAL) | is.na(regCVAP$CV_Tot) | regCVAP$CV_Tot=="Inf"] <- 1
regCVAP$Tot_EligNonReg_prc_FINAL[is.na(regCVAP$Tot_EligNonReg_prc_FINAL)] <- 0 # the model needs values, can't have NAs. Convert NAs to zero and make sure reliability flag is on it


regCVAP = as.data.frame(regCVAP)
head(regCVAP)
summary(regCVAP)

subset(regCVAP, is.na(Tot_EligNonReg_prc))

#### Export finished tract data files to output folder ###
# keep only geoid, fips county, tot elig prc final, and reliability flag
write.csv(regCVAP[,c(1, 6, 15, 8)], paste0(root, "data/output/Elig_NonReg_Pop_TractsAZ.csv"), row.names = FALSE)

# one for the visualization folder
write.csv(regCVAP[,c(1, 6, 15, 8)], paste0(root, "data/output/visualize/Elig_NonReg_Pop_TractsAZ.csv"), row.names = FALSE)

#


