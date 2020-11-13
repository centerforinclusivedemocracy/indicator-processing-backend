# Calculate Eligible Non-Registered Voter Rates, Colorado

library(data.table)
library(dplyr)
library(rgeos)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(sf)
library(lwgeom)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
COroot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/Colorado Siting Tool/data/"

#### Prepare Data ####
# downloaded this data from the CO SOS website. see readme file in folder
gen14 = read.csv(paste0(COroot, "voter/2014GeneralPrecinctTurnout.csv"), stringsAsFactors = FALSE, colClasses = c("Precinct"="character"))
head(gen14)

gen16 = read.csv(paste0(COroot, "voter/2016GeneralTurnoutPrecinctLevel.csv"), stringsAsFactors = FALSE, colClasses = c("Precinct"="character"))
head(gen16)

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties = read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS = sprintf("%05d", siteCounties$FIPS)
site_co = siteCounties[siteCounties$State=="Colorado",]
site_co$COUNTY = toupper(site_co$CountyName)
table(site_co$CountyName)

# temp: get number of voters in our counties of interest
test = gen16[gen16$NAME %in% site_co$CountyName, ]
head(test)
sum(test$Ballots.Cast, na.rm = T)

# read in CVAP data
cvap <- fread(paste0(root, "data/acs/source/CVAP_2013-2017_ACS_csv_files/Tract.csv"),data.table = FALSE, colClasses = c("geoid"="character"))

head(cvap)

# create county fips 
cvap$FIPS <- substr(cvap$geoid, 8, 12)
cvap$GEOID <- substr(cvap$geoid, 8, 18)
head(cvap)

# Grab tracts in the counties of interest
cvap = cvap[cvap$FIPS %in% site_co$FIPS & cvap$lntitle =="Total",]
dim(cvap)
head(cvap)


## Read in the incarcerated population data from the 2010 census (TRACTS)
instPop <- read.csv(paste0(COroot, "institutionalized_pop/DEC_10_SF1_P42_with_ann.csv"), header = TRUE, 
                    stringsAsFactors = FALSE, skip=1,colClasses = c(Id2='character'))

instPop <- instPop[,c(2,4,6)]
colnames(instPop) <- c("GEOID", "TotalGroupQuarters", "Incarc_Adults")

head(instPop)

### non institutionalized pop data
totNonInstPop <- read.csv(paste0(COroot, "institutionalized_pop/ACS_17_5YR_S1810_with_ann.csv"), header = TRUE, stringsAsFactors = FALSE, skip=1, colClasses = c(Id2='character'))
totNonInstPop <- totNonInstPop[,c(2, 4:5)]
colnames(totNonInstPop) <- c("GEOID", "tract_TotNonInstPop", "tract_TotNonInstPop.MOE")
head(totNonInstPop)


### Prepare the registration data #####
# first average the registration rate for 2014 and 2016 general elections
colnames(gen14)[8] <- "Tot.Reg.14"
colnames(gen16)[8] <- "Tot.Reg.16"
reg = full_join(gen14[,c(1,5,10, 8)], gen16[,c(1,5, 10, 8)])
head(reg)

reg$avg.Reg = rowMeans(reg[,c(4:5)], na.rm = T)
colnames(reg)[2:3] <- c("PRECINCT", "County")
head(reg)

# make a version of the precinct code that is just the last three digits (prec number)
reg$precNum <- right(reg$PRECINCT, 3)
head(reg)



##### CAlculate the proportion of each precinct that is in each block ####
## load precinct shapefile
# note this is what was posted online, but is not necessarily the same precinct as the 2016/2014 general elections

### load  files
preclist = list(
  Denver = read_sf(dsn=paste0(COroot, "voter/precincts/Denver County"), layer="DENVER_PRECINCTS_CLEAN_2018"),
  Arapahoe = read_sf(dsn=paste0(COroot, "voter/precincts/Arapahoe County"), layer="2017_Precinct_Boundaries"),
  ElPaso = read_sf(dsn=paste0(COroot, "voter/precincts/El Paso County"), layer="EPC_Precincts"),
  Jefferson = read_sf(dsn=paste0(COroot, "voter/precincts/Jefferson County"), layer="County_Precinct"),
  Adams = read_sf(dsn=paste0(COroot, "voter/precincts/Adams County"), layer="Adams_Precincts"),
  Boulder = read_sf(dsn=paste0(COroot, "voter/precincts/Boulder County"), layer="Precincts"),
  Larimer = read_sf(dsn=paste0(COroot, "voter/precincts/Larimer County"), layer="VoterPrecinct"),
  Weld = read_sf(dsn=paste0(COroot, "voter/precincts/Weld County"), layer="Elections_Precincts"),
  Douglas = read_sf(dsn=paste0(COroot, "voter/precincts/Douglas County"), layer="precinct"),
  Mesa = read_sf(dsn=paste0(COroot, "voter/precincts/Mesa County"), layer="Precinct"),
  Pueblo = read_sf(dsn=paste0(COroot, "voter/precincts/Pueblo County"), layer="Precincts_180313"),
  Garfield = read_sf(dsn=paste0(COroot, "voter/precincts/Garfield County"), layer="VoterPrecincts022118"),
  LaPlata = read_sf(dsn=paste0(COroot, "voter/precincts/La Plata County"), layer="LPC_Voting_Precincts"),
  Broomfield = read_sf(dsn=paste0(COroot, "voter/precincts/Broomfield County"), layer="Precincts"),
  Eagle = read_sf(dsn=paste0(COroot, "voter/precincts/Eagle County"), layer="VoterPrecincts022018"),
  Fremont = read_sf(dsn=paste0(COroot, "voter/precincts/2017_Precincts/"), layer="Fremont_County_Precincts")
)

head(preclist[[3]]) # el paso
head(preclist[[8]]) #  Weld
head(preclist[[7]]) # larimer
head(preclist[[16]]) # fremont--added 6/15/20


# isolate fields, only the precinct number so I can standardize the field names and combine
preclist[[1]] = preclist[[1]][,c(2, 1)]
preclist[[2]] = preclist[[2]][,c(7, 8)]
preclist[[3]] = preclist[[3]][,c(6, 2)]
preclist[[4]] = preclist[[4]][,c(2, 1)] # replace object id with the actual precinct number
preclist[[4]]$OBJECTID <- right(preclist[[4]]$COUNTYPREC, 3)
preclist[[5]] = preclist[[5]][,c(2, 1)]
preclist[[6]] = preclist[[6]][,c(3, 4)]
preclist[[7]] = preclist[[7]][,c(3, 1)] # replace object id with the actual precinct number
preclist[[7]]$OBJECTID <- right(preclist[[7]]$PRECINCT, 3)
preclist[[8]] = preclist[[8]][,c(6, 16)] # preclist[[8]] = preclist[[8]][,c(6, 4)]
preclist[[9]] = preclist[[9]][,c(6, 5)]
preclist[[10]] = preclist[[10]][,c(2,1)]
preclist[[11]] = preclist[[11]][,c(11, 10)]
preclist[[12]] = preclist[[12]][,c(1, 2)]
preclist[[13]] = preclist[[13]][,c(5, 5)]
preclist[[14]] = preclist[[14]][,c(4, 6)]
preclist[[15]] = preclist[[15]][,c(11, 7)]
preclist[[16]] = preclist[[16]][,c(2, 1)]
preclist[[16]]$OBJECTID <- right(preclist[[16]]$id_full, 3)

# rename columns
# create list of names
clnames = c("PRECINCT", "precNum", "geometry")

# rename function
preclist = lapply(preclist, setNames, nm= clnames)

head(preclist[[5]])
head(preclist[[13]])
head(preclist[[11]]) # pueblo
head(as.data.frame(preclist[[7]]))
head(preclist[[16]])

# Format precinct numbers as 3 digit numbers (add leading zeros if necessary)
preclist <- lapply(preclist, function(x) within(x, precNum <- sprintf("%03s", x$precNum)))


# add county name as id
countylist = site_co$CountyName
fips = site_co$FIPS # list of fips 

# cbind the county name and county fips codes to the precinct df
## MAKE SURE THIS ORDER IS THE SAME AS THE PRECLIST ORDER BECAUSE THIS IS A STRAIGHT CBIND NOT A JOIN
preclist = mapply(cbind, preclist, "County" = countylist, SIMPLIFY=FALSE)
preclist = mapply(cbind, preclist, "FIPS" = fips, SIMPLIFY=FALSE)

head(preclist[[7]])
head(preclist[[11]])
head(preclist[[16]])

# load census block geometry
blocksCO <- read_sf(dsn=paste0(root, "data/decennial"), layer="blocksCO")

blocksCO$FIPS <- paste0(blocksCO$STATEFP10, blocksCO$COUNTYFP10) # format full fips code
blocksCO <- blocksCO[,c("COUNTYFP10", "GEOID10", "NAME10", "FIPS")] # keep selected cols

# only keep selected co counties
blocksCO <- blocksCO[blocksCO$FIPS %in% site_co$FIPS, ]
head(blocksCO)


# Project 
# project precincts and blocks 
precprj = lapply(preclist, function(x) x %>% st_transform(6427)) # colorado pcs
# lapply(precprj, function(x) x %>% st_crs()) # check


# prj blocks
blocksprj <-  blocksCO %>% st_transform(6427) # colorado pcs
blocksprj %>% st_crs()


# Calculate area -- units are meters sq
precprj <- lapply(precprj, function(x) within(x, PrecFull_area_m2 <- st_area(x)))

# get area of blocs
blocksprj$BlockFull_area_m2 <- st_area(blocksprj)
head(blocksprj)


# dissolved on precNum...think we may need that (e.g. Pueblo)
precDissolve <- lapply(precprj, function(x) x %>% 
                         group_by(precNum, County, FIPS) %>% 
                         summarize(countSubPrec = length(PRECINCT))) # just to see--get count of precincts in summary 

head(precDissolve[[11]])
head(precDissolve[[1]])
head(precDissolve[[16]])

# Calculate area -- units are meters sq
precDissolve <- lapply(precDissolve, function(x) within(x, PrecFull_area_m2 <- st_area(x)))

## Combine all the precinct data
precAll <- do.call("rbind", precDissolve)

head(precAll)
head(subset(precAll, County=="Adams"))
table(precAll$FIPS, precAll$County) # # ok county/fips combo is ok here

# optional review step: export temp shapefile just to reveiew in qgis
# st_write(precAll, "/Users/lauradaly/Documents/GreenInfo/Contract Work/temp_data/precincts_co_all.shp")

## Intersect two shps
intr <- st_intersection(blocksprj, precAll) # takes about 3-4 min to run full blocks/precinct intersect


# question: how much of the precinct is in each block? 
intr$Intrsct_area <- st_area(intr)
head(intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
intr$prc_Intrsct_area <- intr$Intrsct_area/intr$PrecFull_area_m2

# I want to remove intersect edge cases, so remove instances where FIPS does not equal FIPS.1 (i.e. fips code from teh precincts vs. from the blocks)
intr_clean <- subset(intr, FIPS == FIPS.1) # only where they equal each other

# compare
dim(intr_clean) ; dim(intr)
table(intr_clean$FIPS, intr_clean$FIPS.1)
table(intr_clean$FIPS, intr_clean$County)
table(intr$FIPS, intr$FIPS.1)

# export the intersect shp here so that it can be used for the VBM rates conversion as well

#### NOTE, YOU CAN JUST LOAD THIS CONVERSION FILE HERE AND SKIP THE PRECEDING STEPS--THE INTERSECT TAKES A WHILE ####
# exported as geojson because I couldn't get shapefile working 
write_sf(intr_clean, paste0(COroot, "blocks_prec_conversion_v2_CO.geojson"), delete_dsn=TRUE)


#  the conversion file doesn't have to be spatial
intr_df <- 
  intr_clean %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(intr_df)

## export crosswalk to csv
write.csv(intr_df, paste0(COroot, "intersect_precinct_crosswalk.csv"), row.names = FALSE)


#### OPTIONAL: DO THIS IF YOU DIDN'T RUN THE INTERSECT ABOVE #####
# load conversion file
intr <- fread(paste0(COroot, "intersect_precinct_crosswalk.csv"), data.table = FALSE) # OR load csv?

head(intr)
table(intr$FIPS, intr$County) 

##### END #####

### multiply proportion by registered voters
# first join reg voters
intrReg = merge(intr, reg[,c(7, 3, 6)], by=c("precNum", "County"), all=TRUE, duplicateGeoms=T)
head(intrReg)
dim(intrReg); dim(intr); dim(reg)

# check pueblo
summary(subset(intrReg, County =="Pueblo"))
head(subset(intrReg, County =="Denver"))

# Check nas
summary(intrReg)

checkna <- subset(intrReg, is.na(avg.Reg))
table(checkna$County)
table(intrReg$County)



# multiply to get proportional registration
intrReg$propReg = intrReg$prc_Intrsct_area * intrReg$avg.Reg

# summarize by block
blockReg  <- 
  intrReg %>%
  as.data.frame() %>% 
  dplyr::group_by(GEOID10, NAME10, County, FIPS) %>%
  dplyr::summarize(regBlockTot = sum(propReg, na.rm=T))

head(blockReg)

class(blockReg)

# review
summary(subset(blockReg, County=="Pueblo")$regBlockTot)
summary(subset(blockReg, County=="Larimer"))
summary(subset(blockReg, County=="Weld"))
summary(subset(blockReg, County=="Fremont"))

# export registration stats at the block level--used to build the clusters
blockRegdf <- blockReg %>% as.data.frame() %>% 
  dplyr::select(GEOID10, FIPS, County, regBlockTot) 

head(blockRegdf)
str(blockRegdf)

#### EXPORT: write csv for registration data at block level #####
write.csv(blockRegdf, paste0(root, "data/output/co_Reg_2016.csv"), row.names = FALSE)


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
regCVAP <- full_join(tractReg, cvap[cvap$lntitle=="Total",c(1, 7:10)])

head(as.data.frame(regCVAP))
dim(regCVAP)

### calculate % of the eligible population that is eligible and non-registered 
regCVAP$Tot_EligNonReg_prc  <- (regCVAP$CVAP_EST - regCVAP$reg)/regCVAP$CVAP_EST  # CVAP Total - Total Registered (2014-2016 average) divided by CVAP total

# where cvap is zero, change the infinity (from the divide by zero) to NA
regCVAP$Tot_EligNonReg_prc <- ifelse(regCVAP$CVAP_EST==0, NA, regCVAP$Tot_EligNonReg_prc)
head(regCVAP)

# negative values recode to NA
# but first flag as unreliable
regCVAP$TotElig_flag <- 0

regCVAP$TotElig_flag[regCVAP$Tot_EligNonReg_prc < 0] <- 1 

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

# review
head(regCVAP); dim(regCVAP)

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

# remove na geoid
regCVAP <- regCVAP[!is.na(regCVAP$GEOID), ]

#### Export finished tract data files to output folder ###
# keep only geoid, fips county, tot elig prc final, and reliability flag
write.csv(regCVAP[,c(1, 6, 15, 8)], paste0(root, "data/output/Elig_NonReg_Pop_TractsCO.csv"), row.names = FALSE)

# one for the visualization folder # they are the same but just to have consistent files in each folder
write.csv(regCVAP[,c(1, 6, 15, 8)], paste0(root, "data/output/visualize/Elig_NonReg_Pop_TractsCO.csv"), row.names = FALSE)

#


