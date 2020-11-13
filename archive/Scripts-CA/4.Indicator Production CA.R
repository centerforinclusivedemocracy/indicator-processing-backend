# Combine indicators to make one single data input for the model
# the goal is to merge everything to blocks for the block score/weighted average
library(data.table)
library(dplyr)
library(tidyverse)
library(rgeos)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(sf)
library(lwgeom)
library(scales)
library(foreign)
library(geosphere)
library(purrr)

############# SET UP DATA ############# 
# define data path (Save to object)
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
outputRoot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"


############# CALIFORNIA #########

# ACS # note these were updated with 2018 data in spring 2020
dat <- read.csv(paste0(root, "data/output/ACS_Indicators_TractsCA.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character", "FIPS"="character"))

# review.  reliability flag should be included
head(dat)

# job - load job share data by block
job <- read.csv(paste0(root, "data/output/JobShare_Block_blocksCA.csv"), stringsAsFactors = FALSE, colClasses = c("w_geocode"="character", "w_FIPS"="character"))

# rename columns
colnames(job)[1:2] <- c("GEOID10", "FIPS")
job = job[,c(1,2,5)] #
head(job)

# pop dens - load population density at the block level. 
pop <- read.csv(paste0(root, "data/output/PopDensity_Block_blocksCA.csv"), stringsAsFactors = FALSE, 
               colClasses = c("GEOID10"="character", "TRACTCE10"="character", "COUNTYFP10"="character"))
pop <- pop[,c(5, 2, 20, 21,23, 24)]
colnames(pop)[2:3] <- c("FIPS", "pop2010")
pop$FIPS <- paste0("06", pop$FIPS) # format fips code
head(pop)


# elig # load eligible non-reg population 
elig <- read.csv(paste0(root, "data/output/Elig_NonReg_Pop_TractsCA.csv"), stringsAsFactors = FALSE, 
                colClasses = c("GEOID"="character", "FIPS"="character"))
colnames(elig)[4] <- "ElNonReg_flag"
head(elig)
summary(elig) # look for NAs


# poll  # load polling place share by block
poll <- read.csv(paste0(root, "data/output/PollShare_BlocksCA.csv"), stringsAsFactors = FALSE, colClasses = c("BLOCK_KEY"="character", "FIPS"="character"))
colnames(poll)[1] <-"GEOID10"
head(poll)
summary(poll)

# vbm rate # load VBM use rate by block
vbm <- read.csv(paste0(root, "data/output/VBM_Use_Rate_BlocksCA.csv"), stringsAsFactors = FALSE, colClasses = c("BLOCK_KEY"="character", "FIPS"="character"))
colnames(vbm)[1] <-"GEOID10"
head(vbm)
summary(vbm)

# merge block layers
blockdat <- 
  list(job, pop, poll, vbm) %>% 
  reduce(full_join)

# make a tract id -- the first 11 characters of the block GEOID
blockdat$GEOID <- substr(blockdat$GEOID10, 1, 11)

head(blockdat) # review

# join tract-based data
tractdat  <-  full_join(dat, elig)

# join block data to tract data
dat <- full_join(tractdat, blockdat)
head(dat)

table(dat$FIPS, useNA = "always")
summary(dat)

# check on NAs
check = subset(dat, is.na(Latino.prc))
table(check$FIPS) # most NAs have no fips code attached--likely legacy tract ids merged in from 2010 blocks or SWDB conversion files
summary(check) # note the tot pop (pop2010) is pretty low, prob na reason

## remove NA rows that aren't real blocks/tracts
dat <- dat[!is.na(dat$Latino.prc) & !is.na(dat$CarAccess.prc) & !is.na(dat$NAME) & !is.na(dat$pop2010), ]

summary(dat)


## investigate the NA blocks from the job
naz = subset(dat, is.na(jobShare))
summary(naz)
head(naz)


# change NAs to zero where the blocks don't have data--can't have missing informatino for the model
dat$jobShare[is.na(dat$jobShare)] <- 0
dat$pollShare[is.na(dat$pollShare)] <- 0
dat$TotVBM[is.na(dat$TotVBM)] <- 0
dat$AsnVBM[is.na(dat$AsnVBM)] <- 0
dat$LatVBM[is.na(dat$LatVBM)] <- 0
dat$YouthVBM[is.na(dat$YouthVBM)] <- 0

summary(dat)


##### Standardize the variables #####
# need to be standardized BY/within county

# split into list by county
dat_list <- split(dat, dat$FIPS, drop = FALSE)
head(dat_list[[11]])



# rescale from 0 to 1
# ACS Variables
dat_list <- lapply(dat_list, function(x) within(x, prc.latino.std <- (percent_rank(x$Latino.prc))))
dat_list <- lapply(dat_list, function(x) within(x, dens.cvap.std <- (percent_rank(x$cvapDens))))
dat_list <- lapply(dat_list, function(x) within(x, prc.youth.std <- (percent_rank(x$youth.prc))))
dat_list <- lapply(dat_list, function(x) within(x, prc.nonEngProf.std <- (percent_rank(x$LEP.prc))))
dat_list <- lapply(dat_list, function(x) within(x, prc.pov.std <- (percent_rank(x$BelowPoverty.prc))))
dat_list <- lapply(dat_list, function(x) within(x, prc.disabled.std <- (percent_rank(x$disab.prc))))
dat_list <- lapply(dat_list, function(x) within(x, prc.CarAccess.std <- 1 - (percent_rank(x$CarAccess.prc)))) # note this is the % of people who DO have access to a vehicle, should be inverted

# census variables
dat_list <- lapply(dat_list, function(x) within(x, dens.work.std <- (percent_rank(x$jobShare))))
dat_list <- lapply(dat_list, function(x) within(x, popDens.std <- (percent_rank(x$popDensKM2))))

# voting variables
# VBM rates need to be inverted, because we want to site VCs and DBs where there are not VBM voters
dat_list <- lapply(dat_list, function(x) within(x, rate.vbm.std <- 1 - (percent_rank(x$TotVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.hisvbm.std <- 1 - (percent_rank(x$LatVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.aisvbm.std <- 1 - (percent_rank(x$AsnVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.yousvbm.std <- 1 - (percent_rank(x$YouthVBM))))

# dat_list <- lapply(dat_list, function(x) within(x, prc.ElNonReg.std <- 1 - (percent_rank(x$Tot_EligNonReg_prc_FINAL))))
# dat_list <- lapply(dat_list, function(x) within(x, dens.poll.std <- 1 - (percent_rank(x$pollShare))))
dat_list <- lapply(dat_list, function(x) within(x, prc.ElNonReg.std <- (percent_rank(x$Tot_EligNonReg_prc_FINAL))))
dat_list <- lapply(dat_list, function(x) within(x, dens.poll.std <- (percent_rank(x$pollShare))))


# collapse list
dat <- do.call("rbind", dat_list)
table(dat$FIPS)
head(dat)


# Grab only variables used by the model (standardized indicators) and the reliability flags
dat <- 
dat %>% dplyr::select(GEOID10, GEOID, NAME, FIPS, popACS, State, pop2010, area_km2, popPrcCounty, 
                      prc.latino.std, dens.cvap.std , prc.youth.std,  prc.nonEngProf.std, prc.pov.std,
                      prc.disabled.std, prc.CarAccess.std, dens.work.std,  popDens.std, rate.vbm.std,
                      rate.hisvbm.std, rate.aisvbm.std, rate.yousvbm.std, prc.ElNonReg.std, dens.poll.std,
                      LEP_flag, CarAccess_flag, disab_flag, BelowPoverty_flag, youth_flag, Latino_flag, cvapDens_flag, ElNonReg_flag)

head(dat)

#### Export the standardized model data ####
# write.csv(dat, paste0(outputRoot, "model data/indicators_Stand_Final_CA.csv"), row.names = FALSE)
write.csv(dat, paste0(outputRoot, "model data/indicators_Stand_Final_CA_20200716.csv"), row.names = FALSE)

# #### ALSO NEED TO INCLUDE AND STANDARDIZE THE TRANSIT SCORE--BUT THAT WILL BE CALCULATED IN A SEPARATE PROCESS

