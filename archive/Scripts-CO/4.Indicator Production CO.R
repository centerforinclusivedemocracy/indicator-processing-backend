# Combine indicators to make one single data input for the model

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

############# SET UP DATA ############# 
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
outputRoot = "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"


# the goal is to merge everything to blocks


############# COLORADO #########

# load data

# ACS
dat = read.csv(paste0(root, "data/output/ACS_Indicators_TractsCO.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character", "FIPS"="character"))

# # create reliability flag ## ALREADY DONE
# dat$nonEngProf_flag =ifelse(dat$LEP.CV > 40 | dat$LEP.CV=="Inf" | is.na(dat$LEP.CV) , 1, 0)
# dat$NoCar_flag =ifelse(dat$NoCar.CV > 40 | dat$NoCar.CV=="Inf" | is.na(dat$NoCar.CV) ,1, 0)
# dat$disab_flag =ifelse(dat$disab.CV > 40 | dat$disab.CV=="Inf" | is.na(dat$disab.CV) ,1, 0)
# dat$pov_flag =ifelse(dat$BelowPoverty.CV > 40 | dat$BelowPoverty.CV=="Inf" | is.na(dat$BelowPoverty.CV) ,1, 0)
# dat$youth_flag =ifelse(dat$youth.CV > 40 | dat$youth.CV=="Inf" | is.na(dat$youth.CV) ,1, 0)
# dat$AsianNHL_flag =ifelse(dat$AsianNHL.CV > 40 | dat$AsianNHL.CV=="Inf" | is.na(dat$AsianNHL.CV) ,1, 0)
# dat$BlackNHL_flag =ifelse(dat$BlackNHL.CV > 40 | dat$BlackNHL.CV=="Inf" | is.na(dat$BlackNHL.CV) ,1, 0)
# dat$Latino_flag =ifelse(dat$Latino.CV > 40 | dat$Latino.CV=="Inf" | is.na(dat$Latino.CV) ,1, 0)
# dat$NatAmNHL_flag =ifelse(dat$NatAmNHL.CV > 40 | dat$NatAmNHL.CV=="Inf" | is.na(dat$NatAmNHL.CV) ,1, 0)
# dat$WhiteNHL_flag =ifelse(dat$WhiteNHL.CV > 40 | dat$WhiteNHL.CV=="Inf" | is.na(dat$WhiteNHL.CV) ,1, 0)
# dat$cvap_flag =ifelse(dat$cvapDens.CV > 40 | dat$cvapDens.CV=="Inf" | is.na(dat$cvapDens.CV) ,1, 0)
# head(dat)


# # grab only percentages and the flags
# drops = c("LEP.CV" , "NoCar.CV", "disab.CV", "BelowPoverty.CV", "youth.CV", "AsianNHL.CV", "BlackNHL.CV", 
#           "Latino.CV", "NatAmNHL.CV", "WhiteNHL.CV", "cvapDens.CV")
# 
# dat = dat[, !(names(dat) %in% drops)]
head(dat)

# job
job = read.csv(paste0(root, "data/output/JobShare_Block_blocksCO.csv"), stringsAsFactors = FALSE, colClasses = c("w_geocode"="character", "w_FIPS"="character"))
colnames(job)[1:2] <- c("GEOID10", "FIPS")
job = job[,c(1,2,5)]
head(job)

# pop dens
pop = read.csv(paste0(root, "data/output/PopDensity_Block_blocksCO.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID10"="character", "TRACTCE10"="character", 
                                                                                                                   "COUNTYFP10"="character"))
pop = pop[,c(5, 2, 19, 20, 22, 23)]
colnames(pop)[2:3] <- c("FIPS", "pop2010")
pop$FIPS = paste0("08", pop$FIPS)
head(pop)


# elig
elig = read.csv(paste0(root, "data/output/Elig_NonReg_Pop_TractsCO.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character", "FIPS"="character"))
colnames(elig)[4] <- "ElNonReg_flag"
head(elig)
summary(elig)


# poll place data
poll = read.csv(paste0(root, "data/output/PollShare_BlocksCO.csv"), stringsAsFactors = FALSE, 
                colClasses = c("BLOCK_KEY"="character", "FIPS"="character"))
colnames(poll)[1] <-"GEOID10"
head(poll)
summary(poll)
subset(poll, is.na(GEOID10))


# # vbm rate   
vbm = read.csv(paste0(root, "data/output/VBM_Use_Rate_BlocksCO.csv"), stringsAsFactors = FALSE, 
               colClasses = c("BLOCK_KEY"="character", "FIPS"="character"))
colnames(vbm)[1] <-"GEOID10"
head(vbm)
summary(vbm)
subset(vbm, is.na(GEOID10))


# merge block layers
blockdat = list(job, pop, vbm, poll) %>% reduce(full_join) # poll, vbm   <<- these should be added in once the data exists

# make a tract id
blockdat$GEOID = substr(blockdat$GEOID10, 1, 11)

head(blockdat)
summary(blockdat)

# join tract-based data
tractdat = full_join(dat, elig)

dat = full_join(tractdat, blockdat)
head(dat)

table(dat$FIPS)
summary(dat)

# check on NAs
check = subset(dat, is.na(Latino.prc))
table(check$FIPS) 
summary(check)
head(check)

## remove NA rows that aren't real blocks/tracts
dim(dat)
dat = dat[!is.na(dat$Latino.prc) & !is.na(dat$CarAccess.prc) & !is.na(dat$NAME) , ]
dim(dat)
summary(dat)


# change NAs to zero where the blocks don't have data--can't have missing informatino for the model
dat$jobShare[is.na(dat$jobShare)] <- 0

### uncomment and run these lines if necessary once the voter data is ready
dat$pollShare[is.na(dat$pollShare)] <- 0
dat$TotVBM[is.na(dat$TotVBM)] <- 0
dat$AsnVBM[is.na(dat$AsnVBM)] <- 0
dat$LatVBM[is.na(dat$LatVBM)] <- 0
dat$YouthVBM[is.na(dat$YouthVBM)] <- 0

summary(dat)


##### Standardize the variables #####

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

# dat$prc.latino.std = percent_rank(dat$Latino.prc)
# dat$dens.cvap.std = percent_rank(dat$cvapDens)
# dat$prc.youth.std = percent_rank(dat$youth.prc)
# dat$prc.nonEngProf.std = percent_rank(dat$LEP.prc)
# dat$prc.pov.std = percent_rank(dat$BelowPoverty.prc)
# dat$prc.disabled.std = percent_rank(dat$disab.prc)
# dat$prc.CarAccess.std = 1 - percent_rank(dat$CarAccess.prc) # note this is the % of people who DO have access to a vehicle, should be inverted

# census variables
dat_list <- lapply(dat_list, function(x) within(x, dens.work.std <- (percent_rank(x$jobShare))))
dat_list <- lapply(dat_list, function(x) within(x, popDens.std <- (percent_rank(x$popDensKM2))))

# dat$dens.work.std = percent_rank(dat$jobShare)
# dat$popDens.std = percent_rank(dat$popDensKM2)

# voting variables
# VBM rates need to be inverted, because we want to site VCs and DBs where there are not VBM voters
dat_list <- lapply(dat_list, function(x) within(x, rate.vbm.std <- 1 - (percent_rank(x$TotVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.hisvbm.std <- 1 - (percent_rank(x$LatVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.aisvbm.std <- 1 - (percent_rank(x$AsnVBM))))
dat_list <- lapply(dat_list, function(x) within(x, rate.yousvbm.std <- 1 - (percent_rank(x$YouthVBM))))

# do not invert these 
dat_list <- lapply(dat_list, function(x) within(x, prc.ElNonReg.std <- (percent_rank(x$Tot_EligNonReg_prc_FINAL))))
dat_list <- lapply(dat_list, function(x) within(x, dens.poll.std <- (percent_rank(x$pollShare))))

# dat$rate.vbm.std = 1 - percent_rank(dat$TotVBM)
# dat$rate.hisvbm.std = 1 - percent_rank(dat$LatVBM)
# dat$rate.aisvbm.std = 1 - percent_rank(dat$AsnVBM)
# dat$rate.yousvbm.std = 1 - percent_rank(dat$YouthVBM)
# 
# dat$prc.ElNonReg.std = percent_rank(dat$Tot_EligNonReg_prc_FINAL)
# dat$dens.poll.std = percent_rank(dat$pollShare)

# collapse list
dat <- do.call("rbind", dat_list)
table(dat$FIPS)
head(dat)


# Grab only variables used by the model (standardized) and the reliability flags
dat <- 
  dat %>% dplyr::select(GEOID10, GEOID, NAME, FIPS, popACS, State, pop2010, area_km2, popPrcCounty, 
                        prc.latino.std, dens.cvap.std , prc.youth.std,  prc.nonEngProf.std, prc.pov.std,
                        prc.disabled.std, prc.CarAccess.std, dens.work.std,  popDens.std, rate.vbm.std,
                        rate.hisvbm.std, rate.aisvbm.std, rate.yousvbm.std, prc.ElNonReg.std, dens.poll.std,
                        LEP_flag, CarAccess_flag, disab_flag, BelowPoverty_flag, youth_flag, Latino_flag, cvapDens_flag, ElNonReg_flag)

head(dat)

# ##### OLDER CODE BELOW THIS LINE ####
# # rescale from 0 to 1
# 
# # ACS Variables
# dat$prc.latino.std = percent_rank(dat$Latino.prc)
# dat$dens.cvap.std = percent_rank(dat$cvapDens)
# dat$prc.youth.std = percent_rank(dat$youth.prc)
# dat$prc.nonEngProf.std = percent_rank(dat$LEP.prc)
# dat$prc.pov.std = percent_rank(dat$BelowPoverty.prc)
# dat$prc.disabled.std = percent_rank(dat$disab.prc)
# dat$prc.NoCarAccess.std = percent_rank(dat$NoCar.prc) # note this is the % of people who do NOT have access to a vehicle
# 
# # census variables
# dat$dens.work.std = percent_rank(dat$jobShare)
# dat$popDens.std = percent_rank(dat$popDensKM2)
# 
# # voting variables
# # # VBM rates need to be inverted, because we want to site VCs and DBs where there are not VBM voters
# # dat$rate.vbm.std = 1 - percent_rank(dat$TotVBM)
# # dat$rate.hisvbm.std = 1 - percent_rank(dat$AsnVBM)
# # dat$rate.aisvbm.std = 1 - percent_rank(dat$LatVBM)
# # dat$rate.yousvbm.std = 1 - percent_rank(dat$YouthVBM)
# 
# dat$prc.ElNonReg.std = percent_rank(dat$Tot_EligNonReg_prc_FINAL)
# # dat$dens.poll.std = percent_rank(dat$pollShare)
# 
# 
# # Grab only variables used by the model and the reliability flags
# head(dat)
# 
# finalcols = c("GEOID10", "GEOID", "NAME" , "FIPS", "popACS", "State", "pop2010", "prc.latino.std" ,   
# "dens.cvap.std","prc.youth.std","prc.nonEngProf.std" , "prc.pov.std","prc.disabled.std","prc.NoCarAccess.std", "dens.work.std","popDens.std",   
# "rate.vbm.std","rate.hisvbm.std","rate.aisvbm.std","rate.yousvbm.std","prc.ElNonReg.std","dens.poll.std","nonEngProf_flag","NoCar_flag",    
# "disab_flag","pov_flag","youth_flag","Latino_flag","cvap_flag", "ElNonReg_flag")
# 
# #### NOTE THIS IS STILL MISSING VBM AND POLLING PLACE SHARE VARIABLES ####
# dat = dat[, (names(dat) %in% finalcols)]
# head(dat)

#### Export the standardized model data ####
# write.csv(dat, paste0(outputRoot, "model data/indicators_Stand_Final_CO.csv"), row.names = FALSE)
write.csv(dat, paste0(outputRoot, "model data/indicators_Stand_Final_CO_20200716.csv"), row.names = FALSE)

#### ALSO NEED TO INCLUDE AND STANDARDIZE THE TRANSIT SCORE--BUT THAT WILL BE CALCULATED BY CONTRACTOR





