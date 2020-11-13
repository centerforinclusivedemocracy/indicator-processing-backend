# Combine indicators to make one single data input for the model - For all states
# the goal is to merge everything to blocks for the block score/weighted average
library(data.table)
library(dplyr)
library(tidyverse)
library(foreign)
library(purrr)

############# SET UP DATA ############# 
# define data path (Save to object)
root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/"
outputRoot <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Output"

# list all files in the directory
indicatorFiles <- list.files(path = "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Output", full.names=TRUE) 

# only run az, tx, ga, and mi for now because only those are ready - update as needed
indicatorFiles <- indicatorFiles[(indicatorFiles %like% "AZ") | 
                                   (indicatorFiles %like% "TX") |
                                   (indicatorFiles %like% "GA") |
                                   (indicatorFiles %like% "MI") |
                                   (indicatorFiles %like% "FL") |
                                   (indicatorFiles %like% "NC") |
                                   (indicatorFiles %like% "PA") |
                                   (indicatorFiles %like% "WI")]


###### ACS 
# note these were updated with 2018 data in spring 2020
acsfiles <- indicatorFiles[indicatorFiles %like% "ACS_Indicators" ]
acsfiles

# load acs data for all states
dat <- 
  lapply(acsfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE, colClasses = c("GEOID"="character", "FIPS"="character"), data.table = FALSE)
  }) 

# review.  reliability flag should be included
head(dat[[1]])


###### job - load job share data by block
jobfiles <- indicatorFiles[indicatorFiles %like% "JobShare_Block" ]

job <- 
  lapply(jobfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE, colClasses = c("w_geocode"="character", "w_FIPS"="character"), data.table = FALSE)
  }) 

# select certain columns
job <- lapply(job, function(x) x[,c(1, 2, 5)])

# rename columns
colnamesjobs <- c("GEOID10", "FIPS", "jobShare")
job <- lapply(job, setnames, colnamesjobs)

head(job[[1]])

###### pop dens - load population density at the block level. 
popfiles <- indicatorFiles[indicatorFiles %like% "PopDensity_Block" ]
popfiles

# load data
pop <- 
  lapply(popfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE, 
          colClasses = c("BLOCKID10"="character", "TRACTCE10"="character", "COUNTYFP10"="character", "STATEFP10"="character"), data.table = FALSE)
  })

# format, limit columns
pop <- lapply(pop, function(x) x %>% 
                dplyr::mutate(FIPS = paste0(STATEFP10, COUNTYFP10),
                              pop2010 = value) %>% 
                dplyr::select(BLOCKID10, FIPS, pop2010, area_km2, popDensKM2, popPrcCounty) %>%
                rename("GEOID10" = BLOCKID10)) 

head(pop[[3]])        

###### elig 
eligfiles  <- indicatorFiles[indicatorFiles %like% "Elig_NonReg_Pop" ]
eligfiles

# load eligible non-reg population
elig <- 
  lapply(eligfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE, 
          colClasses = c("GEOID"="character", "COUNTYFP"="character", "STATEFP"="character"), data.table = FALSE)
  })
head(elig[[1]])

# rename col 
elig <- lapply(elig, function(x) x %>% 
                 dplyr::select(GEOID,  COUNTYFP, STATEFP, Tot_EligNonReg_prc_FINAL, TotElig_flag) %>% 
                 rename( "ElNonReg_flag" = TotElig_flag) %>%
                 rename( "Tot_EligNonReg_prc" = Tot_EligNonReg_prc_FINAL))

summary(elig[[4]]) # look for NAs 


###### poll  
pollfiles <- indicatorFiles[indicatorFiles %like% "PollShare_" ]
pollfiles

# load polling place share by block
poll <- 
  lapply(pollfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE,
          colClasses = c("BLOCK_KEY"="character", "FIPS"="character"), data.table = FALSE)
  })

# standardize names
pollnames <- c("GEOID10",  "FIPS", "pollShare", "STATE")
poll <- lapply(poll, setnames, pollnames)

head(poll[[3]])

###### vbm rate 
vbmfiles <- indicatorFiles[indicatorFiles %like% "VBM_Use_Rate_" ]
vbmfiles

# load VBM use rate by block
vbm <- 
  lapply(vbmfiles, function(i){
    fread(i, header=TRUE, stringsAsFactors = FALSE,
          colClasses = c("BLOCK_KEY" = "character", "FIPS"="character"), data.table = FALSE)
  })

# standardize names
vbm <- lapply(vbm, function(x) x %>% 
                    dplyr::select(BLOCK_KEY, FIPS, STATE, TotVBM, LatVBM, AsnVBM, YouthVBM))
vbmnames <- c("GEOID10", "FIPS", "STATE", "TotVBM", "LatVBM", "AsnVBM", "YouthVBM")
vbm <- lapply(vbm, setnames, vbmnames)

head(vbm[[1]])



# merge block layers
blockdat <- 
  list(do.call("rbind", job), 
       do.call("rbind", pop), 
       do.call("rbind", poll), 
       do.call("rbind", vbm)) %>% 
  reduce(full_join)

# make a tract id -- the first 11 characters of the block GEOID
blockdat$GEOID <- substr(blockdat$GEOID10, 1, 11)

head(blockdat) # review

# join tract-based data
# ACS and eligible non-registered voters
tractdat  <-  full_join(do.call("rbind", dat), 
                        do.call("rbind", elig))

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


# change NAs to zero where the blocks don't have data--can't have missing information for the model
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

# these should not be inverted
dat_list <- lapply(dat_list, function(x) within(x, prc.ElNonReg.std <- (percent_rank(x$Tot_EligNonReg_prc))))
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
table(dat$State)


#### Export the standardized model data ####
# split into separate dfs by state
dat_state_export <- split(dat, dat$State, drop = FALSE)

# loop through export
for (i in seq_along(dat_state_export)) {
  filename = paste("indicators_Stand_Final_", names(dat_state_export)[i], ".csv", sep = "")
  write.csv(dat_state_export[[i]], paste0(root, "Indicator_Output/model_data/test/", filename), row.names = FALSE)
}

### done
