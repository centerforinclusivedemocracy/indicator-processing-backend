# Eligible Non-Registered Voter Estimates
# .	Data sources: Statewide Database voter registration data (2014 General Election, 2016 General Election); American Community Survey 5-Year Estimate CVAP (2013-2017); Census 2010. 
# .	Calculation: Convert voter data to the tract level and average voter registration totals for 2014 and 2016. 
# Subtract the average number of registered voters from the citizen voting age population (CVAP). 
# Divide by the total CVAP estimate in the tract. Where the incarcerated population is over 25% of the CVAP, 
# use the ACS 2017 5-yr estimate for non-institutionalized populations instead of CVAP. 
# NOTE: institutionalized population data was downloaded manually but could also be done through census api/tidycensus

library(dplyr)
library(data.table)
library(tidyr)
library(purrr)

############# SET UP DATA ############# 
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
outputRoot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"


#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS <- sprintf("%05d", siteCounties$FIPS)
site_ca <- siteCounties[siteCounties$State=="California",]

# read in CVAP data # using 2013-2017 to better match our election data years (2014-2016)
cvap <- fread(paste0(root, "data/acs/source/CVAP_2013-2017_ACS_csv_files/Tract.csv"), data.table = FALSE, colClasses = c("geoid"="character"))

# create county fips   
cvap$FIPS <- substr(cvap$geoid, 8, 12)
cvap$GEOID <- substr(cvap$geoid, 8, 18)
head(cvap)

# Grab tracts in the counties of interest
cvap <- cvap[(cvap$FIPS %in% site_ca$FIPS) & cvap$lntitle =="Total",] 


## Read in the incarcerated population data from the 2010 census (TRACTS)
instPop <- read.csv(paste0(root, "California Siting Tool/data/institutionalized_pop/DEC_10_SF1_P42_with_ann.csv"), header = TRUE, 
                    stringsAsFactors = FALSE, skip=1,colClasses = c(Id2='character'))

instPop <- instPop[,c(2,4,6)]
colnames(instPop) <- c("GEOID", "TotalGroupQuarters", "Incarc_Adults")

head(instPop)

### non institutionalized pop data 
totNonInstPop <- read.csv(paste0(root, "California Siting Tool/data/institutionalized_pop/ACS_17_5YR_S1810_with_ann.csv"), header = TRUE, 
                          stringsAsFactors = FALSE, skip=1, colClasses = c(Id2='character'))
totNonInstPop <- totNonInstPop[,c(2, 4:5)]
colnames(totNonInstPop) <- c("GEOID", "tract_TotNonInstPop", "tract_TotNonInstPop.MOE")
head(totNonInstPop)



## read in the SWDB registration data into a list 
reg <- list( 
gen14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_registration_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("fips"="character")),
gen16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_registration_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# standardize key field names -- sometimes SWDB data varies the number and order of columns and capitalization from year to year
#reg[[1]] <- reg[[1]][,-3]
dim(reg[[1]]); dim(reg[[2]]) # same number of columns

# convert all column names from gen14 to uppercase
names(reg[[1]])[1:143] <- toupper(names(reg[[1]][1:143]))

# remove the COUNTY field
reg <- lapply(reg, function(x) x %>% dplyr::select(-COUNTY))

# only need to keep the total registration numbers for the eligible non-reg voter pop
reg <- lapply(reg, function(x) x[,1:6])

# conversion list
conver <- list(
  gen14 = fread(paste0(root, "California Siting Tool/data/voter/conversion/state_g14_rg_blk_map.csv"), data.table = FALSE, colClasses = c("fips"="character", "block_key"="character")),
  gen16 = fread(paste0(root, "California Siting Tool/data/voter/conversion/state_g16_rg_blk_map.csv"), data.table = FALSE, colClasses = c("FIPS"="character", "BLOCK_KEY"="character")))

head(conver[[1]])
head(conver[[2]])

# standardize columns and col names between years
conver[[1]] <- conver[[1]][, -3] # remove county col

# convert all column names from gen14 to uppercase
names(conver[[1]])[1:13] <- toupper(names(conver[[1]][1:13]))


# extract only CA counties of interest
reg <- lapply(reg, function(x) x[x$FIPS %in% site_ca$FIPS, ])
conver <- lapply(conver, function(x) x[x$FIPS %in% site_ca$FIPS, ])

# review
head(conver[[1]])
table(reg[[2]]$FIPS)

# remove the TYPE col from the conversion list
conver <- lapply(conver, function(x) x[,-2])



###### Convert to Blocks ####
# collapse lists
reg <- do.call("rbind", reg)
conver <- do.call("rbind", conver)

# join conversion df to registration
regConver <- left_join(reg, conver)


#### Distribute PCTRGPREC across the precinct data ####

### first convert the percentage to a fraction
regConver$pctrgprecFRAC = regConver$PCTRGPREC/100


###  Multiply each precinct value by the proportion PCTRGPREC
# Reg data
regConverPRODUCT <- 
  regConver %>% 
  mutate(TOTREG_R = TOTREG_R*pctrgprecFRAC)


# review
head(regConverPRODUCT); head(regConver) # note how in the PRODUCT version the TOTREG_R field is now propotions of the block

### Sum records by census block  

# aggregate, group by election year, county, block, tract
regConverPRODUCT_agg <- 
  regConverPRODUCT %>%    
    dplyr::group_by(ELECTION, TYPE, FIPS,  BLOCK_KEY, TRACT, BLOCK) %>%    # group recoreds by census block
    dplyr::summarize(TOTREG_R = sum(TOTREG_R, na.rm=TRUE))    # sum all records for all variables (by block) 

head(regConverPRODUCT_agg)
dim(regConverPRODUCT_agg)


#### Aggregate to tract from Block

# Sum by block group id
# create a geoid for CVAP tract join
# Add a block group id.  Note that the first digit in the 4-digit block number indicates the block group number 
# (e.g. block 1019 belongs to block group 1)
regConverPRODUCT_agg$GEOID <- substr(regConverPRODUCT_agg$BLOCK_KEY, 1, 11)  # extract the first digit of the block id


### Sum records by tract to get the voter totals per tract
## We aggregate to the tract level in order to use with CVAP
tractReg <- 
  regConverPRODUCT_agg %>%
   dplyr::group_by(GEOID, ELECTION, TYPE, FIPS, TRACT) %>%   
    dplyr::summarize(TOTREG_R = sum(TOTREG_R, na.rm = T))

head(tractReg)
table(tractReg$FIPS, tractReg$ELECTION) # we want the number of tracts/precicnts to be similar year to year, but may not be exactly the same 


###### Calculate the Eligible Non-Registered Voter Rate ####

### Averge tract registration numbers
head(tractReg)

tractReg <- split(tractReg, tractReg$ELECTION, drop = FALSE)
tractReg <- lapply(tractReg, function(x) x[,-2])
colnames(tractReg[[1]])[5] <- "TOTREG_R_G14"
colnames(tractReg[[2]])[5] <- "TOTREG_R_G16"

# join and calculate mean of two registration years
tractRegAvg <-  
tractReg %>% reduce(full_join) %>%
  mutate(avgReg = rowMeans(cbind(TOTREG_R_G14, TOTREG_R_G16), na.rm=T))

head(tractRegAvg)

### Merge CVAP with the averaged tract registration file in order to calculate the number of people who are eligible to vote but are not registered
regCVAP <- full_join(tractRegAvg, cvap[,c(1, 7:10)])

head(regCVAP)

### calculate % of the eligible population that is eligible and non-registered 
regCVAP$Tot_EligNonReg_prc  <- (regCVAP$CVAP_EST - regCVAP$avgReg)/regCVAP$CVAP_EST  # CVAP Total - Total Registered (2014-2016 average) divided by CVAP total

# where cvap is zero, change the infinity (from the divide by zero) to NA
regCVAP$Tot_EligNonReg_prc <- ifelse(regCVAP$CVAP_EST==0, NA, regCVAP$Tot_EligNonReg_prc)


# negative values recode to NA
# but first flag as unreliable
regCVAP$TotElig_flag <- 0

regCVAP$TotElig_flag[regCVAP$Tot_EligNonReg_prc < 0 | is.na(regCVAP$Tot_EligNonReg_prc)] <- 1 

### Calculate sampling error for CVAP : CV calculation (coefficient of variation). We don't have MOE for the numerator, just calculate the standard CV
regCVAP$CV_Tot <- ((regCVAP$CVAP_MOE/1.645)/regCVAP$CVAP_EST)*100

# if the CV is over 40%, flag as unreliable
regCVAP$TotElig_flag[regCVAP$CV_Tot > 40] <- 1 

### Join the incarcerated & noninstitutionalized population to the CVAP data
regCVAP <- left_join(regCVAP, instPop)

regCVAP <- left_join(regCVAP, totNonInstPop)

head(regCVAP)

#### Calculate the percent of the tract CVAP that is the incarcerated adult population 
regCVAP$incarcPop_prc <- regCVAP$Incarc_Adults/regCVAP$CVAP_EST


### Create a 'final' column, where the % eligible is used EXCEPT if the value is negative or the CV is over 40% or the incarcerated population is > 25%
regCVAP$Tot_EligNonReg_prc_FINAL <- ifelse(regCVAP$incarcPop_prc > 0.25, 
                                                NA, regCVAP$Tot_EligNonReg_prc)

summary(regCVAP$Tot_EligNonReg_prc_FINAL)

# if the tot elig prc is less than 0 (i.e. more registered voters than estimated eligible voters), make 0 
regCVAP$Tot_EligNonReg_prc_FINAL <- ifelse(regCVAP$Tot_EligNonReg_prc < 0, 0, regCVAP$Tot_EligNonReg_prc)


# Now where the incarcerated population is greater than 25%, use the bg estimate for non institutionalized populations (as a replacement for cvap)
# note: we were using the same methodology as the UC Davis CRC index
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
summary(regCVAP$Tot_EligNonReg_prc_FINAL)
#



#### Export finished tract data files to output folder #####
write.csv(regCVAP[!is.na(regCVAP$GEOID) , c(1, 3, 19, 12)], paste0(outputRoot, "Elig_NonReg_Pop_TractsCA.csv"), row.names = FALSE)

# one for the visualization folder -- made a copy for the visualization folder just to keep things consistent, but they are the same (0 replaced NAs)
write.csv(regCVAP[!is.na(regCVAP$GEOID) ,c(1, 3, 19, 12)], paste0(outputRoot, "visualize/Elig_NonReg_Pop_TractsCA.csv"), row.names = FALSE)

