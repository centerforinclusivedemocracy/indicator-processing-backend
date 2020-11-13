# Registered Voter Count (block level)
# .	Data sources: Statewide Database voter registration data (2016 General Election)
# .	Calculation: Convert voter data to the block level for 2016, for use in cluster formation. 

library(dplyr)
library(data.table)
library(tidyr)
library(purrr)

############# SET UP DATA ############# 
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
outputRoot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"


###### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL ####
siteCounties <- read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE, colClasses = c("FIPS"="character"))
site_ca = siteCounties[siteCounties$State=="California",]

## read in the SWDB registration data 
reg <- fread(paste0(root, "California Siting Tool/data/voter/state_g16_registration_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character"))

#  remove county col
reg = reg[, !names(reg) %in% c("COUNTY") ] # remove county

# only need to keep the total registration numbers for the registrant pop
reg = reg[,1:6]

# extract only CA counties of interest
reg = reg[reg$FIPS %in% site_ca$FIPS, ]


# conversion list
conver = fread(paste0(root, "California Siting Tool/data/voter/conversion/state_g16_rg_blk_map.csv"), data.table = FALSE, colClasses = c("FIPS"="character", "BLOCK_KEY"="character"))

# extract vca counties nad remove TYPE column
conver = conver[conver$FIPS %in% site_ca$FIPS, -2 ]
head(conver)


###### Convert to Blocks ####

# join conversion df to registration
regConver = left_join(reg, conver)
head(regConver)

# review
head(subset(regConver, FIPS =="06009"))

## Distribute PCTRGPREC across the precinct data
### first convert the percentage to a fraction
regConver$pctrgprecFRAC = regConver$PCTRGPREC/100


###  Multiply each precinct value by the proportion PCTRGPREC
# Reg data
regConverPRODUCT <- 
  regConver %>% 
  mutate(TOTREG_R = TOTREG_R*pctrgprecFRAC)


# review
head(regConverPRODUCT); head(regConver) #

### Sum records by census block  

# aggregate
regConverPRODUCT_agg =
  regConverPRODUCT %>%    
    dplyr::group_by(ELECTION, TYPE, FIPS,  BLOCK_KEY, TRACT, BLOCK) %>%    # group recoreds by census block
    dplyr::summarize(TOTREG_R = sum(TOTREG_R, na.rm=TRUE))    # sum all records for all variables (by block) 

head(regConverPRODUCT_agg)
dim(regConverPRODUCT_agg)
table(regConverPRODUCT_agg$FIPS, regConverPRODUCT_agg$ELECTION)


# create a geoid for eventual? possible? tract join
# Add a block group id.  Note that the first digit in the 4-digit block number indicates the block group number 
# (e.g. block 1019 belongs to block group 1)
regConverPRODUCT_agg$GEOID = substr(regConverPRODUCT_agg$BLOCK_KEY, 1, 11)  # extract the first digit of the block id

### Sum records by block  to get the reg totals per block 
blockReg <-
  regConverPRODUCT_agg %>%
  dplyr::group_by(BLOCK_KEY, ELECTION, TYPE, FIPS, TRACT) %>%   
  dplyr::summarize(TOTREG_R = sum(TOTREG_R, na.rm = T)) %>% 
  as.data.frame()


###### Prep Registrant Pop per Block Data for export ####

head(blockReg)

blockReg <- 
blockReg %>% 
  select(BLOCK_KEY, FIPS, TOTREG_R) %>% 
  rename("GEOID" = BLOCK_KEY,
         "county"=FIPS,
         "R_totreg_r"=TOTREG_R) %>% 
  mutate(year = 2016) %>% 
  filter(!is.na(GEOID)) %>% 
  as.data.frame()

head(blockReg)
summary(blockReg)
table(blockReg$year)
# GEOID	county	R_totreg_r	year

#### Check against 2010 pop blocks ####
# just to review
library(foreign)

blocksCA <- read.dbf(paste0(root, "data/decennialblocksCA.dbf"))
head(blocksCA)

# blocks in each county with at least some population
table(subset(blocksCA, value > 0)$COUNTYFP10)

# blocks in each county with at least some reg data
table(subset(blockRegAvg, !is.na(R_totreg_r) )$county)


dim(subset(blocksCA, COUNTYFP10 =="019" & value >0))


###### Export finished block data files to output folder #####
write.csv(blockReg, paste0(outputRoot, "ca_Reg_2016.csv"), row.names = FALSE)


