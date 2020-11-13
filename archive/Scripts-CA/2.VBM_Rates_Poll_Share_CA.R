# Calculate the VBM and Poll share rates 

library(foreign) # load libraries
library(rgdal)
library(plyr)
library(dplyr)
library(data.table)

######### 1. DATA PREP #####
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
outputRoot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE, colClasses = c("FIPS"="character"))
site_ca <- siteCounties[siteCounties$State=="California",]


### STATEWIDE DATABASE VOTER DATA 
# conversion files source: http://statewidedatabase.org/d10/g16_geo_conv.html
# http://statewidedatabase.org/d10/g14_geo_conv.html
# http://statewidedatabase.org/d10/g12_geo_conv.html

### Prepare the conversion file
# Read in the conversion files into a list.

# conversion list
conver <- list(
  gen14 = fread(paste0(root, "California Siting Tool/data/voter/conversion/state_g14_rg_blk_map.csv"), data.table = FALSE, colClasses = c("fips"="character", "block_key"="character")),
  gen16 = fread(paste0(root, "California Siting Tool/data/voter/conversion/state_g16_rg_blk_map.csv"), data.table = FALSE, colClasses = c("FIPS"="character", "BLOCK_KEY"="character")))

head(conver[[1]])
head(conver[[2]])

# standardize columns and col names between years -- remove county--doesn't exist in 2016
conver[[1]] <- conver[[1]][, -3]
#conver[[3]] <- conver[[3]][, -1] # remove county

# convert all column names from gen14 to uppercase
names(conver[[1]])[1:13] <- toupper(names(conver[[1]][1:13]))

# extract only VCA counties
conver <- lapply(conver, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("TYPE") ]) # remove Type
head(conver[[1]])


#### Import the precinct files 
# precinct source here: http://statewidedatabase.org/d10/g16.html

## read in the SWDB registration data 
### REGISTRATION DATA
reg <- list( 
  gen14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_registration_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("fips"="character")),
  gen16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_registration_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

names(reg[[1]])

# standardize key field names
reg[[1]] <- reg[[1]][,-3] # remove 'county'

# convert all column names from gen14 to uppercase
names(reg[[1]]) <- toupper(names(reg[[1]]))

# extract only CA counties of interest & remove county col
reg <- lapply(reg, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("COUNTY") ]) # remove county


### VOTER DATA
vote <- list( 
  vote14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_voters_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),
  vote16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_voters_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# extract only CA counties of interest & remove county col
vote = lapply(vote, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("COUNTY") ]) # remove Type


### MAIL BALLOT DATA
mail <- list( 
  mail14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_mailballot_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),
  mail16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_mailballot_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# extract only CA counties of interest & remove county col
mail <- lapply(mail, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("COUNTY") ]) # remove Type


### ABSENTEE VOTER DATA
abs <- list( 
  abs14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_absentees_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),
  abs16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_absentees_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# extract only CA counties of interest & remove county col
abs <- lapply(abs, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("COUNTY") ]) # remove Type


### POLLING PLACE VOTER DATA
poll <- list( 
  poll14 = fread(paste0(root, "California Siting Tool/data/voter/state_g14_poll_voters_by_g14_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),
  poll16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_poll_voters_by_g16_rgprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# extract only CA counties of interest & remove county col
poll <- lapply(poll, function(x) x[x$FIPS %in% site_ca$FIPS, !names(x) %in% c("COUNTY") ]) # remove Type


##### precinct data for vbm rates (precinct only that will NOT be converted to census. note this is a different dataset)
prec  <- 
  list(
# count of total voters 
    votePrec16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_voters_by_g16_rrprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),

# count of absentee voters
    absPrec16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_absentees_by_g16_rrprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")),

# count of mail ballot voters
    mailPrec16 = fread(paste0(root, "California Siting Tool/data/voter/state_g16_mailballot_by_g16_rrprec.csv"), data.table = FALSE, colClasses = c("FIPS"="character")))

# keep only VCA counties
prec <- lapply(prec, function(x) x[x$FIPS %in% site_ca$FIPS, ])
#

######### 2. Create VBM DF ####

### Prepare VBM precinct data list
# vbm file is an aggregate of absentee+mail ballot. Create an aggregated VBM list. Note that all years have the same number of columns
identical(abs[[2]][['RGPREC_KEY']], mail[[2]][['RGPREC_KEY']])  #  verify that precinct names match
identical(abs[[1]][['RGPREC_KEY']], mail[[1]][['RGPREC_KEY']])

# save precinct numbers to a list
pctNum <- lapply(mail, function(x) x[,c("RGPREC_KEY")])  # save just the precinct numbers 

# create matrices for mail and absentee counts
mail_mat <- lapply(mail, function(x) x[,c(6:142)]) # mail : keep only values
mail_mat <- lapply(mail_mat, function(x) sapply(x, as.numeric))

abs_mat <- lapply(abs, function(x) x[,c(6:142)])   # absentee
abs_mat <- lapply(abs_mat, function(x) sapply(x, as.numeric))

# sum the matrices to get the total vbm, then cbind the precinct number
vbm_mat <- mapply("+", mail_mat, abs_mat) # verified that the addition worked.

vbm <- lapply(vbm_mat, function(x) as.data.frame(x))  # convert to data frame

vbm <- mapply(cbind, vbm, "RGPREC_KEY"=pctNum, SIMPLIFY = FALSE)  # cbind precinct ids # verified that columns/records were maintained
head(vbm[[2]])  # total number of vbm

# convert to character
vbm <- lapply(vbm, function(x) within(x, RGPREC_KEY <- as.character(x$RGPREC_KEY)))

#### Create the vbm total (people who voted by mail) for the precint level data
# Aggregate mail + absentee
identical(prec[[2]]$RRPREC_KEY, prec[[3]]$RRPREC_KEY)

pctNum <- prec[[3]][,c(2, 3, 6)]  # save just the precinct numbers 

prec_mail_mat <- prec[[3]][, c(7:143)]
  #lapply(prec_mail, function(x) x[,c(4:140)]) # mail : keep only values

prec_mail_mat <- sapply(prec_mail_mat, as.numeric)
  #lapply(prec_mail_mat, function(x) sapply(x, as.numeric))

prec_abs_mat <- prec[[2]][,c(7:143)]   # absentee
prec_abs_mat <- sapply(prec_abs_mat, as.numeric)

# sum the matrices to get the total vbm, then cbind the precinct number
prec_vbm = cbind(pctNum, prec_mail_mat + prec_abs_mat) # verified that the addition worked.


head(prec_vbm) # total number of people who voted by mail



### identify each precinct file type with a prefix ###
### REGISTRATION
reg = lapply(reg, function(x) {           # id with R for registration
  colnames(x)[6:142] <- paste("R", colnames(x)[6:142], sep = "_") 
  return(x)})

head(reg[[2]]) # check

### VOTER
vote = lapply(vote, function(x) {       # id with V for voter
  colnames(x)[6:142] <- paste("V", colnames(x)[6:142], sep = "_")
  return(x)})

head(vote[[1]]) # check

### VBM
vbm = lapply(vbm, function(x) {           # id with M for vote by mail
  colnames(x)[1:137] <- paste("M", colnames(x)[1:137], sep = "_")
  return(x)})

head(vbm[[1]]) # check


### Polling
poll = lapply(poll, function(x) {           # id with M for vote by mail
  colnames(x)[6:142] <- paste("P", colnames(x)[6:142], sep = "_")
  return(x)})

head(poll[[2]]) # check


######### 3. CONVERSION PROCESS ##### 
head(conver[[1]])
head(reg[[1]][,1:7])

conver_reg = map2_df(conver, reg, inner_join)
head(conver_reg)
table(conver_reg$ELECTION, conver_reg$FIPS, useNA = "always")

conver_vote = map2_df(conver, vote, inner_join)
head(conver_vote)
table(conver_vote$ELECTION, conver_vote$FIPS)


conver_vbm = map2_df(conver, vbm, inner_join)
head(conver_vbm)
table(conver_vbm$ELECTION, conver_vbm$FIPS, useNA = "always")


conver_poll = map2_df(conver, poll, inner_join)
head(conver_poll)
table(conver_poll$ELECTION, conver_poll$FIPS, useNA = "always")


####### Distribute PCTRGPREC across the precinct data

### first convert the percentage to a fraction
conver_reg$pctrgprecFRAC = conver_reg$PCTRGPREC/100 # registration
conver_vote$pctrgprecFRAC = conver_vote$PCTRGPREC/100
conver_vbm$pctrgprecFRAC = conver_vbm$PCTRGPREC/100
conver_poll$pctrgprecFRAC = conver_poll$PCTRGPREC/100



###  Multiply each precinct value by the proportion PCTRGPREC

# Reg data
conver_regPRODUCT <- 
  conver_reg %>% dplyr::mutate_at(.funs=funs(. *pctrgprecFRAC), .vars=vars(contains("R_")))  # multiply where the prefix "R_" is seen

head(conver_regPRODUCT[,c(1:20, 151)]); head(conver_reg[,c(1:20, 151)]) # preview the first 20 columns and compare before and after the distribution of PCTRGPREC

# Voter data
conver_voterPRODUCT <- 
  conver_vote %>% dplyr::mutate_at(.funs=funs(. *pctrgprecFRAC), .vars=vars(contains("V_")))  # multiply where the prefix "V_" is seen


# vbm data
conver_vbmPRODUCT <- 
  conver_vbm %>% dplyr::mutate_at(.funs=funs(. *pctrgprecFRAC), .vars=vars(contains("M_")))  # multiply where the prefix "R_" is seen


# Poll data
conver_pollPRODUCT <- 
  conver_poll %>% dplyr::mutate_at(.funs=funs(. *pctrgprecFRAC), .vars=vars(contains("P_")))  # multiply where the prefix "R_" is seen

head(conver_pollPRODUCT[,c(1:20, 151)]); head(conver_poll[,c(1:20, 151)]) # preview the first 20 columns and compare before and after the distribution of PCTRGPREC


######### 4. Sum records by census block  #####

# aggregate # keep only the numeric columns and the county, year & block id, and don't keep the proprotion because it doesn't make sense to add those values
conver_regPRODUCT_agg =
  conver_regPRODUCT %>%    
    dplyr::select(-TYPE,  -RGPREC_KEY, -RGPREC, -pctrgprecFRAC, -PCTRGPREC, -PCTBLK, -RGTOTREG) %>%
    dplyr::group_by(BLOCK_KEY, FIPS, ELECTION, TRACT, BLOCK) %>%    # group recoreds by census block
    dplyr::summarise_all(sum, na.rm=TRUE) %>%  # sum all records for all variables (by block) 
    as.data.frame()
head(conver_regPRODUCT_agg)

conver_voterPRODUCT_agg =
  conver_voterPRODUCT %>%    
  dplyr::select(-TYPE,  -RGPREC_KEY, -RGPREC, -pctrgprecFRAC, -PCTRGPREC, -PCTBLK, -RGTOTREG) %>%
  dplyr::group_by(BLOCK_KEY, FIPS, ELECTION, TRACT, BLOCK) %>%    # group recoreds by census block
  dplyr::summarise_all(sum, na.rm=TRUE) %>%  # sum all records for all variables (by block) 
  as.data.frame()
head(conver_voterPRODUCT_agg)


conver_vbmPRODUCT_agg =
  conver_vbmPRODUCT %>%    
  dplyr::select(-RGPREC_KEY, -RGPREC, -pctrgprecFRAC, -PCTRGPREC, -PCTBLK, -RGTOTREG) %>%
  dplyr::group_by(BLOCK_KEY, FIPS, ELECTION, TRACT, BLOCK) %>%    # group recoreds by census block
  dplyr::summarise_all(sum, na.rm=TRUE) %>%  # sum all records for all variables (by block) 
  as.data.frame()

conver_pollPRODUCT_agg =
  conver_pollPRODUCT %>%    
  dplyr::select(-TYPE,  -RGPREC_KEY, -RGPREC, -pctrgprecFRAC, -PCTRGPREC, -PCTBLK, -RGTOTREG) %>%
  dplyr::group_by(BLOCK_KEY, FIPS, ELECTION, TRACT, BLOCK) %>%    # group recoreds by census block
  dplyr::summarise_all(sum, na.rm=TRUE) %>%  # sum all records for all variables (by block) 
  as.data.frame()

head(conver_pollPRODUCT_agg)


### Calculate the total Hispanic vbm, vote total, and  registration
conver_vbmPRODUCT_agg$M_TotHisp <- apply(conver_vbmPRODUCT_agg[,c(21:24)], 1, sum)  # blocks
conver_regPRODUCT_agg$R_TotHisp <- apply(conver_regPRODUCT_agg[,c(21:24)], 1, sum)
conver_voterPRODUCT_agg$V_TotHisp <- apply(conver_voterPRODUCT_agg[,c(21:24)], 1, sum)
conver_pollPRODUCT_agg$P_TotHisp <- apply(conver_pollPRODUCT_agg[,c(21:24)], 1, sum)


### Calculate the total Asian vbm and reg 
conver_vbmPRODUCT_agg$M_TotAsian <- apply(conver_vbmPRODUCT_agg[,c(29:52)], 1, sum)  # blocks
conver_regPRODUCT_agg$R_TotAsian <- apply(conver_regPRODUCT_agg[,c(29:52)], 1, sum)
conver_voterPRODUCT_agg$V_TotAsian <- apply(conver_voterPRODUCT_agg[,c(29:52)], 1, sum)
conver_pollPRODUCT_agg$P_TotAsian <- apply(conver_pollPRODUCT_agg[,c(29:52)], 1, sum)


### Calculate the total youth vbm and reg 
conver_vbmPRODUCT_agg$M_TotYouth <- apply(conver_vbmPRODUCT_agg[,c(54, 61, 68, 75, 82, 89, 96, 103)], 1, sum)  # blocks
conver_regPRODUCT_agg$R_TotYouth <- apply(conver_regPRODUCT_agg[,c(54, 61, 68, 75, 82, 89, 96, 103)], 1, sum)
conver_voterPRODUCT_agg$V_TotYouth <- apply(conver_voterPRODUCT_agg[,c(54, 61, 68, 75, 82, 89, 96, 103)], 1, sum)
conver_pollPRODUCT_agg$P_TotYouth <- apply(conver_pollPRODUCT_agg[,c(54, 61, 68, 75, 82, 89, 96, 103)], 1, sum)


## keep only relevant fields
# 
conver_vbmPRODUCT_agg = conver_vbmPRODUCT_agg[,c(1:8, 145:147)]
conver_regPRODUCT_agg = conver_regPRODUCT_agg[,c(1:8, 145:147)]
conver_voterPRODUCT_agg = conver_voterPRODUCT_agg[,c(1:8, 145:147)]
conver_pollPRODUCT_agg = conver_pollPRODUCT_agg[,c(1:8, 145:147)]
head(conver_regPRODUCT_agg)
head(conver_pollPRODUCT_agg)


#### Calculate VBM rates for Youth, Asian Am, Latino, and Total ####
vbmrate <- full_join(conver_vbmPRODUCT_agg, conver_voterPRODUCT_agg)


vbmrate <- 
vbmrate %>%
  dplyr::filter( ELECTION=="g16" ) %>% # only need 2016 for this
  dplyr::group_by(BLOCK_KEY, FIPS, ELECTION, TRACT, BLOCK) %>%
  dplyr::mutate(
    TotVBM = M_TOTREG_R/V_TOTREG_R,
    AsnVBM = M_TotAsian/V_TotAsian,
    LatVBM = M_TotHisp/V_TotHisp,
    YouthVBM = M_TotYouth/V_TotYouth
  ) %>%
  as.data.frame()

head(vbmrate)
table(vbmrate$FIPS, useNA = "always")

# check on NAs
naz =subset(vbmrate, is.na(AsnVBM))

head(naz)
table(naz$FIPS)

# make NAs 0
vbmrate$TotVBM[is.na(vbmrate$TotVBM) & vbmrate$V_TOTREG_R==0] <- 0
vbmrate$AsnVBM[is.na(vbmrate$AsnVBM) & vbmrate$V_TotAsian==0] <- 0
vbmrate$LatVBM[is.na(vbmrate$LatVBM) & vbmrate$V_TotHisp==0] <- 0
vbmrate$YouthVBM[is.na(vbmrate$YouthVBM) & vbmrate$V_TotYouth==0] <- 0

# check
summary(vbmrate)


## Calculate polling place share
pollShareTot =
  conver_pollPRODUCT_agg %>% 
  dplyr::filter(ELECTION =="g16") %>%
  dplyr::group_by(FIPS, ELECTION) %>%
  dplyr::summarize(pollCountyTot = sum(P_TOTREG_R, na.rm=T))
head(pollShareTot)

pollShare =
 conver_pollPRODUCT_agg %>%
  dplyr::filter(ELECTION =="g16") %>%
   dplyr::full_join(pollShareTot) %>%
   dplyr::mutate(
     pollShare = P_TOTREG_R/pollCountyTot
   ) %>%
  as.data.frame()
head(pollShare)

summary(pollShare)


######### 5. Export block-level data for model ######
write.csv(vbmrate[,c(1, 2, 16:19)], paste0(outputRoot, "VBM_Use_Rate_BlocksCA.csv"), row.names = FALSE)

write.csv(pollShare[,c(1, 2, 13)], paste0(outputRoot, "PollShare_BlocksCA.csv"), row.names = FALSE)



######### 6. Prepare tract-level data for visualization ####
conver_voterPRODUCT_agg$GEOID = substr(conver_voterPRODUCT_agg$BLOCK_KEY, 1, 11)
conver_vbmPRODUCT_agg$GEOID = substr(conver_vbmPRODUCT_agg$BLOCK_KEY, 1, 11)
conver_pollPRODUCT_agg$GEOID = substr(conver_pollPRODUCT_agg$BLOCK_KEY, 1, 11)


#### Calculate VBM rates for Youth, Asian Am, Latino, and Total #
votrTract = 
  conver_voterPRODUCT_agg %>%
  dplyr::filter(ELECTION=="g16") %>%
  dplyr::group_by(GEOID, FIPS, ELECTION, TRACT) %>%
  dplyr::summarize(
    V_TOTREG_R = sum(V_TOTREG_R, na.rm=T),
    V_TotHisp = sum(V_TotHisp, na.rm=T),
    V_TotAsian = sum(V_TotAsian, na.rm=T),
    V_TotYouth = sum(V_TotYouth, na.rm=T)
  )

head(votrTract)

vbmTract = 
  conver_vbmPRODUCT_agg %>%
  dplyr::filter(ELECTION=="g16") %>%
  dplyr::group_by(GEOID, FIPS, ELECTION, TRACT) %>%
  dplyr::summarize(
    M_TOTREG_R = sum(M_TOTREG_R, na.rm=T),
    M_TotHisp = sum(M_TotHisp, na.rm=T),
    M_TotAsian = sum(M_TotAsian, na.rm=T),
    M_TotYouth = sum(M_TotYouth, na.rm=T)
  )
head(vbmTract)

vbmrateTract = full_join(vbmTract, votrTract)
head(vbmrateTract)
table(vbmrateTract$FIPS)

vbmrateTract =
  vbmrateTract %>%
  dplyr::group_by(GEOID, FIPS, ELECTION, TRACT) %>%
  dplyr::mutate(
    TotVBM = M_TOTREG_R/V_TOTREG_R,
    AsnVBM = M_TotAsian/V_TotAsian,
    LatVBM = M_TotHisp/V_TotHisp,
    YouthVBM = M_TotYouth/V_TotYouth
  ) %>%
  as.data.frame()

head(vbmrateTract)
table(vbmrateTract$FIPS)
summary(vbmrateTract)

naz = subset(vbmrateTract, is.na(AsnVBM))

# make NAs 0
vbmrateTract$TotVBM[is.na(vbmrateTract$TotVBM) & vbmrateTract$V_TOTREG_R==0] <- 0
vbmrateTract$AsnVBM[is.na(vbmrateTract$AsnVBM) & vbmrateTract$V_TotAsian==0] <- 0
vbmrateTract$LatVBM[is.na(vbmrateTract$LatVBM) & vbmrateTract$V_TotHisp==0] <- 0
vbmrateTract$YouthVBM[is.na(vbmrateTract$YouthVBM) & vbmrateTract$V_TotYouth==0] <- 0

summary(vbmrateTract)

## Calculate polling place share
pollTract = 
  conver_pollPRODUCT_agg %>%
  dplyr::filter(ELECTION=="g16") %>%
  dplyr::group_by(GEOID, FIPS, ELECTION, TRACT) %>%
  dplyr::summarize(
    P_TOTREG_R = sum(P_TOTREG_R, na.rm=T),
    P_TotHisp = sum(P_TotHisp, na.rm=T),
    P_TotAsian = sum(P_TotAsian, na.rm=T),
    P_TotYouth = sum(P_TotYouth, na.rm=T)
  )
head(pollTract)

# calc share
pollShareTotTract =
  pollTract %>% 
  dplyr::group_by(FIPS, ELECTION) %>%
  dplyr::summarize(pollCountyTot = sum(P_TOTREG_R, na.rm=T))
head(pollShareTot)

pollShareTract =
  pollTract %>%
  dplyr::full_join(pollShareTotTract) %>%
  dplyr::mutate(
    pollShare = P_TOTREG_R/pollCountyTot
  ) %>%
  as.data.frame()
head(pollShareTract)


summary(pollShareTract)


######### 7. Export Tract data for visualization ######
write.csv(vbmrateTract[,c(1, 2, 13:16)], paste0(outputRoot, "visualize/VBM_Use_Rate_Tracts_PrecinctsCA.csv"), row.names = FALSE)

write.csv(pollShareTract[,c(1, 2, 10)], paste0(outputRoot, "visualize/PollShare_PrecinctsTractsCA.csv"), row.names = FALSE)
