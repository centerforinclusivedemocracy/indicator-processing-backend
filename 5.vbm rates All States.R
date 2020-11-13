# Voter Data: VBM Rates (total, youth, latino and asian)

library(data.table)
library(dplyr)
options(scipen=999)

# add function
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# root of wd
root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/"


# load Voter data
allCata <- fread(paste0(root, "Indicator_Output/Reg_2016_ALLSTATES.csv"), data.table=FALSE)

head(allCata)

# check to see what states we have ready
table(allCata$STATE) 

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "/Indicator_output/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE,
                         colClasses = c("FIPS"="character"))

# filter to the states that are ready from allCata - update this as needed
site_filtered = siteCounties[(siteCounties$State=="Arizona") |
                               (siteCounties$State=="Texas") |
                               (siteCounties$State=="Georgia") |
                               (siteCounties$State=="Michigan") |
                               (siteCounties$State=="Florida") |
                               (siteCounties$State=="North Carolina") |
                               (siteCounties$State=="Pennsylvania") |
                               (siteCounties$State=="Wisconsin"), ]

# verify the right states and number of counties are filtered per allCata
table(site_filtered$State)

site_filtered$COUNTY = toupper(site_filtered$CountyName)
head(site_filtered)


###### Convert Precincts to Blocks #####
## Read in block-precinct conversion file 

# load conversion/crosswalk files - make sure to update this as more states are run (check states in allCata)
az_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/az_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
ga_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/ga_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
mi_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/mi_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
tx_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/tx_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID"="character", "BLOCKID10"="character"), data.table = FALSE)
fl_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/fl_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
nc_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/nc_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
wi_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/wi_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)
pa_intr <- fread(paste0(root, "Indicator_Inputs/crosswalks/pa_intersect_precinct_crosswalk.csv"), 
                 colClasses=c("BLOCKID10"="character"), data.table = FALSE)

head(ga_intr)
head(fl_intr)
head(mi_intr)
head(nc_intr)
head(wi_intr)
head(pa_intr)
## create a master dataframe of all the crosswalk files - add in new states as they are run
# grab: state fips, county fips, tractce, blockce, blockID, partfl, housing, pop, 
# area_m2, blockfull_area, prec_join, precfull_area, intrsct_area, prc_intrsect_area (in that order!)
az_intr <- az_intr[,c(1:10,28:31)]
colnames(az_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

ga_intr <- ga_intr[,c(1:10,25:28)]
colnames(ga_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

tx_intr <- tx_intr[,c(1:4,34,6:9,11,31,30,32,33)]
colnames(tx_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

mi_intr <- mi_intr[,c(1:10,22:25)]
colnames(mi_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

fl_intr <- fl_intr[,c(1:10,30:33)]
colnames(fl_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

nc_intr <- nc_intr[,c(1:10,65:68)]
colnames(nc_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

wi_intr <- wi_intr[,c(1:10,64:67)]
colnames(wi_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")

pa_intr <- pa_intr[,c(1:10,27:30)]
colnames(pa_intr) <- c("STATEFP","COUNTYFP","TRACTCE","BLOCKCE","BLOCKID","PARTFLG",
                       "HOUSING10","POP10","area_m2","BlockFull_area","PREC_JOIN",
                       "PrecFull_area","Intrsct_area","prc_Intrsct_area")
# merge all dataframes together
all_intr <- rbind(az_intr, tx_intr, ga_intr, mi_intr, fl_intr, nc_intr, wi_intr, pa_intr)
View(all_intr)


### multiply proportion by registered voters
# first join reg voters
intrReg = merge(all_intr, allCata, by="PREC_JOIN", all=TRUE)

head(intrReg)
dim(intrReg)

# Check for NAs # GRC question: we actually have a lot of NAs here - 71 blockID 
# and 18064 total_reg_voters (1.5% of records) - why is that?
subset(intrReg, is.na(BLOCKID))
subset(intrReg, is.na(TOTAL_REGISTERED_VOTERS))


# multiply to get proportional registration for all variables
intrReg <- 
  intrReg %>% 
  mutate(propRegTot = prc_Intrsct_area * TOTAL_2016_VOTERS,
         propRegLat = prc_Intrsct_area * LATINO_2016_VOTERS, 
         propRegAsn = prc_Intrsct_area * ASIAN_2016_VOTERS,
         propRegYth = prc_Intrsct_area * YOUTH_2016_VOTERS,
         
         propMailTot = prc_Intrsct_area * VBM_2016_GENERAL,
         propMailLat = prc_Intrsct_area * LATINO_VBM_2016_GENERAL, 
         propMailAsn = prc_Intrsct_area * ASIAN_VBM_2016_GENERAL, 
         propMailYth = prc_Intrsct_area * YOUTH_VBM_2016_GENERAL)

# summarize by block
blockReg <- 
  intrReg %>%
  dplyr::group_by(BLOCKID) %>%
  dplyr::summarize(regBlockTot = sum(propRegTot, na.rm=T),
                   regBlockLat = sum(propRegLat, na.rm=T), 
                   regBlockAsn = sum(propRegAsn, na.rm=T), 
                   regBlockYth = sum(propRegYth, na.rm=T), 
                   
                   mailBlockTot = sum(propMailTot, na.rm=T), 
                   mailBlockLat = sum(propMailLat, na.rm=T), 
                   mailBlockAsn = sum(propMailAsn, na.rm=T), 
                   mailBlockYth = sum(propMailYth, na.rm=T)) %>% as.data.frame()

head(blockReg)
summary(blockReg)

# now calculate the vbm rates by blocks
blockReg_Final <- 
  blockReg %>% 
  mutate(
    TotVBM = mailBlockTot/regBlockTot,
    LatVBM = mailBlockLat/regBlockLat,
    AsnVBM = mailBlockAsn/regBlockAsn, # should we incorporate some sort of threshold?
    YouthVBM = mailBlockYth/regBlockYth,
    STATEFP = substr(BLOCKID, 1, 2),
    COUNTYFP = substr(BLOCKID, 3, 5),
    COUNTYFP = paste0(STATEFP, COUNTYFP)) %>% 
  rename("BLOCK_KEY" = BLOCKID, 
         "FIPS" = COUNTYFP)

head(blockReg_Final)
summary(blockReg_Final)


blockReg_Final <- 
  blockReg_Final %>% 
  # replace NA with 0 when there are 0 registered voters
  mutate(TotVBM = ifelse(regBlockTot == 0, 0, TotVBM),
         LatVBM = ifelse(regBlockLat == 0, 0, LatVBM),
         AsnVBM = ifelse(regBlockAsn == 0, 0, AsnVBM),
         YouthVBM = ifelse(regBlockYth==0, 0, YouthVBM)) %>% 
  filter(!is.na(BLOCK_KEY)) %>% 
  
  # select only columns needed
  dplyr::select(BLOCK_KEY, STATEFP, FIPS, TotVBM, LatVBM, AsnVBM, YouthVBM) 


head(blockReg_Final)
summary(blockReg_Final)
subset(blockReg_Final, is.na(BLOCK_KEY))

# create STATE abbreviation field for export
blockReg_Final$STATE <-  ifelse(blockReg_Final$STATEFP == "04", "AZ", 
                                ifelse(blockReg_Final$STATEFP == "06", "CA",
                                       ifelse(blockReg_Final$STATEFP == "12", "FL",
                                              ifelse(blockReg_Final$STATEFP == "13", "GA",
                                                     ifelse(blockReg_Final$STATEFP == "26", "MI",
                                                            ifelse(blockReg_Final$STATEFP == "37", "NC",
                                                                   ifelse(blockReg_Final$STATEFP == "42", "PA",
                                                                          ifelse(blockReg_Final$STATEFP == "48", "TX",
                                                                                 ifelse(blockReg_Final$STATEFP == "55", "WI", "check")))))))))

blockReg_Final_export <- split(blockReg_Final, blockReg_Final$STATE, drop = FALSE)

# loop through export
for (i in seq_along(blockReg_Final_export)) {
  filename = paste("VBM_Use_Rate_Blocks", names(blockReg_Final_export)[i], ".csv", sep = "")
  write.csv(blockReg_Final_export[[i]], paste0(root, "Indicator_Output/", filename), row.names = FALSE)
}



##### Convert to Tract #####
## Create Tract ID
blockReg$GEOID = substr(blockReg$BLOCKID, 1, 11)  # extract the first digit of the block id

# create state fips
blockReg$STATEFP <- substr(blockReg$BLOCKID, 1, 2)

# create state abbr field
blockReg$STATE <-  ifelse(blockReg$STATEFP == "04", "AZ", 
                                ifelse(blockReg$STATEFP == "06", "CA",
                                       ifelse(blockReg$STATEFP == "12", "FL",
                                              ifelse(blockReg$STATEFP == "13", "GA",
                                                     ifelse(blockReg$STATEFP == "26", "MI",
                                                            ifelse(blockReg$STATEFP == "37", "NC",
                                                                   ifelse(blockReg$STATEFP == "42", "PA",
                                                                          ifelse(blockReg$STATEFP == "48", "TX",
                                                                                 ifelse(blockReg$STATEFP == "55", "WI", "check")))))))))

head(blockReg)

### Sum records by block group to get the voter totals per block group
tractVBM  <- 
  blockReg %>%
  dplyr::group_by(GEOID, STATEFP, STATE) %>%   
  dplyr::summarize(regBlockTot = sum(regBlockTot, na.rm=T),
                   regBlockLat = sum(regBlockLat, na.rm=T), 
                   regBlockAsn = sum(regBlockAsn, na.rm=T), 
                   regBlockYth = sum(regBlockYth, na.rm=T), 
                   
                   mailBlockTot = sum(mailBlockTot, na.rm=T), 
                   mailBlockLat = sum(mailBlockLat, na.rm=T), 
                   mailBlockAsn = sum(mailBlockAsn, na.rm=T), 
                   mailBlockYth = sum(mailBlockYth, na.rm=T)) %>% 
  as.data.frame() %>% 
  mutate(
    TotVBM = mailBlockTot/regBlockTot,
    LatVBM = mailBlockLat/regBlockLat,
    AsnVBM = mailBlockAsn/regBlockAsn,
    YouthVBM = mailBlockYth/regBlockYth)

head(tractVBM)
summary(tractVBM) 
view(tractVBM)

# final clean up
tractVBM$COUNTYFP <- substr(tractVBM$GEOID, 3, 5)
head(tractVBM)
tractVBM <- 
  tractVBM %>% 
  rename("FIPS"=COUNTYFP) %>% 
  mutate(FIPS = paste0(STATEFP, FIPS)) %>% 
  dplyr::select(GEOID, FIPS, STATE, TotVBM, LatVBM, AsnVBM, YouthVBM)#%>% 
  #filter(!is.na(TotVBM) & !is.na(LatVBM) & !is.na(AsnVBM) & !is.na(YouthVBM))


summary(tractVBM)
head(tractVBM)
subset(tractVBM, is.na(GEOID))
subset(tractVBM, is.na(FIPS))
view(tractVBM)
## Export to tract for visuals
tractVBM_export <- split(tractVBM, tractVBM$STATE, drop = FALSE)

# loop through export
for (i in seq_along(tractVBM_export)) {
  filename = paste("VBM_Use_Rate_Tracts_Precincts", names(tractVBM_export)[i], ".csv", sep = "")
  write.csv(tractVBM_export[[i]], paste0(root, "Indicator_Output/visualize/", filename), row.names = FALSE)
}

#done
