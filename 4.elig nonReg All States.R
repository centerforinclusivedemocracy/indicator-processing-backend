# Calculate Eligible Non-Registered Voter Rates
library(tibble)
library(data.table)
library(dplyr)
library(rgeos)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(sf)
library(lwgeom)
library(foreign)
library(tidycensus)
library(tidyverse)
library(purrr)

#
# add function similar to right() in excel
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}

# set file path
root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/"

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <- read.csv(paste0(root, "/Indicator_output/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE,
                         colClasses = c("FIPS"="character"))
site_az <- siteCounties[siteCounties$State =="Arizona", ]
site_fl <- siteCounties[siteCounties$State =="Florida", ]
site_ga <- siteCounties[siteCounties$State =="Georgia", ]
site_mi <- siteCounties[siteCounties$State =="Michigan", ]
site_nc <- siteCounties[siteCounties$State =="North Carolina", ]
site_pa <- siteCounties[siteCounties$State =="Pennsylvania", ]
site_tx <- siteCounties[siteCounties$State =="Texas", ]
site_wi <- siteCounties[siteCounties$State =="Wisconsin", ]

### Prepare Data ####

# load all states reg data from Catalist
# this data has all states
regAZ <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regFL <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regGA <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regMI <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regNC <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regPA <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regTX <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)
regWI <- fread(paste0(root, "/Indicator_Inputs/Catalist_Voter_Data/Catalyst_Data_All_States_8_04.txt"), data.table=FALSE)

head(regAZ)

# limit to state
regAZ <- regAZ[regAZ$geography %like% "^AZ ", ]
regFL <- regFL[regFL$geography %like% "^FL ", ]
regGA <- regGA[regGA$geography %like% "^GA ", ]
regMI <- regMI[regMI$geography %like% "^MI ", ]
regNC <- regNC[regNC$geography %like% "^NC ", ]
regPA <- regPA[regPA$geography %like% "^PA ", ]
regTX <- regTX[regTX$geography %like% "^TX ", ]
regWI <- regWI[regWI$geography %like% "^WI ", ]



############ AZ CATALIST AND SHP PRE-PROCESSING ################

#create all caps county field name to filter catalist data
site_az$NAMEUPPER <- toupper(site_az$CountyName)
head(site_az)

#filter catalist to relevant counties
regAZ <- regAZ[regAZ$COUNTY %in% site_az$NAMEUPPER, ]
head(regAZ)
dim(regAZ)
table(regAZ$COUNTY)
length(table(regAZ$COUNTY)) # we have preserved all the relevant counties (11 total)

#create field for FIPS join
regAZ$FIPS <- gsub("CNTY-", "", regAZ$COUNTYCODE)

head(regAZ)

#QA fix: change text from x to y in the catalist data to address QA errors and help features join
regAZ$geography <- str_replace(regAZ$geography, "TOWN MEADOWS", "TOWNE MEADOWS")
regAZ$geography <- str_replace(regAZ$geography, "PUERCO EAST", "PUERCO E/W")
regAZ$geography <- str_replace(regAZ$geography, "PUERCO WEST", "PUERCO E/W")


AZgroup1 <- data.frame(regAZ$geography)
head(AZgroup1)

# filter for relevant counties 
AZgroup1 <- data.frame(AZgroup1[((AZgroup1$regAZ.geography %like% "AZ COCHISE") | 
                                   (AZgroup1$regAZ.geography %like% "AZ COCONINO") |
                                   (AZgroup1$regAZ.geography %like% "AZ MARICOPA") |
                                   (AZgroup1$regAZ.geography %like% "AZ MOHAVE") |
                                   (AZgroup1$regAZ.geography %like% "AZ PIMA") |
                                   (AZgroup1$regAZ.geography %like% "AZ YUMA") |
                                   (AZgroup1$regAZ.geography %like% "AZ GILA")), ])
str(AZgroup1)

# the column ended up with a funky name, so rename it "geography"
colnames(AZgroup1)[1] <- "geography"

head(AZgroup1)
tail(AZgroup1)
# View(AZgroup1)

# this separates everything after "PR-" into a new column 
AZgroup1$precName <- gsub("(.*)PR-", "", AZgroup1$geography) 

head(AZgroup1)
# View(AZgroup1)

#uncomment the following line to verify that there are no duplicates
#write.csv(AZgroup1[,c(1, 2)], paste0(root, "AZgroup1.csv"), row.names = FALSE)

# now do the same for group2
AZgroup2 <- data.frame(regAZ$geography)
head(AZgroup2)

# filter for relevant counties 
AZgroup2 <- data.frame(AZgroup2[((AZgroup2$regAZ.geography %like% "AZ APACHE") | 
                                   (AZgroup2$regAZ.geography %like% "AZ NAVAJO") |
                                   (AZgroup2$regAZ.geography %like% "AZ PINAL") |
                                   (AZgroup2$regAZ.geography %like% "AZ YAVAPAI")), ])

# the column ended up with a funky name, so rename it "geography"
colnames(AZgroup2)[1] <- "geography"
head(AZgroup2)
tail(AZgroup2)
# View(AZgroup2)

# this separates everything after "PR-" and a number into a new column 
AZgroup2$precName <- gsub("(.*)PR-[0-9]* ", "", AZgroup2$geography) 
head(AZgroup2)
# View(AZgroup2)

#uncomment the following line to verify that there are no duplicates
#write.csv(AZgroup2[,c(1, 2)], paste0(root, "AZgroup2.csv"), row.names = FALSE)

# now let's merge group1 with group2
AZmerged <- rbind(AZgroup1, AZgroup2)
# View(AZmerged)

# grab just the precinct name field and put back into AZ catalist
head(regAZ)
regAZ <- full_join(regAZ, AZmerged, by = "geography")
head(regAZ)

# create single join field in catalist
regAZ$PREC_JOIN <- paste("AZ", regAZ$COUNTY, regAZ$precName)
head(regAZ)

##QA fix: one duplicate PREC_JOIN was created to merge the field into one
sum(duplicated(regAZ$PREC_JOIN))
dim(regAZ)

##remove catalist duplicates from the other features; 
#summarise numeric columns and get the first value for specified columns
regAZ <- data.frame(regAZ %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24, FIPS, precName), first))) 
#reorder fields
regAZ <- regAZ[,c(14:21, 2:13, 22:28, 1)]

#check that there are no more duplicates
sum(duplicated(regAZ$PREC_JOIN))
dim(regAZ)


#uncomment the following line to verify that there are no duplicates
#write.csv(regAZ[,c(2, 4, 28)], paste0(root, "CAT_JOIN.csv"), row.names = FALSE)

# load in precinct shp
AZ_prec <-  st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/az_2016/az_2016.shp"))
AZ_prec <- st_transform(AZ_prec, 2762)

head(AZ_prec)
table(AZ_prec$COUNTY)
length(table(AZ_prec$COUNTY))

# check how many records in catalist vs precincts 
dim(regAZ); dim(AZ_prec)

# filter precincts to catalist, first by county
AZ_prec <- AZ_prec[AZ_prec$COUNTY %in% regAZ$COUNTY, ]
head(AZ_prec)
dim(AZ_prec) # why dont the number of precincts match the number of records in catalist? 
dim(regAZ)
# still more precincts than catalist records
table(AZ_prec$COUNTY)
length(table(AZ_prec$COUNTY)) # but the correct number of counties are there

head(regAZ)
head(AZ_prec)

# create single join field in precinct shp
AZ_prec$PREC_JOIN <- paste("AZ", AZ_prec$COUNTY, AZ_prec$PRCTNAM)
head(AZ_prec)

# joining based off join fields
AZjoin <- left_join(regAZ, AZ_prec, by = c("PREC_JOIN"))
head(AZjoin)

############ GA CATALIST AND SHP PRE-PROCESSING ################


#create all caps county field name to filter catalist data
site_ga$NAMEUPPER <- toupper(site_ga$CountyName)
head(site_ga)

#filter catalist to relevant counties
regGA <- regGA[regGA$COUNTY %in% site_ga$NAMEUPPER, ]
head(regGA)
dim(regGA)
table(regGA$COUNTY)
length(table(regGA$COUNTY)) # we have preserved all the relevant counties (11 total)

#create field for FIPS join
regGA$FIPS <- gsub("CNTY-", "", regGA$COUNTYCODE)

head(regGA)


GAgroup1 <- data.frame(regGA$geography)
head(GAgroup1)
str(GAgroup1)


# the column ended up with a funky name, so rename it "geography"
colnames(GAgroup1)[1] <- "geography"

head(GAgroup1)
tail(GAgroup1)
# View(GAgroup1)

# this separates everything after "PR-" into a new column 
GAgroup1$precName <- gsub("(.*) - ", "", GAgroup1$geography) 

head(GAgroup1)
# View(GAgroup1)


# grab just the precinct name field and put back into GA catalist
head(regGA)
regGA <- full_join(regGA, GAgroup1, by = "geography")
head(regGA)

# create single join field in catalist
regGA$PREC_JOIN <- paste("GA", regGA$COUNTY, regGA$precName)
head(regGA)

##Check for duplicates in catalist PREC_JOIN
sum(duplicated(regGA$PREC_JOIN))
dim(regGA)

##remove catalist duplicates from the other features; 
#summarise numeric columns and get the first value for specified columns
regGA <- data.frame(regGA %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24, FIPS, precName), first))) 
#reorder fields
regGA <- regGA[,c(14:21, 2:13, 22:28, 1)]
head(regGA)

#check that there are no more duplicates
sum(duplicated(regGA$PREC_JOIN)) #no more dupes!
dim(regGA) #There are 1808!
length(table(regGA$COUNTY))



# load in precinct shp
GA_prec <-  st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/ga_2016/ga_2016.shp"))
GA_prec <- st_transform(GA_prec, 2781)

head(GA_prec)
table(GA_prec$COUNTY)
length(table(GA_prec$COUNTY))

# check how many records in catalist vs precincts 
dim(regGA); dim(GA_prec)

# filter precincts to catalist, first by county
GA_prec <- GA_prec[GA_prec$COUNTY %in% regGA$COUNTY, ]
head(GA_prec)
dim(GA_prec) # why dont the number of precincts match the number of records in catalist? 
dim(regGA)
# slightly less precincts than catalist records
table(GA_prec$COUNTY)
length(table(GA_prec$COUNTY)) # but the correct number of counties are there, 41!

head(regGA)
head(GA_prec)

# create single join field in precinct shp
GA_prec$PREC_JOIN <- paste("GA", GA_prec$COUNTY, GA_prec$PRECINCT_I)
head(GA_prec)

#check that there are no more duplicates
sum(duplicated(regGA$PREC_JOIN))
sum(duplicated(GA_prec$PREC_JOIN))


#GAjoin <- left_join(GA_prec, regGA, by = c("PREC_JOIN"))

# joining based off join fields
GAjoin <- left_join(regGA, GA_prec, by = c("PREC_JOIN"))
head(GAjoin)
dim(GAjoin)

#optional write to CSV to inspect
#write.csv(GAjoin[,c(1, 15:17, 21, 27, 28)], paste0(root, "GAjoin.csv"), row.names = FALSE)

############ MI CATALIST AND SHP PRE-PROCESSING ################

# change all St. to Saint in the master list
site_mi$CountyName <- str_replace(site_mi$CountyName, "St.", "Saint")

# create all caps county field name in the master list to filter catalist data
site_mi$NAMEUPPER <- toupper(site_mi$CountyName)
head(site_mi)

#filter catalist data to relevant counties
regMI <- regMI[regMI$COUNTY %in% site_mi$NAMEUPPER, ]
head(regMI)
dim(regMI)
table(regMI$COUNTY)
length(table(regMI$COUNTY)) # check if we have preserved all the relevant counties (35 total counties in the masterlist)

#create 13 digit field for catalyst to match the VTD field in precinct shp file to join both
##extract county code
regMI$V20 <- gsub("(.*)CNTY-", "", regMI$COUNTYCODE)
##extract precinct code
regMI$V21 <- sub("\\).*", "", sub(".*\\(", "", regMI$geography))
##extract WD code
regMI$V22 <- gsub("(.*)WD ", "", regMI$geography)
##concatenate the codes for the county, presinct, district in that order
regMI$Precinct_VTD <- paste0(regMI$V20,regMI$V21,regMI$V22)
###Check the extracted code
head(regMI)
tail(regMI)

#load in precinct shp
MI_prec <- st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/mi_2016/mi_2016.shp"))
MI_prec <- st_transform(MI_prec, 2808)

head(MI_prec)
table(MI_prec$VTD2016)
length(table(MI_prec$VTD2016))# check if this looks right

#check how many records in catalist vs precincts:there are more precincts than catalist records
dim(regMI); dim(MI_prec)

#merge multiple records for duplicate records per precinct in the catalist
#check number of duplicates
sum(duplicated(regMI$Precinct_VTD))
sum(uniqueN(regMI$Precinct_VTD))
dim(regMI)
#summarize numeric columns and get the first value for specified columns
regMI <- data.frame(regMI %>%     
                      group_by(Precinct_VTD) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24,V20, V21, V22), first))) 

dim(regMI)# this is to check how many were grouped

# filter precincts to catalist
MI_prec <- MI_prec[MI_prec$VTD2016 %in% regMI$Precinct_VTD, ]

head(MI_prec)
dim(MI_prec)

#There are 35 counties in the catalyst data.I added this line of code to see 
# how many counties I have in the precinct shp file and confirmed there are 35 
MI_prec$COUNTYFIPS <- substr(MI_prec$VTD2016, start = 1, stop = 3)
head(MI_prec)
table(MI_prec$COUNTYFIPS)
length(table(MI_prec$COUNTYFIPS)) 

#rename MI VTD field to PREC_JOIN
regMI$PREC_JOIN <- regMI$Precinct_VTD

#join based on the VTD2016 in the precinct shp file and PREC_JOIN in the catalyst csv?
MIjoin <- left_join(regMI, MI_prec, by = c("PREC_JOIN" = "VTD2016"))

MI_prec$PREC_JOIN <- MI_prec$VTD2016

head(MI_prec)
dim(MI_prec)

# optional: write to CSV to inspect
# write.csv(MIjoin[,c(2:6, 8:19, 25, 33, 35)], paste0(root, "MIjoin.csv"), row.names = FALSE)

# head(MIjoin)
# dim(MIjoin)


############ TX CATALIST AND SHP PRE-PROCESSING ################

#create all caps county field name to filter catalist data
site_tx$NAMEUPPER <- toupper(site_tx$CountyName)
head(site_tx)

#filter catalist to relevant counties
regTX <- regTX[regTX$COUNTY %in% site_tx$NAMEUPPER, ]
head(regTX)
dim(regTX)
table(regTX$COUNTY)
length(table(regTX$COUNTY)) # we have preserved all the relevant counties (62 total)

#create field for FIPS join
regTX$FIPS <- gsub("CNTY-", "", regTX$COUNTYCODE)

#create field for PRECNUM join
regTX <- regTX %>% mutate(PRECNUM = substr(geography, nchar(geography)-4+1, nchar(geography)))

head(regTX)
tail(regTX)

# import shapefile 
TX_prec <-  st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/tx_2016/tx_2016.shp"))
TX_prec <- st_transform(TX_prec, 2846)

# check how many records in catalist vs precincts 
dim(regTX); dim(TX_prec)

# create three digit field in precinct data compatible with FIPS code in catalist data 
TX_prec$FIPSJOIN <- sprintf("%03d", TX_prec$CNTY)
head(TX_prec)

# filter precincts to catalist, first by county
TX_prec <- TX_prec[TX_prec$FIPSJOIN %in% regTX$FIPS, ]
head(TX_prec)
dim(TX_prec) # there are more precincts than catalist records, because catalist is missing 
# records, mostly for industrial areas and areas with no voters (which are present in the shapefile)

table(TX_prec$FIPSJOIN)
length(table(TX_prec$FIPSJOIN)) # but the correct number of counties are there

# Calculate area -- 
TX_prec$PrecFull_area <- st_area(TX_prec)

head(regTX)
head(TX_prec)

# create a common join field for both precincts and catalist
TX_prec$PREC_JOIN <- paste("TX", TX_prec$FIPSJOIN, TX_prec$PREC)
head(TX_prec)

regTX$PREC_JOIN <- paste("TX", regTX$FIPS, regTX$PRECNUM)
head(regTX)

# try joining based off join field
TXjoin <- merge(regTX, TX_prec, by = c("PREC_JOIN"))
head(TXjoin)
str(TXjoin)

############ FL CATALIST AND SHP PRE-PROCESSING ################

#change all St. to Saint in the precinct shp file
site_fl$CountyName <- str_replace(site_fl$CountyName, "St.", "Saint")
#filter to the counties of Florida
site_fl <- site_fl[site_fl$State =="Florida", ]
#create all caps county field name to filter catalist data
site_fl$NAMEUPPER <- toupper(site_fl$CountyName)
head(site_fl)
dim(site_fl)

#filter catalist to relevant counties
regFL <- regFL[regFL$COUNTY %in% site_fl$NAMEUPPER, ]
head(regFL)
dim(regFL)
table(regFL$COUNTY)
length(table(regFL$COUNTY)) # we have to preserve all the relevant counties (41 total)

#create field to join catalist and precinct shp file
##create field for PRECNUM join, the precinct numbers are the digits after the last "-" (note the numbers are not consistent some have empty spaces in front of them others have letters)
regFL$PRECNUM <- gsub("(.*)- ", "", regFL$geography)
head(regFL)

#load in precinct shp
FL_prec <- st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/fl_2016/fl_2016.shp"))
FL_prec <- st_transform(FL_prec, 6437)

##check how many records in catalist vs precincts
dim(regFL); dim(FL_prec)
##create field in both records to match, the precinct code in catalist data and in precinct data is not consistent 
##but I noticed there are a maximum of 4 characters for the precinct codes in both records
##The code below is to make the field of 4 characters and replace all empty spaces at the front with leading 0s, and then join it with the 3 letter county name to creat a new identifier
regFL$PRECNUMBER <-str_pad(regFL$PRECNUM, 4, pad = "0")
regFL$PREC_JOIN <- paste0(regFL$COUNTY,regFL$PRECNUMBER)
head(regFL)

FL_prec$PRECNUM <-str_pad(FL_prec$pct, 4, pad = "0")
FL_prec$PREC_JOIN <- paste0(FL_prec$COUNTYNAM,FL_prec$PRECNUM)
head(FL_prec)

#merge multiple records for duplicate records per precinct in the catalist
#check number of duplicates
sum(duplicated(regFL$PREC_JOIN))
sum(uniqueN(regFL$PREC_JOIN))
dim(regFL)
#summarize numeric columns and get the first value for specified columns
regFL <- data.frame(regFL %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24,PRECNUM, PRECNUMBER ), first))) 

dim(regFL)# this is to check how many were grouped

#filter precincts to catalist, use the newly created field in both records
FL_prec <- FL_prec[FL_prec$PREC_JOIN %in% regFL$PREC_JOIN, ]
head(FL_prec)
dim(FL_prec)
table(FL_prec$COUNTYNAM)
length(table(FL_prec$COUNTYNAM)) # the correct number of counties are there


# try joining based off the created join field?
FLjoin <- left_join( FL_prec, regFL, by = c("PREC_JOIN"))
head(FLjoin)
dim(FLjoin)
length(table(FLjoin$COUNTY))
#optional
#write to CSV to inspect
#write.csv(FLjoin[,c(15:19, 2:13, 1)], paste0(root, "FLjoin.csv"), row.names = FALSE)
#head(FLjoin)
#dim(FLjoin)

#FL_reg_prec_anti <- anti_join(regFL,FL_prec, by="PREC_JOIN")
#head(FL_reg_prec_anti)
#length(table(FL_reg_prec_anti $PREC_JOIN))

#root <- "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/temp/QA/FL/"
#st_write(FL_prec, "FL_prec.shp")

#write.csv(FL_reg_prec_anti[,], paste0(root, "FL_reg_prec_anti.csv"), row.names = FALSE)

############ NC CATALIST AND SHP PRE-PROCESSING ################

##filter to the counties of NC
site_nc <- site_nc[site_nc$State =="North Carolina", ]

##create all caps county field name to filter catalist data
site_nc$NAMEUPPER <- toupper(site_nc$CountyName)
head(site_nc)
dim(site_nc)

#filter catalist to relevant counties
regNC <- regNC[regNC$COUNTY %in% site_nc$NAMEUPPER, ]
head(regNC)
dim(regNC)
table(regNC$COUNTY)
length(table(regNC$COUNTY)) # we have to preserve all the relevant counties (54 total)

#create field to join catalist and precinct shp file


regNC$PREC_JOIN <- gsub("(.*)PR-", "", regNC$geography)
regNC$PREC_JOIN <- paste0(regNC$COUNTY , "_" ,regNC$PREC_JOIN)
head(regNC)
write.csv(regNC[,c(26)], paste0(root, "regNC.csv"), row.names = FALSE)
#load in precinct shp
NC_prec <- st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/nc_2016/nc_2016_updated_precinct_names.shp"))
NC_prec <- st_transform(NC_prec, 3358)

##check how many records in catalist vs precincts
dim(regNC); dim(NC_prec)
##create field in both records to match, the precinct code in catalist data and in precinct data is not consistent 
##but I noticed there are a maximum of 4 characters for the precinct codes in both records
##The code below is to make the field of 4 characters and replace all empty spaces at the front with leading 0s, and then join it with the 3 letter county name to creat a new identifier

NC_prec$PREC_JOIN <- NC_prec$PRC_NAME

head(NC_prec)

#merge multiple records for duplicate records per precinct in the catalist
#check number of duplicates
sum(duplicated(regNC$PREC_JOIN))
sum(uniqueN(regNC$PREC_JOIN))
dim(regNC)
sum(duplicated(NC_prec$PREC_JOIN))
#the duplicates in the precinct shp file are in the counties we are not interested in

#summarize numeric columns and get the first value for specified columns
regNC <- data.frame(regNC %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24 ), first))) 

dim(regNC)# this is to check how many were grouped

#filter precincts to catalist, use the newly created field in both records
NC_prec <- NC_prec[NC_prec$PREC_JOIN %in% regNC$PREC_JOIN, ]
head(NC_prec)
dim(NC_prec)
table(NC_prec$COUNTY_NAM)
length(table(NC_prec$COUNTY_NAM)) # the correct number of counties are there


# try joining based off the created join field?
NCjoin <- left_join( NC_prec, regNC, by = c("PREC_JOIN"))
head(NCjoin)
dim(NCjoin)
length(table(NCjoin$COUNTY))

############ WI CATALIST AND SHP PRE-PROCESSING ################

# change all St. to Saint in the master list
site_wi$CountyName <- str_replace(site_wi$CountyName, "St.", "Saint")

#create all caps county field name to filter catalist data
site_wi$NAMEUPPER <- toupper(site_wi$CountyName)
head(site_wi)


#filter catalist to relevant counties
regWI <- regWI[regWI$COUNTY %in% site_wi$NAMEUPPER, ]
head(regWI)
dim(regWI)
table(regWI$COUNTY)
length(table(regWI$COUNTY)) # we have preserved all the relevant counties (29 total)

#create field for FIPS join
regWI$FIPS <- gsub("CNTY-", "", regWI$COUNTYCODE)

head(regWI)


WIgeography <- data.frame(regWI$geography)
head(WIgeography)
str(WIgeography)


# the column ended up with a funky name, so rename it "geography"
colnames(WIgeography)[1] <- "geography"

head(WIgeography)
tail(WIgeography)

# this separates everything after "PR- " into a new column 
WIgeography$precName <- gsub("(.*)PR-", "", WIgeography$geography) 
head(WIgeography)

# this removes the last hyphen and replaces it with a space 
WIgeography$precName <- gsub("(.*) - ","\\1 ", WIgeography$precName)
head(WIgeography)

# this separates everything after " - " into a new column 
WIgeography$precName <- gsub("(.*) - ","", WIgeography$precName)
head(WIgeography)
#QA fixes
WIgeography$precName <- str_replace(WIgeography$precName, "DE FOREST", "DEFOREST")
WIgeography$precName <- str_replace(WIgeography$precName, "ST CLOUD", "ST. CLOUD")
WIgeography$precName <- str_replace(WIgeography$precName, "ST NAZIANZ", "ST. NAZIANZ")
WIgeography$precName <- str_replace(WIgeography$precName, "ST FRANCIS", "ST. FRANCIS")
WIgeography$precName <- str_replace(WIgeography$precName, "ST JOSEPH", "ST. JOSEPH")
WIgeography$precName <- str_replace(WIgeography$precName, "FONTANA", "FONTANA-ON-GENEVA LAKE")


# grab just the join field and put back into WI catalist
head(regWI)
regWI <- full_join(regWI, WIgeography, by = "geography")
head(regWI)

# create single join field in catalist
regWI$PREC_JOIN <- paste("WI", regWI$COUNTY, regWI$precName)
head(regWI)

##Check for duplicates in catalist PREC_JOIN
sum(duplicated(regWI$PREC_JOIN))
dim(regWI)

#WIdupes <- regWI[duplicated(regWI$PREC_JOIN), ]
#WIdupes2 <- regWI[regWI$PREC_JOIN =="WI DANE TOWN OF SUN PRAIRIE WARD 001", ]
#write.csv(regWI[,c(1, 28)], paste0(root, "regWI.csv"), row.names = FALSE)

##remove catalist duplicates from the other features; 
#summarise numeric columns and get the first value for specified columns
regWI <- data.frame(regWI %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24, FIPS, precName), first))) 
#reorder fields
regWI <- regWI[,c(14:21, 2:13, 22:28, 1)]
head(regWI)

#check that there are no more duplicates
sum(duplicated(regWI$PREC_JOIN)) #no more dupes!
dim(regWI) #There are 4526
length(table(regWI$COUNTY)) #There are 29



# load in precinct shp
WI_prec <-  st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/wi_2016/WI_PrctToCty.shp"))
WI_prec <- st_transform(WI_prec, 3071)

# change St. to Saint in the COUNTY field
#WI_prec$COUNTYNAM <- str_replace(WI_prec$COUNTYNAM, "ST.", "SAINT")

#create all caps county field name to match catalist data
WI_prec$COUNTY<- toupper(WI_prec$COUNTY_NAM)
head(WI_prec)
#WI_prec$COUNTY <- (WI_prec$COUNTY_NAM)
#head(WI_prec)
table(WI_prec$COUNTY)
length(table(WI_prec$COUNTY)) #72 before filtering


# filter precincts to catalist, first by county
WI_prec <- WI_prec[WI_prec$COUNTY %in% regWI$COUNTY, ]
head(WI_prec)
dim(WI_prec) # why dont the number of precincts match the number of records in catalist? 
dim(regWI)
# 119 less precincts than catalist records
table(WI_prec$COUNTY)
table(regWI$COUNTY)
length(table(WI_prec$COUNTY)) # but the correct number of counties are there, 29

head(regWI)
head(WI_prec)

# create single join field in precinct shp
WI_prec$PREC_JOIN <- paste("WI", WI_prec$COUNTY, WI_prec$Full_CTV, WI_prec$WARDS)
head(WI_prec)

#QA fixes
WI_prec$PREC_JOIN <- str_replace(WI_prec$PREC_JOIN, "KENOSHA TOWN OF SALEM", "KENOSHA VILLAGE OF SALEM LAKES")
WI_prec$PREC_JOIN <- str_replace(WI_prec$PREC_JOIN, "KENOSHA VILLAGE OF SILVER LAKE WARD 001", "KENOSHA VILLAGE OF SALEM LAKES WARD 011")
WI_prec$PREC_JOIN <- str_replace(WI_prec$PREC_JOIN, "KENOSHA VILLAGE OF SILVER LAKE WARD 002", "KENOSHA VILLAGE OF SALEM LAKES WARD 012")
WI_prec$PREC_JOIN <- str_replace(WI_prec$PREC_JOIN, "KENOSHA VILLAGE OF SILVER LAKE WARD 003", "KENOSHA VILLAGE OF SALEM LAKES WARD 013")

#check that there are no more duplicates
sum(duplicated(regWI$PREC_JOIN))
sum(duplicated(WI_prec$PREC_JOIN))



# joining based off join fields
WIjoin <- left_join(regWI, WI_prec, by = c("PREC_JOIN"))
head(WIjoin)
dim(WIjoin)

#optional write to CSV to inspect
#write.csv(WIjoin[,c(1, 15:17, 21, 27, 28)], paste0(root, "WIjoin.csv"), row.names = FALSE)

############ PA CATALIST AND SHP PRE-PROCESSING ################
#create all caps county field name to filter catalist data
site_pa$NAMEUPPER <- toupper(site_pa$CountyName)
head(site_pa)

#filter catalist to relevant counties
regPA <- regPA[regPA$COUNTY %in% site_pa$NAMEUPPER, ]
head(regPA)
dim(regPA)
table(regPA$COUNTY)
length(table(regPA$COUNTY)) # we have preserved all the relevant counties (44 total)

#create field for FIPS join
regPA$FIPS <- gsub("CNTY-", "", regPA$COUNTYCODE)
head(regPA)


PAgeography <- data.frame(regPA$geography)
head(PAgeography)
str(PAgeography)


# the column ended up with a funky name, so rename it "geography"
colnames(PAgeography)[1] <- "geography"

head(PAgeography)
tail(PAgeography)

# this separates everything after "PR- " into a new column 
PAgeography$precName <- gsub("(.*)PR-", "", PAgeography$geography) 
head(PAgeography)

#############################
#Catalyst QA BULK FIXES
#############################

PAgeography$precName <- gsub("\\.", "", PAgeography$precName)
PAgeography$precName <- gsub(" WD ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" WD", "", PAgeography$precName)
PAgeography$precName <- gsub("# ", "0", PAgeography$precName)
PAgeography$precName <- gsub("#", "0", PAgeography$precName)
PAgeography$precName <- gsub(", ", " ", PAgeography$precName)
PAgeography$precName <- gsub(",", " ", PAgeography$precName)
PAgeography$precName <- gsub(" TWP DIST ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" PRECINCT ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" PRECINCT", "", PAgeography$precName)
PAgeography$precName <- gsub(" PRECICNT", "", PAgeography$precName)
PAgeography$precName <- gsub(" PREC ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" PREC", "", PAgeography$precName)
PAgeography$precName <- gsub(" BR 00 ", " DIST ", PAgeography$precName)
PAgeography$precName <- gsub(" TP 00 ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" TP 00 ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" PCT ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" PCT", "", PAgeography$precName)
PAgeography$precName <- gsub("-", " ", PAgeography$precName)
PAgeography$precName <- gsub(" DIV ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" HL ", " HILLS ", PAgeography$precName)
PAgeography$precName <- gsub(" HT ", " HEIGHTS ", PAgeography$precName)
PAgeography$precName <- gsub(" VLG", " VILLAGE", PAgeography$precName)
PAgeography$precName <- gsub(" FIFTH ", " 05 ", PAgeography$precName)
PAgeography$precName <- gsub("FIRST", "01", PAgeography$precName)
PAgeography$precName <- gsub("SECOND", "02", PAgeography$precName)
PAgeography$precName <- gsub("THIRD", "03", PAgeography$precName)
PAgeography$precName <- gsub("THIRD", "03", PAgeography$precName)
PAgeography$precName <- gsub("FOURTH", "04", PAgeography$precName)
PAgeography$precName <- gsub("FIFTH", "05", PAgeography$precName)
PAgeography$precName <- gsub("SIXTH", "06", PAgeography$precName)
PAgeography$precName <- gsub(" RD", " ROAD", PAgeography$precName)
PAgeography$precName <- gsub(" TWSP ", " ", PAgeography$precName)
PAgeography$precName <- gsub(" SPRINGDAL ", " SPRINGDALE ", PAgeography$precName)
#Here we use negative lookbehind, which can be glossed as "match if you do not see ... on the left". Note: this only works with sub if you use perl = T
PAgeography$precName <- sub("(?<!GALLITZIN|BARR)\\(.*\\)", " ", PAgeography$precName, perl = T)
PAgeography$precName <- sub("(?<!ROCHESTER|TYRONE|NEWTOWN)\\sBORO\\s", " ", PAgeography$precName, perl=TRUE)
PAgeography$precName <- sub("(?<!)\\sBOROUGH\\s", " ", PAgeography$precName, perl=TRUE)
PAgeography$precName <- sub("(?<!BURNSIDE|TREMONT)\\sBOROUGH", " ", PAgeography$precName, perl=TRUE)
PAgeography$precName <- sub("(?<!BURNSIDE|DARBY|TREMONT|ROCHESTER|TYRONE|BURLINGTON|CANTON|MONROE|
                            |ROME|TROY|WYALUSING|CONNOQUENESSING|FAIRVIEW|SLIPPERY ROCK|BENTON|CATAWISSA|
                            |WOODCOCK|PERRY|NESCOPECK|LAKE|MILFORD|ADDISON|CONYNGHAM|NEWTOWN)\\sBORO", " ", PAgeography$precName, perl=TRUE)
PAgeography$precName <- sub("(?<!SPRINGDAL|BALDWIN)\\sDIST\\s", " ", PAgeography$precName, perl=TRUE)
PAgeography$precName <- gsub("S W GREENSBURG", "SOUTHWEST GREENSBURG", PAgeography$precName)
PAgeography$precName <- sub("(?<!ALLEGHENY|CHESTER)\\sW\\s", " ", PAgeography$precName, perl=TRUE)

#############################
#This removes spaces in the front and back of fields; it also addresses extra spaces
PAgeography$precName <- str_trim(PAgeography$precName)
PAgeography$precName <- str_squish(PAgeography$precName)
#############################
# grab just the join field and put back into PA catalist
regPA <- full_join(regPA, PAgeography, by = "geography")
head(regPA)

# create single join field in catalist
regPA$PREC_JOIN <- paste("PA", regPA$COUNTY, regPA$precName)

#############################
#QA Bulk Fixes that were not working in the previous QA section
regPA$PREC_JOIN <- gsub("(?<!DELAWARE CHESTER |DELAWARE DARBY )TOWNSHIP", "", regPA$PREC_JOIN, perl = T)
regPA$PREC_JOIN <- gsub(" W 00 P ", " 0", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 00 ", " ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 00", "", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" WARD ", " ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD ATHENS 1ST WARD", "PA BRADFORD ATHENS WD 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD ATHENS 2ND WARD", "PA BRADFORD ATHENS WD 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD ATHENS 3RD WARD", "PA BRADFORD ATHENS WD 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD ATHENS 4TH WARD", "PA BRADFORD ATHENS WD 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" WARD", "", regPA$PREC_JOIN)
regPA$PREC_JOIN <- sub("(?<!BRADFORD BURLINGTON|HOWARD|BUTLER BUTLER|WILKES BARRE)\\sTWP\\s", " ", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- sub("(?<!BRADFORD BURLINGTON |HOWARD |BUTLER BUTLER |WILKES BARRE |DARLINGTON )TWP", " ", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- gsub(" PHILA ", " PHILADELPHIA ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" UP ", " UPPER ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" ALIQUIPPA ", " ALIQUIPPA 0", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" LOWER SAUCON ", " LOWER SAUCON 0", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" LOWER SALFORD ", " LOWER SALFORD 0", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("EAST 1ST DISTRICT", "EAST1ST DISTRICT", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("EAST 2ND DISTRICT", "EAST2ND DISTRICT", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("ERIE NORTH EAST 1ST", "ERIE NORTH EAST WD 1ST", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("ERIE NORTH EAST 2ND", "ERIE NORTH EAST WD 2ND", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("EAST1ST DISTRICT", "EAST 1ST DISTRICT", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("EAST2ND DISTRICT", "EAST 2ND DISTRICT", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("DISTRICT", "", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" PK ", " PARK ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" TP ", " ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" CASL ", " CASTLE ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" MOUNT ", " MT ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" LEB ", " LEBANON ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD BURLINGTON WEST", "PA BRADFORD WEST BURLINGTON TWP", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA BRADFORD BURLINGTON BORO", "PA BRADFORD BURLINGTON", regPA$PREC_JOIN)
regPA$PREC_JOIN <- sub("(?<!SPRINGDAL|BALDWIN)\\sDIST", " ", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- sub("(?<!ARMSTRONG|BUTLER EVANS|BUTLER KARNS|CAMBRIA JHTN CAMBRIA|CHESTER SPRING|ERIE LAKE|ERIE UNION|
                           |FAYETTE FAYETTE|INDIANA HOMER|LACKAWANNA DICKSON|LAWRENCE ELLWOOD|MERCER GROVE|SCHUYLKILL TOWER|
                           |SOMERSET CENTRAL|VENANGO OIL|SCHUYLKILL MAHANOY|LANCASTER LANCASTER)\\sCITY", " ", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- gsub(" BARRE TWP ", " BARRE CITY ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("NEWTOWN BORO", "NEWTOWN DIST", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("LANCASTER LANCASTER CITY", "LANCASTER LANCASTER WD", regPA$PREC_JOIN)

#############################
#This removes spaces in the front and back of fields; it also addresses extra spaces
regPA$PREC_JOIN <- str_trim(regPA$PREC_JOIN)
regPA$PREC_JOIN <- str_squish(regPA$PREC_JOIN)
#############################
regPA$PREC_JOIN <- gsub(" 03/1ST ", " 3 1 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3/1ST ", " 3 1 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3/1", " 3 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 03/2ND ", " 3 2 ", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" 1ST ", " 1 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 01ST ", " 1 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 2ND ", " 2 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 02ND ", " 2 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3RD ", " 3 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 03RD ", " 3 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 4TH ", " 4 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 04TH ", " 4 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 5TH ", " 5 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 05TH ", " 5 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 6TH ", " 6 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 06TH ", " 6 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 7TH ", " 7 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 07TH ", " 7 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8TH ", " 8 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 08TH ", " 8 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 9TH ", " 9 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 09TH ", " 9 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 10TH ", " 10 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 11TH ", " 11 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 12TH ", " 12 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 13TH ", " 13 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 14TH ", " 14 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 15TH ", " 15 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 16TH ", " 16 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 17TH ", " 17 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 18TH ", " 18 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 19TH ", " 19 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 20TH ", " 20 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 21ST ", " 21 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 22ND ", " 22 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 23RD ", " 23 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 24TH ", " 24 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 25TH ", " 25 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 26TH ", " 26 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 27TH ", " 27 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 41ST ", " 41 ", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" 1ST", " 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 01ST", " 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 2ND", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 02ND", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3RD", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 03RD", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 4TH", " 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 04TH", " 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 5TH", " 5", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 05TH", " 5", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 6TH", " 6", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 06TH", " 6", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 7TH", " 7", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 07TH", " 7", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8TH", " 8", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 08TH", " 8", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 9TH", " 9", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 09TH", " 9", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 10TH", " 10", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 11TH", " 11", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 12TH", " 12", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 13TH", " 13", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 14TH", " 14", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 15TH", " 15", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 16TH", " 16", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 17TH", " 17", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 18TH", " 18", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 19TH", " 19", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 20TH", " 20", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 21ST", " 21", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 22ND", " 22", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 23RD", " 23", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 24TH", " 24", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 25TH", " 25", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 26TH", " 26", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 27TH", " 27", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 41ST", " 41", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub("3RD ", " 3 ", regPA$PREC_JOIN)#part of string

regPA$PREC_JOIN <- gsub(" 1P", " 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 2P", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3P", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 4P", " 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 5P", " 5", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 6P", " 6", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 7P", " 7", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8P ", " 8 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8P", " 8", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 9P", " 9", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 10P", " 10", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 11P", " 11", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 12P", " 12", regPA$PREC_JOIN)


regPA$PREC_JOIN <- gsub(" 1W ", " 1 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 2W ", " 2 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 3W ", " 3 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 4W ", " 4 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 5W ", " 5 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 6W ", " 6 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 7W ", " 7 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8W ", " 8 ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 9W ", " 9 ", regPA$PREC_JOIN)

regPA$PREC_JOIN <- sub("(?<!MANHEIM)\\s1W", " 1", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- sub("(?<!MANHEIM)\\s2W", " 2", regPA$PREC_JOIN, perl=TRUE)
regPA$PREC_JOIN <- gsub(" 3W", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 4W", " 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 5W", " 5", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 6W", " 6", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 7W", " 7", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 8W", " 8", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 9W", " 9", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" LANCASTER MANHEIM 1W", " LANCASTER MANHEIM WD 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" LANCASTER MANHEIM 2W", " LANCASTER MANHEIM WD 2", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" W1", " 1", regPA$PREC_JOIN)  
regPA$PREC_JOIN <- gsub(" W2", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" W3", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" W4", " 4", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" 00 ", " ", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" 0 ", " ", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" 00", " ", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" 0", " ", regPA$PREC_JOIN)# 

#Directionals
regPA$PREC_JOIN <- gsub(" W ", " WEST ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" P ", " 0", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" M ", " MIDDLE ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" E ", " EAST ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" N ", " NORTH ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" S ", " SOUTH ", regPA$PREC_JOIN)

regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTERN", "WEST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "EASTERN", "EAST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "NORTHERN", "NORTH")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOUTHERN", "SOUTH")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "NORTHUMBERLAND UPPER AUGUSTA NE DIST", "NORTHUMBERLAND UPPER AUGUSTA NORTHEAST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "NORTHUMBERLAND UPPER AUGUSTA Nw DIST", "NORTHUMBERLAND UPPER AUGUSTA NORTHWEST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "N/W", "NORTH WEST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "0NE", "NORTHEAST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "0NW", "NORTHWEST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "0SO", "SOUTH")

regPA$PREC_JOIN <- gsub("PA CENTRE [0-9]", "PA CENTRE", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA CENTRE[0-9]", "PA CENTRE", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("PA CENTRE [0-9]", "PA CENTRE", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" LAW ", " LAWRENCE ", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("C'VILLE", "CONNELLSVILLE", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("B'VILLE", "BROWNSVILLE", regPA$PREC_JOIN) 
regPA$PREC_JOIN <- gsub("/EXCHANGE", " EXCHANGE", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("/STRONG", " STRONG", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("UPPER AUGUSTA NE", " UPPER AUGUSTA NORTHEAST", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("UPPER AUGUSTA NW", " UPPER AUGUSTA NORTHWEST", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" 01", " 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 02", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 03", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 04", " 4", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" ONE", " 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" TWO", " 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" THREE", " 3", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" FOUR", " 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" FIVE", " 5", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" SIX", " 6", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub(" 0", "", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub("2/1", "2 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("2/2", "2 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("3/2ND", "3 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("3/2", "3 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("3/4", "3 4", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("3/3", "3 3", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub("1 NORTH", "NORTH 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("2 NORTH", "NORTH 2", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("1 SOUTH", "SOUTH 1", regPA$PREC_JOIN)
regPA$PREC_JOIN <- gsub("2 SOUTH", "SOUTH 2", regPA$PREC_JOIN)

regPA$PREC_JOIN <- gsub(" ALLEGHENY SPRINGDAL", " ALLEGHENY SPRINGDALE", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY ALEPPO 1", " ALLEGHENY ALEPPO", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY BELL ACRES 1", " ALLEGHENY BELL ACRES", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY GLENFIELD 1", " ALLEGHENY GLENFIELD", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY BEN AVON HEIGHTS 1", " ALLEGHENY BEN AVON HEIGHTS", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY BRADFORDWOOD 1", " ALLEGHENY BRADFORD WOODS", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY CHALFANT 1", " ALLEGHENY CHALFANT", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY HAYSVILLE 1", " ALLEGHENY HAYSVILLE", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY KILBUCK 1", " ALLEGHENY KILBUCK", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY CORAOPOLIS 2", " ALLEGHENY CORAOPOLIS 2 1", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY LINCOLN 1", " ALLEGHENY LINCOLN", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY SEWICKLEY HEIGHTS 1", " ALLEGHENY SEWICKLEY HEIGHTS", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY SEWICKLEY HILLS 1", " ALLEGHENY SEWICKLEY HILLS", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY SPRINGDALE 1", " ALLEGHENY SPRINGDALE", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY WALL 1", " ALLEGHENY WALL", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY WEST ELIZABETH 1", " ALLEGHENY WEST ELIZABETH", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY SOUTH VERSAILLES 1", " ALLEGHENY SOUTH VERSAILLES", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY ROSSLYN FARM 1", " ALLEGHENY ROSSLYN FARM", regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ALLEGHENY PENNSBURY VILLAGE 2", " ALLEGHENY PENNSBURY VILLAGE", regPA$PREC_JOIN)#

regPA$PREC_JOIN <- gsub(" ARMSTRONG KISKIMINETAS ORCH HILLS NORTH"," ARMSTRONG KISKIMINETAS ORCHHILLS NORTH",  regPA$PREC_JOIN)#
regPA$PREC_JOIN <- gsub(" ARMSTRONG KISKIMINETAS ORCH HILLS SOUTH"," ARMSTRONG KISKIMINETAS ORCHHILLS SOUTH", regPA$PREC_JOIN)#

regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "YORK LWR", "YORK LOWER")  

regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND ALLEGHENY NO 5 BELLVUE", "WESTMORELAND ALLEGHENY 5")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND ALLEGHENY NO 4 STEWARTS", "WESTMORELAND ALLEGHENY 4")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND ALLEGHENY NO 3 SOBERS", "WESTMORELAND ALLEGHENY 3")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND ALLEGHENY NO 2 MCKEES", "WESTMORELAND ALLEGHENY 2")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND ALLEGHENY NO 1 SHEARERS", "WESTMORELAND ALLEGHENY 1")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 1 FOXHILL", "WESTMORELAND HEMPFIELD FOXHILL")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 2 ALWINE", "WESTMORELAND HEMPFIELD ALWINE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 3 MAPLEWOOD", "WESTMORELAND HEMPFIELD MAPLEWOOD")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 4 CARBON", "WESTMORELAND HEMPFIELD CARBON")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 5 WENDEL HERM", "WESTMORELAND HEMPFIELD WENDEL HERM")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 6 LUXOR", "WESTMORELAND HEMPFIELD LUXOR")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 7 HANNASTOWN", "WESTMORELAND HEMPFIELD HANNASTOWN")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND HEMPFIELD NO 8 BOVARD", "WESTMORELAND HEMPFIELD BOVARD")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "LINCOLN HGTS EST", "LINCOLN HEIGHTS") # OK even if we remove LINCOLN 
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "LINCOLN HGTS WST", "LINCOLN HEIGHTS WEST") # OK even if we remove LINCOLN 
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WEAVERS OLD STND", "WEAVERS OLD STAND") # OK even if we remove WEAVERS OLD
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "MUNIC OF MURRYSVILLE", "MURRYSVILLE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND LOYALHANNA NO ", "WESTMORELAND LOYALHANNA ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND UPPER BURRELL NO ", "WESTMORELAND UPPER BURRELL ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WESTMORELAND EAST HUNTINGDON BESSEMER NO ", "WESTMORELAND EAST HUNTINGDON BESSEMER ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, " HECLA", " HECCLA")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "HUNTINGDON 4 4 PRC", "HUNTINGDON 4 4")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WASHINGTON WASH ", "WASHINGTON WASHINGTON ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WASHINGTON MONON ", "WASHINGTON MONONGAHELA ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "EAST BETHLEHEM 4 W", "EAST BETHLEHEM 4")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "EAST BETHLEHEM 3 W", "EAST BETHLEHEM 3")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "EAST BETHLEHEM 2 W", "EAST BETHLEHEM 2")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "EAST BETHLEHEM 1 W", "EAST BETHLEHEM 1")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "WASHINGTON CANTON ", "WASHINGTON CANTON TWP ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO CORN ", "VENANGO CORNPLANTER ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO CRANBY ", "VENANGO CRANBERRY ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO OILCREEK", "VENANGO OIL CREEK")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO ROCK ", "VENANGO ROCKLAND ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO SUGAR ", "VENANGO SUGAR CREEK ")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET ADDISON BORO", "SOMERSET ADDISON TWP")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET BROTHERSVALLEY", "SOMERSET BROTHERS VALLEY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET INDIAN LAKE BORO", "SOMERSET INDIAN LAKE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET L TURKEYFOOT", "SOMERSET LOWER TURKEYFOOT")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET U TURKEYFOOT", "SOMERSET UPPER TURKEYFOOT")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SOMERSET SOMERSET S/W", "SOMERSET SOMERSET SOUTH WEST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL AUBURN AUBURN", "SCHUYLKILL AUBURN")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL BARRY BARRY", "SCHUYLKILL BARRY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL BLYTHE BLYTHE", "SCHUYLKILL BLYTHE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL BRANCH BRANCH", "SCHUYLKILL BRANCH")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL CASS SOUTH", "SCHUYLKILL CASS")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL CRESSONA CRESSONA", "SCHUYLKILL CRESSONA")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL DEER LAKE DEER LAKE", "SCHUYLKILL DEER LAKE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL DELANO DELANO", "SCHUYLKILL DELANO")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL EAST BRUNSWICK EAST BRUNSWICK", "SCHUYLKILL EAST BRUNSWICK")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL EAST NORWEGIAN EAST NORWEGIAN", "SCHUYLKILL EAST NORWEGIAN")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL EAST UNION EAST UNION", "SCHUYLKILL EAST UNION")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL ELDRED ELDRED", "SCHUYLKILL ELDRED")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL FOSTER FOSTER", "SCHUYLKILL FOSTER")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL FRAILEY FRAILEY", "SCHUYLKILL FRAILEY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL GILBERTON GILBERTON", "SCHUYLKILL GILBERTON")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL GIRARDVILLE GIRARDVILLE", "SCHUYLKILL GIRARDVILLE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL GORDON GORDON", "SCHUYLKILL GORDON")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL HUBLEY HUBLEY", "SCHUYLKILL HUBLEY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL KLINE KLINE", "SCHUYLKILL KLINE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL LANDINGVILLE LANDINGVILLE", "SCHUYLKILL LANDINGVILLE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL MAHANOY MAHANOY", "SCHUYLKILL MAHANOY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL MECHANICSVILLE MECHANICSVILLE", "SCHUYLKILL MECHANICSVILLE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL MIDDLEPORT MIDDLEPORT", "SCHUYLKILL MIDDLEPORT")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL MT CARBON MT CARBON", "SCHUYLKILL MT CARBON")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL NEW CASTLE NEW CASTLE", "SCHUYLKILL NEW CASTLE")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL NEW PHILADELPHIA NEW PHILADELPHIA", "SCHUYLKILL NEW PHILADELPHIA")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL NEW RINGGOLD NEW RINGGOLD", "SCHUYLKILL NEW RINGGOLD")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL NORTH UNION NORTH UNION", "SCHUYLKILL NORTH UNION")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL PORT CLINTON PORT CLINTON", "SCHUYLKILL PORT CLINTON")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL REILLY REILLY", "SCHUYLKILL REILLY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL RINGTOWN RINGTOWN", "SCHUYLKILL RINGTOWN")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL SOUTH MANHEIM SOUTH MANHEIM", "SCHUYLKILL SOUTH MANHEIM")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL SCHUYLKILL SCHUYLKILL", "SCHUYLKILL SCHUYLKILL")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL RYAN RYAN", "SCHUYLKILL RYAN")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL TOWER CITY TOWER", "SCHUYLKILL TOWER CITY")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL TREMONT TREMONT", "SCHUYLKILL TREMONT")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL TREMONT BOROUGH", "SCHUYLKILL TREMONT TWP")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL UNION UNION", "SCHUYLKILL UNION")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL UPPER MAHANTONGO UPPER MAHANTONGO", "SCHUYLKILL UPPER MAHANTONGO")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL WALKER WALKER", "SCHUYLKILL WALKER")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL SAINT", "SCHUYLKILL ST")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL RUSH ELIXIR", "SCHUYLKILL RUSH ELIXR 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL RUSH WEST", "SCHUYLKILL RUSH WEST 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL POTTSVILLE 1", "SCHUYLKILL POTTSVILLE 1 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL POTTSVILLE 2", "SCHUYLKILL POTTSVILLE 2 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL POTTSVILLE 4", "SCHUYLKILL POTTSVILLE 4 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL POTTSVILLE 6", "SCHUYLKILL POTTSVILLE 6 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL PINE GROVE 1", "SCHUYLKILL PINE GROVE 1 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "SCHUYLKILL PINE GROVE 2", "SCHUYLKILL PINE GROVE 2 9")
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO FRANKLIN 2", "VENANGO FRANKLIN 2 2") # Order matters, don't reverse these
regPA$PREC_JOIN <- str_replace(regPA$PREC_JOIN, "VENANGO FRANKLIN 1", "VENANGO FRANKLIN 2 1")

#############################
#This removes spaces in the front and back of fields; it also addresses extra spaces
regPA$PREC_JOIN <- str_trim(regPA$PREC_JOIN)
regPA$PREC_JOIN <- str_squish(regPA$PREC_JOIN)
sum(duplicated(regPA$PREC_JOIN))
dim(regPA)
#############################

##remove catalist duplicates from the other features; 
#summarise numeric columns and get the first value for specified columns
regPA <- data.frame(regPA %>%     
                      group_by(PREC_JOIN) %>% 
                      summarise(across(where(is.numeric), sum, na.rm = TRUE),              
                                across(c(OID_, geography, STATE, COUNTY, CONG, SS, SH, COUNTYCODE, Field20, Field21, Field22, Field23, Field24, FIPS, precName), first))) 
head(regPA)
#reorder fields
regPA <- regPA[,c(14:21, 2:13, 22:28, 1)]

#check that there are no more duplicates
sum(duplicated(regPA$PREC_JOIN))
dim(regPA)

#############################
# load in precinct shp
#############################

PA_prec <-  st_read(paste0(root, "/Indicator_Inputs/Precinct_Shapefiles/pa_2016/pa_2016.shp"))
PA_prec <- st_transform(PA_prec, 6562)

#combine the state and county code
PA_prec$FIPS <- paste0(PA_prec$STATEFP10,PA_prec$COUNTYFP10)
head(PA_prec)

# joining SHAPE to masterlist based off the fIPS code to acquire the county field 
PA_prec <- left_join(PA_prec, site_pa, by = c("FIPS"))
head(PA_prec)

# filter precincts to catalist, first by county
PA_prec <- PA_prec[PA_prec$NAMEUPPER %in% regPA$COUNTY, ]
dim(PA_prec) # why dont the number of precincts match the number of records in catalist? There are 50 precincts more than catalist features
dim(regPA) 
table(PA_prec$NAMEUPPER)
table(regPA$COUNTY)
length(table(PA_prec$NAMEUPPER)) # but the correct number of counties are there, 44


#create all caps precinct field name
PA_prec$PRENAME <- toupper(PA_prec$NAME10)

# Field used to rejoin groups
#PA_prec$UNIQUEID <- paste(PA_prec$COUNTY, PA_prec$NAME10)

# create single join field in precinct shp
PA_prec$PREC_JOIN <- paste("PA", PA_prec$NAMEUPPER, PA_prec$PRENAME)
head(PA_prec$PREC_JOIN)


#############################
#precinct QA BULK FIXES
#############################

PA_prec$PREC_JOIN <- gsub("\\.", "", PA_prec$PREC_JOIN)
PA_prec$PREC_JOIN <- gsub("-", " ", PA_prec$PREC_JOIN)

PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "ALIQUIPPA VTD", "ALIQUIPPAiVTD")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " TWP DIST ", " ")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " TOWNSHIP", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " WARD", "")
PA_prec$PREC_JOIN <- sub("(?<!ALIQUIPPAi )\\sVTD", "", PA_prec$PREC_JOIN, perl=TRUE)
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "ALIQUIPPAiVTD", "ALIQUIPPA VTD")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " PCT", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " MC ", " MC")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "1ST", "1")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "2nd", "2")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "3RD", "3")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " VOTING DISTRICT", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " AMBRIDGE BORO 02", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " BORO 03", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " BORO 04", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " BORO ", " ")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " ED ", " ")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " FALLS CITY 05 ", " FALLS CITY ")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " 03 LOWER SAUCON", " 3")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " 01 HELLERTOWN", " 1")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " LEITHSVILLE", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " SEIDERSVILLE", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " WASSERGASS", "")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "WESTERN", "WEST")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "EASTERN", "EAST")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "SOUTHERN", "SOUTH")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, "NORTHERN", "NORTH")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " FT ", " FOUNTAIN ")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " CHESTER TWP", " CHESTER TOWNSHIP")
PA_prec$PREC_JOIN <- str_replace(PA_prec$PREC_JOIN, " DARBY TWP", " DARBY TOWNSHIP")

#Here we use negative lookbehind, which can be glossed as "match if you do not see ... on the left". Note: this only works with sub if you use perl = T
PA_prec$PREC_JOIN <- sub("(?<!TYRONE |ATHENS |NORTH EAST |LANCASTER |MANHEIM )WD", "", PA_prec$PREC_JOIN, perl = T)
PA_prec$PREC_JOIN <- sub("(?<!BEAVER ROCHESTER |BUTLER BUTLER |BUTLER CONNOQUENESSING |BUTLER FAIRVIEW |BUTLER SLIPPERY ROCK |CRAWFORD VENANGO | 
                         |CRAWFORD WOODCOOK |DELAWARE CHESTER |DELAWARE DARBY |LUZERNE KINGSTON |LUZERNE NESCOPECK |LUZERNE PITTSTON |
                         |LUZERNE PLYMOUTH |DARLINGTON |BURLINGTON |CANTON |BRADFORD MONROE |BRADFORD ROME |BRADFORD TROY |BRADFORD WYALUSING |
                         |CENTRE HOWARD |CLEARFIELD BURNSIDE |COLUMBIA BENTON |CRAWFORD WOODCOCK |ERIE MCKEAN |SANDY LAKE |
                         |PIKE MILFORD |SCHUYLKILL TREMONT |SOMERSET ADDISON )TWP", "", PA_prec$PREC_JOIN, perl = T)
PA_prec$PREC_JOIN <- sub("(?<!SPRINGDALE|BALDWIN|BUCKS NEWTOWN|DALLAS)\\sDIST\\s", " ", PA_prec$PREC_JOIN, perl=TRUE)
PA_prec$PREC_JOIN <- sub("(?<!CHESTER SPRING|BUTLER|CAMBRIA JOHNSTOWN|ERIE LAKE|ERIE UNION|FAYETTE FAYETTE|INDIANA HOMER|
                       |LACKAWANNA DICKSON|LAWRENCE ELLWOOD|LUZERNE WILKES BARRE|MERCER GROVE|SCHUYLKILL MAHANOY|
                       |SCHUYLKILL TOWER|SOMERSET CENTRAL|VENANGO OIL)\\sCITY", " ", PA_prec$PREC_JOIN, perl=TRUE)

PA_prec$PREC_JOIN <- gsub(" 00 ", " ", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub(" 0 ", " ", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub(" 00", " ", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub(" 0", " ", PA_prec$PREC_JOIN)#

#This removes spaces in the front and back of fields; it also addresses extra spaces
PA_prec$PREC_JOIN <- str_trim(PA_prec$PREC_JOIN)
PA_prec$PREC_JOIN <- str_squish(PA_prec$PREC_JOIN)

PA_prec$PREC_JOIN <- gsub("GETTYSBURG 3 1", "GETTYSBURG 3", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("GETTYSBURG 1 2", "GETTYSBURG 1", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("ADAMS HUNTINGTON 1", "ADAMS HUNTINGTON", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("O HARA", "OHARA", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("CARROLL VALLEY", "CARROLL VALLEY 2", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("CARROLL VALLEY 2 BOROUGH", "CARROLL VALLEY 1", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("ALIQUIPPA 1", "ALIQUIPPA", PA_prec$PREC_JOIN)
PA_prec$PREC_JOIN <- gsub("PROVIDENC ", "PROVIDENCE ", PA_prec$PREC_JOIN)

PA_prec$PREC_JOIN <- gsub(" FOUR", " 4", PA_prec$PREC_JOIN)
PA_prec$PREC_JOIN <- gsub(" FIVE", " 5", PA_prec$PREC_JOIN)

PA_prec$PREC_JOIN <- gsub("ALLEGHENY GREEN TREE", "ALLEGHENY GREENTREE", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("ALLEGHENY CORAOPOLIS 2 1", "ALLEGHENY CORAOPOLIS 2", PA_prec$PREC_JOIN)#

PA_prec$PREC_JOIN <- gsub("BEAVER AMBRIDGE 2 1", "BEAVER AMBRIDGE 1", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER AMBRIDGE 2 2", "BEAVER AMBRIDGE 2", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER AMBRIDGE 2 3", "BEAVER AMBRIDGE 3", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER AMBRIDGE 2 4", "BEAVER AMBRIDGE 4", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER AMBRIDGE 2 5", "BEAVER AMBRIDGE 5", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 1", "BEAVER ALIQUIPPA 1", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 2", "BEAVER ALIQUIPPA 2", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 3", "BEAVER ALIQUIPPA 3", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 4", "BEAVER ALIQUIPPA 4", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 5", "BEAVER ALIQUIPPA 5", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 6", "BEAVER ALIQUIPPA 6", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 7", "BEAVER ALIQUIPPA 7", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 8", "BEAVER ALIQUIPPA 8", PA_prec$PREC_JOIN)#
PA_prec$PREC_JOIN <- gsub("BEAVER ALIQUIPPA 1 9", "BEAVER ALIQUIPPA 9", PA_prec$PREC_JOIN)#

#This removes spaces in the front and back of fields; it also addresses extra spaces
PA_prec$PREC_JOIN <- str_trim(PA_prec$PREC_JOIN)
PA_prec$PREC_JOIN <- str_squish(PA_prec$PREC_JOIN)

#check that there are no more duplicates
sum(duplicated(regPA$PREC_JOIN))
dupesregPA <- regPA[duplicated(regPA$PREC_JOIN), ]

sum(duplicated(PA_prec$PREC_JOIN))
dupesPA_prec <- PA_prec[duplicated(PA_prec$PREC_JOIN), ]
head(PA_prec)
#Check the antijoin count
PA_RP_anti <- anti_join(regPA,PA_prec, by="PREC_JOIN")
dim(PA_RP_anti) #catalist features that did not join to precinct features
#write.csv(PA_RP_anti[,], paste0(root, "PA_CATALIST_anti.csv"), row.names = FALSE)

PA_PR_anti <- anti_join(PA_prec,regPA, by="PREC_JOIN")
dim(PA_PR_anti) #precinct features that did not join to catalist features
#write.csv(PA_PR_anti[,c(6,18)], paste0(root, "PA_PREC_anti.csv"), row.names = FALSE)
#st_write(PA_PR_anti, "P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/PA_prec_ANTIJOIN.shp")

#To view the catalist join field
#View(regPA$PREC_JOIN)

#To view the precinct join field
#View(PA_prec$PREC_JOIN)

#To view the catalist antijoins
#View(PA_RP_anti$PREC_JOIN)

#To view the precinct antijoins
#View(PA_PR_anti$PREC_JOIN)

#To view the catalist duplicates
#View(dupesregPA) 

#To view the precinct duplicates
#View(dupesPA_prec)

#write.csv(AZgroup1[,c(1, 2)], paste0(root, "AZgroup1.csv"), row.names = FALSE)

#joining based off join fields
#PAjoin <- left_join(regPA, PA_prec, by = c("PREC_JOIN"))
#head(PAjoin)
#dim(PAjoin)
############ merge all state files here ##################

# merge all site files here - add in new states as ready
site_all <- rbind(site_tx, site_az, site_mi, site_ga, site_fl, site_nc, site_wi, site_pa)
head(site_all)

################ CVAP and institutionalized pop data prep ######################
cvap <- fread(paste0(root, "/Indicator_Inputs/acs/source/CVAP_2013-2017_ACS_csv_files/Tract.csv"),
              data.table = FALSE, colClasses = c("geoid"="character"))
head(cvap)

# create county fips 
cvap$FIPS <- substr(cvap$geoid, 8, 12)
cvap$GEOID <- substr(cvap$geoid, 8, 18)

# limit to total CVAP estimate and counties of interest
cvap <- cvap[(cvap$lntitle =="Total") & cvap$FIPS %in% site_all$FIPS, ]

head(cvap); dim(cvap)


## Read in the incarcerated population data from the 2010 census (TRACTS)
# use census api/tidycensus instead for more efficient bulk processing 
census_api_key("51573c47ae359911158805a21be007c619090982", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

# took this line from another script; unsure if it's needed
options(tigris_use_cache = TRUE)

#create new fields in site_all for census api call
site_all$STATEID <- substr(site_all$FIPS,1,2)
site_all$COUNTYID <- substr(site_all$FIPS,3,5)

instPop <- map2_dfr(
  site_all$STATEID, site_all$COUNTYID,
  ~ get_decennial(
    geography = "tract",
    variables = c("P042001", "P042003"),
    state = .x,
    county = .y,
    year = 2010,
    geometry = FALSE
  )
)

#we have our variables in rows where they should be columns, so we recast
instPop <- data.table(instPop)
instPop <- dcast(instPop, instPop$GEOID~instPop$variable, value.var = "value")

#rename the columns
instPop <- instPop[,c(1,2,3)]
colnames(instPop) <- c("GEOID", "TotalGroupQuarters", "Incarc_Adults")

head(instPop)

# optional: export instPop data to csv to inspect
# write.csv(instPop, paste0(root, "Indicator_Inputs/institutionalized_pop.csv"), row.names = FALSE)

### non institutionalized pop data
totNonInstPop <- map2_dfr(
  site_all$STATEID, site_all$COUNTYID,
  ~ get_acs(
    geography = "tract",
    variables = "S1810_C01_001",
    state = .x,
    county = .y,
    year = 2016,
    geometry = FALSE
  )
)

totNonInstPop <- totNonInstPop[,c(1,4,5)]
colnames(totNonInstPop) <- c("GEOID", "tract_TotNonInstPop", "tract_TotNonInstPop.MOE")
head(totNonInstPop)
dim(totNonInstPop)

# optional: export NoninstPop data to csv to inspect
# write.csv(totNonInstPop, paste0(root, "Indicator_Inputs/total_noninstitutionalized_pop.csv"), row.names = FALSE)


##### CAlculate the proportion of each precinct that is in each block - READ ME BEFORE RUNNING ####
# The intersect processing takes a WHILE! If you've already run the intersects before and have the intersect csvs, you can
# skip to the section below titled "Create Master Lists" and uncomment the block of code that imports the intersect csvs directly.

############# ARIZONA INTERSECT PROCESSING ###############

# load census block geometry
blocksAZ <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksAZ.shp"))
blocksAZ <- st_transform(blocksAZ, 2762)
head(blocksAZ)
# Calculate area -- 
AZ_prec$PrecFull_area <- st_area(AZ_prec)

# get area of blocks
blocksAZ$BlockFull_area = st_area(blocksAZ)
blocksAZ <- st_make_valid(blocksAZ)
head(blocksAZ)

## Intersect two shps
AZ_intr <- st_intersection(st_buffer(blocksAZ, 0), st_buffer(AZ_prec, 0)) 


# question: how much of the precinct is in each block? 
AZ_intr$Intrsct_area <- st_area(AZ_intr)
head(AZ_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
AZ_intr$prc_Intrsct_area  <- as.numeric(AZ_intr$Intrsct_area)/as.numeric(AZ_intr$PrecFull_area)

head(AZ_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
AZ_intr_df <- 
  AZ_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(AZ_intr_df)

## export crosswalk to csv
write.csv(AZ_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/az_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# GEORGIA INTERSECT PROCESSING ################
# load census block geometry
blocksGA <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksGA.shp"))
blocksGA <- st_transform(blocksGA, 2781)

# Calculate area -- 
GA_prec$PrecFull_area <- st_area(GA_prec)

# get area of blocks
blocksGA$BlockFull_area = st_area(blocksGA)
blocksGA <- st_make_valid(blocksGA)
head(blocksGA)

## Intersect two shps
GA_intr <- st_intersection(st_buffer(blocksGA, 0), st_buffer(GA_prec, 0)) 


# question: how much of the precinct is in each block? 
GA_intr$Intrsct_area <- st_area(GA_intr)
head(GA_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
GA_intr$prc_Intrsct_area  <- as.numeric(GA_intr$Intrsct_area)/as.numeric(GA_intr$PrecFull_area)

head(GA_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
GA_intr_df <- 
  GA_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(GA_intr_df)

## export crosswalk to csv
write.csv(GA_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/ga_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# MICHIGAN INTERSECT PROCESSING ##################

# load census block geometry
blocksMI <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksMI.shp"))
blocksMI <- st_transform(blocksMI, 2808)

# Calculate area -- 
MI_prec$PrecFull_area <- st_area(MI_prec)

# get area of blocks
blocksMI$BlockFull_area = st_area(blocksMI)
blocksMI <- st_make_valid(blocksMI)
head(blocksMI)

## Intersect two shps
MI_intr <- st_intersection(st_buffer(blocksMI, 0), st_buffer(MI_prec, 0)) 


# question: how much of the precinct is in each block? 
MI_intr$Intrsct_area <- st_area(MI_intr)
head(MI_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
MI_intr$prc_Intrsct_area  <- as.numeric(MI_intr$Intrsct_area)/as.numeric(MI_intr$PrecFull_area)

head(MI_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
MI_intr_df <- 
  MI_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(MI_intr_df)

## export crosswalk to csv
write.csv(MI_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/mi_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# TEXAS INTERSECT PROCESSING ##################

# load census block geometry
blocksTX <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksTX.shp"))
blocksTX <- st_transform(blocksTX, 2846)
blocksTX$FIPS <- paste(blocksTX$STATEFP10,blocksTX$COUNTYFP10, sep = "")

# get area of blocks
blocksTX$BlockFull_area = st_area(blocksTX)
head(blocksTX)

# calculate the intersect
TX_intr <- st_intersection(st_buffer(blocksTX, 0), st_buffer(TX_prec, 0))

# question: how much of the precinct is in each block? 
TX_intr$Intrsct_area <- st_area(TX_intr)
head(TX_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
TX_intr$prc_Intrsct_area  <- as.numeric(TX_intr$Intrsct_area)/as.numeric(TX_intr$PrecFull_area)

head(TX_intr)

# convert BLOCKID field to string, otherwise it will forced to float
TX_intr$BLOCKID <- as.character(TX_intr$BLOCKID10)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
TX_intr_df <- 
  TX_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(TX_intr_df)

## export crosswalk to csv
write.csv(TX_intr_df, paste0(root, "Indicator_Inputs/crosswalks/tx_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# FLORIDA INTERSECT PROCESSING ##################

# load census block geometry
blocksFL <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksFL.shp"))
blocksFL <- st_transform(blocksFL, 6437)

# Calculate area -- 
FL_prec$PrecFull_area <- st_area(FL_prec)

# get area of blocks
blocksFL$BlockFull_area = st_area(blocksFL)
blocksFL <- st_make_valid(blocksFL)
head(blocksFL)

## Intersect two shps
FL_intr <- st_intersection(st_buffer(blocksFL, 0), st_buffer(FL_prec, 0)) 


# question: how much of the precinct is in each block? 
FL_intr$Intrsct_area <- st_area(FL_intr)
head(FL_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
FL_intr$prc_Intrsct_area  <- as.numeric(FL_intr$Intrsct_area)/as.numeric(FL_intr$PrecFull_area)

head(FL_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
FL_intr_df <- 
  FL_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(FL_intr_df)

## export crosswalk to csv
write.csv(FL_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/fl_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# NORTH CAROLINA INTERSECT PROCESSING ################

# load census block geometry
blocksNC <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksNC.shp"))
blocksNC <- st_transform(blocksNC, 3358)

# Calculate area -- 
NC_prec$PrecFull_area <- st_area(NC_prec)

# get area of blocks
blocksNC$BlockFull_area = st_area(blocksNC)
blocksNC <- st_make_valid(blocksNC)
head(blocksNC)

## Intersect two shps
NC_intr <- st_intersection(st_buffer(blocksNC, 0), st_buffer(NC_prec, 0)) 


# question: how much of the precinct is in each block? 
NC_intr$Intrsct_area <- st_area(NC_intr)
head(NC_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
NC_intr$prc_Intrsct_area  <- as.numeric(NC_intr$Intrsct_area)/as.numeric(NC_intr$PrecFull_area)

head(NC_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
NC_intr_df <- 
  NC_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(NC_intr_df)

## export crosswalk to csv
write.csv(NC_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/nc_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# WISCONSIN in INTERSECT PROCESSING ################

# load census block geometry
blocksWI <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksWI.shp"))
blocksWI <- st_transform(blocksWI, 3071)

# Calculate area -- 
WI_prec$PrecFull_area <- st_area(WI_prec)

# get area of blocks
blocksWI$BlockFull_area = st_area(blocksWI)
blocksWI <- st_make_valid(blocksWI)
head(blocksWI)

## Intersect two shps
WI_intr <- st_intersection(st_buffer(blocksWI, 0), st_buffer(WI_prec, 0)) 


# question: how much of the precinct is in each block? 
WI_intr$Intrsct_area <- st_area(WI_intr)
head(WI_intr)

# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
WI_intr$prc_Intrsct_area  <- as.numeric(WI_intr$Intrsct_area)/as.numeric(WI_intr$PrecFull_area)

head(WI_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
WI_intr_df <- 
  WI_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(WI_intr_df)

## export crosswalk to csv
write.csv(WI_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/wi_intersect_precinct_crosswalk.csv"), row.names = FALSE)

############# PENNSYLVANIA INTERSECT PROCESSING ###############

# load census block geometry
blocksPA <- st_read(paste0(root, "Indicator_Inputs/Census_Blocks/blocksPA.shp"))
blocksPA <- st_transform(blocksPA, 6562)

# Calculate area -- 
PA_prec$PrecFull_area <- st_area(PA_prec)

# get area of blocks
blocksPA$BlockFull_area = st_area(blocksPA)
blocksPA <- st_make_valid(blocksPA)
head(blocksPA)

## Intersect two shps
PA_intr <- st_intersection(st_buffer(blocksPA, 0), st_buffer(PA_prec, 0)) 


# question: how much of the precinct is in each block? 
PA_intr$Intrsct_area <- st_area(PA_intr)
head(PA_intr)


# if the intersecting area is 100% of the original block area, then 100% of that block is within the precinct, 
# but we need to allocate data from the precincts to the blocks. 
# so what percent of the intersecting area is the original precinct size?
PA_intr$prc_Intrsct_area  <- as.numeric(PA_intr$Intrsct_area)/as.numeric(PA_intr$PrecFull_area)

head(PA_intr)

# Create the crosswalk/conversion file 
# the conversion file doesn't have to be spatial
PA_intr_df <- 
  PA_intr %>% 
  as.data.frame() %>% 
  dplyr::select(-geometry)
head(PA_intr_df)

## export crosswalk to csv
write.csv(PA_intr_df, paste0(root, "/Indicator_Inputs/crosswalks/pa_intersect_precinct_crosswalk.csv"), row.names = FALSE)

######### Create Master Lists and Calcuate Proportions for All States ##########

# OPTIONAL: DO THIS IF YOU DIDN'T RUN THE INTERSECT ABOVE (Because that takes a min) #
# load conversion file
# AZ_intr <- read.csv(paste0(root, "Indicator_Inputs/crosswalks/az_intersect_precinct_crosswalk.csv"), 
#                      stringsAsFactors = FALSE, colClasses = c("BLOCKID10"="character"))
# GA_intr <- read.csv(paste0(root, "Indicator_Inputs/crosswalks/ga_intersect_precinct_crosswalk.csv"), 
#                      stringsAsFactors = FALSE, colClasses = c("BLOCKID10"="character"))
# MI_intr <- read.csv(paste0(root, "Indicator_Inputs/crosswalks/mi_intersect_precinct_crosswalk.csv"), 
#                     stringsAsFactors = FALSE,colClasses = c("BLOCKID10"="character"))
# TX_intr <- read.csv(paste0(root, "Indicator_Inputs/crosswalks/tx_intersect_precinct_crosswalk.csv"), 
#                     stringsAsFactors = FALSE, colClasses = c("BLOCKID10"="character"))  
# FL_intr <- read.csv(paste0(root, "Indicator_Inputs/crosswalks/fl_intersect_precinct_crosswalk.csv"), 
#                     stringsAsFactors = FALSE, colClasses = c("BLOCKID10"="character"))  

# create giant intersect dataframe
# we need to create subsets for each state: grab the state code, county code, block ID, block area,
# precinct area, precinct join, intersect area, and percent intersect area fields
head(PA_intr_df)

AZ_intr_df <- AZ_intr_df[,c(1,2,5,10,28:31)]
colnames(AZ_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PREC_JOIN", "PrecinctFull_area", "Intrsct_area", "prc_Intrsct_area")

GA_intr_df <- GA_intr_df[,c(1,2,5,10,25:28)]
colnames(GA_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PREC_JOIN", "PrecinctFull_area", "Intrsct_area", "prc_Intrsct_area")

MI_intr_df <- MI_intr_df[,c(1,2,5,10,22:25)]
colnames(MI_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PREC_JOIN", "PrecinctFull_area", "Intrsct_area", "prc_Intrsct_area")

TX_intr_df <- TX_intr_df[,c(1,2,34,11,30:33)]
colnames(TX_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PrecinctFull_area", "PREC_JOIN", "Intrsct_area", "prc_Intrsct_area")

FL_intr_df <- FL_intr_df[,c(1,2,5,10,31,30,32,33)]
colnames(FL_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PrecinctFull_area", "PREC_JOIN", "Intrsct_area", "prc_Intrsct_area")

NC_intr_df <- NC_intr_df[,c(1,2,5,10,66,65,67,68)]
colnames(NC_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PrecinctFull_area", "PREC_JOIN", "Intrsct_area", "prc_Intrsct_area")

WI_intr_df <- WI_intr_df[,c(1,2,5,10,65,64,66,67)]
colnames(WI_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PrecinctFull_area", "PREC_JOIN", "Intrsct_area", "prc_Intrsct_area")

PA_intr_df <- PA_intr_df[,c(1,2,5,10,28,27,29,30)]
colnames(PA_intr_df) <- c("STATEFP", "COUNTYFP", "BLOCKID", "BlockFull_area", 
                          "PrecinctFull_area", "PREC_JOIN", "Intrsct_area", "prc_Intrsct_area")
head(PA_intr_df)
all_intr <- rbind(AZ_intr_df, GA_intr_df, MI_intr_df, TX_intr_df, FL_intr_df, NC_intr_df, WI_intr_df, PA_intr_df)

head(all_intr)


#################################################################################
# create giant registered voters dataframe
# first need to rename MI VTD field to PREC_JOIN
#regMI$PREC_JOIN <- regMI$Precinct_VTD
#regMI$preName <- regMI$V21
#regTX$preName <- regTX$PRECNUM


head(regFL)
head(regTX)
head(regAZ)
head(regMI)
head(regGA)
head(regNC)
head(regWI)
head(regPA)

regAll <-  rbind(regTX[,c(2:20,28)], regAZ[,c(2:20,28)], regMI[,c(15:21,2:13,30)], regGA[,c(2:20,28)],
                 regFL[,c(15:21,2:13,1)], regNC[,c(15:21,2:13,1)], regWI[,c(2:20,28)], regPA[,c(2:20,28)])


# export registered voters dataframe to CSV for scripts 5-6
write.csv(regAll, paste0(root, "Indicator_Output/Reg_2016_ALLSTATES.csv"), row.names = FALSE)

### multiply proportion by registered voters
head(regAll)

# first join reg voters
all_intrReg <- merge(all_intr, regAll, by = "PREC_JOIN", all=TRUE, duplicateGeoms=T)
head(all_intrReg)
dim(all_intrReg); dim(all_intr); dim(regAll)

# QA fix: if the intersect area is less than .001m^2, convert to 0 along with prc_intersrct_area
all_intrReg$Intrsct_area <- ifelse(as.numeric(all_intrReg$Intrsct_area) < 0.001, 0.0, all_intrReg$Intrsct_area)
all_intrReg$prc_Intrsct_area <- ifelse(as.numeric(all_intrReg$Intrsct_area) == 0, 0.0, all_intrReg$prc_Intrsct_area)

# multiply to get proportional registration
all_intrReg$propReg <- all_intrReg$prc_Intrsct_area * all_intrReg$TOTAL_REGISTERED_VOTERS

# summarize by block
all_blockReg  <- data.frame(
  all_intrReg %>%
    as.data.frame() %>% 
    dplyr::group_by(BLOCKID) %>%
    dplyr::summarize(regBlockTot = sum(propReg, na.rm=T)))


head(all_blockReg)

# export registration stats at the block level--used to build the clusters
all_blockRegdf <-data.frame( 
  all_blockReg %>% 
    as.data.frame() %>% 
    dplyr::select(BLOCKID, regBlockTot)) 

head(all_blockRegdf)

# add fields for state and county FIPS
all_blockRegdf$STATEFP <- substr(all_blockRegdf$BLOCKID, 1, 2)
all_blockRegdf$COUNTYFP <- substr(all_blockRegdf$BLOCKID, 3, 5)

# create STATE abbreviation field
all_blockRegdf$STATE <-  ifelse(all_blockRegdf$STATEFP == "04", "AZ", 
                                  ifelse(all_blockRegdf$STATEFP == "06", "CA",
                                         ifelse(all_blockRegdf$STATEFP == "12", "FL",
                                                ifelse(all_blockRegdf$STATEFP == "13", "GA",
                                                       ifelse(all_blockRegdf$STATEFP == "26", "MI",
                                                              ifelse(all_blockRegdf$STATEFP == "37", "NC",
                                                                     ifelse(all_blockRegdf$STATEFP == "42", "PA",
                                                                            ifelse(all_blockRegdf$STATEFP == "48", "TX",
                                                                                   ifelse(all_blockRegdf$STATEFP == "55", "WI", "check")))))))))

#rearrange 
all_blockRegdf <- all_blockRegdf[,c(1,3,4,5,2)]
head(all_blockRegdf)
str(all_blockRegdf)


## Export registration blocks for model clusters
# split by state
all_blockRegdf_export <- split(all_blockRegdf, all_blockRegdf$STATE, drop = FALSE)

# loop through export
for (i in seq_along(all_blockRegdf_export)) {
  filename = paste("Reg_2016_", names(all_blockRegdf_export)[i], ".csv", sep = "")
  write.csv(all_blockRegdf_export[[i]], paste0(root, "Indicator_Output/", filename), row.names = FALSE)
}



######### Convert to Tracts ########
## Create Tract ID
all_blockRegdf$GEOID <- substr(all_blockRegdf$BLOCKID, 1, 11)  # extract the first digit of the block id

head(all_blockRegdf)

### Sum records by block group to get the voter totals per block group
all_tractReg  <- 
  all_blockRegdf %>%
  dplyr::group_by(GEOID, COUNTYFP, STATEFP, STATE) %>%   
  dplyr::summarize(reg = sum(regBlockTot, na.rm = T)) %>%
  as.data.frame()

head(all_tractReg); dim(all_tractReg)


######### CAlculate the Eligible non reg voter pop #########

### Merge CVAP with the averaged tract registration file in order to calculate 
### the number of people who are eligible to vote but are not registered
regCVAP <- full_join(all_tractReg, cvap[ ,c(1, 7:10)])

head(as.data.frame(regCVAP))
dim(regCVAP)
summary(regCVAP)
subset(regCVAP, is.na(GEOID))

# remove the one row with no geoid
regCVAP <- regCVAP[regCVAP$GEOID !="", ]

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

subset(regCVAP, is.na(Tot_EligNonReg_prc_FINAL))

#### Export finished tract data files to output folder ###
# keep only geoid, fips county, tot elig prc final, and reliability flag
# grab columns and split by state
regCVAPsubset <- regCVAP[,c(1,2,3,4,18,11)]
regCVAP_export <- split(regCVAPsubset, regCVAP$STATE, drop = FALSE)

# loop through export
for (i in seq_along(regCVAP_export)) {
  filename = paste("Elig_NonReg_Pop_Tracts", names(regCVAP_export)[i], ".csv", sep = "")
  write.csv(regCVAP_export[[i]], paste0(root, "Indicator_Output/", filename), row.names = FALSE)
}


# one for the visualization folder
for (i in seq_along(regCVAP_export)) {
  filename = paste("Elig_NonReg_Pop_Tracts", names(regCVAP_export)[i], ".csv", sep = "")
  write.csv(regCVAP_export[[i]], paste0(root, "Indicator_Output/visualize/", filename), row.names = FALSE)
}

# done
