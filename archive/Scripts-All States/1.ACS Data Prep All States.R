# Gather Data For All Versions of the Siting Tool, 2020 Election Cycle

# California = 15 VCA Counties
# Colorado = 15 most populous counties + 1 Fremont County, added 6/15/20
# Texas = 1 most populous county 
# Arizona = 1 most populous county
library(dplyr)
library(scales)
library(data.table)
library(tidycensus)
library(purrr)
library(tidyverse)
library(spdep)
library(rgeos)
library(maptools)
library(rgdal) # load mapping libraries
library(raster)
library(sp)
library(sf)

# set file path
root <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/" 

# set output root
outputRoot <- "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/data/output/"

############# SET UP DATA ############# 
# Census api key:
# NEW USERS SHOULD GET THEIR OWN CENSUS API KEYS: https://api.census.gov/data/key_signup.html
# Google tidycensus to read more about how this library works/best practices
census_api_key("45c000b06fce43fbd4e7631396229ca66612bc9c", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")
 
#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties <-  read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), 
                          stringsAsFactors = FALSE, colClasses = c("FIPS"="character"))

head(siteCounties)


## Using tidycensus, load the variables for 2014-2018 acs
v18 <- load_variables(2018, "acs5", cache = TRUE)

mystates = c("CA", "CO", "AZ", "TX")

# Create a df that can interact with tidycensus
siteCounties$county_code = substr(siteCounties$FIPS, 3, 5) 

# use built-in dataset 'fips_codes' to select hte fips codes I want
my_counties <- 
  fips_codes %>%
  filter(state %in% mystates) # filter only with my state of interest

my_counties$FIPS <- paste0(my_counties$state_code, my_counties$county_code)
my_counties <-  my_counties %>% filter(FIPS %in% siteCounties$FIPS)

#
############# 0. Block Population Density (2010 Census) ####
# prepare block 2010 population data
dec <- load_variables(2010, "sf1", cache = TRUE)

options(tigris_use_cache = TRUE)

# just the data - not the block geometries 
# this takes a few minutes, so ideally you don't need to run it every time, just use the exported block population data.
popBlock <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_decennial(
    geography = "block",
    variables = "P001001", # total pouplation
    state = .x,
    county = .y,
    year = 2010,
    geometry = FALSE
  )
)

head(popBlock)

# export
write.csv(popBlock, paste0(root, "data/decennial/population_Block_2010_Decennial.csv"), row.names = FALSE)

############# 1. Calculate CVAP Population Share ############# 
# read in CVAP data # using the updated CVAP for the demographic components of the model
cvap = fread(paste0(root, "data/acs/source/CVAP_2014-2018_ACS_csv_files/Tract.csv"), data.table = FALSE)

# review
head(cvap)

# create county fips 
cvap$FIPS <- substr(cvap$geoid, 8, 12)
cvap$GEOID <- substr(cvap$geoid, 8, 18) # create tract geoid
head(cvap)

# Grab tracts in the counties of interest
cvap <- cvap[(cvap$FIPS %in% siteCounties$FIPS) & cvap$lntitle =="Total", ]

# split into list by county
cvapDens <- split(cvap, cvap$FIPS, drop = FALSE)

# calculate the share of the county's cvap population that is within the census tract
# this is why we have the list--to calculate the density by county (There are other ways to do this as well)
cvapDens <- lapply(cvapDens, function(x) within(x, cvapDens <- x$cvap_est/sum(x$cvap_est, na.rm=T))) # out of the county's total CVAP, this tract has X% 

# Calculate the margin of error and CV for this derived estimate
cvapDens <- lapply(cvapDens, function(x) within(x, cvapTot.MOE <- moe_sum(x$cvap_moe, x$cvap_est)))
cvapDens <- lapply(cvapDens, function(x) within(x, cvapDens.MOE <- moe_prop(x$cvap_est, sum(x$cvap_est), x$cvap_moe, x$cvapTot.MOE)))
cvapDens <- lapply(cvapDens, function(x) within(x, cvapDens.CV <- ((x$cvapDens.MOE/1.645)/x$cvapDens)*100))

# collapse list to one big df
cvapDensDF <- do.call("rbind", cvapDens)

# keep relevant columns only
cvapDensDF <- cvapDensDF[,c(10, 1, 9, 7,8, 11:14)]

head(cvapDensDF)
colnames(cvapDensDF)[2] <- "NAME" # rename name field

# we use the cv = 40% threshold for unreliability
dim(subset(cvapDensDF, cvapDens.CV > 40)) # there are 48 unreliable estimates for this indicator

# EXPORT
write.csv(cvapDensDF, paste0(root, "data/acs/CVAP_ShareOfCVAPPopulation.csv"), row.names = FALSE)


############# 2. % Limited English Proficient ############# 
# create list of variables
lepVars <- setNames(c(v18[v18$name %in% c("B06007_005", "B06007_008"), ]$name), 
                    c(rep("LEP", length(v18[v18$name %in% c("B06007_005", "B06007_008"), ]$name))))

# Get the LEP vars for al counties -- call specific counties, survey type and year, variables
lepTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = lepVars,
    state = .x,
    county = .y,
    summary_var = "B06007_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)

# calling this function takes a minute--all the tract level acs data--thats why I export to csv, so this data can be used later without having to re-run the ACS api calls
# export acs data
write.csv(lepTract, paste0(root, "data/acs/LEP.csv"), row.names = FALSE)



############# 3. % Vehicle Access #############
# NOTE I revised this 02/2020 to match the 2018 Siting Tool--Make variable "Car Access" and then invert in the index.

carVars <- setNames(c(v18[v18$name %like% "B25044_" & !v18$label %like% "No vehicle available" & 
                            !v18$name %in% c("B25044_001", "B25044_002", "B25044_009"), ]$name), 
                    c(rep("CarAccess", length(v18[v18$name %like% "B25044_" & !v18$label %like% "No vehicle available" & 
                                                !v18$name %in% c("B25044_001", "B25044_002", "B25044_009"), ]$name))))


# get lack of car data
carTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = carVars,
    state = .x,
    county = .y,
    summary_var = "B25044_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)


# export acs data
write.csv(carTract, paste0(root, "data/acs/NoCarAccess.csv"), row.names = FALSE)


############# 4. % Disabled Population ############# 
disabVars <- setNames(c(v18[v18$name %like% "B23024_" & (v18$label %like% "!!With a disability" & !v18$label %like% "!!With a disability!!"), ]$name), 
                      c(rep("disab", 2)))


# get disabled population data
disabTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = disabVars,
    state = .x,
    county = .y,
    summary_var = "B23024_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)


# export acs data
write.csv(disabTract, paste0(root, "data/acs/DisabledPop.csv"), row.names = FALSE)

############# 5. % Population in Poverty #############
povVars <- c(BelowPoverty ="B17001_002")


# get poverty population data
povTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = povVars,
    state = .x,
    county = .y,
    summary_var = "B17001_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)


# export acs data
write.csv(povTract, paste0(root, "data/acs/PovertyPop.csv"), row.names = FALSE)


############# 6. % Youth Population #############
youthVar = setNames(c("B01001_007", "B01001_008", "B01001_009", "B01001_010", "B01001_031", "B01001_032", "B01001_033", "B01001_034"), c(rep("youth", 8)))

# get youth population data
youthTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = youthVar,
    state = .x,
    county = .y,
    summary_var = "B01001_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)


# export acs data
write.csv(youthTract, paste0(root, "data/acs/YouthPop.csv"), row.names = FALSE)

############# 7. % Race and Ethnicity by Tract #####
raceVarList <- c(Latino = "B03002_012", WhiteNHL = "B03002_003",  BlackNHL = "B03002_004", AsianNHL = "B03002_006", NatAmNHL = "B03002_005", Total="B03001_001")


# get race/eth population data
raceEthTract <- map2_dfr(
  my_counties$state_code, my_counties$county_code,
  ~ get_acs(
    geography = "tract",
    variables = raceVarList,
    state = .x,
    county = .y,
    summary_var = "B03002_001",
    year = 2018,
    survey = "acs5",
    geometry = FALSE
  )
)


# export acs data
write.csv(raceEthTract, paste0(root, "data/acs/RaceEthPop.csv"), row.names = FALSE)


# split into list by variable
raceEthTractList <- split(raceEthTract, raceEthTract$variable, drop = FALSE)
head(raceEthTractList[[1]])


############# CALCULATE ALL ACS INDICATORS #####

# read in all the ACS tract variable here--time-consuming to reload the data from tidycensus
cvapDensDF = read.csv(paste0(root, "data/acs/CVAP_ShareOfCVAPPopulation.csv"), stringsAsFactors = FALSE, colClasses = c("FIPS"="character", "GEOID"="character"))
lepTract = read.csv(paste0(root, "data/acs/LEP.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
carTract = read.csv(paste0(root,"data/acs/NoCarAccess.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
disabTract = read.csv(paste0(root,"data/acs/DisabledPop.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
povTract = read.csv(paste0(root,"data/acs/PovertyPop.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
youthTract = read.csv(paste0(root,"data/acs/YouthPop.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
raceEthTract = read.csv(paste0(root, "data/acs/RaceEthPop.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))


# grab the total pop
totPopACS = raceEthTract[raceEthTract$variable=="Total", ]

# remove total pop from race/eth df because total pop/total pop doesn't make sense
raceEthTract = raceEthTract[raceEthTract$variable != "Total", ]

# split into list by variable
raceEthTractList <- split(raceEthTract, raceEthTract$variable, drop = FALSE)

# review: each df in the list has all counties, but they are separated by race/ethnicity. We'll use this later in the scropt
head(raceEthTractList[[2]])

# create list with all variables
acsVars = list(
  lep = lepTract, 
  car = carTract,
  disab = disabTract,
  pov = povTract,
  youth = youthTract
)

# append the race/eth list
acsVars <- append(acsVars, raceEthTractList)

# make column for county fips
acsVars <- lapply(acsVars, function(x) within(x, FIPS <- substr(x$GEOID, 1, 5)))

## MODEL DATA
# suummarize by indicator by tract -- some variables need to be collapsed/summed, for example the LEP variables
acsVarsSum_mod <-  
  lapply(acsVars, function(x) 
  x %>% 
    dplyr::group_by(GEOID, NAME, FIPS, variable) %>%
    dplyr::summarize(
      count = sum(estimate, na.rm = T),
      count.MOE = moe_sum(moe, estimate),
      univ = mean(summary_est),
      univ.MOE = mean(summary_moe),
      prc   = count/univ,
      prc.MOE = moe_prop(count, univ, count.MOE, univ.MOE),
      prc.CV  = ((prc.MOE/1.645)/prc)*100) %>% 
      mutate(flag = ifelse(prc.CV > 40 | is.na(prc.CV), 1, 0),
                  prc = ifelse(count == 0 | univ == 0, 0, prc)))  # THERE CAN BE NO NAs, REPLACE WITH ZERO
# if numerator = 0, then let the data show 0, if univ is 0 make NA. Either way the flag is most likely to be unreliable, but I want to show estimated (unreliable) 0% vs. NA (no universe est)

head(acsVarsSum_mod[[1]])


## WEB DATA--LET THERE BE NA. 
# note: right now the website converts NAs to 0s in the visualization (I was told). Maybe in the future this should be changed? I'll leave this here for now in case 
acsVarsSum_viz <-  lapply(acsVars, function(x) x %>%
                        dplyr::group_by(GEOID, NAME, FIPS, variable) %>%
                        dplyr::summarize(
                          count = sum(estimate, na.rm = T),
                          count.MOE = moe_sum(moe, estimate),
                          univ   = mean(summary_est),
                          univ.MOE = mean(summary_moe),
                          prc   = count/univ,
                          prc.MOE = moe_prop(count, univ, count.MOE, univ.MOE),
                          prc.CV  = ((prc.MOE/1.645)/prc)*100) %>% 
                        mutate(flag = ifelse(prc.CV > 40 | is.na(prc.CV), 1, 0),
                               prc = ifelse(count == 0 & univ !=0, 0,
                                            ifelse(univ == 0, NA, prc))))

head(acsVarsSum_viz[[1]])


# review
summary(acsVarsSum_mod[[4]]$prc)
summary(acsVarsSum_viz[[4]]$prc) # web visualization version has 54 NAs, model version has none (all converted to zero)


#### Prepare ACS Data -- eventually will Make data format wide, one record per tract : MODEL DATA #####
# rename columns so that the variable becomes part of the column name
acsVarsSum_mod <-  
  lapply(acsVarsSum_mod, function(x) 
  setnames(x, 
           old=c('prc', 'prc.CV', 'flag'), 
           new = c(paste0(x$variable[1], '.prc'), paste0(x$variable[1],'.CV'), paste0(x$variable[1], '_flag'))))

#### Prepare ACS Data -- eventually will Make wide, one record per tract : WEB VIZ DATA #####
# rename columns so that the variable becomes part of the column name
acsVarsSum_viz <-  
  lapply(acsVarsSum_viz, function(x) 
  setnames(x, 
           old=c('prc', 'prc.CV', 'flag'), 
           new = c(paste0(x$variable[1], '.prc'), paste0(x$variable[1],'.CV'), paste0(x$variable[1], '_flag'))))


head(acsVarsSum_mod[[4]])
head(acsVarsSum_viz[[4]])

# review--make sure that the _m (model) version has no NAs in the prc column and the _v (visual) version can have NAs
summary(acsVarsSum_mod[[4]])
summary(acsVarsSum_viz[[4]])


#just keep tract ID columns and the final prc and cv flag (don't keep cv col tho..no need)
acsVarsSum_mod <- lapply(acsVarsSum_mod, function(x) x[,c(1, 2, 3, 9, 12)])
acsVarsSum_viz <- lapply(acsVarsSum_viz, function(x) x[,c(1, 2, 3, 9,  12)])

# MAKE WIDE -- join all tracts together from each df, one column per indicator
acsVarsDF_mod  <- 
  acsVarsSum_mod %>% 
  reduce(full_join) %>%
  as.data.frame()

head(acsVarsDF_mod)

# MAKE WIDE -- join all tracts together from each df, one column per indicator
acsVarsDF_viz  <- 
  acsVarsSum_viz %>% 
  reduce(full_join) %>%
  as.data.frame()

head(acsVarsDF_viz)

# review
summary(acsVarsDF_mod)
summary(acsVarsDF_viz)

# Join in the tract-based CVAP density data
head(cvapDensDF)

# Add CV flag
cvapDensDF$cvapDens_flag <- ifelse(cvapDensDF$cvapDens.CV > 40, 1, 0)

# join to ACS vars and keep only necessary cols
acsVarsDFFinal_mod = full_join(acsVarsDF_mod, cvapDensDF[,c(1:3, 6, 10)])
acsVarsDFFinal_viz = full_join(acsVarsDF_viz, cvapDensDF[,c(1:3, 6, 10)])

tail(acsVarsDFFinal_mod)
head(acsVarsDFFinal_viz)

# join in total population data. This will be used when the block population isn't availalbe(new tracts?)
colnames(totPopACS)[4] <- "popACS"

acsVarsDFFinal_mod  = left_join(acsVarsDFFinal_mod, totPopACS[,c(1, 2, 4)])
acsVarsDFFinal_viz  = left_join(acsVarsDFFinal_viz, totPopACS[,c(1, 2, 4)])

# split by state in order to export 1 per state
# create short label

# model data
acsVarsDFFinal_mod$State = ifelse(acsVarsDFFinal_mod$NAME %like% ", Arizona", "AZ", 
                              ifelse(acsVarsDFFinal_mod$NAME %like% ", California", "CA",
                                     ifelse(acsVarsDFFinal_mod$NAME %like% ", Colorado", "CO",
                                            ifelse(acsVarsDFFinal_mod$NAME %like% ", Texas", "TX", "check"))))

# web visual
acsVarsDFFinal_viz$State = ifelse(acsVarsDFFinal_viz$NAME %like% ", Arizona", "AZ", 
                                  ifelse(acsVarsDFFinal_viz$NAME %like% ", California", "CA",
                                         ifelse(acsVarsDFFinal_viz$NAME %like% ", Colorado", "CO",
                                                ifelse(acsVarsDFFinal_viz$NAME %like% ", Texas", "TX", "check"))))

# split
acsVarsFinal_mod <- split(acsVarsDFFinal_mod, acsVarsDFFinal_mod$State, drop = FALSE)
acsVarsFinal_viz <- split(acsVarsDFFinal_viz, acsVarsDFFinal_viz$State, drop = FALSE)


#### Export finished tract data files to output folder ####

# export loop 
for (i in seq_along(acsVarsFinal_mod)) {
  filename = paste("ACS_Indicators_Tracts", names(acsVarsFinal_mod)[i], ".csv", sep = "")
  write.csv(acsVarsFinal_mod[[i]], paste0(outputRoot, filename), row.names = FALSE)
}

# one for the visualize folder
for (i in seq_along(acsVarsFinal_viz)) {
  filename = paste("ACS_Indicators_Tracts", names(acsVarsFinal_viz)[i], ".csv", sep = "")
  write.csv(acsVarsFinal_viz[[i]], paste0(outputRoot,"visualize/", filename), row.names = FALSE)
}

# done

