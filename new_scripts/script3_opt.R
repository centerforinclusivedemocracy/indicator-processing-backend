# Block population density - we want to know where the most dense areas are for vote center siting

# load external sources
source("libraries.R")
source("globals.R")
options(scipen = 999) # disables scientific notation for the session

# Read in counties 
siteCounties <- read.csv(paste0(root, "/Indicator_output/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE, colClasses = c("FIPS"="character"))
siteCounties$COUNTYFP10 <- substr(siteCounties$FIPS, 3, 5)

## PREPARE BLOCK DATA -- reduce to only counties of interest, export reduced shapefiles #####
# read in original block data - I downloaded this block data manually (from the census), but you can also use tidycensus or other automated processes
# https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2010.html

blocksAZ <- read_sf(dsn=paste0(root, "/Indicator_Inputs/Census_Blocks"), layer="tl_2010_04_tabblock10")
blocksCO <- read_sf(dsn=paste0(root, "/Indicator_Inputs/Census_Blocks"), layer="tl_2010_08_tabblock10")
# blocksCA <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_06_Califonia_pophu") 
# blocksFL <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_12_Florida_pophu")
# blocksGA <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_13_Georgia_pophu")
# blocksMI <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_26_Michigan_pophu")
# blocksNC <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_37_NorthCarolina_pophu")
# blocksPA <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_42_Pennsylvania_pophu")
# blocksTX <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_48_Texas_pophu")
# blocksWI <- read_sf(dsn="P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks", layer="tabblock2010_55_Wisconsin_pophu")

# Grab only the counties we want - make sure the index is set to the right column after the state name

blocksAZ = blocksAZ[blocksAZ$COUNTYFP10 %in% siteCounties[siteCounties$State=="Arizona", 6], ]
blocksCO = blocksCO[blocksCO$COUNTYFP10 %in% siteCounties[siteCounties$State=="Colorado", 6], ]
# blocksCA = blocksCA[blocksCA$COUNTYFP10 %in% siteCounties[siteCounties$State=="California", 4], ]
# blocksFL = blocksFL[blocksFL$COUNTYFP10 %in% siteCounties[siteCounties$State=="Florida", 4], ]
# blocksGA = blocksGA[blocksGA$COUNTYFP10 %in% siteCounties[siteCounties$State=="Georgia", 4], ]
# blocksMI = blocksMI[blocksMI$COUNTYFP10 %in% siteCounties[siteCounties$State=="Michigan", 4], ]
# blocksNC = blocksNC[blocksNC$COUNTYFP10 %in% siteCounties[siteCounties$State=="North Carolina", 4], ]
# blocksPA = blocksPA[blocksPA$COUNTYFP10 %in% siteCounties[siteCounties$State=="Pennsylvania", 4], ]
# blocksTX = blocksTX[blocksTX$COUNTYFP10 %in% siteCounties[siteCounties$State=="Texas", 4], ]
# blocksWI = blocksWI[blocksWI$COUNTYFP10 %in% siteCounties[siteCounties$State=="Wisconsin", 4], ]

# ## Calculate area -- units are meters sq
# st_crs(blocksCA)

head(blocksCA)

blocksAZ$area_m2 = st_geod_area(blocksAZ)
blocksCO$area_m2 = st_geod_area(blocksCO)
# blocksCA$area_m2 = st_geod_area(blocksCA)
# blocksFL$area_m2 = st_geod_area(blocksFL)
# blocksGA$area_m2 = st_geod_area(blocksGA)
# blocksMI$area_m2 = st_geod_area(blocksMI)
# blocksNC$area_m2 = st_geod_area(blocksNC)
# blocksPA$area_m2 = st_geod_area(blocksPA)
# blocksTX$area_m2 = st_geod_area(blocksTX)
# blocksWI$area_m2 = st_geod_area(blocksWI)

head(blocksCA)

setwd(paste0(root, "/Indicator_Inputs/Census_Blocks/"))

#Export reduced & prepared shapefiles here:

write_sf(blocksAZ, "blocksAZ.shp")  
write_sf(blocksCO, "blocksCO.shp")  
# write_sf(blocksCA, "blocksCA.shp")
# write_sf(blocksFL, "blocksFL.shp")
# write_sf(blocksGA, "blocksGA.shp")
# write_sf(blocksMI, "blocksMI.shp")
# write_sf(blocksNC, "blocksNC.shp")
# write_sf(blocksPA, "blocksPA.shp")
# write_sf(blocksTX, "blocksTX.shp")
# write_sf(blocksWI, "blocksWI.shp")

crs(blocksCA)
head(blocksCA)

##### READ IN BLOCK dbfs HERE ####

blocksAZ <- read.dbf(paste0(root, "/Indicator_Inputs/Census_Blocks/blocksAZ.dbf"), as.is = TRUE)
blocksCO <- read.dbf(paste0(root, "/Indicator_Inputs/Census_Blocks/blocksCO.dbf"), as.is = TRUE)
# blocksCA <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksCA.dbf", as.is = TRUE)
# blocksFL <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksFL.dbf", as.is = TRUE)
# blocksGA <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksGA.dbf", as.is = TRUE)
# blocksMI <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksMI.dbf", as.is = TRUE)
# blocksNC <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksNC.dbf", as.is = TRUE)
# blocksPA <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksPA.dbf", as.is = TRUE)
# blocksTX <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksTX.dbf", as.is = TRUE)
# blocksWI <- read.dbf("P:/proj_a_d/CCEP/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Inputs/Census_Blocks/blocksWI.dbf", as.is = TRUE)

## read in block pop data
popBlock <- read.csv(paste0(root, "/Indicator_Output/decennial/population_Block_2010_Decennial_2.csv"), stringsAsFactors = FALSE, colClasses = c("GEOID"="character"))
colnames(popBlock)[1] <- "BLOCKID10"  # rename to match dbf col name
# This isn't right - blocksAZ data doesn't have BLOCKID10 field
popBlock$GEOID10 <- substring(popBlock$BLOCKID10,1,15)
head(popBlock)

# join block to block pop data 
blocksAZ <- left_join(blocksAZ, popBlock)
blocksCO <- left_join(blocksCO, popBlock)
# blocksCA <- left_join(blocksCA, popBlock)
# blocksFL <- left_join(blocksFL, popBlock)
# blocksGA <- left_join(blocksGA, popBlock)
# blocksMI <- left_join(blocksMI, popBlock)
# blocksNC <- left_join(blocksNC, popBlock)
# blocksPA <- left_join(blocksPA, popBlock)
# blocksTX <- left_join(blocksTX, popBlock)
# blocksWI <- left_join(blocksWI, popBlock)

# join block to block pop data 
# Create list
blocks <- list("blocksAZ" = blocksAZ,
               "blocksCO" = blocksCO)
# blocks <- list("blocksAZ" = blocksAZ,
#                "blocksCA" = blocksCA,
#                "blocksFL" = blocksFL,
#                "blocksGA" = blocksGA,
#                "blocksMI" = blocksMI,
#                "blocksNC" = blocksNC,
#                "blocksPA" = blocksPA,
#                "blocksTX" = blocksTX,
#                "blocksWI" = blocksWI)
# head(popBlock) 
# convert squared meters to squared km
blocks <- lapply(blocks, function(x) within(x, area_km2 <- x$area_m2/1000000))
blocks <- lapply(blocks, function(x) within(x, popDensM2  <- x$value/x$area_m2)) # calc pop density by sq m
blocks <- lapply(blocks, function(x) within(x, popDensKM2 <- x$value/x$area_km2)) # calc pop density by sq km

head(blocks[[1]])

# calculate the share of the county's block population that is in each block
blocks <- lapply(blocks, function(x) x %>% 
                   group_by(COUNTYFP10) %>%
                   mutate(popPrcCounty = value/sum(value)) %>%
                   as.data.frame())

head(blocks$blocksCO)


# export revised block dbfs
for (i in seq_along(blocks)) {
  filename = paste(names(blocks)[i], ".dbf", sep = "")
  write.dbf(blocks[[i]], paste0(decennialRoot, filename))
}

# Export finished block data files to output folder
for (i in seq_along(blocks)) {
  filename = paste("PopDensity_Block_", names(blocks)[i], ".csv", sep = "")
  write.csv(blocks[[i]], paste0(root, "/Indicator_Output/", filename), row.names = FALSE)
}

### Aggregate to tract (for display purposes on website only)
head(blocks$blocksCA)

blocksTract <- lapply(blocks, function(x) within(x, GEOID_Tract <- substr(x$BLOCKID10, 1, 11)))
head(blocksTract$blocksTX)
tail(blocksTract$blocksCA)

# Aggregate values by tract ID
blocksTract <- lapply(blocksTract, function(x) x %>%
                        group_by(GEOID_Tract, COUNTYFP10) %>%
                        summarize(pop = sum(value),
                                  area_m2 = sum(area_m2),
                                  area_km2 = sum(area_km2),
                                  popDensM2 = pop/area_m2,
                                  popDensKM2 = pop/area_km2))

head(blocksTract$blocksAZ)

# calculate the share of the county's block population that is in each block -- looking for the distribution of people in the county, not just dense blocks
blocksTract <- lapply(blocksTract, function(x) x %>% 
                        group_by(COUNTYFP10) %>%
                        mutate(popPrcCounty = pop/sum(pop)) %>%
                        as.data.frame())

head(blocksTract$blocksAZ)
tail(blocksTract$blocksTX)

## Export as CSV
# export revised block dbfs
for (i in seq_along(blocksTract)) {filename = paste("PopDensity_Tract_", names(blocksTract)[i], ".csv", sep = "")
write.csv(blocksTract[[i]], paste0(root, "/Indicator_Output/visualize/", filename), row.names = FALSE)}