# load external sources
source("libraries.R")
source("globals.R")

COCounties <- c('Arapahoe',	'El Paso',	'Jefferson',	'Adams',	'Boulder',	'Larimer',	'Weld',	'Douglas',	'Mesa',	'Pueblo',	
                'Garfield',	'La Plata',	'Broomfield',	'Eagle', 'Fremont')

# create list with fips codes
site_co <- data.frame(FIPS = fips(state = "CO", county = COCounties), 
                      NAME = COCounties, 
                      State="Colorado",
                      GEOID=fips(state = "CO", county = COCounties))
# head(site_co)

# export list to use as source list of counties going forward
write.csv(site_co, paste0(root, "/Indicator_Inputs/Siting_Counties_MasterList_CO_test.csv"), row.names = FALSE)