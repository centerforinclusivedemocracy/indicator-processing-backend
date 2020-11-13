# Set up data
# Added Fremont County in CO on 6/15/20, Calaveras County in CA 04/2020

library(usmap)
library(ggplot2)
library(dplyr)
library(scales)
library(data.table)

# dir
root = "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"


#### Create list of Counties included in the siting tool ####
# create list of VCA counties with FIPS codes 

# Get list of CA counties with FIPS # calaveras county addd 04/2020
CACounties <-  c('Amador', 'Butte','El Dorado', 'Fresno','Los Angeles','Madera','Mariposa','Napa','Nevada','Orange','Sacramento', 
               'San Mateo','Santa Clara','Tuolumne','Calaveras')

site_ca <- data.frame(FIPS = fips(state="CA", county = CACounties), CountyName = CACounties, State="California")

# Get list of CO counties with FIPS
COCounties <- c('Denver',	'Arapahoe',	'El Paso',	'Jefferson',	'Adams',	'Boulder',	'Larimer',	'Weld',	'Douglas',	'Mesa',	'Pueblo',	
               'Garfield',	'La Plata',	'Broomfield',	'Eagle', 'Fremont')

# create list with fips codes
site_co <- data.frame(FIPS = fips(state = "CO", county = COCounties), CountyName = COCounties, State="Colorado")
head(site_co)

# create df with AZ county
site_az <- data.frame(FIPS = fips(state = "AZ", county = "Maricopa"), CountyName = "Maricopa", State = "Arizona" )

# create df with TX county
site_tx = data.frame(FIPS = fips(state = "TX", county = "Harris"), CountyName = "Harris", State="Texas" )

# combine all lists of siting counties
siteCounties = bind_rows(site_ca, site_co, site_az, site_tx)


# export list to use as source list of counties going forward
write.csv(siteCounties, paste0(root, "data/admin/Siting_Counties_MasterList.csv"), row.names = FALSE)
