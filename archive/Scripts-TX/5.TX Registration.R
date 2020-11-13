# TX 2016 registered voters by block
# used in initial clustering? 
library(foreign)
library(dplyr)
library(data.table)

root = "/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/"
TXroot ="/Users/lauradaly/Documents/GreenInfo/Contract Work/Siting Tool/Texas Siting Tool/data/"

#### READ IN HERE COUNTIES INCLUDED IN THE SITING TOOL 
siteCounties = read.csv(paste0(root, "data/admin/Siting_Counties_MasterList.csv"), stringsAsFactors = FALSE)
siteCounties$FIPS = sprintf("%05d", siteCounties$FIPS)
site_tx = siteCounties[siteCounties$State=="Texas",]

#### Prepare Data ####
# load 2016 reg data
gen16 = read.csv(paste0(TXroot, "voter/All_Voting_2016General-LD.csv"), stringsAsFactors = FALSE, colClasses = c("PCT..NUM."="character"))
gen16$ELECTION = "2016 General"
colnames(gen16)[1] <- "PRECINCT"


### load block-precinct conversion  
intr <- read.dbf(paste0(TXroot, "blocks_prec_conversion_TX.dbf"))
intr$GEOID10 <- as.character(intr$GEOID10)
colnames(intr)[4] <- "PRECINCT" # names get cut off in the dbf export
colnames(intr)[7] <- "prc_Intrsct_area"
head(intr)

#### Convert precincts to blocks #####
### multiply proportion by registered voters
# first join reg voters
intrReg = merge(intr, gen16[,c(1,2)], by="PRECINCT", all=TRUE)

# multiply to get proportional registration
intrReg$propReg = intrReg$prc_Intrsct_area * intrReg$REG.TOTAL

# summarize by block
blockReg <- 
  intrReg %>%
  group_by(GEOID10, NAME10) %>%
  summarize(regBlockTot = sum(propReg, na.rm=T))

blockReg$regBlockTotNum = as.numeric(blockReg$regBlockTot)
head(blockReg)

class(blockReg)
head(subset(blockReg, regBlockTotNum > 100))
summary(blockReg)

#### Export registration by block ####
# this is used in the initial cluster formation process
# GEOID10	FIPS	County	regBlockTot

blockRegexport <- 
  blockReg %>% 
  as.data.frame() %>% 
  mutate(FIPS = "48201",
         County = "Harris",
         regBlockTot = regBlockTotNum) %>% 
  dplyr::select(GEOID10, FIPS, County, regBlockTot) %>% 
  dplyr::filter(!is.na(GEOID10))

head(blockRegexport)
summary(blockRegexport)
# check for nas
subset(blockRegexport, is.na(GEOID10))

# export -- note this is a 2016 average for reg
write.csv(blockRegexport, paste0(root, "data/output/tx_Reg_2016.csv"))
