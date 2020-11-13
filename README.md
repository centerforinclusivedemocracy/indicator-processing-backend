# indicator-processing-backend
This set of R scripts gathers and processes census demographics and voter data to generate block-level inputs for the full model, along with tract-level outputs for displaying on the website.

## History
Laura Daly, while working at CID (formerly CCEP), wrote these R scripts to gather and process data for the original 5 counties that were analyzed. Later, they were updated to run the next set of 33 counties that were analyzed. She trained GreenInfo Network analysts in running the scripts, who then modified them to run for the 358 counties in the final analysis prior to election 2020.

in `/archive/` there are a set of folders which represent the original state of Laura's code as she transferred them to GreenInfo in July of 2020.

## Script Overview

**Script 0**  
`0.Data Set Up.R`  
Imports spreadsheet of counties we want to run indicator for


**Script 1**  
`1.ACS Data Prep AllStates GRC 0713.R`  

From Census API, building data frames of different indicators (Demos, car access, disabled).

**Script 2**  
`2.Worker Share All States Update`  

Worker density data - LODES calculations, live/work in same county. Manual download


**Script 3**  
`3.Block Population Density All States.R`  

Block SHP, filters to matched counties, read in population, output SHP
Identifies largest population density blocks


**Script 4**  
`4.elig nonReg All States.R`  
Joins Catalist voter data to precinct bounds and performs spatial intersect with blocks. Calculates Eligible/Non-Registered population


**Script 5**  
`5.vbm rates All States.R`  
Calculates vote by mail rate


**Script 6**  
`6.polling place All States.R`  
Share of Polling Place Voters. Assume that if someone isn't a mail voter, they voted in person.


**Script 7**
`7.Indicator Production All States.R`  
Assembles tract files for web display prep
Assembles block files for python pipeline input

## Script Diagrams
GreenInfo Network has developed diagrams which show how data flows and is calculated through the scripts. They are currently available only at this [private Google Drive link](https://drive.google.com/drive/folders/1xtRIxjWLgepcMO1arBe9eG-DL1O899qa).
