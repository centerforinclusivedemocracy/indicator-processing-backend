# Can we chain a series of .Rmds to output all the QA checks from the scripts?
# assertthat / assertive

# Global
# set file path
root <- "C:/Users/stetkiew/Desktop/CID/EXPANSION_Vote_Center_Siting_Tool/data" 

# set output root
outputRoot <- "C:/Users/stetkiew/Desktop/CID/EXPANSION_Vote_Center_Siting_Tool/data/Indicator_Output"

# Script 1
acs_map <- function (variable, summaryVar) {
  map2_dfr(
    my_counties$state_code, my_counties$county_code,
    ~ get_acs(
      geography = "tract",
      variables = variable,
      state = .x,
      county = .y,
      summary_var = summaryVar,
      year = 2018,
      survey = "acs5",
      geometry = FALSE
    )
  )
}

flag_type <- c('flag_zero', 'flag_na')
tract_sum <- function(flag_type){
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
             prc = if(flag_type == 'flag_zero') {ifelse(count == 0 | univ == 0, 0, prc)} 
                    else {ifelse(count == 0 & univ !=0, 0, ifelse(univ == 0, NA, prc))}
             )
  )
}

ifelse_chain <- function (data_source) {
  ifelse(data_source$NAME %like% ", Arizona", "AZ",
         ifelse(data_source$NAME %like% ", California", "CA",
                ifelse(data_source$NAME %like% ", Florida", "FL",
                       ifelse(data_source$NAME %like% ", Georgia", "GA",
                              ifelse(data_source$NAME %like% ", Michigan", "MI",
                                     ifelse(data_source$NAME %like% ", North Carolina", "NC",
                                            ifelse(data_source$NAME %like% ", Pennsylvania", "PA",
                                                   ifelse(data_source$NAME %like% ", Texas", "TX",
                                                          ifelse(data_source$NAME %like% ", Wisconsin", "WI", "check")))))))))
}
