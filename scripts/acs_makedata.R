################################################################################
##
## FILE: Make data for County level median income
## NAME: acs_makedata.R
## AUTH: Will Doyle and Benjamin Skinner
## INIT: 30 October 2015
##
################################################################################

## PURPOSE --------------------------------------------------------------------
##
## Grab ACS Data and put it into a CSV file for use in map application
##
## -----------------------------------------------------------------------------

## clear memory
rm(list=ls())

## libraries
libs <- c('acs', 'dplyr', 'readr')
lapply(libs, require, character.only = TRUE)

## local directories
ddir <- '../data/'

## set ACS table name (median income)
acs_tab_name <- 'B22008'

## add ACS API key (see README.md on how to get it)
acs_key <- '<insert your ACS key here>'

## -----------------------------------------------------------------------------
## FUNCTION
## -----------------------------------------------------------------------------

getCountyACS <- function(endyear = 2012,         # end year of ACS pull
                         span = 5,               # length of span (1,3,5)
                         table_name = 'B22008',  # table name (char)
                         key) {                  # person ACS API key (char)

    ## message
    message('\nDownloading ACS data\n')

    ## make geographies
    all_county <- geo.make(state ="*", county = '*')

    ## pull table (ignore warnings about endyear; all is good)
    acstab <- acs.fetch(endyear = endyear,
                        span = 5,
                        geography = all_county,
                        table.number = table_name,
                        key = key)

    ## set values
    acstabgeo <- geography(acstab)
    acs_value <- as.numeric(estimate(acstab[,1]))
    acs_se <- as.numeric(standard.error(acstab[,1]))

    ## NOTE: Standard errors can be big. To fix this problem, we'll
    ## only use estimates where the ratio of estimate to SE is above
    ## a certain amount

    ## set target ratio; check ratio of SE to estimate
    target_value <- 2
    index <- (acs_value / acs_se)

    ## Plug in missing when SE is too big (larger than target)
    acs_value[index < target_value & is.na(index) == FALSE] <- NA

    ## Need full GEOIDS; to do this, will combine state, county and
    ## tract fips for a total of 11 digits:
    ##
    ## state = 2
    ## count = 3
    ## tract = 6

    ## format exiting values to string to make sure leading zeros
    acstabgeo$state <- sprintf('%02s', acstabgeo$state)
    acstabgeo$county <- sprintf('%03s', acstabgeo$count)
    
    ## create full geoid
    acs_fips <- paste0(acstabgeo$state, acstabgeo$county)
    
    ## make data frame; name columns 
    acs_data <- data.frame(acs_fips,acstabgeo$state,acstabgeo$county,acs_value)
    names(acs_data) <- c('fips','state','county','medinc')

    ## return data frame
    message('\nComplete!\n')
    return(acs_data)
}

## -----------------------------------------------------------------------------
## Get ACS data for all counties and write to disk
## -----------------------------------------------------------------------------

acs_medinc_2015 <- getCountyACS(endyear = 2012,
                                span = 5,
                                table_name = acs_tab_name,
                                key = acs_key)
## write output
write_csv(acs_medinc_2015, path = paste0(ddir, 'acs_data.csv'))

## -----------------------------------------------------------------------------
## END FILE
## =============================================================================
