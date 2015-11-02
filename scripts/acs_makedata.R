
library(acs)

acs_tab_name <- 'B22008'

acskey <- 'b27a265fe0dc7c49bd9281d6bc778637f10685e3'
## ------------------------------------------------------
## get ACS data for state
## ------------------------------------------------------

## message
message('Downloading ACS data')

## make geographies
all_county <- geo.make(state ="*", county = '*')

year=2012
## pull table (ignore warnings about endyear; all is good)
acstab <- acs.fetch(endyear = year,
                    span = 5,
                    geography = all_county,
                    table.number = acs_tab_name, key = acs_key)

## set values
acstabgeo <- geography(acstab)
acs_value <- as.numeric(estimate(acstab[,1]))
acs_se <- as.numeric(standard.error(acstab[,1]))

## NOTE: Standard errors can be big. To fix this problem, we'll
## only use estimates where the ratio of estimate to se is above
## a certain amount

## set target ratio; check ratio of SE to estimate
target_value <- 2
index <- (acs_value / acs_se)

## Plug in missing when se is too big
## (larger than target percent of estimate)
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
acs_data <- data.frame(acs_geoid,acstabgeo$state,acstabgeo$county, acs_value)
names(acs_data) <- c("fips","state","county",'income')

write_csv(acs_data,path="../data/acs_data.csv")
