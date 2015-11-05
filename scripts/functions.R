################################################################################
## FUNCTIONS
################################################################################

instPointsSpatial <- function(dat,                  # merged score data
                              outcome_var,          # outcome variable
                              proj,                 # projection to use
                              save.file = TRUE      # option to save result
                              ) {

    ## pop longitude and latitude from data frame
    dat <- dat %>% filter(!is.na(outcome_var))
    inst_points <- data.frame(dat %>% select(longitud,latitude))
    dat <- data.frame(dat %>% select(-latitude, -longitud))
    
    ## creates a spatial points data frame using long and lat from IPEDS
    inst_sp <- SpatialPointsDataFrame(coords = inst_points,
                                      data = dat, 
                                      proj4string = proj)

    ## save spatial data frame
    filename <- paste0(mdir, 'institutional_points.geojson')
    if(save.file) {
        geojson_write(inst_sp, file = filename)
    }
    
    ## return
    return(inst_sp)
}

krigeMap <- function(ddir,                          # data directory
                     dat,                           # merged scorecard data
                     inst_sp,                       # institution spatial
                     acsdat,                        # acs data
                     outcome_var,                   # variable to krige
                     select_group,                  # which colleges to use
                     groupname,                     # text group name for file
                     sf,                            # shapefile
                     stfipsvec,                     # vector of state fips
                     proj = CRS('+init=epsg:4269'), # projection to use
                     save.file = TRUE               # option to save result
                     ) {

    ## ////////////////////////////////////////////////////////////////////
    ## This function will krige the outcome data and return a spatial
    ## object with the proper data. It takes a long time to run so it
    ## defaults to save the output, which can then be called the next time.
    ## The save may be overwritten by switching save.file to FALSE.
    ## ////////////////////////////////////////////////////////////////////
    
    ## =================================
    ## SET UP DATA
    ## =================================

    ## drop missing observations
    dat <- dat %>% filter(!is.na(outcome_var), group %in% select_group)
    
    ## include only selected institutions
    inst_sp <- inst_sp[inst_sp@data$group %in% select_group,]
    
    ## set outcomevar
    inst_sp@data$outcome_var <- as.numeric(unlist(inst_sp@data[outcome_var]))
    
    ## only include complete cases
    inst_sp <- inst_sp[is.na(inst_sp@data$outcome_var) == FALSE,]
    
    ## get appropriate shapefile
    county_shape <- readOGR(dsn = ddir,
                            layer = sf,
                            verbose = FALSE)

    ## lower names in attached spatial data
    names(county_shape@data) <- tolower(names(county_shape@data))

    ## drop unnecessary geographic variables to reduce file size
    cols <- c('state','county','name')
    county_shape@data <- county_shape@data[,cols]
    
    ## subset to 50 states
    county_shape <- county_shape[county_shape@data$state %in% stfipsvec,]

    ## create fips from state and county (assumes they are characters)
    county_shape@data$fips <- paste0(county_shape@data$state,
                                     county_shape@data$county)

    ## add ACS data
    acsdat$fips <- sprintf('%05s', acsdat$fips)
    county_shape@data <- data.frame(county_shape@data,
                                    acsdat[match(county_shape@data$fips,
                                                 acsdat$fips),])

    ## drop redundant columns
    county_shape@data$fips.1 <- NULL
    county_shape@data$state.1 <- NULL
    county_shape@data$county.1 <- NULL
  
    ## correct projection: epsg:4269
    county_shape <- spTransform(county_shape, proj)

    ## simplify to reduce file size
    simple <- gSimplify(county_shape, tol = 0.001, topologyPreserve = TRUE)

    ## need to return spatial dataframe
    county_shape <- SpatialPolygonsDataFrame(simple, data = county_shape@data)

    ## reformat FIPS codes so they have leading zeros
    dat$stfips <- sprintf('%02s', dat$stfips)

    ## recorrect projection: epsg:4269
    county_shape <- spTransform(county_shape, proj)

    ## =================================
    ## KRIGE OVER AREAS
    ## =================================

    ## inverse distance weighted interpolation
    idw.fit <- idw(outcome_var ~ 1,
                   locations = inst_sp,
                   newdata = county_shape,
                   na.action = na.pass)

    ## =================================
    ## MAKE/MERGE PREDICTIONS
    ## =================================
    
    ## pull predictions, round, and make integer
    idw.fit.preds <- data.frame(as.integer(round(idw.fit$var1.pred)))
    
    ## get ids and make them rownames
    row.names(idw.fit.preds) <- as.character(county_shape$fips)
    row.names(county_shape) <- as.character(county_shape$fips)
    
    ## column names
    names(idw.fit.preds) <- 'outcome_var'
        
    ## merge back into data frame
    county_shape <- spCbind(county_shape, idw.fit.preds)

    ## =================================
    ## MISCELLANEOUS CLEANUP
    ## =================================

    ## convert to numeric
    county_shape@data$medinc <- as.numeric(county_shape@data$medinc)

    ## create percentage of county median income data
    county_shape@data$pctvar <- county_shape@data$outcome_var /
        county_shape@data$medinc
    
    ## state names 
    county_shape@data$stfips <- sprintf("%02s", county_shape@data$state)
    county_shape@data <- left_join(county_shape@data, state_fips, by='stfips')
    
    ## =================================
    ## SAVE
    ## =================================

    ## map data file ./dir/name
    mapname <- paste0(mdir, 'county_shape_', outcome_var, '_',
                      groupname, '_', year, '.geojson' )
    if(save.file) {
        geojson_write(county_shape, file = mapname)
    }

}
  
mapSC <- function(countylayer,          # county map data
                  instlayer,            # institution points layer
                  var_describe,         # description for map
                  outcome_var           # outcome variable in text
                  ) {

    ## ////////////////////////////////////////////////////////////////////
    ## This function produces a leaflet map from the spatial objects
    ## ////////////////////////////////////////////////////////////////////

    ## drop Alaska and Hawaii and etc.
    dropfips <- c('02','03','14','15','43','52','64','72')
    countylayer <- countylayer[!countylayer@data$state %in% dropfips,]
  
    ## pull right variable from instlayer
    instlayer@data$outcome_var <- as.numeric(unlist(instlayer@data[outcome_var]))
  
    ## =================================
    ## SET COLORS
    ## =================================

    ## read in diverging palette
    my.colors <- brewer.pal(11,'RdYlGn')

    ## create color ramp (red == bad ---> green == good)
    my.pal <- colorRampPalette(colors = c(my.colors[1],
                                          my.colors[6],
                                          my.colors[11]))(10)
    
    ## income palette
    inc_pal <- colorQuantile(pal = my.pal,
                             county_shape@data$medinc,
                             n = 10)
    
    ## attendee palette
    attend_pal <- colorQuantile(pal = my.pal,
                                county_shape@data$outcome_var,
                                n = 10)
    ## percent palette
    pct_pal <- colorQuantile(pal = my.pal,
                             county_shape@data$pctvar,
                             n = 10)
    
    ## =================================
    ## ADJUST LABELS
    ## =================================
    
    ## add break and colon to variable description for popup
    var_describe <- paste0('<br/>', var_describe, ': $')
     
    ## =================================
    ## CREATE POPUPS
    ## =================================

    ## county layer popup
    county_popup <- paste0(paste0(countylayer@data$name, ' County, '),
                           countylayer@data$stabbr,
                           var_describe, 
                           prettyNum(countylayer@data$outcome_var,
                                     big.mark = ',',
                                     scientific = FALSE,
                                     digits = 0),
                           '<br/> County median income: $',
                           prettyNum(countylayer@data$medinc,
                                     big.mark = ',',
                                     scientific = FALSE,
                                     digits = 0),
                           '<br/> Attendee income as % of county income: ',
                           percent(countylayer@data$pctvar)
                           )

    ## institutional object popup
    inst_popup <- paste0(as.character(instlayer@data$instnm.x),
                         var_describe,
                         prettyNum(instlayer@data$outcome_var,
                                   big.mark = ',',
                                   scientific = FALSE,
                                   digits = 0)
                        )
                             
    ## set bounding box for plotting
    b <- as.vector(bbox(countylayer))
    
    ## expands limits slighlty
    b[1] <- b[1] * 1.03
    b[2] <- b[2] - (b[2] * .03)
    b[3] <- b[3] - (b[3] * .03)
    b[4] <- b[4] * 1.03
   
    ## =================================
    ## CREATE LEAFLET MAP
    ## =================================
    
    county_map <- leaflet(countylayer) %>%
        ## overlay county median income    
        addPolygons(stroke = TRUE,
                    opacity = .25,
                    fill = TRUE,
                    color = ~inc_pal(countylayer@data$medinc),
                    fillOpacity = .65,
                    popup = county_popup,
                    group = 'County Median Income'
                    ) %>%
        ## overlay county earnings of attendees    
        addPolygons(stroke = TRUE,
                    opacity = .25,
                    fill = TRUE,
                    color = ~attend_pal(countylayer@data$outcome_var),
                    fillOpacity = .65,
                    popup = county_popup,
                    group = 'Income of Attendees'
                    ) %>%
        ## overlay percentage measure   
        addPolygons(stroke = TRUE,
                    opacity = .25,
                    fill = TRUE,
                    color = ~pct_pal(countylayer@data$pctvar),
                    fillOpacity = .65,
                    popup = county_popup,
                    group = 'Attendee Income as a % of County Income'
                    ) %>%
        ## legend
        addLegend('bottomleft',
                  pal = attend_pal,
                  title = 'Percentiles',
                  values = countylayer@data$outcome_var
                  ) %>%
        ## overlay instititions
        addMarkers(lng = instlayer@coords[,1],
                   lat = instlayer@coords[,2],
                   icon = schoolIcon,
                   pop = inst_popup,
                   group = 'Institutions'
                   ) %>%
        ## layer control for institutions
        addLayersControl(
            overlayGroups = c('Institutions',
                              'County Median Income',
                              'Income of Attendees',
                              'Attendee Income as a % of County Income'
                              ),
            options = layersControlOptions(collapsed = FALSE)
        ) %>%
        ## leave institutions off by default  
        hideGroup('Institutions') %>%
        hideGroup('County Median Income') %>%
        hideGroup('Attendee Income as a % of County Income')%>%
      
        ## add tiles        
        addProviderTiles('Stamen.TonerLite',
                         options = list(minZoom = 4,
                                        zoom = 4,
                                        maxZoom = 13)
                                                  ) %>%
        ## set boundaries
        setMaxBounds(lng1 = b[1],
                     lat1 = b[2],
                     lng2 = b[3],
                     lat2 = b[4]) 

    ## =================================
    ## RETURN MAP
    ## =================================
    
    return(county_map)
}


