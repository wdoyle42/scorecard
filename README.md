# Visualizing Results From College Scorecard Data

This project visualizes the data from the college report card, [https://collegescorecard.ed.gov/data/](https://collegescorecard.ed.gov/data/), using R.

## To create HTML page using RStudio

You can simply run `scorecard_county_map.Rmd` in RStudio using the `Knit HTML` button at the top of the screen.

## To create HTML page using R and `rmarkdown`

To create the HTML file from the primary script file, open an R session in the `./scripts` directory and use

```r,
rmarkdown::render('scorecard_county_map.Rmd')
```

## Required data

Aside from the files hosted in the repository, a few other larger data sources must be in the `./data/` folder:

* `MERGED2011_PP.csv` from the [College Scorecard Data](https://collegescorecard.ed.gov/data/)
* `gz_2010_us_050_00_500k.*` cartographic boundary files from the [U.S. Census Bureau](https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html). Download the 500k file from the page (or directly [from this link](http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_500k.zip)) and unzip all files in the `./data` directory


## Required R packages 

The following R packages and their dependencies are required:

```r,
libs <- c('dplyr',                 # make data wrangling easier
          'gstat',                 # kriging
          'geojsonio',             # to handle geojson data type
          'htmlwidgets',           # way to bind javascript libraries
          'knitr',                 # combine R code and markdown
          'leaflet',               # interactive mapping module
          'maptools',              # mapping projections
          'RColorBrewer',          # create color palettes
          'readr',                 # faster reading of large csv files
          'rgdal',                 # interact with gdal GIS framework
          'rgeos',                 # interact with geos GIS framework
          'tidyr')                 # make data wrangling easier
```

[`rmarkdown`](https://github.com/rstudio/rmarkdown) is also required to produce the final HTML file if you choose not to use RStudio.  

Note that the geospatial packages (e.g., `rgdal` and `rgeos`) may require the installation of geospatial libraries on your local machine. For OS X, many can be installed using prepackaged binaries [found here](http://www.kyngchaos.com/software/frameworks).
