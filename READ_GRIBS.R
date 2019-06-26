library(rNOMADS)
library(gdal)
library(here)
source('libraries.R')


"https://www.nco.ncep.noaa.gov/pmb/products/gfs/gfs.t00z.pgrb2.0p25.f006.shtml" %>% GET() %>% 
  htmlParse() %>% readHTMLTable()

info_grib<- GribInfo(here::here('Gribs/20190624/19062400.gfs.t00z.0p25.pgrb2f000'))
all_variables<- info_grib$inventory %>% str_split(":") %>% sapply(function(x) x[4]) %>% unique()
all_levels<- info_grib$inventory %>% str_split(":") %>% sapply(function(x) x[5]) %>% unique()

x<- ReadGrib(here::here('Gribs/20190624/19062400.gfs.t00z.0p25.pgrb2f000'),
             variables = all_variables,
             levels = all_levels)
