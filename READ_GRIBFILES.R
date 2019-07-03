library(rNOMADS)
library(here)
source('libraries.R')

'
PARA EL PÁQUETE NOMADS HAY QUE TENER EN CUENTA QUE ES NECESARIO
PODER COMPLIAR WGRIB2, PARA ELLO HAY QUE DESCARGARLO 

ftp://ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz
http://www.ftp.cpc.ncep.noaa.gov/wd51we/wgrib2/wgrib2.tgz


EXTRAERLO
>> tar -xvf wgrib2.tar


ENTRAR A LA CARPETA grib2
>> cd grib2

ELEGIR EL COMPILADOR C (DEPENDIENDO DEL KERNEL? K USEMOS)
sh/bash:    >> export CC=gcc
csh/tcsh:   >> setenv CC gcc

Y EJECUTARLO USANDO MAKEFILE

linux:      >> make
MacOS:      >> gmake
Unix:       ???    ask system administrators

'

x<- rNOMADS::GribInfo(here::here('Gribs/20190620/gfs.t00z.pgrb2.0p25.f000'))

urls.out <- CrawlModels(abbrev = "gfs_0p25")

x<- rNOMADS::ArchiveGribGrab(abbrev = "gfs_0p25", )



library(rgdal)

x_gdal<- readGDAL(here::here('Gribs/20190620/2019062019062000.gfs.t00z.0p25.pgrb2f000'))
x_info<- GDALinfo(here::here('Gribs/20190620/2019062019062000.gfs.t00z.0p25.pgrb2f000'))
