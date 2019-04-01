install.packages("RNetCDF")
install.packages("stringr")
library(RNetCDF)
library(stringr)

dir_path_spain<- "/usr1/uems/runs/spain1"
folders_spain<- list.dirs(path = dir_path_spain)
netcdf_folder<- folders_spain[str_detect(folders_spain, "wrfprd")]
netcdf_files<- list.files(netcdf_folder, full.names = T)

NC_prueba<- open.nc(netcdf_files[2])

lons=var.get.nc(NC_prueba,"XLONG",unpack=TRUE)
lats=var.get.nc(NC_prueba,"XLAT",unpack=TRUE)


i<-1:nrow(lons)
j<-1:ncol(lons)
k<-1:(ncol(lons)*nrow(lons))
LONS_COL<-1:(ncol(lons)*nrow(lons))
LONS_COL[k]<-lons[i,j]
LATS_COL<-1:(ncol(lons)*nrow(lons))
LATS_COL[k]<-lats[i,j]



#"ACCUMULATED TOTAL CUMULUS PRECIPITATION"  
RAINC<- var.get.nc(NC_prueba, "RAINC", unpack = TRUE)   
#"ACCUMULATED TOTAL GRID SCALE PRECIPITATION"
RAINNC<- var.get.nc(NC_prueba, "RAINNC", unpack = TRUE)
#ACCUMULATED SHALLOW CUMULUS PRECIPITATION
RAINNC<- var.get.nc(NC_prueba, "RAINSH", unpack = TRUE)


#ACCUMULATED SNOW
ACSNOW<- var.get.nc(NC_prueba, "ACSNOW", unpack = TRUE)
#ACCUMULATED TOTAL GRID SCALE SNOW AND ICE
SNOWNC<- var.get.nc(NC_prueba, "SNOWNC", unpack = TRUE)
#FLAG INDICATING SNOW COVERAGE (1 FOR SNOW COVER)"
SNOWC<- var.get.nc(NC_prueba, "SNOWC", unpack = TRUE)


#"Minimum Shelter Temperature"
T02_MIN<- var.get.nc(NC_prueba, "T02_MIN", unpack = TRUE)
#"Maximum Shelter Temperature"
T02_MAX<- var.get.nc(NC_prueba, "T02_MAX", unpack = TRUE)
#"Mean Shelter Temperature" 
T02_MEAN<- var.get.nc(NC_prueba, "T02_MEAN", unpack = TRUE)

#U wind component of maximum 10 M wind speed
V10_MAX<- var.get.nc(NC_prueba, "V10_MAX", unpack = TRUE)
#U wind component of maximum 10 M wind speed
U10_MAX<- var.get.nc(NC_prueba, "U10_MAX", unpack = TRUE)

#Maximum 10 M wind speed"
S10_MAX<- var.get.nc(NC_prueba, "S10_MAX", unpack = TRUE)
#"Maximum 10 M wind gust speed"
G10_MAX<- var.get.nc(NC_prueba, "G10_MAX", unpack = TRUE)
#"Instantaneous 10 M wind gust speed potential
GUST10M<- var.get.nc(NC_prueba, "GUST10M", unpack = TRUE)

#Mean 10 M wind speed between output times" ;
S10_MEAN<- var.get.nc(NC_prueba, "S10_MEAN", unpack = TRUE)
#V wind component of mean 10 M wind speed" ;
V10_MEAN<- var.get.nc(NC_prueba, "V10_MEAN", unpack = TRUE)
#U wind component of mean 10 M wind speed" ;
U10_MEAN<- var.get.nc(NC_prueba, "U10_MEAN", unpack = TRUE)







Tabla<-data.frame(LONS_COL,LATS_COL, RAINC[i,j][k], RAINNC[i,j][k], U10_MEAN[i,j][k], U10_MAX[i,j][k],
                  V10_MEAN[i,j][k], V10_MAX[i,j][k],S10_MEAN[i,j][k],GUST10M[i,j][k],G10_MAX[i,j][k],T02_MEAN[i,j][k],
                  T02_MAX[i,j][k],T02_MIN[i,j][k],SNOWC[i,j][k],
                  SNOWNC[i,j][k],ACSNOW[i,j][k])




lonsU<- var.get.nc(NC_prueba, "XLONG_U", unpack = TRUE)
latsU<- var.get.nc(NC_prueba, "XLAT_U", unpack = TRUE)
i_u<-1:nrow(lonsU)
j_u<-1:ncol(lonsU)
k_u<-1:(ncol(lonsU)*nrow(lonsU))
LONS_COL_u<-1:(ncol(lonsU)*nrow(lonsU))
LONS_COL_u[k_u]<-lonsU[i_u,j_u]
LATS_COL_u<-1:(ncol(lonsU)*nrow(lonsU))
LATS_COL_u[k_u]<-latsU[i_u,j_u]

lonsV<- var.get.nc(NC_prueba, "XLONG_V", unpack = TRUE)
latsV<- var.get.nc(NC_prueba, "XLAT_V", unpack = TRUE)
i_v<-1:nrow(lonsV)
j_v<-1:ncol(lonsV)
k_v<-1:(ncol(lonsV)*nrow(lonsV))
LONS_COL_v<-1:(ncol(lonsV)*nrow(lonsV))
LONS_COL_v[k_v]<-lonsV[i_v,j_v]
LATS_COL_v<-1:(ncol(lonsV)*nrow(lonsV))
LATS_COL_v[k_v]<-latsV[i_v,j_v]




Bottom_Top_ZNU<- var.get.nc(NC_prueba, "ZNU", unpack = TRUE)
Bottom_Top_STAG_ZNW<- var.get.nc(NC_prueba, "ZNW", unpack = TRUE)

U_comp<-  var.get.nc(NC_prueba, "U", unpack = TRUE)
V_comp<-  var.get.nc(NC_prueba, "V", unpack = TRUE)


Tabla_U_level<-data.frame(LONS_COL_u,LATS_COL_u, U_comp[i_u,j_u,40][k_u])
Tabla_V_level<-data.frame(LONS_COL_v,LATS_COL_v, V_comp[i_v,j_v,40][k_v])

