#install.packages("RNetCDF")
#install.packages("stringr")
#install.packages("lubridate")

library(lubridate)
library(RNetCDF)
library(stringr)


dir_path_spain<- "/usr1/uems/runs/spain1"
dir_path_europe<- "/usr1/uems/runs/europe1/"
folders_spain<- list.dirs(path = dir_path_spain)
folders_europe<- list.dirs(path=dir_path_europe)

netcdf_folder_spain<- folders_spain[str_detect(folders_spain, "wrfprd")]
netcdf_files-spain<- list.files(netcdf_folder_spain, full.names = T)

netcdf_folder_europe<- folders_europe[str_detect(folders_europe, "wrfprd")]
netcdf_files_europe<- list.files(netcdf_folder_europe, full.names = T)


netcdf_files<- netcdf_files_europe

netcdf_list<- list()
vec_time<- vector()
for (n_files in 1:length(netcdf_files)) {
  NC_prueba<- open.nc(netcdf_files[n_files])
  
  
  lons<-var.get.nc(NC_prueba,"XLONG",unpack=TRUE)
  lats<-var.get.nc(NC_prueba,"XLAT",unpack=TRUE)
  
  
  i<-1:nrow(lons)
  j<-1:ncol(lons)
  k<-1:(ncol(lons)*nrow(lons))
  LONS_COL<- 1:(ncol(lons)*nrow(lons))
  LATS_COL<- 1:(ncol(lons)*nrow(lons))
  LONS_COL[k]<-lons[i,j]
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
  LONS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
  LATS_COL_u<- 1:(ncol(lonsU)*nrow(lonsU))
  LONS_COL_u[k_u]<-lonsU[i_u,j_u]
  LATS_COL_u[k_u]<-latsU[i_u,j_u]
  
  lonsV<- var.get.nc(NC_prueba, "XLONG_V", unpack = TRUE)
  latsV<- var.get.nc(NC_prueba, "XLAT_V", unpack = TRUE)
  i_v<-1:nrow(lonsV)
  j_v<-1:ncol(lonsV)
  k_v<-1:(ncol(lonsV)*nrow(lonsV))
  LONS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
  LATS_COL_v<- 1:(ncol(lonsV)*nrow(lonsV))
  LONS_COL_v[k_v]<-lonsV[i_v,j_v]
  LATS_COL_v[k_v]<-latsV[i_v,j_v]
  
  
  Bottom_Top_ZNU<- var.get.nc(NC_prueba, "ZNU", unpack = TRUE)
  Bottom_Top_STAG_ZNW<- var.get.nc(NC_prueba, "ZNW", unpack = TRUE)
  
  U_comp<-  var.get.nc(NC_prueba, "U", unpack = TRUE)
  V_comp<-  var.get.nc(NC_prueba, "V", unpack = TRUE)
  
  
  Tabla_U_level<-data.frame(LONS_COL_u,
                            LATS_COL_u, 
                            U_comp[i_u,j_u,40][k_u])
  Tabla_V_level<-data.frame(LONS_COL_v,
                            LATS_COL_v, 
                            V_comp[i_v,j_v,40][k_v])
  
  
  time<-  var.get.nc(NC_prueba, "XTIME", unpack = TRUE)
  time1<- as.data.frame(utcal.nc("minutes since 2019-04-01 00:00:00", time))
  time2<- paste(time1$year,"-",time1$month,"-",
                 time1$day," ",time1$hour,"-",
                 time1$minute,"-",time1$second, sep = '')
  time3<- ymd_hms(time2)
  
  list_ch<- list(Tabla,
                 Tabla_V_level,
                 Tabla_U_level,
                 Bottom_Top_ZNU,
                 Bottom_Top_STAG_ZNW)
  names(list_ch)<- c("Variable", "V","U", "ZNU", "ZNW")
  
  netcdf_list[[n_files]]<- list_ch 
  vec_time[n_files]<- as.character(time3)
  
}


names(netcdf_list)<- vec_time

