library(here)
source(here::here('libraries.R'))


dir_path_spain<- "/usr1/uems/runs/spain1"
dir_path_europe<- "/usr1/uems/runs/europe1/"
folders_spain<- list.dirs(path = dir_path_spain)
folders_europe<- list.dirs(path=dir_path_europe)

netcdf_folder_spain<- folders_spain[str_detect(folders_spain, "wrfprd")]
netcdf_files_spain<- list.files(netcdf_folder_spain, full.names = T)

netcdf_folder_europe<- folders_europe[str_detect(folders_europe, "wrfprd")]
netcdf_files_europe<- list.files(netcdf_folder_europe, full.names = T)





#Esta funcion crea una lista con todas la variables que nos interesan
list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)

list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)



# Europe pressure and Temperature ---------------------------------------------------------

vec_days_str<- names(list_europe)
vec_days<- which(str_detect(vec_days_str, "14:00:00"))

for (days in vec_days){
  CSV_Europe<- list_europe[[days]]$Variable[ ,c('lon', 'lat', 
                                                              'T02_MAX',
                                                              'T02_MIN',
                                                              'HGT',
                                                              'PSFC')]
  CSV_Europe$Presion<- (CSV_Europe$PSFC/100)*(1-(0.0065*CSV_Europe$HGT)/((CSV_Europe$T02_MAX-273.15)+0.0065*CSV_Europe$HGT+273.15))^(-5.257)
  CSV_Europe$T02_MAX<- NULL
  CSV_Europe$HGT<- NULL
  CSV_Europe$PSFC<- NULL
  CSV_Europe$T02_MIN<- CSV_Europe$T02_MIN-273.15 
  
  colnames(CSV_Europe)<- c("lon","lat","Temp","Presion")
  nombre<- paste0("España_2019-04-03") 
  
  nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
  nombre_ent<- paste0("europe_",nombre[1])
  
  
  write.table(CSV_europe, here::here(paste0(nombre_ent,'.CSV')), 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
  
}


# España temperatura ------------------------------------------------------


vec_days_str<- names(list_espana)
vec_days<- which(str_detect(vec_days_str, "14:00:00"))

for (days in vec_days){
  CSV_Espana<- list_espana[[days]]$Variable[ ,c('lon', 'lat','T02_MAX')]
  CSV_Espana$T02_MAX<- CSV_Espana$T02_MAX-273.15 
  
  colnames(CSV_Espana)<- c("lon","lat","Temp")
  nombre<- paste0("España_2019-04-03") 
  
  nombre<- vec_days_str[days] %>% str_split(., " ") %>% unlist(.)
  nombre_ent<- paste0("Espana_",nombre[1])
  
  
  write.table(CSV_Espana, here::here(paste0(nombre_ent,'.CSV')), 
              sep = ";",
              dec = ".", 
              row.names = F,
              quote = F)
  
  
  }
