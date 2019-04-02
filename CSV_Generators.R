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
#En principio esto habría que ejecutarlo una vez que tenemos los ficheros 
#Del modelo. 
Hoy<- as.character(lubridate::today())
list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)


if(!file.exists(paste0(here::here('Data/Europa/'),"/Europe_",Hoy,".RDS"))){
  saveRDS(list_europe, file = paste0(here::here('Data/Europa/'),"/Europe_",Hoy,".RDS"))
}
if(!file.exists(paste0(here::here('Data/Espana/'),"/Espana_",Hoy,".RDS"))){
  saveRDS(list_espana, file = paste0(here::here('Data/Espana/'),"/Espana_",Hoy,".RDS"))
}


# Europe pressure and Temperature ---------------------------------------------------------
CSV_generator_Europe(list_europe)

# España temperatura ------------------------------------------------------

CSV_generator_Spain(list_espana)
