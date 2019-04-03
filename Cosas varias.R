##Creamos una carpeta con la fecha de ejecucion del modelo, donde irán contenidos
## Tanto los CSV como los archivos RDS. 

list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
folder_europa<- str_remove_all(as.character(fecha_exe_modelo_europa),"-")
path_europe<- paste0(here::here('Data/Europa/'),folder_europa,"/")

if(!dir.exists(path_europe)){dir.create(path_europe)}
if(!file.exists(paste0(path_europe,"Europe_",foler_europa,".RDS"))){
  saveRDS(list_europe, file = paste0(path_europe,"Europe_",folder_europa,".RDS"))
}


list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)
folder_spain<- str_remove_all(as.character(fecha_exe_modelo_spain),"-")
path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/")

if(!dir.exists(path_espana)){dir.create(path_espana)}
if(!file.exists(paste0(path_espana,"Espana_",folder_spain,".RDS"))){
  saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
}


# Europe pressure and Temperature ---------------------------------------------------------
CSV_generator_Europe(list_europe, path_europe)

# España temperatura ------------------------------------------------------

CSV_generator_Spain(list_espana, path_espana)
