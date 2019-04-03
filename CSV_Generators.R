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


fecha_exe_modelo_europa<- first_date(netcdf_files = netcdf_files_europe)
if(is.na(fecha_exe_modelo_europa)){print("Modelo Europa, sin ejecutar")}else{
  if(fecha_exe_modelo_europa<lubridate::today()){
  print(paste0("El modelo de europa se ha ejecutado hace: ", 
               as.numeric(difftime(lubridate::today(),fecha_exe_modelo_europa,units = "days" ))," dias"))
  }else{print("Modelo de Europa actualizado disponible")}
}

fecha_exe_modelo_spain<- first_date(netcdf_files = netcdf_files_spain)
if(is.na(fecha_exe_modelo_spain)){print("Modelo España, sin ejecutar")}else{
  if(fecha_exe_modelo_spain<lubridate::today()){
    print(paste0("El modelo de España se ha ejecutado hace: ", 
                 as.numeric(difftime(lubridate::today(),fecha_exe_modelo_spain,units = "days" ))," dias"))
  }else{print("Modelo de España actualizado disponible")}
}

#Esta funcion crea una lista con todas la variables que nos interesan
#En principio esto habría que ejecutarlo una vez que tenemos los ficheros 
#Del modelo. 

list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)




##Creamos una carpeta con la fecha de ejecucion del modelo, donde irán contenidos
## Tanto los CSV como los archivos RDS. 
folder_spain<- str_remove_all(as.character(fecha_exe_modelo_spain),"-")
folder_europa<- str_remove_all(as.character(fecha_exe_modelo_europa),"-")

path_europe<- paste0(here::here('Data/Europa/'),folder_europa,"/")
path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/")


if(!file.exists(paste0(path_europe,"Europe_",Hoy,".RDS"))){
  saveRDS(list_europe, file = paste0(path_europe,"Europe_",Hoy,".RDS"))
}
if(!file.exists(paste0(path_espana,"Espana_",Hoy,".RDS"))){
  saveRDS(list_espana, file = paste0(path_espana,"Espana_",Hoy,".RDS"))
}


# Europe pressure and Temperature ---------------------------------------------------------
CSV_generator_Europe(list_europe, path_europe)

# España temperatura ------------------------------------------------------

CSV_generator_Spain(list_espana, path_espana)
