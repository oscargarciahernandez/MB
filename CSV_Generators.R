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
  }else{
    print("Modelo de Europa actualizado disponible")
    list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
    folder_europa<- str_remove_all(as.character(fecha_exe_modelo_europa),"-")
    path_europe<- paste0(here::here('Data/Europa/'),folder_europa,"/")
    
    if(!dir.exists(path_europe)){dir.create(path_europe)}
    if(!file.exists(paste0(path_europe,"Europe_",foler_europa,".RDS"))){
      saveRDS(list_europe, file = paste0(path_europe,"Europe_",folder_europa,".RDS"))
    }
  }
}



fecha_exe_modelo_spain<- first_date(netcdf_files = netcdf_files_spain)

if(is.na(fecha_exe_modelo_spain)){print("Modelo España, sin ejecutar")}else{
  if(fecha_exe_modelo_spain<lubridate::today()){
    print(paste0("El modelo de España se ha ejecutado hace: ", 
                 as.numeric(difftime(lubridate::today(),fecha_exe_modelo_spain,units = "days" ))," dias"))
  }else{
    print("Modelo de España actualizado disponible")
    list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)
    folder_spain<- str_remove_all(as.character(fecha_exe_modelo_spain),"-")
    path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/")
    
    if(!dir.exists(path_espana)){dir.create(path_espana)}
    if(!file.exists(paste0(path_espana,"Espana_",folder_spain,".RDS"))){
      saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
    }
    
    CSV_generator_Spain(list_espana, path_espana)
    
  }
}









