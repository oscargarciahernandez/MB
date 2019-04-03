library(here)
source(here::here('libraries.R'))

dir_path_europe<- "/usr1/uems/runs/europe1/"
folders_europe<- list.dirs(path=dir_path_europe)

netcdf_folder_europe<- folders_europe[str_detect(folders_europe, "wrfprd")]
netcdf_files_europe<- list.files(netcdf_folder_europe, full.names = T)

fecha_exe_modelo_europa<- first_date(netcdf_files = netcdf_files_europe)

if(is.na(fecha_exe_modelo_europa)){print("Modelo Europa, sin ejecutar")}else{
  if(fecha_exe_modelo_europa<lubridate::today()){
    print(paste0("El modelo de europa se ha ejecutado hace: ", 
                 as.numeric(difftime(lubridate::today(),
                                     fecha_exe_modelo_europa,units = "days" ))," dias"))
  }else{
    print("Modelo de Europa actualizado disponible")
    folder_europa<- str_remove_all(as.character(fecha_exe_modelo_europa),"-")
    path_europe<- paste0(here::here('Data/Europa/'),folder_europa,"/")
    
    if(!dir.exists(path_europe)){dir.create(path_europe)}else{
      
      if(length(list.files(path_europe))>0){
        
        print(paste0("CSV's generados anteriormente en la carpeta", path_europe))
        
      }else{
        
        list_europe<- get_netcdf_list(netcdf_files = netcdf_files_europe)
        saveRDS(list_europe, file = paste0(path_europe,"Europe_",folder_europa,".RDS"))
        CSV_generator_Europe(list_europe, path_europe)
        print(paste0("CSV's generados correctamente en la carpeta", path_europe))
        
      }
    }
    
  }
}

