library(here)
source(here::here('libraries.R'))

dir_path_spain<- "/usr1/uems/runs/spain1/wrfprd"
folders_spain<- list.dirs(path = dir_path_spain)
netcdf_folder_spain<- folders_spain[str_detect(folders_spain, "wrfprd")]
netcdf_files_spain<- list.files(netcdf_folder_spain, full.names = T)



fecha_exe_modelo_spain<- first_date(netcdf_files = netcdf_files_spain)

if(is.na(fecha_exe_modelo_spain)){print("Modelo Peninsula, sin ejecutar")}else{
  
  if(fecha_exe_modelo_spain<lubridate::today()){
    
    print(paste0("El modelo de España se ha ejecutado hace: ", 
                 as.numeric(difftime(lubridate::today(),
                                     fecha_exe_modelo_spain,units = "days" ))," dias"))
  }else{
    
    print("Modelo de España actualizado disponible")
    
    folder_spain<- str_remove_all(as.character(fecha_exe_modelo_spain),"-")
    path_espana<- paste0(here::here('Data/Espana/'),folder_spain,"/")
    if(!dir.exists(path_espana)){dir.create(path_espana)}
      
    if(length(list.files(path_espana))>1){#Estaria bien detectar numero de csv, no numero de archivos
      print(paste0("CSV's generados anteriormente en la carpeta", path_espana))        
    }else{
      
      list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain) #Posible fallo aqui
      saveRDS(list_espana, file = paste0(path_espana,"Espana_",folder_spain,".RDS"))
      CSV_generator_Spain(list_espana, path_espana)
      print(paste0("CSV's generados correctamente en la carpeta", path_espana))
      
    }
    
  }
}





#CONSTRUIR HISTORICO
'
DIA<- "20190504"

netcdf_files_spain<- list.dirs("/media/asus/Elements") %>% .[str_detect(.,DIA)] %>% .[1] %>% 
  list.files(full.names = T)

path_espana<- paste0(here::here("Data/Espana/"),DIA,"/")
if(!dir.exists(path_espana)){dir.create(path_espana)}
list_espana<- get_netcdf_list(netcdf_files = netcdf_files_spain)
saveRDS(list_espana, file = paste0(path_espana,"Espana_",DIA,".RDS"))
'


