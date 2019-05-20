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
    
    if(!dir.exists(path_europe)){dir.create(path_europe)}
      
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


library(RCurl)
###CARGAR USUSARIOS Y CONTRASEÑAS DE FTP'S
Variables<- read.table(here::here('FTPs.CSV'),
                       stringsAsFactors = F, quote = "", sep = ";") 
for (i in 1:length(Variables[,1])) {assign(Variables[i,1], Variables[i,2])}


####DETECTAR CSV'S DE HOY
fecha_hoy<- now() %>% as.character() %>% str_split(" ") %>% 
  .[[1]] %>% .[1] %>% ymd() %>% as.character() %>% str_replace_all("-","")
Lista_nuevos<- list.files(here::here('Data/'), recursive = T) %>% .[str_detect(.,".CSV")] %>% 
  .[str_detect(.,fecha_hoy)] %>% paste0(here::here('Data/'),.)

#####SUBIR CSV's Europa
Europa_ftp<- Lista_nuevos[str_detect(Lista_nuevos,"Europe_")]
if(!length(Europa_ftp)==0){
  for (i in 1:length(Europa_ftp)) {
    ftpUpload(Europa_ftp[i],paste0("ftp://",usr_mb,":",pass_mb,"@",url_mb,"/ElMundo/Europa/",
                                   Europa_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  
}
