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


#####SUBIR CSV's ESPAÑA
Espana_ftp<- Lista_nuevos[str_detect(Lista_nuevos,"Espana_")]
if(!length(Espana_ftp)==0){
  for (i in 1:length(Espana_ftp)) {
    ftpUpload(Espana_ftp[i],paste0("ftp://",usr_mb,":",pass_mb,"@",url_mb,"/ElMundo/Peninsula/",
                                   Espana_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  
}

#####SUBIR CSV's Europa
Europa_ftp<- Lista_nuevos[str_detect(Lista_nuevos,"Europa_")]
if(!length(Europa_ftp)==0){
  for (i in 1:length(Europa_ftp)) {
    ftpUpload(Europa_ftp[i],paste0("ftp://",usr_mb,":",pass_mb,"@",url_mb,"/ElMundo/Europa/",
                                   Europa_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  
}



#####SUBIR CSV's PARQUES
PARQUES_ftp<- Lista_nuevos[str_detect(Lista_nuevos,"LaSia|ElCerro|Lubian")]
if(!length(PARQUES_ftp)==0){
  for (i in 1:length(PARQUES_ftp)) {
    ftpUpload(PARQUES_ftp[i],paste0("ftp://",usr_mb,":",pass_mb,"@",url_mb,"/LaSia/",
                                   PARQUES_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  
}



#####SUBIR CSV's PARQUES
BELESAR_ftp<- Lista_nuevos[str_detect(Lista_nuevos,"Belesar_")] %>% .[!str_detect(.,"comprobacion")]
if(!length(BELESAR_ftp)==0){
  '
  for (i in 1:length(BELESAR_ftp)) {
    ftpUpload(BELESAR_ftp[i],paste0("ftp://",usr_dhi,":",pass_dhi,"@",url_dhi,"/",
                                    BELESAR_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  '
  
  for (i in 1:length(BELESAR_ftp)) {
    ftpUpload(BELESAR_ftp[i],paste0("ftp://",usr_mb,":",pass_mb,"@",url_mb,"/Embalses/",
                                    BELESAR_ftp[i] %>% str_split("/") %>% .[[1]] %>% .[length(.)]))
  }
  
}
