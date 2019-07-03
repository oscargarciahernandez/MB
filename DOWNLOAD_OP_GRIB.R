library(RCurl)
library(request)
library(XML)
library(rvest)
library(stringr)
library(here)
source('libraries.R')


#FUNCION DOWNLOAD
bdown=function(INPUT_FRAME){
  url<- INPUT_FRAME$OPERATIONAL_LINKS %>% as.character()
  file<- INPUT_FRAME$NOMBRE_ARCHIVOS %>% as.character()
  
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

# DESCARGA DE GRIBS -------------------------------------------------------
# DESCARGAR DESDE UCAR 122 MB
DOWNLOAD_UCAR<- FALSE
if(DOWNLOAD_UCAR){
  DATE_to_download<-  now() %>% as.Date() %>% as.character() %>% str_replace_all('-',"")
  PATH_GRIBS<- here::here("Gribs/") %>% paste0(., DATE_to_download)
  if(!dir.exists(PATH_GRIBS)){dir.create(PATH_GRIBS, recursive = T)} 
  
  
  
  URL_BASE<- paste0("http://soostrc.comet.ucar.edu/data/grib/gfsp25/", DATE_to_download,"/grib.t00z/")
  OPERATIONAL_LINKS<- URL_BASE %>% GET() %>% htmlParse() %>% 
    xpathSApply("//a/@href") %>% paste0(URL_BASE, .) %>%  .[str_detect(., ".0p25.")] %>% .[1:97]
  
  #SOLO QUEREMOS HASTA 120 HORAS... ES EL ARCHIVO 121
  
  NOMBRE_ARCHIVOS<- OPERATIONAL_LINKS %>% str_remove(URL_BASE) %>% paste0(PATH_GRIBS,"/",  . )
  
  INPUT_FUN<- cbind(OPERATIONAL_LINKS, NOMBRE_ARCHIVOS) %>% as.data.frame() %>%  split(seq(nrow(.)))
  
  library(parallel)
  mclapply(INPUT_FUN,
           bdown,
           mc.cores =  getOption("mc.cores",1L ))
  
  
}



# NOMADS... GFS MÁS PESADOS, HASTA 350 MB

DOWNLOAD_NOMADS<- TRUE
if(DOWNLOAD_NOMADS){
  
  DATE_to_download<-  now() %>% as.Date() %>% as.character() %>% str_replace_all('-',"")
  
  PATH_GRIBS<- here::here("Gribs/") %>% paste0(., DATE_to_download)
  if(!dir.exists(PATH_GRIBS)){dir.create(PATH_GRIBS)} 
  
  
  URL_BASE<- paste0("https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/gfs.", DATE_to_download,"/00/")
  OPERATIONAL_LINKS<- URL_BASE %>% GET() %>% htmlParse() %>% 
    xpathSApply("//a/@href") %>% paste0(URL_BASE, .) %>% .[str_detect(., "gfs.t00z.pgrb2.0p25.f")] %>% 
    .[!str_detect(.,".idx")]
  
  
  OPERATIONAL_LINKS<- OPERATIONAL_LINKS[1:121]
  
  
  NOMBRE_ARCHIVOS<- OPERATIONAL_LINKS %>% str_remove(URL_BASE) %>% paste0(PATH_GRIBS,"/",  . )
  
  INPUT_FUN<- cbind(OPERATIONAL_LINKS, NOMBRE_ARCHIVOS) %>% as.data.frame() %>%  split(seq(nrow(.)))
  
  
  
  
  library(parallel)
  mclapply(INPUT_FUN,
           bdown,
           mc.cores =  getOption("mc.cores",10L ))
  
  
  
}


