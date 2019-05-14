library(request)
library(XML)
library(xml2)

URL_SAIH<- "http://saih.chminosil.es/index.php?url=/datos/mapas/mapa:H1/area:HID/acc:1"
URL_CAUDALES_MINOSIL<- URL_SAIH %>% GET() %>% content(.,"text",encoding = "ISO-8859-1") %>% htmlParse() %>% 
  xpathSApply( "//a/@href") %>% .[str_detect(.,"tag:")] %>% .[str_detect(.,"ACQ")]





if(!dir.exists(here::here('Data/Parques/Belesar/CAUDALES_SAIH'))){dir.create(here::here('Data/Parques/Belesar/CAUDALES_SAIH'))}
URL_BASE<- "http://saih.chminosil.es/"
URL_CAUDAL<- URL_CAUDALES_MINOSIL %>% str_replace("graficas", "graficas_numeros") %>% paste0(URL_BASE,.,"&historia=")


ESTACIONES_LIST<- list()
for (N_RIO in 1:length(URL_CAUDAL)) {
  
  
  CAUDAL_list<- list()
  i<- 0
  while(TRUE) {
    url1<- paste0(URL_CAUDAL[N_RIO],i)
    CAUDAL<- httr::GET(url1)
    html_encoded<-content(CAUDAL, "text", encoding = "ISO-8859-1")
    CAUDAL_REAL<-readHTMLTable(htmlParse(html_encoded), rm_nodata_cols = F)
    
    if(length(CAUDAL_REAL)==1){break}else{
      
      CAUDAL_REAL2<- CAUDAL_REAL[[2]]
      colnames(CAUDAL_REAL2)<- c("Date", "M3S")
      CAUDAL_REAL2<- CAUDAL_REAL2 [2:length(CAUDAL_REAL2$Date),]
      CAUDAL_REAL2$Date<- dmy_hm(CAUDAL_REAL2$Date,tz=Sys.timezone()) 
      CAUDAL_REAL2$Date<- with_tz(CAUDAL_REAL2$Date, tzone="UTC")
      
      j<- i+1
      CAUDAL_list[[j]]<- CAUDAL_REAL2
      i<- i+1
      if(j==1){NOMBRE_ESTACION<- CAUDAL_REAL[[1]]$V2[1] %>% as.character() %>% 
          str_split("") %>% .[[1]]%>% .[1:4] %>% paste(collapse = "")}
      
    }
    
  }
  
  
  ESTACIONES_LIST[[NOMBRE_ESTACION]]<- CAUDAL_list
}



ESTACIONES_TABLAS<- lapply(ESTACIONES_LIST, function(EL){
  Tabla_caudales<- bind_rows(EL)
  Tabla_caudales$M3S<- Tabla_caudales$M3S %>% str_replace(",", ".") %>% as.numeric()
  return(Tabla_caudales)
  })

NOMBRE_ARCHIVO<- now() %>% str_split(" ") %>% .[[1]] %>% .[1:2] %>% paste0(collapse = "_")
saveRDS(ESTACIONES_TABLAS, paste0(here::here('Data/Parques/Belesar/CAUDALES_SAIH/ESTACIONES_SAIH_Q_'), 
                                  NOMBRE_ARCHIVO, ".RDS"))
