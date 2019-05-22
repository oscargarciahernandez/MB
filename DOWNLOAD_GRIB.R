library(here)
source('libraries.R')


library(RCurl)
bdown=function(url, file){
  
  f = CFILE(file, mode="wb")
  a = curlPerform(url = url, writedata = f@ref, noprogress=FALSE)
  close(f)
  return(a)
}

url_gfs_http<- "https://nomads.ncdc.noaa.gov/data/gfs4/201905/20190505/"
Tabla_gfs<- url_gfs_http %>% GET() %>% htmlParse() %>% readHTMLTable() %>% .[[1]]

Tabla_gfs2<- Tabla_gfs[3:nrow(Tabla_gfs),]

Gribs<- Tabla_gfs2$Name %>% as.character() %>%  .[str_detect(., ".grb2")] %>% .[complete.cases(.)]

#48 es la position 17
Gribs %>% str_extract("[[:digit:]]{3}.grb2") %>% str_remove(".grb2") %>%
  as.numeric() 

path_gribs<- here::here('Gribs/')
if(!dir.exists(path_gribs<- here::here('Gribs/'))){dir.create(path_gribs<- here::here('Gribs/'))}


##MUY LENTO
'
for (i in 1:17) {
  download.file(paste0(url_gfs_http, Gribs[i]), destfile = paste0(path_gribs, Gribs[i]))
  
}
'

for (i in 1:17) {
  bdown(paste0(url_gfs_http, Gribs[i]), file = paste0(path_gribs, Gribs[i]))
  
}


######FORMATO NAME
###  QUIERO:  19043000.gfs.t00z.0p50.pgrb2f000
###  TENGO:    gfs_4_20190504_0000_000.grb2

#DIA+PARTE_FIJA+HORA


NOMBRE_PARTE_FIJA<- ".gfs.t00z.0p50.pgrb2f"



###PONGO DOS MANERAS DE HACER LO DEL NOMBRE PORQUE ME EQUIVOQUE Y TUVE QUE CAMBIAR EL NOBMRE
#DE LOS ARCHIVOS DE 2 PASOS... PARA LA SIGUIENTE VEZ... HAY QUE UNIFICAR ESTOS DOS PASOS EN 1
# ESO YA QUEDA PARA LA SIGUIENTE VEZ... ME DA PEREZA AHORA MISMO.

Horas<- list.files(path_gribs, full.name=T) %>% str_extract("[[:digit:]]{3}.grb2") %>% str_remove(".grb2")
'
Horas<- list.files(path_gribs, full.name=T) %>% str_extract("pgrb2f.{3}") %>% str_remove("pgrb2f")
'

DIA<- list.files(path_gribs, full.name=T) %>% str_extract("gfs_4_[[:digit:]]{8}") %>% str_remove("gfs_4_")

'
DIA<- list.files(path_gribs, full.name=T) %>% str_split("/") %>% sapply(function(x) x[length(x)]) %>% 
  str_remove(".{2}") %>% paste0(.,"00")
'


new_name<- paste0(DIA, NOMBRE_PARTE_FIJA, Horas)

file.rename(from =list.files(path_gribs, full.name=T), to= paste0(path_gribs, new_name) )
