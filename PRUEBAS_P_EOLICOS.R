library(here)
source('libraries.R')

lista_parques<- list()
for (i in 1:4) {
  PARQUE<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/Datos_Produccion_Disponibilidad.xlsx') %>% read.xls(sheet = i)
  
  PARQUE$FHUTC<- PARQUE$FHUTC %>% dmy_hm()
  lista_parques[[i]]<- PARQUE
}

saveRDS(lista_parques, here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))

# SACAMOS INFO KMZ Y POTENCIAS --------------------------------------------
#SACAMOS INFO DE LOS PARQUES
INFO_PARQUES<- here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/Datos_Produccion_Disponibilidad.xlsx') %>% read.xls(sheet = 5)

#SACAMOS LONGITUD Y LATITUD

LonLat_Parques<- Extract_Lon_Lat_from_Kmz(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/'))

cbind(INFO_PARQUES[,c(3:5)], LonLat_Parques) %>% as.data.frame() %>% write.csv(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/INFO_PARQUES.csv'))
      




#TRATAR DATOS  ---------------------------------------------------------
DATA_PARQUES<- read.csv(here::here('Data/Parques/PRUEBA_EOLICOS/windfarm/INFO_PARQUES.csv'))
DATA_PARQUES

world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf()+
  geom_point(data= DATA_PARQUES, aes(x= Longitude, y= Latitude), color= "red")



TABLA_PARQUES<- readRDS(here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS')) %>% bind_rows()
colnames(TABLA_PARQUES)<- c('PARQUE', 'DATE', 'PRUDUCCION_MWH', 'DISPONIBILIDAD')
head(TABLA_PARQUES)


TABLA_PARQUES %>% group_split(PARQUE) %>% lapply(function(x) print(paste(x$PARQUE %>% unique(),": Rango",x$DATE %>% range)))




lista_parques<- TABLA_PARQUES %>%  group_split(PARQUE) 

for (i in 1:length(lista_parques)) {
  x<- lista_parques[[i]]
  x$PARQUE<- DATA_PARQUES[i,1]
  x$CAPACIDAD<- DATA_PARQUES[i,2]
  x$N_TURBINAS<- DATA_PARQUES[i,3]
  x$LON<- DATA_PARQUES[i,5]
  x$LAT<- DATA_PARQUES[i,6]
  lista_parques[[i]]<- x
}

TABLA_PARQUES<- lista_parques %>% bind_rows()
saveRDS(TABLA_PARQUES, here::here('Data/Parques/PRUEBA_EOLICOS/Historico_PE.RDS'))





