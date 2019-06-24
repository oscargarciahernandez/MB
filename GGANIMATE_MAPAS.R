library(OpenStreetMap)
library(here)
source('libraries.R')








x<- here::here('Data/Espana/20190620/Espana_20190620.RDS') %>% 
  readRDS() %>% lapply(function(y) y$Variable) %>% bind_rows(.id = "Date")

w<- x$lon %>% range() %>% .[2]
e<- x$lon %>% range() %>% .[1]
n<- x$lat %>% range() %>% .[2]
s<- x$lat %>% range() %>% .[1]



ul <- round(c(n,w),digits = 3)  #Upper Left
lr <- round(c(s,e), digits = 3)  #Lower Right




# Descargar y guardar mapas -----------------------------------------------

#Se puede seleccionar el tipo de mapa a descargar
# si no pones nada descarga todos los mapas disponibles
#Se puede cambiar la resolución, pero esta por defecto en 
# 40 numtiles
download_maps(ul,lr, res=40)



# Plotear mapas -----------------------------------------------------------

map.latlon<- list.dirs(here::here('Mapas/'), recursive = F) %>% list.files(full.names = T) %>%
  .[str_detect(.,".RDS")] %>% .[2] %>% 
  readRDS()


x1<- x %>% group_split(lon, lat) %>% lapply(function(y) {
  y$RAINC<- c(0,diff(y$RAINC) )
  return(y)}) %>% bind_rows()
x1$RAINC<- ifelse(x1$RAINC==0, NA, x1$RAINC)
x1<- x1[complete.cases(x1),]

x1$Date<- ifelse(nchar(x1$Date)<11, paste(x1$Date, "00:00:00"), x1$Date) %>% ymd_hms()


library(gganimate)



pmap2<-autoplot(map.latlon)+
  geom_point(data= x1, 
             aes(x=lon, y=lat, color= RAINC), 
             alpha= 0.2,size= 3, shape=15) +
  scale_color_gradientn(colours = c("blue", "darkgreen","red"),
                        na.value = NA,
                        breaks= seq(min(x1$RAINC, 
                                        max(x1$RAINC, 
                                            8)))) +
  theme_bw() +
  coord_fixed(1.3)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  labs(title = 'Date: {frame_time}')+
  transition_time(Date)
  
library(gifski)

animate(pmap2,renderer =  gifski_renderer(), fps = 2, nframes = nrow(x1 %>%
                                                                       group_split(lon,lat) %>% 
                                                                       .[[1]] %>% 
                                                                       nrow(.)))






x2<- x1 %>% group_split(Date) %>% .[[19]]

autoplot(map.latlon)+
  geom_point(data= x2, 
             aes(x=lon, y=lat, color= RAINC), 
             alpha= 0.2,size= 3, shape=15) +
  scale_color_gradientn(colours = c("blue", "darkgreen","red"),
                        na.value = NA,
                        breaks= seq(min(x1$RAINC, 
                                        max(x1$RAINC, 
                                            8)))) +
  theme_bw() +
  coord_fixed(1.3)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())
