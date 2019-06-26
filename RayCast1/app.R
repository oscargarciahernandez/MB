#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(leaflet.extras)
library(OpenStreetMap)
library(ggplot2)
library(gganimate)
library(gifski)
library(shinyWidgets)
library(tidyverse)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
    navbarPage("METEOBIT",
               tabPanel("Home",
                        headerPanel(
                            title = span( 
                                tags$img(src="backgroud_home3_title2.jpg", width = '100%'), align= "top-left")),
                        h2("RayCast is a powerfull tool for Lightning Potential Index (LPI) estimation."),
                        h3("Lightning Potential Index (LPI) represents the probability of Cloud-to-Ground
                           (CG) lightnings. "),
                        h3("RayCast consists on different step proccess for achive a great accuracy LPI forecast.
                          First at all, using data from several Weather Forecast
                          models, the convective forces of the atmosphere are calculated.
                          This first forecast, is improve using data assimilation from 
                           information obtained from radar and weather stations, and also, 
                           using Machine Learning algorithms. 
                           In case of detection of a high LPI (in case of thunderstorm), 
                           a new round of simulation from Weather Forecast Models is done
                           with a higher spatiotemporal resolution for the genration of 
                           a hyper-local LPI forecast for the next few hours with high accuracy 
                           performance")),
               tabPanel("RayCast",
                        fluidPage(
                            sliderInput("Date_slider", label = "Date: ", 
                                        min = "2019-06-20 01:00:00" %>% ymd_hms(),
                                        max= "2019-06-25 00:00:00"%>% ymd_hms(),
                                        value = "2019-06-20 01:00:00" %>% ymd_hms(),
                                        step = "01:00:00" %>%hms()),
                            leafletOutput("mymap",
                                          width = "100%", 
                                          height = "800")
                            
                        )
                       
                        
               ),
               tabPanel("Showcase",
                        p("This GIF shows the LPI stimation during the electric storm
                          from 20th  to 25th of june above Iberian Peninsula"),
                        imageOutput("animation",
                                    width = "800",
                                    height = "800")
               ))
    
    
)

    
    

   
   

    
        
        
    


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$animation<- renderImage({
        list(src = "data/outfile.gif",
             contentType = 'image/gif',
             width = "100%",
             height = "100%"
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    
    data_rayos<- reactive({
        x<- here::here("RayCast1/data/20190620/Espana_20190620.RDS") %>% 
            readRDS() %>% lapply(function(y) y$Variable) %>% bind_rows(.id = "Date")
        
        x1<- x %>% group_split(lon, lat) %>% lapply(function(y) {
            y$RAINC<- c(0,diff(y$RAINC) )
            return(y)}) %>% bind_rows()
        x1$RAINC<- ifelse(x1$RAINC==0, NA, x1$RAINC)
        x1<- x1[complete.cases(x1),]
        
        x1$Date<- ifelse(nchar(x1$Date)<11, paste(x1$Date, "00:00:00"), x1$Date) %>% ymd_hms()
        rayos<- x1 %>% filter(Date == input$Date_slider) 
        
        quakes.df <- rayos %>% dplyr::mutate(
            mag.level = cut(RAINC,seq(0,10, by=1))) %>%
            split(.$mag.level)
    })

    output$mymap <- renderLeaflet({
        
        l <- leaflet() %>% 
            addProviderTiles(providers$Esri.OceanBasemap,
                             options =providerTileOptions(minZoom = 5, 
                                                          maxZoom = 10) ) %>% 
            setView(lng = -3, lat = 40, zoom = 5) %>% 
            setMaxBounds(lng1 = -15, lat1 = 50,
                         lng2 =  5,lat2 = 30)
        
        names(data_rayos()) %>%
            purrr::walk( function(df) {
                l <<- l %>%
                    addCircleMarkers(data=data_rayos()[[df]],
                               lng=~lon, lat=~lat,
                               group = df,opacity = 0.5,fill = "red",
                               radius = ~RAINC) %>% 
                    addHeatmap(data= data_rayos()[[df]], lng = ~lon, lat = ~lat, 
                               intensity = ~RAINC,
                               blur = 50,
                               max = 0.05, 
                               radius =30 )
            })
        l <- l %>%
            addLayersControl(
                overlayGroups = names(data_rayos()),
                options = layersControlOptions(collapsed = TRUE)) %>%
            addMiniMap(tiles = providers$Esri.OceanBasemap, width = 120, height=80)
        
        
        
        

        
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
