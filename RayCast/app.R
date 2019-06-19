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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("RayCast App"),
    sidebarLayout(position = "right",
        sidebarPanel("Nada"),
        mainPanel(p("RayCast es una herramienta que se encarga de predecir 
          descargas eléctricas con el fin de poder preveer caidas de rayos 
          sobre las lineas de transporte y distribución de energía eléctrica 
                    y de esta manera, poder preveer cortes de suministro"),
                  leafletOutput("mymap",
                                width = "800", 
                                height = "800"))   
    )

    
        
        
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mymap <- renderLeaflet({
        x<- here::here('Data/Espana/20190617/Espana_20190617.RDS') %>% readRDS() %>% .[[length(.)]] %>% .$Variable
        rayos<- x[runif((runif(1, 0, 100) %>% round()),1, nrow(x)) %>% round(),]
        
        
        
        quakes.df <- rayos %>% dplyr::mutate(
            mag.level = cut(G10_MAX,seq(0,10, by=1))) %>%
            split(.$mag.level)
        l <- leaflet() %>% 
            addProviderTiles(providers$Esri.OceanBasemap,
                             options =providerTileOptions(minZoom = 5, 
                                                          maxZoom = 10) ) %>% 
            setView(lng = -3, lat = 40, zoom = 5) %>% 
            setMaxBounds(lng1 = -15, lat1 = 50,
                         lng2 =  5,lat2 = 30)
        
        names(quakes.df) %>%
            purrr::walk( function(df) {
                l <<- l %>%
                    addCircleMarkers(data=quakes.df[[df]],
                               lng=~lon, lat=~lat,
                               group = df,opacity = 0.5,fill = "red",
                               radius = ~G10_MAX) %>% 
                    addHeatmap(data= quakes.df[[df]], lng = ~lon, lat = ~lat, 
                               intensity = ~G10_MAX,
                               blur = 50,
                               max = 0.05, 
                               radius =30 )
            })
        l <- l %>%
            addLayersControl(
                overlayGroups = names(quakes.df),
                options = layersControlOptions(collapsed = FALSE)) %>%
            addMiniMap(tiles = providers$Esri.OceanBasemap, width = 120, height=80)
        
        
        
        

        
        })
}

# Run the application 
options(browser ='/usr/lib/firefox/firefox.sh' )
shinyApp(ui = ui, server = server)
