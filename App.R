library(shiny)
library(caret)
library(doMC)
library(here)
source(here::here('libraries.R'))


# Import Model data -------------------------------------------------------
Obs_Data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
  .[str_detect(.,"Obs_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("Obs_|.RDS") %>% 
                                             str_replace("--"," ") %>% ymd_hms())] %>% readRDS()

WRF_data<- list.files(here::here('Data/Parques/Belesar/Historico/WEB/PM/'), full.names = T) %>% 
  .[str_detect(.,"WRF_")] %>% .[which.max( str_split(., "/") %>% lapply(., function(x) x[length(x)])%>%  str_remove("WRF_|.RDS") %>% 
                                             str_replace("--"," ") %>% ymd_hms())] %>% readRDS()



Tabla_1<- Obs_Data %>% mutate(diff_nivel=c(NA,diff(nivel))) 
Tabla_1<- left_join(Tabla_1, WRF_data[[23]]$D1[,c("Date", "prep_hourly")], by="Date")



# Predecir aportacion a través de differencia de nivel --------------------

Tabla_DN_IF<- Tabla_1
Tabla_DN_IF[,c("Vol","Temp", "porcentaje","prep_hourly")]<- NULL
Tabla_DN_IF<- Tabla_DN_IF[complete.cases(Tabla_DN_IF),]
Tabla_DN_IF$aport_SMA<- SMA(Tabla_DN_IF$aport, 12)
Tabla_DN_IF$difnivel_SMA<- SMA(Tabla_DN_IF$diff_nivel, 12)
Tabla_DN_IF<- Tabla_DN_IF[complete.cases(Tabla_DN_IF),]


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Belesar Predictive model"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      

# Elegimos que gráficas queremos mostrar  ---------------------------------
      checkboxGroupInput("checkGroup", label = h3("Gráficas"), 
                         choices = list("Prediccion de diff nivel" = 1,
                                        "Prediccion inflow" = 2, 
                                        "Ambas" = 3),
                         selected = NULL),

# Elegimos metodo y Tunelength para inflow predicction -------------------------------------------------
      selectInput(inputId = "Method_inflow",
                  label = "Choose method for predict inflow from diff nivel:",
                  choices = c("svmLinear", "lm", "other")),
      # Input: Selecciona parámetro tuneLenght
      numericInput(inputId = "TuneLength1",
                   label = "Number of tune values:",
                   value = 10),

# Elegimos metodo y tuneLength para diff nivel prediction -----------------
      selectInput(inputId = "Method_diff",
                  label = "Choose method for predict diff nivel from WRF rain:",
                  choices = c("svmLinear", "lm", "other")),
      # Input: Selecciona parámetro tuneLenght
      numericInput(inputId = "TuneLength2",
                   label = "Number of tune values:",
                   value = 10),

# Elegimos periodo de predicción  -----------------------------------------
dateRangeInput("Date_prediccion", h3("Rango de predicción"), start =Tabla_DN_IF)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #Cortamos los datos en Entrenamiento y predicción 
  T_DN_IF_data<- Tabla_DN_IF[Tabla_DN_IF$Date< ymd("2019/01/25"), ]
  P_DN_IF_data<- Tabla_DN_IF[Tabla_DN_IF$Date> ymd("2019/01/25"), ]
  
  
  
  # Return the requested inflow method ----
  inflow_method <- reactive({
    switch(input$Model_inflow,
           "svmLinear" = "svmLinear",
           "lm" = "lm",
           "other" = "other")
  })
  
  
  modelo_DN_AP<- train(aport_SMA ~ difnivel_SMA,
                       data=T_DN_IF_data,
                       method=inflow_method,
                       tuneLength=input$TuneLength)
  
  
  
  ggplot(data = P_DN_IF_data)+
    geom_line(aes(y=P_DN_IF_data$aport, 
                  x=P_DN_IF_data$Date), 
              alpha=0.5)+
    geom_line(aes(y=P_DN_IF_data$aport_SMA, 
                  x=P_DN_IF_data$Date), 
              alpha=0.8)+
    ylab("Aportacion [m³/s]")+
    xlab(paste(range(P_DN_IF_data$Date), collapse = "\n"))+
    geom_line(aes(y=predict(modelo, newdata= P_DN_IF_data),
                  x=Date), 
              col="red", lty=2)+
    theme_light()   
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
