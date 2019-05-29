library('ggplot2')
library('forecast')
library('tseries')
library(TTR)
library(here)
source('libraries.R')


Observed_Data<- here::here('Data/Parques/Belesar/Historico/WEB/PM/') %>% list.files(full.names = T) %>% 
  .[str_detect(., "Obs")] %>% .[length(.)]%>% readRDS()

Observed_Data %>% ggplot(aes(x=Date))+geom_line(aes(y=nivel))+theme_light()
Observed_Data<- Observed_Data %>% mutate(diffnivel=tsclean(c(0,diff(nivel))),
                         diffnivel1=SMA(diffnivel,24),
                         diffnivel7=SMA(diffnivel,24*7))

Observed_Data%>% 
  ggplot(aes(x=Date))+
  geom_line(aes(y=diffnivel), col="black", alpha = 0.6)+
  geom_line(aes(y=diffnivel1), col="red")+
  geom_line(aes(y=diffnivel7), col="green")+
  theme_light()


count_ma = ts(na.omit(Observed_Data$nivel), frequency=400)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)



adf.test(count_ma, alternative = "stationary")

Acf(count_ma)
Pacf(count_ma)




# rugarcah ----------------------------------------------------------------
###IMPLEMENTACION DE MODELO GARCH EN R Y CREACIÓN DE PRIEMER PAQUETE. 
#ESTA PRIMERA PARTE ES OMITIBLE... ERA UN INTENTO DE IMPLEMENTAR UN MODELO GARCH
# MANUALMENTE.... 

# MODELO GARCH MANUAL -----------------------------------------------------

estimar.y <- function(alpha0, alphas)
{
  y.estimado <- matrix(0,
                       nrow=(length(y.serie)),
                       ncol = 1)
  
  for (t in 2:length(y.serie))
  {
    sumatoria <- alpha0
    for (i in 1: orden.p)
    {
      if ((t - i) > 0)
      {
        sumatoria <- sumatoria +
          (alphas[i] *
             y.serie[t-i])
      }
    }
    y.estimado[t] <- sumatoria
  }
  return(y.estimado)
}
estimar.sigma2<-function(delta0, deltas,
                         bethas, error)
{
  sigma2.estimado<-matrix(0,
                          nrow=(length(y.serie)),
                          ncol = 1)
  
  for (t in 3:length(y.serie))
  {
    sumatoria <- delta0
    
    for (i in 1:orden.p)
    {
      if ((t-i)>0)
      {
        sumatoria<-sumatoria+
          (deltas[i] *
             (error[t - i] ^ 2))
      }
    }
    for (i in 1:orden.q)
    {
      if ((t-i)>0)
      {
        sumatoria<-sumatoria+
          (bethas[i] *
             (sigma2.estimado[t - i]))
      }
    }
    sigma2.estimado[t] <- sumatoria
  }
  return(sigma2.estimado)
}
calcular.error <- function(y.serie ,
                           y.estimado)
{
  error<-matrix(0,
                nrow=(length(y.serie)),
                ncol = 1)
  for (t in 2:length(y.serie))
  {
    error[t]<-(y.serie[t]-y.estimado[t])
  }
  return(error)
}
calcular.logL<-function(sigma2.estimado,
                        error)
{
  Tiempo <- length(sigma2.estimado)
  parte1<--((Tiempo - 3) / 2) * log(2*pi)
  
  parte2<--0.5* sum(
    log(sigma2.estimado[3:Tiempo]))
  parte3<--0.5*sum(
    (error[3:Tiempo]^2)/
      sigma2.estimado[3:Tiempo])
  logL = parte1 + parte2 + parte3
  return(logL)
}
Garch.fit <- function(param)
{
  alpha0 <- param[1]
  alphas <- param[2:(orden.p + 1)]
  delta0 <- param[(orden.p + 2)]
  deltas <- param[(orden.p + 3):
                    (2*orden.p + 2)]
  bethas <- param[(2*orden.p + 3):
                    (2*orden.p + 2 + orden.q)]
  y.estimado <- estimar.y(alpha0, alphas)
  error <- calcular.error(y.estimado)
  sigma2.estimado<-estimar.sigma2(delta0,
                                  deltas,
                                  bethas,
                                  error)
  logL<-calcular.logL(sigma2.estimado, error)
  return(((-1)*logL))
}
Garch.summary <- function(model)
{
  o.p = model$orden[1]
  o.q = model$orden[2]
  
  alpha0<-model$coef[1]
  alphas<-model$coef[2:(o.p + 1)]
  delta0<-model$coef[(o.p + 2)]
  deltas<-model$coef[(o.p + 3):(2*o.p + 2)]
  bethas<-model$coef[(2*o.p+3):(2*o.p+2+o.q)]
  
  cat("Mod. Est.,Garch(p=",o.p,",q=", o.q,")")
  cat("Coeficientes \n")
  cat("Alpha0: ", alpha0, " \n")
  cat("Alphas: ", alphas, " \n")
  cat("Delta0: ", delta0, " \n")
  cat("Deltas: ", deltas, " \n")
  cat("Bethas: ", bethas, " \n\n")
  plot(model$sigma2)
}
Garch <- function(y, orden = c(0,0))
{
  # Declaración Variables Globales
  orden.p<- orden[1]
  orden.q<- orden[2]
  # Declaración Variables
  cantVar <- 2+2*orden.p+orden.q
  
  # Definición de los Límites
  limInferior<- 0
  limSuperior<- 100  
  
  # Optimización
  Optim<-nlminb(rep(0.00001, cantVar),
                Garch.fit,
                lower = limInferior,
                upper = limSuperior,
                control =list( eval.max=50,
                               iter.max=50))
  modelo <- list(
    coef= Optim$par,
    sigma2=ts(sigma2.estimado),
    series = y,
    orden = orden)
  
  Garch.summary(modelo)
  return(modelo)
}


# MODELO GARCH ugarch -----------------------------------------------------
library(rugarch)


#ESPECIFICACIONES DEL MODELO 
'
MODELOS:
“sGARCH”, “fGARCH”, “eGARCH”,
“gjrGARCH”, “apARCH” and “iGARCH” and “csGARCH”.
#ORDEN
garchOrder The ARCH (q) and GARCH (p) orders.
SUBMODELO
submodel If the model is “fGARCH”, valid submodels are “GARCH”, “TGARCH”,
“AVGARCH”, “NGARCH”, “NAGARCH”, “APARCH”,“GJRGARCH” and “ALL-
  GARCH”
'

ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1),
                                 submodel = NULL, external.regressors = NULL, variance.targeting = FALSE),
           mean.model = list(armaOrder = c(1, 1), include.mean = TRUE, archm = FALSE,
                             archpow = 1, arfima = FALSE, external.regressors = NULL, archex = FALSE),
           distribution.model = "norm", start.pars = list(), fixed.pars = list())




#UGARCH FIT 
'
SOLVERS
“nlminb”, “solnp”, “lbfgs”, “gosolnp”, “nloptr” or “hybrid”
'


ugarchfit(spec, data, out.sample = 0, solver = "gosolnp", solver.control = list(),
          fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all',
                             trunclag = 1000),
          numderiv.control = list(grad.eps=1e-4, grad.d=0.0001,
                                  grad.zero.tol=sqrt(.Machine$double.eps/7e-7), hess.eps=1e-4, hess.d=0.1,
                                  hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2))


#UGARCH FORECAST... ME PARECE CRUCIAL ENTENDER QUE EL N.AHEAD Y EL OUT.SAMPLE (VAN DE LA MANO)

ugarchforecast(fitORspec, data = NULL, n.ahead = 10, n.roll = 0, out.sample = 0,
               external.forecasts = list(mregfor = NULL, vregfor = NULL),
               trunclag = 1000)




#ESPECIFICACIONES
GARCH_SPEC<-ugarchspec(variance.model = list(model = "csGARCH", garchOrder = c(10,10),
                                             submodel = NULL,
                                             external.regressors = NULL,
                                             variance.targeting = FALSE),
                       mean.model = list(armaOrder = c(10, 10),
                                         include.mean = TRUE,
                                         archm = FALSE,
                                         archpow = 1, 
                                         arfima = FALSE, 
                                         external.regressors = NULL, 
                                         archex = FALSE),
                       distribution.model = "norm", 
                       start.pars = list(), 
                       fixed.pars = list())



#FIT
DATA_fit<- na.omit(Observed_Data$diffnivel)
OUTSAMPLE<- round(length(DATA_fit)*0.15) 

FIT_UGARCH<-ugarchfit(GARCH_SPEC, DATA_fit, out.sample = 0, solver = "lbfgs", solver.control = list(),
                      fit.control = list(stationarity = 1, fixed.se = 0, scale = 0, rec.init = 'all',
                                         trunclag = 1000),
                      numderiv.control = list(grad.eps=1e-4, grad.d=0.0001,
                                              grad.zero.tol=sqrt(.Machine$double.eps/7e-7), hess.eps=1e-4, hess.d=0.1,
                                              hess.zero.tol=sqrt(.Machine$double.eps/7e-7), r=4, v=2))

HORAS_F<- 244

FORECAST<- ugarchforecast(FIT_UGARCH, data = NULL, n.ahead = HORAS_F, n.roll = OUTSAMPLE, out.sample = OUTSAMPLE,
                          external.forecasts = list(mregfor = NULL, vregfor = NULL),
                          trunclag = 1000)


x<- FORECAST@forecast$seriesFor %>% as.data.frame()
forecast_x<- data.frame(matrix(nrow = length(x[,1]))) %>% mutate(Tseries= x[, length(x)],
                                    Date= Observed_Data$Date[(nrow(Observed_Data)+1 - nrow(x)):nrow(Observed_Data)])



CUT_DATA<- Observed_Data[(nrow(Observed_Data)+1 - nrow(x)):nrow(Observed_Data),]

ggplot(CUT_DATA, aes(x=Date))+
  geom_line(aes(y=diffnivel), col="black", alpha = 0.4)+
  geom_line(aes(y=diffnivel1), col="red", alpha=0.7)+
  geom_line(aes(y=diffnivel7), col="green", alpha=0.7)+
  geom_line(data=forecast_x,  aes(x=Date, y=Tseries), col="orange")+
  theme_light()

