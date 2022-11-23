library(tidyverse)
require(readxl)
require(magrittr)
require(astsa)
library(janitor)
library(ggplot2)
require(forecast)
require(lmtest)
require(tseries)
library(TSA)
require(tsoutliers)
library(stats)
library(TSstudio)

base <- read_excel("anexo_ipc_oct22.xlsx", 
                   sheet = "13", skip = 6)
head(base)
ipc <- base[ 14:158,]
ipc %>% clean_names()
ipc <- clean_names(ipc)
ipc

datos <- data.frame(fecha=seq(as.Date("2010/1/1"),
                              as.Date("2022/1/1"), "months"), ipc$indice, ipc$mensual,
                    ipc$ano_corrido, ipc$anual)
datos
# limpiando los datos
datos <- clean_names(datos)
names(datos)
 
head(datos)
 


# con el indice

 
datos %>% ggplot(aes(x=fecha, y=ipc_indice))+
  geom_line(col="blue")
 

# con variación mensual, anual y lo que va del año corrido

## mensual (interesante)

 
datos %>% ggplot(aes(x=fecha, y=ipc_mensual))+
  geom_line(col="blue")
 

# Anual

 
datos %>% ggplot(aes(x=fecha, y=ipc_anual))+
  geom_line(col="blue")
 

## año corrido

 
datos %>% ggplot(aes(x=fecha, y=ipc_ano_corrido))+
  geom_line(col="blue")
 



# ARIMA 

 
ts_ipc <- ts(datos$ipc_mensual, start=c(2010,1), frequency = 12)
modelo1_ipc <- auto.arima(ts_ipc,
                          stepwise = FALSE, approximation = FALSE)

 


 
modelo1_ipc %>% coeftest()
 

 
modelo1_ipc
 

 
par(mfrow = c(1,1))
acf(ts_ipc)
pacf(ts_ipc)
 

Como la serie mensual presenta tendencia se procede a realizar una diferencia


# Justificacion del modelo a utilizar y alternativas


# verificacion de supuestos

 
modelo1_ipc %>% checkresiduals()
 

 
modelo1_ipc$residuals %>% shapiro.test()
modelo1_ipc$residuals %>% jarque.bera.test()
 

 
qqnorm(modelo1_ipc$residuals)
qqline(modelo1_ipc$residuals)
 

 
Box.test(modelo1_ipc$residuals)
 


# Tranformamos intentando estabilizar varianza

 
ipc_lambda <- BoxCox.lambda(datos$ipc_mensual)
ipc_lambda
 



 
ipc_trans <- BoxCox(datos$ipc_mensual,
                    lambda=ipc_lambda)
 


 
ipc_transf <- data.frame(datos$fecha, ipc_trans)
colnames(ipc_transf)[1] <- c("fecha")
colnames(ipc_transf)[2] <- c("ipc_mensual")
head(ipc_transf)
 

 
ipc_transf %>% ggplot(aes(x=fecha, y=ipc_mensual))+
  geom_line(col="red")
 

# ARIMA 

 
ts_ipc_transf <- ts(ipc_transf$ipc_mensual, start=c(2010,1), frequency = 12)
modelo1_ipc_transf <- auto.arima(ts_ipc_transf,
                                 stepwise = FALSE, approximation = FALSE)

 


 
modelo1_ipc_transf %>% coeftest()
 
 
modelo1_ipc_transf
 


# verificacion de supuestos

 
modelo1_ipc_transf %>% checkresiduals()
 

 
modelo1_ipc_transf$residuals %>% shapiro.test()
modelo1_ipc_transf$residuals %>% jarque.bera.test()
 

 
qqnorm(modelo1_ipc_transf$residuals)
qqline(modelo1_ipc_transf$residuals)
 
# se transforman los datos en escala logaritmo natural
lm_ipc <- log(datos$ipc_mensual)
is.na(lm_ipc)
ipc_log <- data.frame(datos$fecha, lm_ipc)
colnames(ipc_log)[1] <- c("fecha")
colnames(ipc_log)[2] <- c("ipc_mensual")
head(ipc_log)
ipc_log %>% ggplot(aes(x=fecha, y=ipc_mensual))+
  geom_line(col="purple")
# ARIMA 
ts_ipc_log <- ts(ipc_log$ipc_mensual, start=c(2010,1), frequency = 12)
modelo1_ipc_transf <- auto.arima(ts_ipc_log,
                                 stepwise = FALSE, approximation = FALSE)
modelo1_ipc_transf %>% coeftest()
log(modelo1_ipc) %>% checkresiduals()
# puesto que hay valores NA no se puede avanzar más 
# por lo cua lse procede a utilizar la serie sabiendo que no cumple con el supuesto de normalidad
predict(modelo1_ipc, n.ahead = 9)
# grafico
ts <- ts_ipc
ts_par <- ts_split(ts, sample.out = 9)
train <- ts_par$train
test <- ts_par$test
ts_info(train)
ts_info(test)
md <- modelo1_val
fc <- forecast(md, h = 9)
test_forecast(actual = ts,
              forecast.obj = fc,
              test = test)
class(ts_valor[1:20])
# seleccion del mejor delta
delta <- seq(0.05, 0.95, 0.05)
aic_1 <- vector()
ljungbox1 <- vector()
i = 0
for(d in delta){
  i = i+1
  modelo_outl <- tso(ts_ipc, delta=d)
  aic_1[i] <- modelo_outl$fit$aic
  ljungbox1[i] <- checkresiduals(modelo_outl$fit,
                                 plot = FALSE)$p.value
}
which.min(aic_1)
delta[16]
ljungbox1[16]
# outliers
mod_outlier <- tso(ts_ipc, delta=0.7)
mod_outlier
## con el optimo
mod_outlier_opt <- tso(ts_ipc, delta=0.8)
mod_outlier_opt

# intervenciones

 
modelo2_ipc <- arimax(datos$ipc_mensual, order=c(1, 0, 0), 
                      seasonal = list(order = c(0L, 1, 1), period = 12),
                      xtransf= data.frame(
                        enero_17=1 * (seq_along(datos$ipc_mensual) == 85),
                        abril_20=1 * (seq_along(datos$ipc_mensual) == 124),
                        enero_22=1 * (seq_along(datos$ipc_mensual) == 145)),
                      transfer=list(c(0, 0), c(0, 0), c(0, 0)))

 


 
modelo2_ipc %>% checkresiduals()
 

 
modelo2_ipc$residuals %>% shapiro.test()
modelo2_ipc$residuals %>% jarque.bera.test()
 

 
qqnorm(modelo2_ipc$residuals)
qqline(modelo2_ipc$residuals)

 


 
modelo2_ipc %>% coeftest()
 

