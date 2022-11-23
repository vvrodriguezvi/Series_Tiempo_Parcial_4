## BASE DE EXPORTACIONES DE CAFÉ
## 0. Cargar librerías necesarias
library(tidyverse)
require(TSA)
require(readxl)
require(magrittr)
require(astsa)
library(janitor)
library(ggplot2)
require(forecast)
require(tseries)
require(lmtest)
library(stats)
library(TSstudio)
## 1. Depuramos la base, para que solo tenga las variables que necesitamos: Fecha y los datos del café

base_compl <- read_excel("anexo-exportaciones-cafe-carbon-petroleo-ferroniquel-no-tradicionales-sep22.xlsx", 
                          skip=12) %>% clean_names()
## la base va desde enero del 1992, hasta septiembre 2022

# Seleccionamos la fila 4, relacionada con las exportaciones en  y
# las columnas 1 hasta 442:
base <- base_compl[1:442, c(1,3)] %>% 
  rename("valor" = "miles_de_dolares_fob_3") %>%
  na.omit(base)
# Al final de cada año había una fila con un total por año, se quitó de la base.
x <- seq(13, 399, by=13)
base <- base[-(x), ]
## Se tienes datos mensuales, se crea data frama con las fechas(enero 1992-sep 2022)
exp_cafe <-  data.frame(fecha=seq(as.Date("1992/1/1"),
                                   as.Date("2022/9/1"), "months"),
                         valor=base[1:369, 2])
## Se divide el valor de las exportaciones en miles de dolares por 100.000
exp_cafe$valor <- exp_cafe$valor/1000
exp_cafe %>% ggplot(aes(x=fecha, y=valor))+
  geom_line(col="blue")+labs(x ="Fecha", y="Exportaciones de café (millón USD)")

#Se observa un cambio de nivel con pendientes no nulas, de 1992-2000
#Se observan 2 picos, algo no normlaes a la tendencia de la serie
# entre(2017 y en 2020 también podría ser)

## Ajustar un auto arima
ts_valor <- ts(exp_cafe$valor, start=c(1992,1),
               frequency = 12)

par(mfrow = c(1,2))
exp_cafe$valor %>%  acf(lag.max=50)
exp_cafe$valor %>% pacf(lag.max=50)
## EN LA ACF

# Se observa un decaimiento lento "cola"(se debe aplicar diferencia, d>=1)
# se observa estacionalidad en cada k*12

## En la PACF

# Se observa un decaimineto rápido (corte) en el lag 1, se observan otros 
# lags que sobresalen de las bandas de confianza pero no son muy significativos

## para la primera diferencia

par(mfrow = c(1,2))
exp_cafe$valor %>% diff() %>% acf(lag.max=50)
exp_cafe$valor %>%diff() %>% pacf(lag.max=50)

## aplica primera diferencia y observar las acf y pacf para identificar p,q

## Modelo 1 con auto-arima, sin considerar intervenciones

modelo1_val <- auto.arima(ts_valor,
                          stepwise = FALSE,
                          approximation = FALSE)

## ESTIMACION DE LOS COEFICIENTES DEL MODELO 1

modelo1_val %>% coeftest()

modelo1_val %>% checkresiduals(lag=25)
modelo1_val$residuals %>% qqnorm()
modelo1_val$residuals %>% qqline()

modelo1_val$residuals %>% shapiro.test()
modelo1_val$residuals %>% jarque.bera.test()

# Se observa no normalidad, ni incorrelación.
# Se observa don picos en los residuales, en 2017 aprox y en 2021??

# Se divide la base, antes y despues de intervencion Serie de ene 92- sep 2016

ts_valor_pre <- window(ts_valor, start = c(1992,1),
                        end=c(2016,9))
modelo_int <- auto.arima(ts_valor_pre, stepwise = FALSE,
           approximation = FALSE)


modelo2_val <- arimax(ts_valor, order=c(1, 1, 2),
                      seasonal = list(order = c(2, 0, 0)),
                      xtransf=data.frame(
                      sep2016a=1*(seq_along(ts_valor) == 297)),
                      transfer=list(c(1, 0)))

modelo2_val %>% coeftest()
modelo2_val$residuals %>% shapiro.test()

#
















## predicciones

ts <- ts_valor
ts_par <- ts_split(ts, sample.out = 12)

train <- ts_par$train

test <- ts_par$test

ts_info(train)
ts_info(test)

md <- modelo1_val
fc <- forecast(md, h = 12)
test_forecast(actual = ts,
              forecast.obj = fc,
              test = test)
class(ts_valor[1:20])
