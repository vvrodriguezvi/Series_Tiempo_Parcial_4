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
require(tsoutliers)

base_compl <- read_excel("anexo-exportaciones-cafe-carbon-petroleo-ferroniquel-no-tradicionales-sep22.xlsx", 
                         skip=12) %>% clean_names()

base <- base_compl[1:442, c(1,3)] %>% 
  rename("valor" = "miles_de_dolares_fob_3") %>%
  na.omit(base)
x <- seq(13, 399, by=13)
base <- base[-(x), ]
exp_cafe <-  data.frame(fecha=seq(as.Date("1992/1/1"),
                                  as.Date("2022/9/1"), "months"),
                        valor=base[1:369, 2])

exp_cafe$valor <- exp_cafe$valor/100000

serie_original <- ts(exp_cafe$valor, start=c(1992,1),
               frequency = 12)

# serie transformada boxcox, estabilizar varianza

cafe_lambda <- BoxCox.lambda(serie_original)
cafe_lambda
serie_boxcox <- BoxCox(serie_original,
                         lambda=cafe_lambda)

serie_log <- log(serie_original)
# grafico ambas series

win.graph()
par(mfrow=c(1,2))
serie_original %>% plot(main="Serie Original",
                        las=1)
serie_boxcox %>% plot(main="Serie Transformada - boxcox",
                         las=1)
win.graph()
par(mfrow=c(1,2))
serie_original %>% plot(main="Serie Original",
                        las=1)
serie_log %>% plot(main="Serie Transformada - log",
                   las=1)

serieOriginal_decompose <- decompose(serie_original, type="multiplicative")

serieBox_decompose <- decompose(serie_boxcox, type="additive")
plot(serieBox_decompose)
# ajustar modelos con la serie boxcox

# Modelo 1: autoarima para la serie original

modelo1 <- auto.arima(serie_original, stepwise = FALSE,
                      approximation = FALSE)
modelo1 %>% checkresiduals()
modelo1 %>% coeftest()
modelo1$residuals %>% shapiro.test()
modelo1$residuals %>% qqnorm()
modelo1$residuals %>% qqline()

# Modelo 2: Autoarima con la serie transformada

modelo2 <- auto.arima(serie_boxcox, stepwise = FALSE,
                      approximation = FALSE)
modelo2 %>% checkresiduals()
modelo2 %>% coeftest()
modelo2$residuals %>% shapiro.test()
modelo2$residuals %>% qqnorm()
modelo2$residuals %>% qqline()

# Modelo 3: Serie original con valores outliers

delta <- seq(0.05, 0.95, 0.05)
aic_1 <- vector()
ljungbox1 <- vector()
i = 0

for(d in delta){
  i = i+1
  modelo_outl <- tso(serie_original, delta=d)
  aic_1[i] <- modelo_outl$fit$aic
  ljungbox1[i] <- checkresiduals(modelo_outl$fit,
                                 plot = FALSE)$p.value
}

aic1 <- which.min(aic_1)
delta1 <- delta[aic1]
delta1

modelo3 <- tso(serie_original, delta=0.65)


modelo3_val <- arimax(serie_original, order=c(1, 1, 1),
                      seasonal = list(order = c(0, 0, 2)),
                      xtransf=data.frame(
                        
                        mayo1997a=1*(seq_along(serie_original) == 65),
                        diciembre2010a=1*(seq_along(serie_original) == 228),
                        
                        abril2016a=1*(seq_along(serie_original) == 292),
                        dic2016a=1*(seq_along(serie_original) == 300),
                        
                        julio2021a=1*c(rep(0,354),rep(1,15))),
                        
                        transfer=list(c(1, 0),c(1, 0),c(0, 0),
                                    c(0, 0), c(0, 0)))


modelo3_val %>% coeftest()

# Modelo 3: Serie transformada con valores outliers

delta <- seq(0.05, 0.95, 0.05)
aic_1 <- vector()
ljungbox1 <- vector()
i = 0

for(d in delta){
  i = i+1
  modelo_outl <- tso(serie_boxcox, delta=d)
  aic_1[i] <- modelo_outl$fit$aic
  ljungbox1[i] <- checkresiduals(modelo_outl$fit,
                                 plot = FALSE)$p.value
}

which.min(aic_1)

modelo4 <- tso(serie_boxcox, delta=0.95)


modelo3_val <- arimax(serie_boxcox, order=c(0, 1, 1),
                      seasonal = list(order = c(2, 0, 0)),
                      xtransf=data.frame(
                        
                        junio1994a=1*(seq_along(serie_boxcox) == 30),
                        
                        sept2004a=1*(seq_along(serie_boxcox) == 153),
                        dic2016a=1*(seq_along(serie_boxcox) == 300),
                      
                        transfer=list(c(0, 0),c(0, 0),c(0, 0),
                                      c(0, 0), c(1, 0), c(0,0),
                                      c(1,0))))
modelo3_val %>% coeftest()

