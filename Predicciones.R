# predicciones
# modelo 1: modelo sin intervención: serie original
exp_cafe %>% dim()
exp_cafe %>% ggplot(aes(x=fecha, y=valor))+geom_line(col="blue")
exp_cafe %>% head(4)
exp_cafe %>% tail(4)

serie_original <- ts(exp_cafe$valor, start=c(1992,1),
                frequency = 12)
modelo1 <- auto.arima(serie_original, stepwise = FALSE,
                      approximation = FALSE)
modelo1 %>% coeftest()
predict(modelo1, n.ahead = 12)

train1 <- window(serie_original, start=time(serie_original)[1],
                end = time(serie_original)[length(serie_original) - 12])
ts_info(train1)

test1 <- window(serie_original, start = time(serie_original)[length(serie_original)
                                          - 12 + 1],
               end = time(serie_original)[length(serie_original)])
ts_info(test1)

# modelo 1: train

modelo1.train <- auto.arima(train1, stepwise = FALSE,
                            approximation = FALSE)
checkresiduals(modelo1.train)

# Asumiendo que los residuales del modelo provienen de una distribución normal, entonces evaluamos la precisión del modelo
# mediante la función accuracy del paquete forecast

fore1 <- forecast(modelo1.train, h=12)
accuracy(fore1, test)

test_forecast(actual = serie_original, forecast.obj = fore1,
              test = test)

naive_model1 <- naive(train1, h = 12)
test_forecast(actual = serie_original,
              forecast.obj = naive_model1,
              test = test)

#NAIVE PARA ESTACIONALIDAD

accuracy(naive_model1, test)
snaive_model1 <- snaive(train1, h = 12)
test_forecast(actual = serie_original,
              forecast.obj = snaive_model1,
              test = test)

accuracy(snaive_model1, test)

#Modelo 2: tso de la serie original

delta <- seq(0.1, 0.90, 0.1)
aic_1 <- vector()
ljungbox1 <- vector()
i = 0

for(d in delta){
  i = i+1
  modelo_outl <- tso(train1, delta=d)
  aic_1[i] <- modelo_outl$fit$aic
  ljungbox1[i] <- checkresiduals(modelo_outl$fit,
                                 plot = FALSE)$p.value
}

which.min(aic_1)
delta[8]
ljungbox1[8]

modelo_train2 <- tso(train1, delta=0.8)
modelo_train2

modelo_train2$fit %>% checkresiduals()
modelo_train2$fit$residuals %>% shapiro.test()
modelo_train2$fit$residuals %>% jarque.bera.test()

npred <- 12
newxreg <- outliers.effects(modelo_train2$outliers,                             
                            length(train1) + npred)
newxreg <- ts(newxreg[-seq_along(train1),],
              start =time(serie_original)[length(serie_original)
                                          - 12 + 1])
fore2 <- forecast(modelo_train2$fit, h=12,
                  xreg = newxreg)

accuracy(fore1, test1)
accuracy(fore2, test1)

df_train <- data.frame(fecha=
                         exp_cafe$fecha[1:length(train1)],
                       real=
                         exp_cafe$valor[1:length(train1)],
                       pred1 = modelo1.train$fitted,
                       pred2 = modelo_train2$fit$fitted)

df_train %>% ggplot(aes(x=fecha, y=real), col="black")+
  geom_line()+
  geom_line(aes(x=fecha, y=pred1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=pred2),col="red", lty=3)

df_test <- data.frame(fecha=
                       exp_cafe$fecha[358:369],
                     real=
                       exp_cafe$valor[358:369],
                     pred1 = fore1$mean, pred2 = fore2$mean,
                     li1=fore1$lower[,2], ls1=fore1$upper[,2],
                     li2=fore2$lower[,2], ls2=fore2$upper[,2])

df_test %>% ggplot(aes(x=fecha, y=real), col="black")+
  geom_line()+
  geom_line(aes(x=fecha, y=pred1),col="blue")+
  geom_line(aes(x=fecha, y=li1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=ls1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=pred2),col="red", lty=3)+
  geom_line(aes(x=fecha, y=li2),col="red", lty=3)+
  geom_line(aes(x=fecha, y=ls2),col="red", lty=3)

##------------------------------------------------------------------------

modelo2 <- auto.arima(serie_boxcox, stepwise = FALSE,
                      approximation = FALSE)
modelo2 %>% coeftest()

predict(modelo2, n.ahead = 12)

train2 <- window(serie_boxcox, start=time(serie_boxcox)[1],
                 end = time(serie_boxcox)[length(serie_boxcox) - 12])
ts_info(train2)

test2 <- window(serie_boxcox, start = time(serie_boxcox)[length(serie_boxcox)
                                                             - 12 + 1],
                end = time(serie_boxcox)[length(serie_boxcox)])
ts_info(test2)

# modelo 3: train

modelo2.train <- auto.arima(train2, stepwise = FALSE,
                            approximation = FALSE)
checkresiduals(modelo2.train)

# Asumiendo que los residuales del modelo provienen de una distribución normal, entonces evaluamos la precisión del modelo
# mediante la función accuracy del paquete forecast

fore3 <- forecast(modelo2.train, h=12)
accuracy(fore3, test)

test_forecast(actual = serie_boxcox, forecast.obj = fore3,
              test = test)

naive_model1 <- naive(train2, h = 12)
test_forecast(actual = serie_boxcox,
              forecast.obj = naive_model1,
              test = test)

fore3_1 <- forecast(modelo2.train, h=12)


#NAIVE PARA ESTACIONALIDAD

accuracy(naive_model1, test)
snaive_model1 <- snaive(train2, h = 12)
test_forecast(actual = serie_boxcox,
              forecast.obj = snaive_model1,
              test = test)

accuracy(snaive_model1, test)

#Modelo 2: tso de la serie original

delta <- seq(0.1, 0.90, 0.1)
aic_1 <- vector()
ljungbox1 <- vector()
i = 0

for(d in delta){
  i = i+1
  modelo_outl <- tso(train2, delta=d)
  aic_1[i] <- modelo_outl$fit$aic
  ljungbox1[i] <- checkresiduals(modelo_outl$fit,
                                 plot = FALSE)$p.value
}

which.min(aic_1)
delta[8]
ljungbox1[8]

modelo_train2 <- tso(train2, delta=0.8)
modelo_train2

modelo_train2$fit %>% checkresiduals()
modelo_train2$fit$residuals %>% shapiro.test()
modelo_train2$fit$residuals %>% jarque.bera.test()

npred <- 12
newxreg <- outliers.effects(modelo_train2$outliers,                             
                            length(train2) + npred)
newxreg <- ts(newxreg[-seq_along(train2),],
              start =time(serie_boxcox)[length(serie_boxcox)
                                          - 12 + 1])
fore4 <- forecast(modelo_train2$fit, h=12,
                  xreg = newxreg)

accuracy(fore3, test2)
accuracy(fore4, test2)

df_train <- data.frame(fecha=
                         exp_cafe$fecha[1:length(train2)],
                       real=
                         exp_cafe$valor[1:length(train2)],
                       pred1 = InvBoxCox(modelo2.train$fitted, lambda = cafe_lambda),
                       pred2 = InvBoxCox(modelo_train2$fit$fitted, lambda = cafe_lambda))

df_train %>% ggplot(aes(x=fecha, y=real), col="black")+
  geom_line()+
  geom_line(aes(x=fecha, y=pred1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=pred2),col="red", lty=3)

df_test <- data.frame(fecha=
                        exp_cafe$fecha[358:369],
                      real=
                        exp_cafe$valor[358:369],
                      pred1 = fore3$mean, pred2 = fore4$mean,
                      li1=fore3$lower[,2], ls1=fore3$upper[,2],
                      li2=fore4$lower[,2], ls2=fore4$upper[,2])

df_test %>% ggplot(aes(x=fecha, y=real), col="black")+
  geom_line()+
  geom_line(aes(x=fecha, y=pred1),col="blue")+
  geom_line(aes(x=fecha, y=li1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=ls1),col="blue", lty=2)+
  geom_line(aes(x=fecha, y=pred2),col="red", lty=3)+
  geom_line(aes(x=fecha, y=li2),col="red", lty=3)+
  geom_line(aes(x=fecha, y=ls2),col="red", lty=3)












