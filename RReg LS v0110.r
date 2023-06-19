
library(PerformanceAnalytics)
library(AER)
library(car)
library(lmtest)
library(MASS)
library(nortest)
library(ppcor)
library(ggfortify)
library(lawstat)


# REGRESION SIMPLE
#analisis robustez de datos.

plot(datos$VD~datos$VI)

# analisis previo grafico
scatterplotMatrix(datos)
chart.Correlation(datos)


# Obtenci?n del Modelo para analisis de supuestos
mod1<-lm(VD~VI,data = datos)

# valores atipicos
plot(mod1,5)


#S1. analisis de residuales
mean(mod1$residuals)

#S3. normalidad de los errores o cualquiera que CUMPLA Libreria NORTEST
shapiro.test(mod1$residuals)
plot(mod1,2)

#s4. mirar grafico residuales vs valores estimados
plot(mod1,1)
autoplot(mod1,1)
lawstat::runs.test(mod1$residuals)  # H0 Errores independientes



#S5 Autocorrelacion  H0 : autocorrelacion =0
dwtest(mod1)
acf(mod1$residuals)


# s6 homocedasticidad  H0 : Homocedasticidad
plot(mod1,3)
bptest(mod1)





#Analisis correlaciones
#analisis existencia de R H0: R=0

cor(predict(mod1),datos$Y,method = "pearson") # entrega la correlacion general

cor.test(~Y+x,data=datos,method = "pearson")# evalua la existencia de relacion entre Y y X o lo que quiera


#analisis existencia modelo
anova(mod1)

#analisis coeficientes.
summary(mod1)






#grafica de la prediccion
plot(datos$costo,datos$Calidad)
abline(a=mod1$coefficients[1],b=mod1$coefficients[2],col="red",lwd=5)


