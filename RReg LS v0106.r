
library(PerformanceAnalytics)
library(AER)
library(car)
library(lmtest)
library(MASS)
library(nortest)
library(ppcor)


# REGRESION SIMPLE
#analisis robustez de datos.

plot(datos$VD~datos$VI)

# analisis previo grafico
scatterplotMatrix(datos)


# Obtenci?n del Modelo para analisis de supuestos
mod1<-lm(VD~VI,data = datos)

# analisis Independencia, Normalidad, Heterocedasticidad, Aporte de datos


par(mfrow=c(2,2))
plot(mod1)

par(mfrow=c(1,1))

#S1. analisis de residuales
mean(mod1$residuals)

#S3. normalidad de los errores o cualquiera que CUMPLA Libreria NORTEST
shapiro.test(mod1$residuals)

#s4. mirar grafico residuales vs valores estimados

#S5 Autocorrelacion
dwtest(mod1)


# s6 homocedasticidad
bptest(mod1)





#Analisis correlaciones
#analisis existencia de R H0: R=0
cor.test(datos$Calidad,datos$costo,method = "pearson")

#analisis existencia modelo
anova(mod1)

#analisis coeficientes.
summary(mod1)






#grafica de la prediccion
plot(datos$costo,datos$Calidad)
abline(a=mod1$coefficients[1],b=mod1$coefficients[2],col="red",lwd=5)


