library(AER) #herramientas regresion y econometria
library(car) # regresion
library(doBy) #Estadisticas agrupadas
library(dplyr) #manejo de dataframes
library(e1071) # funciones de probabilidad
library(EnvStats) # informacion estadistica mas completa
library(foreign) #lectura datos distintas fuentes
library(GGally)
library(ggplot2) #mejoras de graficos
library(ISLR) #

library(lmtest) # test de analisis de regresion
library(MASS) # analisis estadisstico mas avanzado
library(nortest) #pruebas d enormalidad
library(PerformanceAnalytics) # Analisis de Riesgo y Regresion
library(ppcor) #correlaciones parciales y semi
library(psych) #procedimientos psicometricos y estadisticos

library(tidyverse)#anexo manejo de datos


d01<-na.omit(d01$x9)
datos<-na.omit(d01[,-1])

modfull1<-lm(y~.,data = datos)
summary(modfull1)


chart.Correlation(d01)

# LIMPIAR cuando sea necesario
# datos2<-dplyr::select(datos,-APAR)
# chart.Correlation(datos2)
# datos3<-dplyr::select(datos2,-IAFS)
# chart.Correlation(datos3)
# plot(datos1$Pub_redes,datos1$Ventas)

# analizar la viabilidad de limpiar la colinealidad
# si no puede, definir un procedimiento de apoyo (forward, Backward, Stepwise)

#generar modelo full
modfull1<-lm(MS~.,data = datos16)
summary(modfull1)

#analizar supuesto de Colinealidad
vif(modfull1)

#Visulizar probable colinealidades
chart.Correlation(datos16)

#generar modelo simple de acuerdo a lo estimado
mod1<-lm(MS~.,data=datos)





#con procedimiento
#definir modelo a buscar
scopemod1=formula(modfull1)
scopemod1
#################################################


#modelo final metodo forward
modfwd1=step(object = mod1,scope = scopemod1,direction = "forward")
summary(modfwd1)
vif(modfwd1)
#################################################

#modelo final metodo backward
modbwd1=step(object = modfull1,scope = scopemod1,direction = "backward")
summary(modbwd1)
vif(modbwd1)
#################################################


#modelo final stepwise
modstep1=step(object = modfull1,scope = scopemod1,direction = "both")
summary(modstep1)
vif(modstep1)
#################################################


#modelo Limpieza colinealidad

dat1=dplyr::select(datos,MS,PLATO,NDVI,DVI)
chart.Correlation(datos16)
moddat16<-lm(MS~FPAR+BORDE+SAVI,data = datos16)
vif(moddat16)

summary(moddat16)



#analisis homocedasticidad H0: Homocedasticidad
bptest(moddat16)

#analisis autocorrelacion H0: bp
dwtest(moddat16)

#analisis Supuesto 1
mean(moddat16mean$residuals)

#analisis normalidad errores
shapiro.test(moddat16$residuals)
