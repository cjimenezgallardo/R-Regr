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


datos<-RLM_inv_los_rios
d2<-dplyr::select(datos,-PLATO,-DVI)
#asegurarse que el el numero de variables sea menor que el numero de datos.

# Crear el modelo
modfull1<-lm(MS~.,data = d2)
summary(modfull1)

# si el modelo presenta inconsistencias 
# analizar la viabilidad de limpiar la colinealidad
vif(modfull1)
chart.Correlation(d2)

#Proceder a descartar variables independientes que esten correlacionadas en funcion del
#menor aporte a la VD.


d2<-dplyr::select(datos,-PLATO,-APAR)

#volver a crear el modelo y evaluar
modfull1<-lm(MS~.,data = d2)
summary(modfull1)

#analizar supuesto de Colinealidad
vif(modfull1)  #un VIF correcto es el valor 1, por tanto, todos los mayores valores 
#son indicadores de multicolinealidad

#Visulizar probable colinealidades
chart.Correlation(d2)





# si no puede, definir un procedimiento de apoyo (forward, Backward, Stepwise)
#generar modelo simple de acuerdo a lo estimado
mod1<-lm(MS~FAPAR,data=d2) 

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

#supuesto media del error =0
mean(modfull1$residuals)

#supuesto Varianza Minima. para el primer modelo aceptado es correcto


#Supuesto de Normalidad
shapiro.test(modfull1$residuals)

#supuesto independecia
plot(modfull1)


#analisis homocedasticidad H0: Homocedasticidad
bptest(modfull1)

#analisis autocorrelacion H0: bp
dwtest(modfull1)

