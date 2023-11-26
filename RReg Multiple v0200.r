library(AER) #herramientas regresion y econometria
library(car) # regresion
library(doBy) #Estadisticas agrupadas
library(dplyr) #manejo de dataframes
library(e1071) # funciones de probabilidad
library(EnvStats) # informacion estadistica mas completa
library(foreign) #lectura datos distintas fuentes
library(GGally)
library(ISLR) #
library(ggfortify)
library(corrplot)
library(lawstat)
library(lmtest) # test de análisis de regresión
library(MASS) # análisis estadístico mas avanzado
library(nortest) #pruebas d normalidad
library(PerformanceAnalytics) # Análisis de Riesgo y Regresión
library(ppcor) #correlaciones parciales y semi
library(psych) #procedimientos metempsicosis y estadísticos
library(tidyverse)#anexo manejo de datos



# datos de ejemplo
Dat_PSU <- read_excel("PSU_diagnostico_01.xlsx") # los datos se encuentran en carpeta Datos para Ejercitar.



# creando el primer modelo
names(Dat_PSU) # reconociendo las variables.

# describir las variables y posibles asociaciones TEORICAS ENTRE ELLAS.
modelo01<- lm(rend1~.,data = Dat_PSU)

#NOTA: Frente a un modelo R Múltiple, siempre es aconsejable disminuir el numero de variables independientes.

# al existir muchas variables, es aconsejable reconocer problemas de colinealidad 
# Análisis de colinealidad
vif(modelo01)

#Si existen errores es necesario revisar el modelo
summary(modelo01)
chart.Correlation(Dat_PSU)

#otra forma de presentarlos.
ggpairs(Dat_PSU, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

corrplot(cor(Dat_PSU),method = "color",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.col = "black",
         tl.srt =45 )

# se observa la existencia de un problema en la variable PROMPSU, y la colinealidad perfecta en PEM

dat2<- dplyr::select(Dat_PSU,-PROMPSU,-PEM)

modelo02<-lm(rend1~.,data = dat2)

# ANalisis de colinealidad
vif(modelo02)

chart.Correlation(dat2,method = "pearson",histogram = TRUE)


#ARREGLAR hasta encontrar un VIF muy cercano a 1
dat2<- dplyr::select(Dat_PSU,-PROMPSU,-PEM,-HIST,-PSUPOND,-LENG)

#LUEGO



#busqueda de datos atípicos

plot(modelo02,5)
plot(modelo02,1)
influencePlot(modelo02)

# Análisis de Supuestos

#supuesto media del error =0
mean(modelo02$residuals)

#supuesto Varianza Minima. para el primer modelo aceptado es correcto


#Supuesto de Normalidad
shapiro.test(modelo02$residuals)

#supuesto independecia
lawstat::runs.test(modelo02$residuals)

#analisis homocedasticidad H0: Homocedasticidad
bptest(modfull1)
plot(modelo02,3)

#analisis autocorrelacion H0: No hay
dwtest(modfull1)



#Evaluacion Calidad Modelo

# Evaluar parámetros del modelo
summary(modelo02) 


# Evaluar Correlacion
cor(predict(modelo02),dat2$rend1,method = "pearson") # entrega la correlacion general

cor.test(predict(modelo02),dat2$rend1,data=dat2,method = "pearson")# evalua la existencia de relacion entre Y y X o lo que quiera


#Evaluar Modelo
summary(aov(modelo02))






# EXPLICAR EL MODELO
