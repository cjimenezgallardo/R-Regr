library(ISLR)
library(caret)
library(tibble)
library(e1071)
library(tidyverse)
library(PerformanceAnalytics)
library(vcd)
library(ggplot2)


datos<-PSU_diagnostico_01
datos$salta=c(0)
datos$salta[datos$rend1>=60]=c(1)



#grafica de correlaciones
chart.Correlation(datos)

###########################################################################
#consejo
#en pos de una validacion del modelo, generar 2 archivos, uno de generacion y uno de testeo
datosindex=createDataPartition(datos$salta,p=0.6)$Resample1 #0.6, valor de corte en cantidad 60% para un grupo
dat_testeo=datos[-datosindex,]
dat_desarrollo=datos[datosindex,]


#################################################################
#ajuste por modelo logit
mglm1<-glm(salta~MATM,data = datos,family = binomial) # puede ser poisson

summary(mglm1)

#################################################################
#revisar AIC AKAIKE.

#################################################################


#Exprension sigmoide
valormgl=predict(object = mglm1,newdata = data.frame(MATM=750))
1/(1+exp(-valormgl))

VX=750 # variable auxiliar
exp(mglm1$coefficients[1]+mglm1$coefficients[2]*VX)/(1+exp(mglm1$coefficients[1]+mglm1$coefficients[2]*VX))

######################################################################################################
# Vizualizar gráfica del modelo.
ggplot(data = datos, aes(x = MATM, y = salta)) +
  geom_point(aes(color = as.factor(MATM)), shape = 1) + 
  stat_function(fun = function(x){predict(mglm1,
                                          newdata = data.frame(MATM = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresion logistica",
       y = "") +
  theme(legend.position = "none")




summary(mglm1)

##################################################################
#evaluacion del modelo
#se genera un analisis entre el modelo solo sin variable y se compara contra el modelo con

dif_residuos <- mglm1$null.deviance - mglm1$deviance

# Grados libertad
gl <- mglm1$df.null - mglm1$df.residual

# p-value
p_value <- pchisq(q=dif_residuos,df=gl,lower.tail = FALSE)


paste("Diferencia de residuos:", round(dif_residuos, 4))
paste("Grados de Libertad:",gl)
paste("p-value:", p_value)

anova(mglm1,test = "Chisq")

##################################################################
#Matriz de confusion
predicciones <- ifelse(test = mglm1$fitted.values > 0.6, yes = 1, no = 0)
matriz_confusion <- table(mglm1$model$salta, predicciones,
                          dnn = c("observado", "esperado"))
matriz_confusion

###################################################################
#Analisis Capacidad predictiva (matriz de confusion)

r=(matriz_confusion[1,1]+matriz_confusion[2,2])/(matriz_confusion[1,1]+matriz_confusion[1,2]+matriz_confusion[2,1]+matriz_confusion[2,2])
r

####################################################################
#evaluacion de la estimacion
#Exprension sigmoide
 valormgl=predict(object = mglm1,newdata = data.frame(MATM=697))
1/(1+exp(-valormgl))


#Expresion logistica
VX=750 # variable auxiliar
exp(mglm1$coefficients[1]+mglm1$coefficients[2]*VX)/(1+exp(mglm1$coefficients[1]+mglm1$coefficients[2]*VX))


mglm1
# b0= -29.0872 y b1= 0.0458




