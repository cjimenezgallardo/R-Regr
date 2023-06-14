library(PerformanceAnalytics)


datos$Salta[datos$rend1>=60]=c(1)
datos$Salta[datos$rend1<60]=c(0)

#grafica de correlaciones
chart.Correlation(datos)

#visualizar diferencias entre los 0-1
t.test(subset(datos,Salta==0)$MATM,subset(datos,Salta==1)$MATM)

library(ggplot2)
#visualizar por boxplot diferencias para el factor.
ggplot(data = datos, aes(x = as.factor(datos$Salta), y = MATM, color = as.factor(datos$Salta))) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

#consejo
library(caret)
#en pos de una validacion del modelo, generar 2 archivos, uno de generacion y uno de testeo
datosindex=createDataPartition(datos$Salta,p=0.6)$Resample1 #0.6, porcentaje de elementos para el bootstrap
dat_testeo=datos[-datosindex,]
dat_desarrollo=datos[datosindex,]

ggplot(dat_desarrollo, aes(x = MATM, y = Salta)) +
  geom_jitter(width = 0.5, height = 0.03, alpha = .2) +
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial")) +
  labs(y = expression(hat(P)(Salta)))


#primer modelo glm
mgl1<-glm(Salta~MATM,data = dat_desarrollo, family = "binomial")
summary(mgl1)

#visualizacion predicciones
head(predict(mgl1,type="response"))

#geracion de predicciones
desarrollo_predictivo <- ifelse(predict(mgl1, type = "response") > 0.6, 1, 0)
head(desarrollo_predictivo)

#Analisis Capacidad predictiva (matriz de confusion)
des_tabla<-table(predicted=desarrollo_predictivo,actual=dat_desarrollo$Salta)
des_tabla
r=(des_tabla[1,1]+des_tabla[2,2])/(des_tabla[1,1]+des_tabla[1,2]+des_tabla[2,1]+des_tabla[2,2])

r

calc_class_err <- function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(actual = dat_desarrollo$Salta, predicted = desarrollo_predictivo)

confusionMatrix(des_tabla,positive = "1")


