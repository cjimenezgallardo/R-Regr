
#Regresion modelo exponencial simple Y=a*exp(b*x)+e
#Plot 
plot(datos$x,datos$y)

#obtencion de modelo linealizado
mod_exp1<-lm(log(y)~x,data = datos)

#obtencion de modelo linealizado
mod_exp2<-lm(log(y)~x-1,data = datos)

#analisis supuestos
mean(mod_exp1$residuals)
shapiro.test(mod_exp1$residuals)

#analisis Coeficiente
summary(mod_exp1)

#anova modelo
anova(mod_exp1)

#configuracion de parametros
inicio1<-list(b0=mod_exp1$coefficients[1],b1=mod_exp1$coefficients[2])

#ajuste de modelos
model1<-nls(y~b0*exp(b1*x),data = datos,start = inicio1)

#grafica
plot(datos$x,datos$y)
lines(datos$x,predict(model1,list(x=datos$x)),col="blue",lwd=3)
