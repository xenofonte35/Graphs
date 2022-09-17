#modeloVAR:https://youtu.be/AhSENS5Ka1U
#el modelo VAR parte de un efoque ateorico 
#separa los efectos pasados que explican al vector de las variables
#endogenas a traves de su pasado o mediante variables autoregresivas
#el mas sencillo es el bivariado, pero tiene hasta 12 ordenes
MEXUSA <- read_csv("MEXUSA.csv")
library(tidyverse)
library(lubridate)
library(car)
library(urca)
library(tseries)
library(astsa)
library(forecast)
library(foreign)
library(timsac)
library(vars)
library(lmtest)
library(mFilter)
library(dynlm)
library(nlme)
library(quantmod)
library(xts)
attach(MEXUSA)
names(MEXUSA)
#convertir a series de tiempo
mex<-ts(MEXUSA[,1],start = c(1994,1),frequency = 4)
usa<-ts(MEXUSA[,2],start = c(1994,1),frequency = 4)
mex
usa
ndiffs(mex)
ndiffs(usa)#dice que debemos diferenciar una vez
ts.plot(mex,usa,col=c("blue","red"))
#diferenciamos una vez las dos series
mex1<-diff(mex)
usa1<-diff(usa)
ts.plot(mex1,usa1,col=c("blue","red"))
#generamos pruebas de granger de causalidad
#Ho:el PIB de USA no causa en el sentido de granger al de MEX>0.05
#Ha:el PIB de USA si causa en el sentido de granger al de MEX>0.05
grangertest(mex1~usa1,order=2)
#se acepta la Ha con dos retrasos
#Ho:el MEX de USA no causa en el sentido de granger al de USA>0.05
#Ha:el MEX de USA si causa en el sentido de granger al de USA>0.05
grangertest(usa1~mex1,order=1)
#se acepta la Ha en todos los retrasos del 1 al 12
evar<-cbind(mex1,usa1)
print(evar)
#vamos a ver cual es orden de nuestro modelo VAR
VARselect(evar,lag.max = 12)
var1<-VAR(evar,p=4)
var1#arroja los coeficientes tanto de la primera ecuacion como de la segunda
#pruebas de especificacion
summary(var1)#si todos los numero que arroja el modelo son
#menores que 1, cumplimos con las condiciones de estabilidad
#que el numero correcto de rezagos si es 4
#podemos graficarlo
plot(var1)

# ejemplo con TC y BMV
TCBMV <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/R/TCBMV.csv")
attach(TCBMV)
names(TCBMV)
TCBMV
#vamos a convertir este data frame a una serie de tiempo
#empezamos por el tipo de cambio que lo llamamos tms2
tms2<-ts(TCBMV[,1],start=c(1993,1),freq=12)
#luego seguimos con la BMV que llamamos tp
tp<-ts(TCBMV[,2],start=c(1993,1),freq=12)
#vamos a generar logaritmos
ltm2<-log(tms2)
ltp<-log(tp)
#calculamos cuantas veces debe ser diferenciada una variable para lograr la estacionariedad
ndiffs(ltm2)#el resultado arroja que es 1. una vez debemos diferenciar
ndiffs(ltp)
#hay que trabajar con el mismo numero de diffs, sino se desbalancea la base de datos
#graficamos
ts.plot(ltp,ltm2,col=c("blue","red"))
#diferenciamos una vez tanto el TC como la BMV
dltp<-diff(ltp)
dltm2<-diff(ltm2)
#ahora graficamos
ts.plot(dltp,dltm2,col=c("blue","red"))
#generamos pruebas de causalidad de granger para ver que afecta a que
#esta es la primera parte de los modelos VAR, la causalidad
grangertest(dltp~dltm2,order=1)
#Ho:el tipo de cambio (dltm2)no causa en el sentido de Granger el IPCBMV(dltp)
#Ha:el tipo de cambio (dltm2)si causa en el sentido de Granger el IPCBMV(dltp)
#el resultado me arroja una p-value de 001364, con lo cual rechazo la Ho
#probamos al reves
grangertest(dltm2~dltp,order=1)
#Ho:la BMV(dltp)no causa en el sentido de Granger el TC(dltm2)
#Ho:la BMV(dltp)si causa en el sentido de Granger el TC(dltm2)
#El resultado arroja una p-value de 6.595e-05 *** muy pequeña por lo que
#rechazamos la hipotesis nula
#debemos hacer la prueba hasta doce atrasos
#creo un objeto con las primeras diferencias
vardltm2<-ts(dltm2,start = 1993,freq=12)
vardltp<-ts(dltp,start = 1993,freq=12)
ejvar<-cbind(vardltm2,vardltp)
#con ejvar uno la primera diferencia de ltm2 y ltp
ejvar
#Proceso VAR para saber el orden de nuestro modelo
VARselect(ejvar,lag.max = 12)
#el resultado nos dice que el orden es de 1. Así que lo tomamos
var1<-VAR(ejvar,p=1)
var1#esto nos da los coeficientes para la primera y segunda ecuacion mas la constante
#hacemos las pruebas de especificacion del modelo VAR
summary(var1)
#nos fijamos en las raíces del polinomio. Si son todos menores que uno
#estamos cumpliendo con la condicion de estabilidad.
#En este ejercicio, se cumplen 0.2184 y 0.2061
#ahora graficamos para observar el ACF y el PACF
plot(var1)
#usamos el comando de prueba de autocorrelacion serial
seriala<-serial.test(var1,lags.pt = 1, type = "PT.asymptotic")
#le pedimos que la imprima. Revisamos el p-value
seriala$serial
#Ho:los residuales no estan correlacionados >0.05 no rechazar la Ho
#Ha:los residuales si estan correlacionados <0.05 si rechazar la Ho
#la p-value es 2.2e-16 menor a 0.05 por tanto rechazamos la Ho
#se aprobo la prueba de autocorrelacion serial
#ahora procedemos a aplicar las prueba norma de los residuales
normalidad<-normality.test(var1)
normalidad$jb.mul
#nos fijamos en la curtosis y en el sesgo
#Ho:los residuales se distribuyen normalmente >0.05 No rechazar Ho
#Ha:los residuales no se distribuyen normalmente <0.05 Rechazar Ho
# como el resultado nos arroja un p-value muy pequeño, concluimos
# que no hay una distribucion normal en los errores
#llevamos 3 pruebas: estabilidad, autocorrelacion serial y normalida de resid
#ahora sigue la prueba de homocedasticidad de la var. de los resid
#revisamos si la varianza de los residuales es constante o no
arch1<-arch.test(var1, lags.multi = 1)
arch1$arch.mul
#Ho:la varianza de los residuales es constante
#Ha:la varianza de los residuales no es constante
#el resultado arroja una p-value de 1.423e-09, con lo cual
#se conclucluye que la varianza de los residuales no es constante
#ahora hacemos la siguiente prueba:
#Impulso respuesta de la BMV(dltp) ante una innovacion del TC(dltm2)
var1_irflp=irf(var1,response = "vardltp",n.ahead = 8,boot = TRUE)
var1_irflp
#esto nos dice como responde la BMV ante un impulso del TC
#nos pone la banda hacia abajo con un 95% de confianza. Lo graficamos
plot(var1_irflp)
#dos graficos. analisis
#ahora descomponemos la varianza ante un innovacion de la BMV(dltp)
VAR1_DESVAR_bmv<-fevd(var1,n.ahead = 50)$vardltp
VAR1_DESVAR_bmv
#ahora descomponemos la varianza ante un innovacion del TC(dltm2)
VAR1_DESVAR_tc<-fevd(var1,n.ahead = 50)$vardltm2
VAR1_DESVAR_tc

#fuente: https://youtu.be/bVyIrow238o