###Cargar datos

library("readxl")
library("survival")
library("survminer")
library(brms)
library(rstan)


## Leer salida parte 3
#setwd("C:/Users/daago/OneDrive/Documentos/Unal/Maestría Estadística/2022-II/Trabajo de Grado/code/5. Curva C-R/salidas/Pruebas")
getwd()
exceldata = read_excel("salidas/04-Promedio.xlsx")

exceldata
dfdata = data.frame(exceldata)
dfdata
names(dfdata)
#dfdata$lag = lag(dfdata$pm25_pred,-8)
#lag(dfdata$pm25_pred,k=8)
dfdata$lnHR <- log(dfdata$HR)
dfdata

min <- min(dfdata$pm25_pred)
min
mod1 <- lm(lnHR~log(pm25_pred-min+1),data=dfdata)
#mod1 <- lm(lnHR~I(pm25_pred-13.87939),data=dfdata)
summary(mod1)
anova(mod1)

newx <- seq(min(dfdata$pm25_pred), max(dfdata$pm25_pred), length.out=100)
preds <- predict(mod1,newdata = data.frame(pm25_pred=newx),interval = "predict",level=0.95)
#preds <- predict(mod1,newdata = data.frame(pm25_pred=newx),interval = "confidence",level=0.95)
preds <- exp(preds)
#preds


par(mfrow=c(1,1))
plot(dfdata$pm25_pred,dfdata$HR, xlab = expression('Niveles anuales estimados de PM'[2.5]),
     ylab = 'Cociente de riesgo')
#plot(dfdata$pm25_pred,dfdata$HR, xlab = expression('Annual estimates of PM'[2.5]),
     #ylab = 'Hazard Ratio')
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey', border = NA)
#add fitted regression line
#abline(mod1)

#add dashed lines for confidence bands
lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,1], lty = 'dashed', col = 'red')
points(dfdata$pm25_pred,dfdata$HR)
grid()

###Supuestos en general
par(mfrow=c(2,2))
plot(mod1)

shapiro.test(mod1$residuals)


mean(mod1$residuals)
library(car) # para el test de durbin-watson 

durbinWatsonTest(mod1,max.lag = 8)

plot(dfdata$pm25_pred,mod1$residuals, xlab = expression('Niveles anuales estimados de PM'[2.5]),
     ylab = 'Hazard Ratio')


mod2 <- lm(lnHR~log(lag(pm25_pred,-8)-min+1),data=dfdata)
durbinWatsonTest(mod2)
plot(mod2)




####Leer datos de localidades 

#setwd("C:/Users/daago/OneDrive/Documentos/Unal/Maestría Estadística/2022-II/Trabajo de Grado/code/2. Modelamiento PM25/salidas")
#getwd()
exceldata1 = read_excel("../2. Modelamiento PM25/salidas/pm25_pred_anual.xlsx")

exceldata1
dflocalidad = data.frame(exceldata1)
dflocalidad
names(dflocalidad)
preds_loc <- predict(mod1,dflocalidad,interval = "confidence",level=0.95)
preds_loc <- data.frame(exp(preds_loc))
dflocalidad$HR <- preds_loc$fit
dflocalidad$HR_l <- preds_loc$lwr
dflocalidad$HR_u <- preds_loc$upr
dflocalidad

###Guardar
library("writexl")
write_xlsx(dflocalidad,"salidas/04-HR_Localidad.xlsx")

