###Cargar datos

library("readxl")
library("survival")
library("survminer")
library(brms)
library(rstan)


## Leer salida parte 3
#setwd("C:/Users/daago/OneDrive/Documentos/Unal/Maestría Estadística/2022-II/Trabajo de Grado/code/2. Modelamiento PM25")
getwd()
exceldata = read_excel("salidas/01-XG.xlsx")
#exceldata = read_excel("02-XG.xlsx")

exceldata
dfdata = data.frame(exceldata)
dfdata
names(dfdata)


mod1 <- lm(pm25_test ~ pm25_RF, data =dfdata)
summary(mod1)

newx <- seq(min(dfdata$pm25_RF), max(dfdata$pm25_RF), length.out=100)
preds <- predict(mod1,newdata = data.frame(pm25_RF=newx),interval = "predict",level=0.95)
#preds <- predict(mod1,newdata = data.frame(pm25_RF=newx),interval = "confidence",level=0.95)

#preds


par(mfrow=c(1,1))
par(mar=c(5,6,4,1)+.1)
#plot(dfdata$pm25_RF,dfdata$pm25_test,ylab = expression('Estimaciones mensuales de PM' [2.5]),
#     xlab = expression('Observaciones mensuales de PM'[2.5]),cex.lab=1.5,cex.axis=1.5, cex.main=1.5)
plot(dfdata$pm25_RF,dfdata$pm25_test,ylab = expression('Monthly estimates of PM' [2.5]),
     xlab = expression('Monthly observations of PM'[2.5]),cex.lab=1.5,cex.axis=1.5, cex.main=1.5)
#abline(a=0,b=1)
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey', border = NA)
#add fitted regression line
#abline(mod1)

#add dashed lines for confidence bands
lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')
lines(newx, preds[ ,1], lty = 'dashed', col = 'red')
abline(a=0,b=1)
points(dfdata$pm25_RF,dfdata$pm25_test)
grid()
