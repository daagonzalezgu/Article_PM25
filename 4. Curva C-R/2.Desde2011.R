###Cargar datos

library("readxl")
library("survival")
library("survminer")

## Leer salida parte 3
#setwd("C:/Users/daago/OneDrive/Documentos/Unal/Maestría Estadística/2022-II/Trabajo de Grado/code/3. Cruce Informacion/salidas")
getwd()
exceldata = read_excel("../3. Cruce Informacion/salidas/Datos_para_R.xlsx")


exceldata
dfdata2009 = data.frame(exceldata)
names(dfdata2009)
summary(dfdata2009)

##Categoricas como factores
dfdata2009$sexo <- as.factor(dfdata2009$sexo)
dfdata2009$casado <- as.factor(dfdata2009$casado)
#dfdata$no_asegurado <- as.factor(dfdata$no_asegurado)
summary(dfdata2009)

df0 <-subset(dfdata2009,ANO >=2011)
dfdata2010<-df0


####1. Solo sexo

cox.mod1 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom ,data = dfdata2010)
summary(cox.mod1)
cox.zph(cox.mod1)


###2. Agregar edad 

cox.mod2 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    edad_18 + edad_19 + edad_20+edad_21 + edad_22 + edad_23+
                    edad_24 + edad_25 + edad_26,data = dfdata2010)
summary(cox.mod2)
cox.zph(cox.mod2)


###3- Agregar estado civil

cox.mod3 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_1+EstCiv_2+EstCiv_3+EstCiv_4+EstCiv_5+EstCiv_6,data = dfdata2010)
cox.mod3 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2,data = dfdata2010)

summary(cox.mod3)
cox.zph(cox.mod3)


####4- Agregar regimen de salud 

cox.mod4 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2+ Seg_social_1+Seg_social_2+
                    Seg_social_3+Seg_social_4,data = dfdata2010)
cox.mod4 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2+ Seg_social_2,data = dfdata2010)

summary(cox.mod4)
cox.zph(cox.mod4)



###5 Agregar grupo etnico

cox.mod5 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2+Seg_social_2+Grupo_1+Grupo_2+Grupo_3+Grupo_4+Grupo_5+Grupo_6+Grupo_9,data = dfdata2010)
cox.mod5 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2+Seg_social_2+Grupo_2,data = dfdata2010)

summary(cox.mod5)
cox.zph(cox.mod5)

##6- Agregar nivel educativo




cox.mod6 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_pred_prom +
                    EstCiv_2+Seg_social_2+Edu_1+Edu_2+
                    Edu_3+Edu_4+Edu_5+Edu_6+Edu_7+Edu_8+Edu_9+Edu_10+Edu_11+Edu_12,data = dfdata2010)
dfdata2010$pm25_prom <- (dfdata2010$pm25_pred_prom)*(-1)
cox.mod6 <- coxph(Surv(tiempo, estatus) ~sexo+pm25_pred+pm25_prom +
                    EstCiv_2+NoSubsidiado+Edu_12,data = dfdata2010)

summary(cox.mod6)
cox.zph(cox.mod6)




preds <- predict(cox.mod6, newdata = dfdata2010,
                 type = "risk", se.fit = TRUE)

dfdata2010$HR <- preds$fit
dfdata2010$lcl <- preds$fit - 1.96*preds$se.fit
dfdata2010$ucl <- preds$fit + 1.96*preds$se.fit
#dfdata

####HR
#plot(dfdata2010$pm25_pred,dfdata2010$HR, xlab = expression('Niveles anuales estimados de PM'[2.5]),
#     ylab = 'Hazard Ratio')
plot(dfdata2010$pm25_pred,dfdata2010$HR, xlab = expression('Niveles anuales estimados de PM'[2.5]),
     ylab = 'Cociente de riesgo')
grid()


boxplot(dfdata2010$HR)
summary(dfdata2010$HR)

plot(dfdata2010$pm25_pred_prom,dfdata2010$HR)

###Guardar
library("writexl")
write_xlsx(dfdata2010,"salidas/03-ModeloFinal.xlsx")


