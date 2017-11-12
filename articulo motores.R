#base de datos(ajustes)

datos<-read.csv2(dec=".",file="articulo analisis.csv")
datos$t_mezclador=as.factor(datos$t_mezclador)
datos$t_regulador=as.factor(datos$t_regulador)
datos$u_mezclador=as.factor(datos$u_mezclador)
datos$modo =as.factor(datos$modo)
#ajustes de los modelos para las 5 variables respuesta

mod1 <- with(datos,lm(cons_combus~t_mezclador+modo+t_regulador+u_mezclador)) #combustible
mod2 <- with(datos,lm(rendimiento_v~t_mezclador+modo+t_regulador+u_mezclador)) #rendimiento
mod3 <- with(datos,lm(dosado_r~t_mezclador+modo+t_regulador+u_mezclador)) #dosado
mod4 <- with(datos,lm(CO~t_mezclador+modo+t_regulador+u_mezclador)) # CO
mod5 <- with(datos,lm(CH4~t_mezclador+modo+t_regulador+u_mezclador)) #CH4

#verificacion de variables significativas

anova(mod1)### aqui aceptamos tipo de mezclador

anova(mod2)#### aqui se acepta el regulador

anova(mod3)#aqui aceptacion la ubicacion del mezclador y el tipo de mezclador

anova(mod4)#aqui aceptacmos el tipo de mezclador

anova(mod5)### todo bien todo bonito

#graficos para cada variable de respuesta
nombres=c("consumo","Rend vol","Dosado relativo","CO","CH4")
for(i in 3:7){
  par(mfrow=c(2,2))
  boxplot(datos[,i]~datos$t_regulador,xlab="Regulador",ylab=nombres[i-2])
  boxplot(datos[,i]~datos$modo,xlab="potencia",ylab=nombres[i-2])
  boxplot(datos[,i]~datos$u_mezclador,xlab="Ubicación",ylab=nombres[i-2])
  boxplot(datos[,i]~datos$t_mezclador,xlab="Mezclador",ylab=nombres[i-2])
}
#en general se difiere mucho con la variable ubicacion del mezclador

#graficos de residuales y influencia para CO y el Dosado

plot(mod4)  #CO
plot(mod3)  #Dosado


####  lado oscuro de la regresion
require(gamlss)
                                   #consumo de combustible
fd1<- fitDist(datos$cons_combus,type="realplus")
histDist(datos$cons_combus,family=names(fd1$fits[1]),ylim=c(0,0.6),main="")
modelito1<-with(datos,gamlss(cons_combus~t_mezclador+modo+t_regulador+u_mezclador,family=WEI3))
summary(modelito1)## vemos que el tipo de mesclador no es significativo
#dado las otras variables
modelito1<-with(datos,gamlss(cons_combus~modo+t_regulador+u_mezclador,family=WEI3))

plot(modelito)
plot(mod1)

                               #rendimiento volumetrico
hist(datos$rendimiento_v)
fd2<- fitDist(datos$rendimiento_v,type="realplus")
histDist(datos$rendimiento_v,family=names(fd2$fits[3]),ylim=c(0,6),main="")
modelito2<-with(datos,gamlss(rendimiento_v~t_mezclador+modo+t_regulador+u_mezclador,family=IG))
summary(modelito2)#vemos que t_regulador y u_mezclador no son significantes dadas las otras
modelito2<-with(datos,gamlss(rendimiento_v~t_mezclador+modo,family=IG))

                                    #dosado relativo
with(datos,hist(dosado_r))
fd3<- fitDist(datos$dosado_r,type="realplus")
histDist(datos$dosado_r,family=names(fd3$fits[3]),ylim=c(0,1.2),main="")
modelito3<-with(datos,gamlss(dosado_r~t_mezclador+modo+t_regulador+u_mezclador,family=BCCGo))
summary(modelito3)

                                #CO
with(datos,hist(CO))
fd4<- fitDist(datos$CO,type="realplus")
histDist(datos$CO,family=names(fd4$fits[2]),ylim=c(0,1.2),main="")
modelito4<-with(datos,gamlss(CO~t_mezclador+modo+t_regulador+u_mezclador,family=GG,control=gamlss.control(n.cyc=200)))
summary(mod4)#vemos que el t_mezclador no es significante
modelito4<-with(datos,gamlss(CO~modo+t_regulador+u_mezclador,family=GG,control=gamlss.control(n.cyc=100)))
# el puto gamlss no funciona

                                    #CH4

with(datos,hist(CH4))
fd5<- fitDist(datos$CH4,type="realplus")
histDist(datos$CH4,family=names(fd5$fits[4]),ylim=c(0,2.3),main="")
modelito5<-with(datos,gamlss(CH4~t_mezclador+modo+t_regulador+u_mezclador,family=GIG))
summary(modelito5)#t_regulador y u_mezclador no son significantes
modelito5<-with(datos,gamlss(CH4~t_mezclador+modo,family=GIG))


### las comparaciones se haran con modelo$y
matriz<-matrix(c(fitted(modelito1),datos$cons_combus,fitted(modelito2),datos$rendimiento_v,fitted(modelito3),datos$dosado_r,fitted(modelito4),datos$CO,fitted(modelito5),datos$CH4),byrow=F,nrow=60)
variable<-c("combus","rendi","dosado","CO","CH4")
contador=1
correlacion=0
for(i in c(1,3,5,7,9)){
  correlacion[contador]<-cor(matriz[,i],matriz[,i+1])
  contador=contador+1
}
tabla=rbind(variable,correlacion)

#tarea numero 5 (creo) variable combustible
fd1<- fitDist(datos$cons_combus,type="realplus")
