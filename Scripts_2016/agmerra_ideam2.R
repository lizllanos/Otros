mean22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.5){
    x=NA
  }else{x=mean(a,na.rm=T)}
  
  return(x)
} 


##TMIN

obs=read.csv(file = "Agmerra vs stations/tmin_obs.csv",header = T)
agmerra=read.csv(file = "Agmerra vs stations/agmerra_tmin.csv",header = T)
obs_monthly=aggregate(obs[-3:-1],list(month=obs$month,year=obs$year),mean22)
agmerra_monthly=aggregate(agmerra[,-1],list(month=obs$month,year=obs$year),mean22)

x11()
par(mfrow=c(4,3))

for(i in 1:11){
plot(obs[,i+3],col="red",type="l",main=names(obs)[i+3],ylab="Temperatura Mínima")
lines(agmerra[,i+1],type="l")
}

plot(1, type="n", axes=F, xlab="", ylab="")
legend("topright",c("Observado", "AgMERRA"),col=c("red","black"), lwd = c(2,2),bty="n",cex=3)




x11()
par(mfrow=c(4,3))

for(i in 1:11){
  
plot(obs_monthly[,i+2],col="red",type="l",main=names(obs)[i+3],ylab="Temperatura Mínima")
lines(agmerra_monthly[,i+2],type="l")
}

plot(1, type="n", axes=F, xlab="", ylab="")
legend("topright",c("Observado", "AgMERRA"),col=c("red","black"), lwd = c(2,2),bty="n",cex=3)


##TMAX

obs=read.csv(file = "Agmerra vs stations/tmax_obs.csv",header = T)
agmerra=read.csv(file = "Agmerra vs stations/agmerra_tmax.csv",header = T)
obs_monthly=aggregate(obs[-3:-1],list(month=obs$month,year=obs$year),mean22)
agmerra_monthly=aggregate(agmerra[-3:-1],list(month=obs$month,year=obs$year),mean22)

x11()
par(mfrow=c(4,3))

for(i in 1:11){
  plot(obs[,i+3],col="red",type="l",main=names(obs)[i+3],ylab="Temperatura Máxima")
  lines(agmerra[,i+3],type="l")
}

plot(1, type="n", axes=F, xlab="", ylab="")
legend("topright",c("IDEAM Station", "AgMERRA"),col=c("red","black"), lwd = c(2,2),bty="n",cex=3)



x11()
par(mfrow=c(4,3))

for(i in 1:11){
  
  plot(obs_monthly[,i+2],col="red",type="l",main=names(obs)[i+3],ylab="Temperatura Máxima")
  lines(agmerra_monthly[,i+2],type="l")
}

plot(1, type="n", axes=F, xlab="", ylab="")
legend("topright",c("Observado", "AgMERRA"),col=c("red","black"), lwd = c(2,2),bty="n",cex=3)


########################
#Analisis descriptivo y de correlacion daily

descript <- function(object){ #Función para descriptivas de las variables
  n=length(object)
  Media=round(mean2(object, na.rm=T),3);  Varianza=round(var(object, na.rm=T),3);  Desv.Est=round(sqrt(Varianza),3)
  Mediana=round(as.numeric(quantile(object,probs=0.5, na.rm=T)),3);  Coef.Var=round((sqrt(Varianza)/Media)*100,3)
  Mín=round(min(object,na.rm=T),2); Máx=round(max(object,na.rm=T),2)
  Datos.NA=round(sum(is.na(object)),2)
  Porcentaje.NA=round((Datos.NA/length(object))*100,3)
  result=cbind(n,Mín,Máx,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA,Porcentaje.NA)
}

sapply(obs[-3:-1],descript)
sapply(agmerra[-3:-1],descript)

M=cor(obs[-3:-1],agmerra[-3:-1],use="pairwise.complete.obs")

library(RColorBrewer)
library(corrplot)
x11()
corrplot(M, type="upper", order="hclust", col=col,tl.col="black", tl.srt=45)


x11()
par(mfrow=c(4,3))


for(i in 1:11){
plot(obs[,i+3],agmerra[,i+3],pch=20,cex.lab=1,main=paste(names(obs)[i+3]),ylab="AgMERRA", xlab="IDEAM Station")
fit=lm(obs[,i+3]~agmerra[,i+3])
abline(fit,col="red",lwd=2)
mylabel = bquote(italic(R)^2 == .(format(summary(fit)$adj.r.squared, digits = 3)))
legend("bottomright", bty="n", legend=mylabel,cex=1.5)
}


#monthly
#Analisis descriptivo y de correlacion daily

sapply(obs_monthly[-2:-1],descript)
sapply(agmerra_monthly[-2:-1],descript)

M=cor(obs_monthly[-2:-1],agmerra_monthly[-2:-1],use="pairwise.complete.obs")

library(RColorBrewer)
library(corrplot)
x11()
corrplot(M, type="upper", order="hclust", col=col,tl.col="black", tl.srt=45)


x11()
par(mfrow=c(4,3))


for(i in 1:11){
  plot(obs_monthly[,i+2],agmerra_monthly[,i+2],pch=20,cex.lab=1,main=paste(names(obs_monthly)[i+2]),ylab="AgMERRA", xlab="IDEAM Station")
  fit=lm(obs_monthly[,i+2]~agmerra_monthly[,i+2])
  abline(fit,col="red",lwd=2)
  mylabel = bquote(italic(R)^2 == .(format(summary(fit)$adj.r.squared, digits = 3)))
  legend("bottomright", bty="n", legend=mylabel,cex=1.5)
}
