datos=read.table("clipboard",header=T,dec=",")
attach(datos)

x11()
par(mfrow=c(3,1))
station=names(datos[-3:-1])
plot(datos[,4],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[1],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)

plot(datos[,5],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[2],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2,xlim=c(0,300))

plot(datos[,6],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[3],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)


x11()
par(mfrow=c(1,3))
hist(datos[,4],main=station[1],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,5],main=station[2],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,6],main=station[3],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))

x11()
boxplot(datos[-3:-1],ylab="Precipitacion")

plot(1:dim(datos)[1],data_gendatos[,i+3],,main=paste("datos_",station[i]),type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(datos[,i+3],na.rm=T)),col="blue",lwd=1.2,  xaxt="n")
lines(1:dim(tmax)[1],tmax[,i+3],col="red",lwd=1.2)
axis(side=1,labels=seq(min(tmax$year),max(tmax$year),3),at=seq(1,dim(tmax)[1],by=1080),las=2)


precip1=ts(precip,start=min(precip$year),frequency=365)


precip1=ts(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum2),start=min(tmax$year),frequency=12)



#######Tmax


datos=read.table("clipboard",header=T,dec=".")
attach(datos)

x11()
par(mfrow=c(3,1))
station=names(datos[-3:-1])
plot(datos[,4],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[1],ylim=c(20,41))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)

plot(datos[,5],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[2],ylim=c(20,41))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)

plot(datos[,6],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[3],ylim=c(20,41))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)


x11()
par(mfrow=c(1,3))
hist(datos[,4],main=station[1],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,5],main=station[2],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,6],main=station[3],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))

x11()
boxplot(datos[-3:-1],ylab="Temp. Máx.")

plot(1:dim(datos)[1],data_gendatos[,i+3],,main=paste("datos_",station[i]),type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(datos[,i+3],na.rm=T)),col="blue",lwd=1.2,  xaxt="n")
lines(1:dim(tmax)[1],tmax[,i+3],col="red",lwd=1.2)
axis(side=1,labels=seq(min(tmax$year),max(tmax$year),3),at=seq(1,dim(tmax)[1],by=1080),las=2)


precip1=ts(precip,start=min(precip$year),frequency=365)


precip1=ts(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum2),start=min(tmax$year),frequency=12)



#####tmin


datos=tmin
attach(datos)

x11()
par(mfrow=c(3,1))
station=names(datos[-3:-1])
plot(datos[,4],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[1],ylim=c(10,32))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)

plot(datos[,5],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[2],ylim=c(10,32))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)

plot(datos[,6],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[3],ylim=c(10,32))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=720),las=2)


x11()
par(mfrow=c(1,3))
hist(datos[,4],main=station[1],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,5],main=station[2],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,6],main=station[3],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))

x11()
boxplot(datos[-3:-1],ylab="Temp. Mín.")

plot(1:dim(datos)[1],data_gendatos[,i+3],,main=paste("datos_",station[i]),type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(datos[,i+3],na.rm=T)),col="blue",lwd=1.2,  xaxt="n")
lines(1:dim(tmax)[1],tmax[,i+3],col="red",lwd=1.2)
axis(side=1,labels=seq(min(tmax$year),max(tmax$year),3),at=seq(1,dim(tmax)[1],by=1080),las=2)


precip1=ts(precip,start=min(precip$year),frequency=365)


precip1=ts(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum2),start=min(tmax$year),frequency=12)




##########SALDAÑA#################
datos=read.table("clipboard",header=T,dec=".")

attach(datos)
x11()
par(mfrow=c(2,2))
station=names(datos[-3:-1])
plot(datos[,4],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[1],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)

plot(datos[,5],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[2],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)

plot(datos[,6],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[3],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)

plot(datos[,7],type="b",xlab="",ylab="Precipitación", xaxt="n",main=station[4],ylim=c(0,300))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)


x11()
par(mfrow=c(1,3))
hist(datos[,4],main=station[1],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,5],main=station[2],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,6],main=station[3],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))

x11()
boxplot(datos[-3:-1],ylab="Precipitacion")






######tmax salda;a
datos=tmin

x11()
par(mfrow=c(3,1))
station=names(datos[-3:-1])
plot(datos[,4],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[1],ylim=c(13,34))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)

plot(datos[,5],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[2],ylim=c(13,34))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)

plot(datos[,6],type="b",xlab="",ylab="Temp. Máx", xaxt="n",main=station[3],ylim=c(13,34))
axis(side=1,labels=seq(min(year),max(year),2),at=seq(1,dim(datos)[1],by=732),las=2)


x11()
par(mfrow=c(1,3))
hist(datos[,4],main=station[1],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,5],main=station[2],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))
hist(datos[,6],main=station[3],xlab="Precipitación",xlim=c(0,300),ylim=c(0,10000))

x11()
boxplot(datos[-3:-1],ylab="Temp. Mín.")

