rm(list=ls())
require(lubridate)
require(Kendall)
require(DescTools)
require(ggplot2)
require(grid)
library(scales)


min2=function(a,na.rm=T){
  return(min(a,na.rm=T))
}

max2=function(a,na.rm=T){
  return(max(a,na.rm=T))
}

mean2=function(a,na.rm=T){
  return(mean(a,na.rm=T))
}

sd2=function(a,na.rm=T){
  return(sd(a,na.rm=T))
}

sum2=function(a,na.rm=any(!is.na(a))){
  return(sum(a,na.rm=any(!is.na(a))))
}

descript_st=function(object,var){
  object=object
  var="tmin"
  if(var=="precip"){
    Mín=round(min(object,na.rm=T),2); Máx=round(max(object,na.rm=T),2)
    Datos.NA=round(sum(is.na(object)),2)
    Porcentaje.NA=round((Datos.NA/length(object))*100,3)
    result=cbind(Mín, Máx,Datos.NA,Porcentaje.NA)
  }else{
    
    min1=mean2(object)-3*sd2(object)
    max1=mean2(object)+3*sd2(object)
    pos=which(object < min1 | object >max1)
    atip=round((length(pos)/length(object))*100,2)
    
    Mín=round(min(object,na.rm=T),2); Máx=round(max(object,na.rm=T),2)
    Datos.NA=round(sum(is.na(object)),2)
    Porcentaje.NA=round((Datos.NA/length(object))*100,3)
    
#     pos4=which(abs(diff(object))>=10)
#     pos.4=unique(sort(c(pos4,pos4+1)))
#     
 
z=with(rle(object),cbind(lengths,cumsum(lengths),values))
if(sum2(z[,1]<5)!=nrow(z)){ 
  posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
  
  consec=rbind(z[posc,])
  impares=c()
  for(j in 1:(nrow(consec))){
    if(j%%2!=0) impares<-c(impares,j)}
  
  unos=rep(1,length(object))
  for(k in 1:length(impares)){
    pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
    unos[pos]<-NA
  }
  
  posc1=which(is.na(unos)) 
  x=length(posc1)
}else {posc1=0
       x=0}

    
    result=cbind(Mín, Máx,Datos.NA,Porcentaje.NA,"% Atipicos"=atip,"% Consecutivos"=round(x/length(object)*100,2))
    
       
  }
  return(result)
}


new_files=function(object,station,i){
  z=with(rle(object[,2]),cbind(lengths,cumsum(lengths),values))
  if(sum2(z[,1]<5)!=nrow(z)){ 
    posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
    
    consec=rbind(z[posc,])
    impares=c()
    for(j in 1:(nrow(consec))){
      if(j%%2!=0) impares<-c(impares,j)}
    
    unos=rep(1,length(object[,2]))
    for(k in 1:length(impares)){
      pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
      unos[pos]<-NA
    }
    
    posc1=which(is.na(unos)) 
    x=length(posc1)
  }else {posc1=0
         x=0}
  
  object[posc1,2]<-NA
  write.table(object,paste(station[i],"_qc_tmax.txt"),row.names=F,quote=F)
}

fechas=function(x){
  return(cbind("Año Inicio"=as.character(year(min(x))),"Año Fin"=as.character(year(max(x)))))
}

Kend_Test <- function(Estaciones){
  stat=Estaciones
  Vcal=MannKendall(stat)$sl[1]     ###Tendencia
  
  vp=ifelse(Vcal<0.05,"NO","SI")
  return(paste(round(Vcal,2),vp,sep=" - "))
  
}

Umann <- function(Estaciones){
  
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  
  valp=wilcox.test(part1,part2)$p.value
  u=ifelse(valp<0.05,"NO","SI")
  return(paste(round(valp,2),u,sep=" - "))
}

par.sk <- function(Estaciones){
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp1=SiegelTukeyTest(part1,part2)$p.value
  
  f=ifelse(valp1<0.05,"NO","SI")
  return(paste(round(valp1,2),f,sep=" - "))
  
  
  
}


sapply(1:length(station_find),function(j){new_files(Datos[[j]],station_find,j)})


####VARIABLE TMAX
setwd("C:/Users/lllanos/Desktop")
dir.create("Temperatura Máxima2")
dir.create(paste("Temperatura Máxima2/daily"))
dir.create(paste("Temperatura Máxima/daily/data"))

setwd("C:/Users/lllanos/Desktop/Temperatura Máxima2")



####daily tmax

rutOrigen="S:\\observed\\weather_station\\col-ideam\\daily-raw\\tmax-per-station"
files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
nom.files=as.numeric(nom.files[-length(nom.files)])

idstation=read.csv("catalog_tmax.csv",header=T)
attach(idstation)


CODIGO=idstation$CODIGO*10

where <- match( CODIGO,nom.files)
station_find=nom.files[where[which(!is.na(where))]]
station_find_n=idstation[which(where!="NA"),2]
ESTADO=as.numeric(CODIGO %in% station_find)


station_find22=cbind(idstation[,c(1,2,7)],CODIGO)
station_find2=station_find22[which(ESTADO!=0),]
station_find2=station_find2[order(station_find2$DPTO),]
station_not_found=station_find22[which(ESTADO==0),]
nom_dpt=unique(station_find22$DPTO)

write.csv(station_find2,"station_find.csv",row.names=F)
write.csv(station_not_found,"station_not_found.csv",row.names=F)

file.copy(paste(rutOrigen,"\\",station_find2[,4],"_raw_tmax.txt",sep=""),"C:/Users/lllanos/Desktop/Temperatura Máxima/daily/data/")

Datos <- lapply(paste(rutOrigen,"\\",station_find2[,4],"_raw_tmax.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

dim_d=sapply(Datos,nrow)
e=do.call("rbind",Datos)
e2=cbind("station"=rep(station_find2[,2],dim_d),"dpto"=rep(station_find2[,1],dim_d),"COD"=rep(station_find2[,4],dim_d),"Elev"=rep(station_find2[,3],dim_d),e)
e2$Date=as.Date(as.character(e2$Date),format ="%Y%m%d")

e2$all=paste(e2$dpto,"-",e2$station)


desc=aggregate(e2[,6],list(e2$station,e2$dpto),descript_st)


desc=aggregate(e2[,6],list(e2$station),descript_st)
desc2=aggregate(e2$Date,list(e2$station,e2$dpto),fechas)
result=cbind(desc,desc2[,3])

result2=as.matrix(result)

colnames(result2)=c("Estación","Dpto","Mín","Máx","# datos falt","% datos falt","% Datos atip","% Datos consec","Año inicio", "Año final")
result2=cbind(result2[,c(2,1,3:10)],"Elev"=station_find2[,3])

####Graficos########

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))
a+facet_wrap( ~ all,ncol=6)+xlab("")+ylab("Temperatura máxima (°C)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS.jpg", width=18, height=13)

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))
a+facet_wrap( ~ all,ncol=6)+xlab("")+ylab("Temperatura máxima (°C)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS1.jpg", width=18, height=13)


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = Date, y = Value))+geom_line()+facet_wrap( ~ station)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Temperatura máxima (°C)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = station, y = Value))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Temperatura máxima (°C)")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"),width=10, height=8)}

####analisis de homogeneidad######



kt=aggregate(e2[,6],list(e2$station,e2$dpto),Kend_Test)
um=aggregate(e2[,6],list(e2$station,e2$dpto),Umann)
sk=aggregate(e2[,6],list(e2$station,e2$dpto),par.sk)


homo=cbind("Tendencia"=kt[,3],"Est. en media"=um[,3],"Est. en varianza"=sk[,3])

result22=cbind(result2,homo)
write.csv(result22,"resultados_elv.csv",row.names=F)




####monthly precipitation

rutOrigen="S:\\observed\\weather_station\\col-ideam\\monthly-raw\\tmax-per-station"
files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
nom.files=as.numeric(nom.files)

idstation_m=station_not_found
attach(idstation_m)


CODIGO_m=CODIGO[which(ESTADO==0)]

where <- match(CODIGO_m,nom.files)
station_find=nom.files[where[which(where!="NA")]]
station_find_n=idstation_m[which(where!="NA"),2]
ESTADO_m=as.numeric(CODIGO_m %in% station_find)


write.csv(cbind(idstation_m[,c(1,2,4)],CODIGO_m,ESTADO_m),"monthly/station_find.csv",row.names=F)

station_find2=cbind(idstation_m[,c(1,2,4)],CODIGO_m,ESTADO_m)
station_find2=station_find2[which(ESTADO_m!=0),-5]
nom_dpt=unique(station_find2$DPTO)

file.copy(paste(rutOrigen,"\\",station_find,"_raw_prec.txt",sep=""),"C:/Users/lllanos/Desktop/Precipitacion/monthly/data/")

Datos <- lapply(paste(rutOrigen,"\\",station_find,"_raw_prec.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

dim_d=sapply(Datos,nrow)
e=do.call("rbind",Datos)
e2=cbind("station"=rep(station_find_n,dim_d),"dpto"=rep(station_find2[,1],dim_d),"COD"=rep(station_find2[,4],dim_d),"Elev"=rep(station_find2[,3],dim_d),e)
e2$Date=as.Date(as.character(paste(e2$Date,"01",sep="")),format ="%Y%m%d")
e2$all=paste(e2$dpto,"-",e2$station)


desc=aggregate(e2[,6],list(e2$Elev,e2$station,e2$dpto),descript_st)
desc2=aggregate(e2$Date,list(e2$station,e2$dpto),fechas)
result=cbind(desc,desc2[,3])

result2=as.matrix(result)

colnames(result2)=c("Elev","Estación","Dpto","Mín","Máx","# datos falt","% datos falt","Año inicio", "Año final")
result2=result2[,c(3,2,1,4:9)]

####Graficos########


x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))
a+facet_wrap( ~ all,ncol=4)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS.jpg")

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))
a+facet_wrap( ~ all,ncol=4)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS1.jpg")


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = Date, y = Value))+geom_line()+facet_wrap( ~ station)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Precipitación (mm)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = station, y = Value))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Precipitación (mm)")+
    theme(axis.text.x = element_text(size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"))}


####analisis de homogeneidad######


kt=aggregate(e2[,6],list(e2$station,e2$dpto),Kend_Test)
um=aggregate(e2[,6],list(e2$station,e2$dpto),Umann)
sk=aggregate(e2[,6],list(e2$station,e2$dpto),par.sk)


homo=cbind("Tendencia"=kt[,3],"Est. en media"=um[,3],"Est. en varianza"=sk[,3])

result22=cbind(result2,homo)
write.csv(result22,"resultados.csv",row.names=F)



#########################
###################
##################

####VARIABLE PRECIPITACION
dir.create("Precipitacion")
dir.create(paste("Precipitacion/daily"))
dir.create(paste("Precipitacion/daily/data"))

setwd("C:/Users/lllanos/Desktop/Precipitacion")



####daily precipitation

rutOrigen="S:\\observed\\weather_station\\col-ideam\\daily-raw-new\\prec-per-station"
  files <-list.files(rutOrigen,pattern="\\.txt$")
  nom.files<-substring(files,1,nchar(files)-13)
  nom.files=as.numeric(nom.files)
  
idstation=read.table("clipboard",header=T)
attach(idstation)


CODIGO=CODIGO*10

where <- match( CODIGO,nom.files)
station_find=nom.files[where[which(where!="NA")]]
station_find_n=idstation[which(where!="NA"),2]
ESTADO=as.numeric(CODIGO %in% station_find)


station_find22=cbind(idstation[,c(1,2,4)],CODIGO)
station_find2=station_find22[which(ESTADO!=0),]
station_find2=station_find2[order(station_find2$DPTO),]
nom_dpt=unique(station_find2$DPTO)

write.csv(station_find2,"station_find.csv",row.names=F)

file.copy(paste(rutOrigen,"\\",station_find,"_prec_raw.txt",sep=""),"C:/Users/lllanos/Desktop/Precipitacion/daily/data/")

Datos <- lapply(paste(rutOrigen,"\\",station_find2[,4],"_prec_raw.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

dim_d=sapply(Datos,nrow)
e=do.call("rbind",Datos)
e2=cbind("station"=rep(station_find2[,2],dim_d),"dpto"=rep(station_find2[,1],dim_d),"COD"=rep(station_find2[,4],dim_d),"Elev"=rep(station_find2[,3],dim_d),e)
e2$Date=as.Date(as.character(e2$Date),format ="%Y%m%d")

e2$all=paste(e2$dpto,"-",e2$station)


desc=aggregate(e2[,6],list(e2$station,e2$dpto),descript_st)
desc2=aggregate(e2$Date,list(e2$station,e2$dpto),fechas)
result=cbind(desc,desc2[,3],station_find2[,3])

result2=as.matrix(result)

colnames(result2)=c("Estación","Dpto","Mín","Máx","# datos falt","% datos falt","Año inicio", "Año final","Elev")

####Graficos########

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))
a+facet_wrap( ~ all,ncol=8,nrow=10)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")
 
ggsave("ALL_STATIONS.jpg", width=18, height=13)

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))
a+facet_wrap( ~ all,ncol=8,nrow=10)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS1.jpg", width=18, height=13)


for(i in 1:length(nom_dpt)){
ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = Date, y = Value))+geom_line()+facet_wrap( ~ station)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Precipitación (mm)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))+
   theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = station, y = Value))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Precipitación (mm)")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"),width=10, height=8)}

####analisis de homogeneidad######



kt=aggregate(e2[,6],list(e2$station,e2$dpto),Kend_Test)
um=aggregate(e2[,6],list(e2$station,e2$dpto),Umann)
sk=aggregate(e2[,6],list(e2$station,e2$dpto),par.sk)


homo=cbind("Tendencia"=kt[,3],"Est. en media"=um[,3],"Est. en varianza"=sk[,3])

result22=cbind(result2,homo)
write.csv(result22,"resultados_elv.csv",row.names=F)




####monthly precipitation

rutOrigen="S:\\observed\\weather_station\\col-ideam\\monthly-raw\\prec-per-station"
files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
nom.files=as.numeric(nom.files)

 idstation_m=idstation[which(ESTADO==0),]
 attach(idstation_m)


CODIGO_m=CODIGO[which(ESTADO==0)]

where <- match(CODIGO_m,nom.files)
station_find=nom.files[where[which(where!="NA")]]
station_find_n=idstation_m[which(where!="NA"),2]
ESTADO_m=as.numeric(CODIGO_m %in% station_find)


write.csv(cbind(idstation_m[,c(1,2,4)],CODIGO_m,ESTADO_m),"monthly/station_find.csv",row.names=F)

station_find2=cbind(idstation_m[,c(1,2,4)],CODIGO_m,ESTADO_m)
station_find2=station_find2[which(ESTADO_m!=0),-5]
nom_dpt=unique(station_find2$DPTO)

file.copy(paste(rutOrigen,"\\",station_find,"_raw_prec.txt",sep=""),"C:/Users/lllanos/Desktop/Precipitacion/monthly/data/")

Datos <- lapply(paste(rutOrigen,"\\",station_find2[,4],"_raw_prec.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

dim_d=sapply(Datos,nrow)
e=do.call("rbind",Datos)
e2=cbind("station"=rep(station_find2[,2],dim_d),"dpto"=rep(station_find2[,1],dim_d),"COD"=rep(station_find2[,4],dim_d),"Elev"=rep(station_find2[,3],dim_d),e)
e2$Date=as.Date(as.character(paste(e2$Date,"01",sep="")),format ="%Y%m%d")
e2$all=paste(e2$dpto,"-",e2$station)


desc=aggregate(e2[,6],list(e2$Elev,e2$station,e2$dpto),descript_st)
desc2=aggregate(e2$Date,list(e2$station,e2$dpto),fechas)
result=cbind(desc,desc2[,3])

result2=as.matrix(result)

colnames(result2)=c("Elev","Estación","Dpto","Mín","Máx","# datos falt","% datos falt","Año inicio", "Año final")
result2=result2[,c(3,2,1,4:9)]

####Graficos########


x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))
a+facet_wrap( ~ all,ncol=4)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS.jpg")

x11()
a=ggplot(e2, aes(x = Date, y = Value))+geom_line(aes(colour=dpto))
a+facet_wrap( ~ all,ncol=4)+xlab("")+ylab("Precipitación (mm)")+ theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

ggsave("ALL_STATIONS1.jpg")


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = Date, y = Value))+geom_line()+facet_wrap( ~ station)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Precipitación (mm)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"),limits = as.Date(c('1980-01-01','2013-12-31')))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}


for(i in 1:length(nom_dpt)){
  ggplot(e2[which(e2$dpto==nom_dpt[i]),], aes(x = station, y = Value))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Precipitación (mm)")+
    theme(axis.text.x = element_text(size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"))}


####analisis de homogeneidad######


kt=aggregate(e2[,6],list(e2$station,e2$dpto),Kend_Test)
um=aggregate(e2[,6],list(e2$station,e2$dpto),Umann)
sk=aggregate(e2[,6],list(e2$station,e2$dpto),par.sk)


homo=cbind("Tendencia"=kt[,3],"Est. en media"=um[,3],"Est. en varianza"=sk[,3])

result22=cbind(result2,homo)
write.csv(result22,"resultados.csv",row.names=F)

