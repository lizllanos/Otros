rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/tmin-per-station" #Ruta donde se encuentran los archivos .txt

files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
nom.files=as.numeric(nom.files[-length(nom.files)])

idstation=read.table("clipboard",header=T)
CODIGO=idstation[,1]

where <- match( CODIGO,nom.files)
station_find=nom.files[where[which(!is.na(where))]]
station_find_n1=idstation[which(where!="NA"),2]

Datos <- lapply(paste(rutOrigen,"/",station_find,"_raw_tmin.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})



mean22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.5){
    x=NA
  }else{x=mean(a,na.rm=T)}
  
  return(x)
} 

sum22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.3){
    x=NA
  }else{x=sum(a,na.rm=T)}
  
  return(x)
}

library(reshape)
monthly_p=function(x){
  clim=matrix(NA,12,4)
  date=as.Date(as.character(x[,1]),format="%Y%m%d")
  year=as.numeric(format(date,"%Y"))
  month=as.numeric(format(date,"%m"))
  m=round(aggregate(x[,2],list(Mes=month,Año=year),mean22),2)
  clim[,1]=aggregate(m[,3],list(Mes=m$Mes),min,na.rm=T)[,2]
  clim[,2]=aggregate(m[,3],list(Mes=m$Mes),quantile,probs=0.3333333,na.rm=T)[,2]
  clim[,3]=aggregate(m[,3],list(Mes=m$Mes),quantile,probs=0.666667,na.rm=T)[,2]
  clim[,4]=aggregate(m[,3],list(Mes=m$Mes),max,na.rm=T)[,2]
  colnames(clim)=c("min","q.33","q.66","max")
  rownames(clim)=month.abb
  
  xx=melt(clim)
  xx[order(xx[,1]),]
  xx[,1]=factor(xx[,1],levels=month.abb)
  xn=xx[order(xx[,1]),]
  
  final=xn[,3]
 
  # names(final)=" "
  return(final)
  }


final_all=t(sapply(Datos,monthly_p))

colnames(final_all)=paste(xn[,1],xn[,2],sep="-")
rownames(final_all)=station_find

write.csv(final_all,"cuantiltmin.csv")
