#######################################################
###Script para organizar inputs para RClimTool
####Lizeth Llanos
#######################################################

rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/tmax-per-station" #Ruta donde se encuentran los archivos .txt
rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/tmax-per-station" #Ruta donde se encuentran los archivos .txt

x=seq(as.Date("1980/1/1"), as.Date("2014/12/31"), "days") #Definir periodo que se desea analizar

fechas=format(x,"%Y%m%d")
fechas=cbind.data.frame("Date"=fechas,"NA")

files <-list.files(rutOrigen,pattern="\\.txt$")
#files=read.table("clipboard")
nom.files<-substring(files,1,nchar(files)-4)
Datos <- lapply(paste(rutOrigen,"\\",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
Datos <- lapply(paste(rutOrigen,"/",files[,1],"_raw_tmax.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

names(Datos)=nom.files


#Cuales de las estaciones contienen info sobre Tmax, Tmin y Rain
TempMax=Datos[grep("tmax",nom.files)]
TempMin=Datos[grep("tmin",nom.files)]
Rain   =Datos[grep("prec",nom.files)]

#ID's de las estaciones involucradas en cada variable
IDtmax=substring(names(TempMax),1,nchar(names(TempMax))-9)
IDtmin=substring(names(TempMin),1,nchar(names(TempMin))-9)
IDrain=substring(names(Rain),1,nchar(names(Rain))-9)

datostmax=as.data.frame(matrix(NA,nrow(fechas),length(TempMax)))
datostmin=as.data.frame(matrix(NA,nrow(fechas),length(TempMin)))
datosprecip=as.data.frame(matrix(NA,nrow(fechas),length(Rain)))

for(j in 1:length(TempMax)) {  
  
final=merge(fechas,TempMax[[j]],by="Date",all.x=T)
datostmax[,j]=final[,3]
}

for(j in 1:length(TempMin)) {  
  
  final=merge(fechas,TempMin[[j]],by="Date",all.x=T)
  datostmin[,j]=final[,3]
}


for(j in 1:length(Rain)) {  
  
  final=merge(fechas,Rain[[j]],by="Date",all.x=T)
  datosprecip[,j]=final[,3]
}

year=as.numeric(substr(fechas[,1],1,4))
month=as.numeric(substr(fechas[,1],5,6))
day=as.numeric(substr(fechas[,1],7,8))


tmaxfin=cbind(day,month,year,datostmax)
tminfin=cbind(day,month,year,datostmin)
precipfin=cbind(day,month,year,datosprecip)

names(tmaxfin)=c("day","month","year",IDtmax)
names(tminfin)=c("day","month","year",IDtmin)
names(precipfin)=c("day","month","year",IDrain)


#Se guardan los archivos en formato .csv con la info organizada
write.csv(tmaxfin,paste(rutOrigen,"/tmax_all.csv",sep=""),row.names=F)
write.csv(tminfin,paste(rutOrigen,"/tmin_all.csv,sep="")",row.names=F)
write.csv(precipfin,paste(rutOrigen,"/precip_all.csv",sep=""),row.names=F)
