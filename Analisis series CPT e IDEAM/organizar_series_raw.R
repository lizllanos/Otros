#######################################################
###Script para organizar inputs para RClimTool
####Lizeth Llanos
#######################################################
# rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/tmax-per-station" #Ruta donde se encuentran los archivos .txt
# rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/tmin-per-station" #Ruta donde se encuentran los archivos .txt
rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/prec-per-station" #Ruta donde se encuentran los archivos .txt


files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
nom.files=as.numeric(nom.files[-length(nom.files)])

idstation=read.table("clipboard",header=T) #Cargar base con código y nombre de la estación
CODIGO=idstation[,1]

where <- match( CODIGO,nom.files)
station_find=nom.files[where[which(!is.na(where))]]
station_find_n1=idstation[which(where!="NA"),2]



x=seq(as.Date("1981/1/1"), as.Date("2013/12/31"), "days") #Definir periodo que se desea analizar

fechas=format(x,"%Y%m%d")
fechas=cbind.data.frame("Date"=fechas,"NA")

# files <-list.files(rutOrigen,pattern="\\.txt$")
# nom.files<-substring(files,1,nchar(files)-4)
#Datos <- lapply(paste(rutOrigen,"\\",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
Datos <- lapply(paste(rutOrigen,"/",station_find,"_raw_prec.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

#names(Datos)=nom.files


#Cuales de las estaciones contienen info sobre Tmax, Tmin y Rain
# TempMax=Datos
# TempMin=Datos
Rain   =Datos


# datostmax=as.data.frame(matrix(NA,nrow(fechas),length(TempMax)))
# datostmin=as.data.frame(matrix(NA,nrow(fechas),length(TempMin)))
datosprecip=as.data.frame(matrix(NA,nrow(fechas),length(Rain)))

# for(j in 1:length(TempMax)) {  
#   
# final=merge(fechas,TempMax[[j]],by="Date",all.x=T)
# datostmax[,j]=final[,3]
# }
# 
# for(j in 1:length(TempMin)) {  
#   
#   final=merge(fechas,TempMin[[j]],by="Date",all.x=T)
#   datostmin[,j]=final[,3]
# }


for(j in 1:length(Rain)) {  
  
  final=merge(fechas,Rain[[j]],by="Date",all.x=T)
  datosprecip[,j]=final[,3]
}

year=as.numeric(substr(fechas[,1],1,4))
month=as.numeric(substr(fechas[,1],5,6))
day=as.numeric(substr(fechas[,1],7,8))


# tmaxfin=cbind(day,month,year,datostmax)
# tminfin=cbind(day,month,year,datostmin)
precipfin=cbind(day,month,year,datosprecip)

# names(tmaxfin)=c("day","month","year",as.character(station_find_n1))
# names(tminfin)=c("day","month","year",as.character(station_find_n1))
names(precipfin)=c("day","month","year",as.character(station_find_n1))


#Se guardan los archivos en formato .csv con la info organizada
# write.csv(tmaxfin,paste("tmax_all.csv",sep=""),row.names=F)
# write.csv(tminfin,paste("tmin_all.csv",sep=""),row.names=F)
write.csv(precipfin,paste("cordoba_precip_all.csv",sep=""),row.names=F)
