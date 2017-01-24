################################
#Script para crear datos de entrada para RClimTool
################################

###Para utilizar éste script se debe tener en cuenta los siguientes aspectos:

###1. Los archivos deben estar en formato .txt
###2. Para nombrar los archivos debe ser de la sgte manera IDstation_variable por ejemplo
###   Estacion1_tmax.txt
###3. Dentro de cada archivo se debe tener esta secuencia de información month, day, year, valor


rm(list=ls())

input=function(rutOrigen){
files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-4)
Datos <- lapply(paste(rutOrigen,"\\",files,sep=""),function(x){read.table(x,header=T,sep="\t")})
names(Datos)=nom.files


#Cuales de las estaciones contienen info sobre Tmax, Tmin y Rain
TempMax=Datos[grep("tmax",nom.files)]
TempMin=Datos[grep("tmin",nom.files)]
Rain   =Datos[grep("prec",nom.files)]

#ID's de las estaciones involucradas en cada variable
IDtmax=substring(names(TempMax),1,nchar(names(TempMax))-5)
IDtmin=substring(names(TempMin),1,nchar(names(TempMin))-5)
IDrain=substring(names(Rain),1,nchar(names(Rain))-5)

##Para tmax
year=as.numeric(substr(TempMax[[1]][,1],1,4))
month=as.numeric(substr(TempMax[[1]][,1],5,6))
day=as.numeric(substr(TempMax[[1]][,1],7,8))

datostmax=as.data.frame(matrix(0,nrow(TempMax[[1]]),length(TempMax)))

for(i in 1:length(TempMax)){
  datostmax[,i]=as.vector(TempMax[[i]][2])
}

tmaxtofin=data.frame(month,day,year,datostmax)
colnames(tmaxtofin)=c("month","day","year",IDtmax)
write.csv(tmaxtofin,paste0(rutOrigen,"\\","tmax_to_Est",".csv"),row.names=F)


##Para tmin
year1=as.numeric(substr(TempMin[[1]][,1],1,4))
month1=as.numeric(substr(TempMin[[1]][,1],5,6))
day1=as.numeric(substr(TempMin[[1]][,1],7,8))

datostmin=as.data.frame(matrix(0,nrow(TempMin[[1]]),length(TempMin)))

for(i in 1:length(TempMin)){
  datostmin[,i]=as.vector(TempMin[[i]][2])
}

tmintofin=data.frame(month1,day1,year1,datostmin)
colnames(tmintofin)=c("month","day","year",IDtmin)
write.csv(tmintofin,paste0(rutOrigen,"\\","tmin_to_Est",".csv"),row.names=F)



##Para precip
year2=as.numeric(substr(Rain[[1]][,1],1,4))
month2=as.numeric(substr(Rain[[1]][,1],5,6))
day2=as.numeric(substr(Rain[[1]][,1],7,8))

datosprecip=as.data.frame(matrix(0,nrow(Rain[[1]]),length(Rain)))

for(i in 1:length(Rain)){
  datosprecip[,i]=as.vector(Rain[[i]][2])
}

preciptofin=data.frame(month2,day2,year2,datosprecip)
colnames(preciptofin)=c("month","day","year",IDrain)
write.csv(preciptofin,paste0(rutOrigen,"\\","precip_to_Est",".csv"),row.names=F)
}

rutOrigen=gfile(text="Seleccione ubicación datos de entrada para RClimTool",type="selectdir")
input(rutOrigen)
