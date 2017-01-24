###########################################################
###Código para leer datos diarios de wunderground #########
###########################################################

# Leer líneas desde la página

install.package('plyr')
require(plyr)

#Función para guardar datos en .csv
read.data=function(ID_station,day,year,month,dir){
  
  data=list()
  
  for(i in 1:length(day)){
    
    web_page <- readLines(paste("http://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=",ID_station,"&day=",day[i],"&year=",year[i],"&month=",month[i],"&graphspan=day&format=1",sep=""))
    mypattern<-'<br>'
    datap <-grep(mypattern,web_page[2:length(web_page)],invert=TRUE,value=TRUE)
    
    
    if(length(datap)!=0){
      
      datap <-as.data.frame(datap)
      splitdat = do.call("rbind", strsplit(as.character(datap[,1]), ","))
      data[[i]]=splitdat
    }
    
  }
  
  dataf=do.call("rbind.fill.matrix",data[which(data!="NULL")])
  dataf=as.data.frame(dataf)
  colnames(dataf) = unlist(strsplit(gsub(mypattern,'',web_page[2]), ","))
  write.csv(dataf, paste(dir,"data.csv"), quote=F, row.names=F) ###Si el Excel está en español poner write.csv2
  
}


###Definición de parámetros para llevar a cabo lectura de datos

dir="C:/Users/lllanos/Desktop/" #Direccion destino de los archivos
ID_station="IATLNTIC2" ##Estación a consultar
  
##Fecha inicial
date_1=as.Date("2013/11/20") #Formato Year/Month/Day

##Fecha final
date_2=as.Date("2014/02/25") #Formato Year/Month/Day

##Secuencia de fechas
all_dates=seq.Date(date_1,date_2,"day")

year=substring(all_dates,1,4)
month=substring(all_dates,6,7)
day=substring(all_dates,9,10)


###Finalmente se ejecuta la función para guardar el .csv
read.data(ID_station,day,year,month,dir)
