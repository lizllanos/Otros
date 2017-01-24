######################################################
##Función para Crear Mapas de Correlación de Pearson##
######Equipo de Modelación - CIAT Enero 2015##########
######################################################

mapa_correl=function(colombia,ruta,regiones,meses,t){
  
  datos=read.table(paste(ruta,regiones,"/",meses,"/Pearsons_correlation",i,".txt",sep=""),skip=3,colClasses=c("character","numeric","numeric","numeric"))
  names(datos)=c("Estacion","Lat","Long","Pearson_Correl")
  datos$Pearson_Correl=round(datos$Pearson_Correl,3)
  datos2=datos[datos$Pearson_Correl!=99999,]
  
  col2=extent(-78,-72,0,10)
  colombia=crop(colombia,col2)
  
  colombia@data$id <- rownames(colombia@data)
  colombia2 <- fortify(colombia, region="id")
  p <- ggplot(colombia2, aes(x=long,y=lat))
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.4)
  p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Pearson_Correl),size=2.3)+ 
    geom_point(data=datos2,aes(x=Long, y=Lat),shape = 1,size = 2.3,colour = "black")
  p <- p + scale_color_gradient2(name="", low = "red", mid="light yellow", high = "dark green", 
                                 midpoint=0, limits=c(-0.7,0.7),guide="colourbar",
                                 breaks=seq(-0.7,0.7,by=.1),labels=paste(round(seq(-0.7,0.7,by=.1),2)))
  p <- p + coord_equal()
  p <- p + theme(legend.key.height=unit(3,"cm"),legend.key.width=unit(1,"cm"),
                 legend.text=element_text(size=13),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=13),
                 axis.title=element_text(colour="black",size=15,face="bold"))
  p <- p +labs(title = paste("Pearson Correlation -",month.name[i])) 
  
  tiff(paste(ruta,"ccoef_map_",meses,".tif",sep=""), height=2048,width=1500,res=300,
       pointsize=4,compression="lzw")
  print(p)
  dev.off()
}

##Usando la Función para todos los Meses##

require(raster)
require(grid)
require(ggplot2)
require(rgeos)
require(gridExtra)


colombia=shapefile("C:/Users/lllanos/Desktop/colombia_deps/colombia_depts.shp") #se carga el shapefile
ruta="C:/Users/lllanos/Desktop/colombia_deps/"
regiones = c('All') #Indica la región que se va a analizar
meses = c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic')

for(i in 1:length(meses)){
  #i <- 1
  mapa_correl(colombia,ruta,regiones,meses[i],i)
  print(meses[i])}


###############################################
#####creacion del mapa promedio y boxplot######
###############################################

mapa_promedio_box<-function(colombia,ruta,zona,regiones){
  
  listArch=lapply(paste(ruta,regiones,"/",meses,"/Pearsons_correlation",1:12,".txt",sep=""),function(x){read.table(x,skip=3,colClasses=c("character","numeric","numeric","numeric"))}) #Funcion para cargar de todos los archivos con pearson's correlation
  listArch2=lapply(paste(ruta,regiones,"/",meses,"/ClimatologyAverages",1:12,".txt",sep=""),function(x){t(read.table(x,header=T,as.is=T,skip=2))})#Funcion para cargar de todos los archivos con la climatologia
  
  #Organizando los datos para graficarlos con ggplot2
  mergec=do.call("rbind",listArch2)
  mergec[which(mergec[,3]==99999),3]<-NA  ##Reemplazando datos faltantes por NA's
  
  merge=do.call("rbind",listArch)
  merge$month=sort(rep(1:12,112))
  merge$month=factor(merge$month,labels=meses)
  merge[which(merge[,4]==99999),4]<-NA
  merge$zona=rep(zona[,2],12)
  merge$clima=mergec[,3]
  
  average=aggregate(merge[,4],list(merge[,2],merge[,3],merge[,1]),function(x)mean(x,na.rm=T)) #Se crean promedios por estación
  datos=cbind(average[,3],average[,1:2],average[,4])
  
  
  ##Mapa promedio 
  
  names(datos)=c("Estacion","Lat","Long","Pearson_Correl")
  datos$Pearson_Correl=round(datos$Pearson_Correl,3)
  datos2=datos[!is.na(datos$Pearson_Correl),]
  
  col2=extent(-78,-72,0,10)
  colombia=crop(colombia,col2)
  
  colombia@data$id <- rownames(colombia@data)
  colombia2 <- fortify(colombia, region="id")
  p <- ggplot(colombia2, aes(x=long,y=lat))
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.4)
  p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Pearson_Correl),size=2.3)+ 
    geom_point(data=datos2,aes(x=Long, y=Lat),shape = 1,size = 2.3,colour = "black")
  p <- p + scale_color_gradient2(name="", low = "red", mid="light yellow", high = "dark green", 
                                 midpoint=0, limits=c(-0.4,0.4),guide="colourbar",
                                 breaks=seq(-0.4,0.4,by=.1),labels=paste(round(seq(-0.4,0.4,by=.1),2)))
  p <- p + coord_equal()
  p <- p + theme(legend.key.height=unit(3,"cm"),legend.key.width=unit(1,"cm"),
                 legend.text=element_text(size=13),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=13),
                 axis.title=element_text(colour="black",size=15,face="bold"))
  p <- p +labs(title = paste("Pearson Correlation - Average")) 
  
  tiff(paste(ruta,"ccoef_map_average.tif",sep=""), height=2048,width=1500,res=300,
       pointsize=4,compression="lzw")
  print(p)
  dev.off()
  
  
  
  ####Gráficos boxplot con climatología
  
  ggplot(merge,aes(as.factor(month),V4))+geom_boxplot()+
    scale_y_continuous(breaks=seq(-0.7, 0.7, 0.2))+ylab("Pearson's correlation")+xlab("")
  ggsave("pearson_by_month.tiff")
  
  #x11()
  plot1=ggplot(merge,aes(as.factor(month),V4))+geom_boxplot(aes(fill=zona))+
    scale_y_continuous(breaks=seq(-0.7, 0.7, 0.2))+ylab("Pearson's correlation")+xlab("")+facet_wrap(~zona,1,3)+ 
    theme(legend.position="none")#+labs(title="Pearson's correlation")
  
  averagec=aggregate(merge$clima,list(merge[,5],merge[,6]),function(x)mean(x,na.rm=T))
  
  #x11()
  plot2=ggplot(merge,aes(as.factor(month),clima))+geom_boxplot(aes(fill=zona))+ylab("Precipitación (mm) - Climatología")+xlab("")+facet_wrap(~zona,1,3)+
    theme(legend.position="none") #+labs(title="Climatología")
  
  ggsave("pearson_by_zone.tiff",width = 12)
  
  
  
  x11()
  grid.arrange(plot1, plot2, ncol=1)
  ggsave("pearson_by_zone_clim.tiff",width = 12)
}
zona=read.table(paste(ruta,"zona.txt",sep=""),header=T) #Se carga la tabla con la clasificacion por zona de las estaciones







