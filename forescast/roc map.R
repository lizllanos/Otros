######################################################
##Función para Crear Mapas de Correlación de Pearson##
######Equipo de Modelación - CIAT Enero 2015##########
######################################################

mapa_roc=function(colombia,ruta,regiones,meses,t){
  
  datos=read.table(paste(ruta,regiones,"/",meses,"/ROC_above",t,".txt",sep=""),skip=3,colClasses=c("character","numeric","numeric","numeric"))
  names(datos)=c("Estacion","Lat","Long","ROC_Above")
  datos$Pearson_Correl=round(datos$ROC_Above,3)
  datos2=datos[datos$Pearson_Correl!=99999,]
  
  col2=extent(-78,-72,0,10)
  colombia=crop(colombia,col2)
  
  colombia@data$id <- rownames(colombia@data)
  colombia2 <- fortify(colombia, region="id")
  p <- ggplot(colombia2, aes(x=long,y=lat))
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3)
  p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Pearson_Correl),size=2.3)+ 
    geom_point(data=datos2,aes(x=Long, y=Lat),shape = 1,size = 2.3,colour = "black")
  p <- p + scale_color_gradient2(name="", low = "red", mid="light yellow", high = "dark green", 
                                 midpoint=0.5, limits=c(0,1),guide="colourbar",
                                 breaks=seq(0,1,by=.1),labels=paste(round(seq(0,1,by=.1),2)))
  p <- p + coord_equal()
  p <- p + theme(legend.key.height=unit(3,"cm"),legend.key.width=unit(1,"cm"),
                 legend.text=element_text(size=13),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=13),
                 axis.title=element_text(colour="black",size=15,face="bold"))
  p <- p +labs(title = paste("ROC Above -",month.name[i])) 
  
  tiff(paste(ruta,"roc_map_",meses,".tif",sep=""), height=2048,width=1500,res=300,
       pointsize=4,compression="lzw")
  print(p)
  dev.off()
}

##Usando la Función para todos los Meses##

require(raster)
require(grid)
require(ggplot2)
require(rgeos)

colombia=shapefile("D:/Cartography/Shape Colombia/colombia_depts.shp")
ruta="C:/Users/darango/Desktop/Pronosticos/Resultados/"
regiones = c('All')
meses = c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic')

for(i in 1:length(meses)){
  #i <- 1
  mapa_roc(colombia,ruta,regiones,meses[i],i)
  print(meses[i])}



###############################################
#####creacion del mapa promedio y boxplot######
###############################################
zona=read.table("clipboard",header=T)
pos=match(listArch[[1]][,1],zona[,1])

i=1:12
all=paste(ruta,regiones,"/",meses,"/ROC_above",i,".txt",sep="")

listArch=lapply(all,function(x){read.table(x,skip=3,colClasses=c("character","numeric","numeric","numeric"))})

merge=do.call("rbind",listArch)
merge$month=sort(rep(1:12,112))
merge[which(merge[,4]==99999),4]<-NA
merge$zona=rep(zona[,2],12)
average=aggregate(merge[,4],list(merge[,2],merge[,3],merge[,1]),function(x)mean(x,na.rm=T))
datos=cbind(average[,3],average[,1:2],average[,4])

ggplot(merge,aes(as.factor(month),V4))+geom_boxplot()+
  scale_y_continuous(breaks=seq(0, 1, 0.2))+ylab("ROC Above")+xlab("")

ggsave("ROC_by_month.tiff")

merge$month=factor(merge$month,labels=meses)
ggplot(merge,aes(as.factor(zona),V4))+geom_boxplot(aes(fill=zona))+
  scale_y_continuous(breaks=seq(0, 1, 0.2))+ylab("ROC Above")+xlab("")+facet_grid(~month)+ scale_x_discrete(breaks=NULL)

ggsave("ROC_by_zone.tiff",width = 12)

#ggplot(merge,aes(as.factor(V1),V4))+geom_boxplot()

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
p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3)
p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Pearson_Correl),size=2.3)+ 
  geom_point(data=datos2,aes(x=Long, y=Lat),shape = 1,size = 2.3,colour = "black")
p <- p + scale_color_gradient2(name="", low = "red", mid="light yellow", high = "dark green", 
                               midpoint=0.4, limits=c(0,.8),guide="colourbar",
                               breaks=seq(0,.8,by=.1),labels=paste(round(seq(0,0.8,by=.1),2)))
p <- p + coord_equal()
p <- p + theme(legend.key.height=unit(3,"cm"),legend.key.width=unit(1,"cm"),
               legend.text=element_text(size=13),
               panel.background=element_rect(fill="white",colour="black"),
               axis.text=element_text(colour="black",size=13),
               axis.title=element_text(colour="black",size=15,face="bold"))
p <- p +labs(title = paste("ROC Above - Average")) 

tiff(paste(ruta,"ROC_map_average.tif",sep=""), height=2048,width=1500,res=300,
     pointsize=4,compression="lzw")
print(p)
dev.off()
