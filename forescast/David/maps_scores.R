######################################################
##Función para Crear Mapas con los resultados de CPT##
######Equipo de Modelación - CIAT Enero 2015##########
######################################################
id_station=read.table("clipboard",header=T)
mapas=function(colombia,ruta,name,min,mid,max,by,long_max,long_min,lat_min,lat_max,id_station=NULL){
  
  datos=read.table(paste(ruta,"/",name,".txt",sep=""),skip=3,colClasses=c("character","numeric","numeric","numeric"))
  names(datos)=c("Estacion","Lat","Long","Pearson_Correl")
  datos$Pearson_Correl=round(datos$Pearson_Correl,3)
  datos2=datos[datos$Pearson_Correl!=99999,]
  datos2=datos2[!duplicated(datos2[,2:3]),]  ##Eliminar duplicados, estaciones con iguales coordenadas
  
  if(!is.null("id_station")){id_station=datos2[,1]}
  
  col2=extent(long_max,long_min,lat_min,lat_max)
  colombia=crop(colombia,col2)
  
  colombia@data$id <- rownames(colombia@data)
  colombia2 <- fortify(colombia, region="id")
  p <- ggplot(colombia2, aes(x=long,y=lat))
  p <- p + geom_polygon(aes(fill=hole,group=group),fill="grey 80")
  p <- p + scale_fill_manual(values=c("grey 80","grey 80"))
  p <- p + geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3)
  p <- p + geom_point(data=datos2, aes(x=Long, y=Lat, map_id=Estacion,col=Pearson_Correl),size=2)+
           geom_point(data=datos2,aes(x=Long, y=Lat),shape = 1,size = 2,colour = "black")
  p <- p+geom_text(data=datos2,aes(label = substring(id_station[,1],1,5), x = Long, y = Lat-0.04),size=1.7) 
  p <- p + scale_color_gradient2(name="", low = "red", mid="light yellow", high = "dark green", 
                                 midpoint=mid, limits=c(min,max),guide="colourbar",
                                 breaks=seq(min,max,by=by),labels=paste(round(seq(min,max,by=by),2)))
  p <- p + coord_equal()
  p <- p + theme(legend.key.height=unit(1.7,"cm"),legend.key.width=unit(1,"cm"),
                 legend.text=element_text(size=10),
                 panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=10),
                 axis.title=element_text(colour="black",size=12,face="bold"))

  p <- p +labs(title = paste(name)) 
  
  tiff(paste(ruta,"/",name,".tif",sep=""), height=2048,width=1500,res=300,
       pointsize=4,compression="lzw")
  print(p)
  dev.off()
}

##Usando la Función para todos los Meses##

require(raster)
require(grid)
require(ggplot2)
require(rgeos)

colombia=shapefile("C:/Users/lllanos/Desktop/Pronosticos/shapefile/colombia_depts.shp") #Modificar ruta de la ubicación del shapefile
ruta="C:/Users/lllanos/Desktop/Pronosticos/Resultados Ene15" #Modificar ruta con ubicación de las salidas de CPT

##Pearson Score
names=list.files(ruta,pattern="Pearson") #Poner la palabra clave para filtrar todos los archivos para el indicador deseado

for(i in 1:length(names)){
  name = strsplit(names[i],".txt")[[1]]
  mapas(colombia,ruta,name,-1,0,1,0.2,-75,-76.5,7.8,9.3,id_station)}

##Spearman Score
names=list.files(ruta,pattern="Spearman")

for(i in 1:length(names)){
  name = strsplit(names[i],".txt")[[1]]
  mapas(colombia,ruta,name,-1,0,1,0.2,-75,-76.5,7.8,9.3,id_station)}

##HIT Score
names=list.files(ruta,pattern="HIT")

for(i in 1:length(names)){
name = strsplit(names[i],".txt")[[1]]
mapas(colombia,ruta,name,-100,0,100,20,-75,-76.5,7.8,9.3,id_station)}

##ROCA Score
names=list.files(ruta,pattern="ROCA")

for(i in 1:length(names)){
  name = strsplit(names[i],".txt")[[1]]
  mapas(colombia,ruta,name,0,0.5,1,0.1,-75,-76.5,7.8,9.3,id_station)}

##ROCB Score
names=list.files(ruta,pattern="ROCB")

for(i in 1:length(names)){
  name = strsplit(names[i],".txt")[[1]]
  mapas(colombia,ruta,name,0,0.5,1,0.1,-75,-76.5,7.8,9.3,id_station)}

