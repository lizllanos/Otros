
######################################################################
######################################################################
##########--------------Interfaz Versión 2.1.0 -----------############
######################################################################
######################################################################

#---------------------------------------------------------------------
####--------Paquetes necesarios para correr la aplicación---------####
#---------------------------------------------------------------------

# install.packages(ggplot2) #Este paquete sirve para realizar gráficos más elaborados
# install.packages(rtf) #con este paquete se genera el informe automático en Word
# install.packages(gWidgets) #Paquete para generar interfaz grafica
# install.packages(gWidgetsRGtk2) #Paquete para generar interfaz grafica
# install.packages(RMAWGEN) #Este realiza la funcion del llenado de datos
# install.packages(Kendall) #Necesario para correr la prueba de Mann-Kendall
# install.packages(tseries) #Prueba jarque bera
# install.packages(grid) #para crear división de graficos ggplot2
# install.packages(car) #para recodificar variables
# install.packages(reshape) #para modificar base de datos para graficos ggplot2


require(ggplot2) #Este paquete sirve para realizar gráficos más elaborados
require(rtf) #con este paquete se genera el informe automático en Word
require(gWidgets) #Paquete para generar interfaz grafica
require(gWidgetsRGtk2) #Paquete para generar interfaz grafica
require(RMAWGEN) #Este realiza la funcion del llenado de datos
require(Kendall) #Necesario para correr la prueba de Mann-Kendall
require(tseries) #Prueba jarque bera
require(grid) #para crear división de graficos ggplot2
require(car) #para recodificar variables
require(reshape) #para modificar base de datos para graficos ggplot2
options("guiToolkit"="RGtk2") #Selecciona las herramientas para la interfaz grafica


#---------------------------------------------------------------------
####-----------------Funciones para cargar datos-----------------#####
#---------------------------------------------------------------------

fun1 <-function(h,...){ 
  print(do.call(h$action, list(get(svalue(h$obj)))))
} 

cargar=function(file){
  datos=read.table(file,header=T,sep=",")
  
}

cargar_tmin=function(){
gfile.LIST <- list(
  title = "gfile",
  help = "gfile",
  action = list(
    beginning = "tmin=cargar(gfile(", 
    ending = "))"),
  arguments = list(
    "type"=list(
      type= "gdroplist",
      do.quote= TRUE,
      items=c("open", "save", "selectdir"))))

ggenericwidget(gfile.LIST,cont=gwindow("Cargar Temp. Mínima",width=300,height=100))

}

cargar_tmax=function(){
  gfile.LIST <- list(
    title = "gfile",
    help = "gfile",
    action = list(
      beginning = "tmax=cargar(gfile(", 
      ending = "))"),
    arguments = list(
      "type"=list(
        type= "gdroplist",
        do.quote= TRUE,
        items=c("open", "save", "selectdir"))))
  
  ggenericwidget(gfile.LIST,cont=gwindow("Cargar Temp. Máxima",width=300,height=100))
  
}

cargar_tmean=function(){
  gfile.LIST <- list(
    title = "gfile",
    help = "gfile",
    action = list(
      beginning = "tmean=cargar(gfile(", 
      ending = "))"),
    arguments = list(
      "type"=list(
        type= "gdroplist",
        do.quote= TRUE,
        items=c("open", "save", "selectdir"))))
  
  ggenericwidget(gfile.LIST,cont=gwindow("Cargar Temp. Media",width=300,height=100))
  
}

cargar_precip=function(){
  gfile.LIST <- list(
    title = "gfile",
    help = "gfile",
    action = list(
      beginning = "precip=cargar(gfile(", 
      ending = "))"),
    arguments = list(
      "type"=list(
        type= "gdroplist",
        do.quote= TRUE,
        items=c("open", "save", "selectdir"))))
  
  ggenericwidget(gfile.LIST,cont=gwindow("Cargar Precipitación",width=300,height=100))
  
}

#---------------------------------------------------------------------
####--------Funciones para realizar Analisis descriptivo---------#####
#---------------------------------------------------------------------

cultivoh=function(object){
  sub=cbind(object,"nueva"=paste(object$year,object$month,object$day,sep="-"))
  sub$nueva<- as.Date(sub$nueva, format ="%Y-%m-%d") 
  get.rows   <- sub$nueva >= as.Date(paste(svalue(añoh),svalue(mesh),"1",sep="-")) & sub$nueva <= as.Date(paste(svalue(añoh1),svalue(mesh1),"1",sep="-")) 
  data <- sub[get.rows,-length(sub) ] 
  return(data)
}

cultivod=function(object){
  sub=cbind(object,"nueva"=paste(object$year,object$month,object$day,sep="-"))
  sub$nueva<- as.Date(sub$nueva, format ="%Y-%m-%d") 
  get.rows   <- sub$nueva >= as.Date(paste(svalue(añod),svalue(mesd),"1",sep="-")) & sub$nueva <= as.Date(paste(svalue(añod1),svalue(mesd1),"1",sep="-")) 
  data <- sub[get.rows,-length(sub) ] 
  return(data)
}

cultivoi=function(object){
  sub=cbind(object,"nueva"=paste(object$year,object$month,object$day,sep="-"))
  sub$nueva<- as.Date(sub$nueva, format ="%Y-%m-%d") 
  get.rows   <- sub$nueva >= as.Date(paste(svalue(añoi),svalue(mesi),"1",sep="-")) & sub$nueva <= as.Date(paste(svalue(añoi1),svalue(mesi1),"1",sep="-")) 
  data <- sub[get.rows,-length(sub) ] 
  return(data)
}


descript <- function(object){ #Función para descriptivas de las variables
  n=length(object)
  Media=round(mean(object, na.rm=T),3);  Varianza=round(var(object, na.rm=T),3);  Desv.Est=round(sqrt(Varianza),3)
  Mediana=round(as.numeric(quantile(object,probs=0.5, na.rm=T)),3);  Coef.Var=round((sqrt(Varianza)/Media)*100,3)
  Mín=round(min(object,na.rm=T),0); Máx=round(max(object,na.rm=T),0)
  Datos.NA=round(sum(is.na(object)),0)
  Porcentaje.NA=round((Datos.NA/length(object))*100,3)
  result=cbind(n,Mín,Máx,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA,Porcentaje.NA)
  }

descript2=function(object){
  if(svalue(mesd)!="" && svalue(mesd1)!="" && svalue(añod)!="" && svalue(añod1)!="") {object=cultivod(object)
  }else{object=object}
  
  d=sapply(object[-3:-1],descript)
  row.names(d) <-c("n","Mín","Máx","Media","Varianza","Desv.Est","Mediana","CV %","NA","NA %")
  d=as.table(d)
  names(dimnames(d)) <- c(" ", paste("Variable",svalue(nom_val1)))
  return(d)
  
          }

descriptna=function(object){
  Datos.NA=round(sum(is.na(object)),0)
  Porcentaje.NA=round((Datos.NA/length(object))*100,3)
  result=Porcentaje.NA
  return(Datos.NA)
  }

descriptna2=function(object){
d=sapply(object[-3:-1],descriptna)
return(cbind(d))
}

#---------------------------------------------------------------------
####----------Funciones para generar graficos clásicos-----------#####
#---------------------------------------------------------------------


plot2=function(){ #Genera gráfico plot
  object=eval(parse(text=svalue(nom_val1)))
  plot.default.LIST <- list(
    title = "plot.default",
    help = "plot.default",
    action = list(
      beginning = "x11();plot.default(", 
      ending = ")"),
    arguments = list(
      "x"=list(
        type="gdroplist",
        items=ls(object),
        editable =T
      ),
      "y"=list(
        type="gdroplist",
        items=ls(object),
        editable =T
      ),
      "type"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'p'"),
          
      "xlim"=list(
        type= "gedit",
        text="NULL"),
      "ylim"=list(
        type= "gedit",
        text="NULL"),
      
      "main"=list(
        type= "gedit",
        text="'Titulo del grafico'"),
      "xlab"=list(
        type= "gedit",
        text="'Etiqueta eje x'"),
      "ylab"=list(
        type= "gedit",
        text="'Etiqueta eje y'")))
  ggenericwidget(plot.default.LIST,cont=gwindow("Gráfico Plot"))
}

hist2=function(){ #Genera grafico histograma
  object=eval(parse(text=svalue(nom_val1)))
  hist.LIST <- list(
    title = "hist",
    help = "hist",
    action = list(
      beginning = "x11();hist(", 
      ending = ")"),
    arguments = list(
      "x"=list(
        type="gdroplist",
        items=ls(object),
        editable =T
      ),
      "main"=list(
        type= "gedit",
        text="'Titulo del grafico'"),
      "xlab"=list(
        type= "gedit",
        text="'Etiqueta eje x'"),
      "ylab"=list(
        type= "gedit",
        text="'Etiqueta eje y'")))
  
  ggenericwidget(hist.LIST,container=gwindow("Gráfico Histograma")) 
}

boxplot2=function(){#Genera grafico boxplot
  object=eval(parse(text=svalue(nom_val1)))
  boxplot.LIST <- list(
  title = "boxplot",
  help = "boxplot",
  
  assignto = TRUE,
  action = list(
    beginning = "x11();boxplot(", 
    ending = ")"),
  arguments = list(
    "x"=list(
      type="gdroplist",
      items=ls(object),
      editable =T),
      "main"=list(
      type= "gedit",
      text="'Titulo del grafico'"),
    "ylab"=list(
      type= "gedit",
      text="'Etiqueta eje y'")))
  ggenericwidget( boxplot.LIST ,container=gwindow("Gráfico Boxplot"))
  
}

qqnorm2=function(){ #Genera grafico qq-norm
  object=eval(parse(text=svalue(nom_val1)))
  qqnorm.default.LIST <- list(
    title = "qqnorm.default",
    help = "qqnorm.default",
    variableType = "",
    action = list(
      beginning = "x11();qqnorm.default(", 
      ending = ")"),
    arguments = list(
      "y"=list(
        type="gdroplist",
        items=ls(object),
        editable =T
      ),
      "ylim"=list(
        type= "gedit",
        text=""),
      "main"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'Normal Q-Q Plot'"),
      
      "xlab"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'Theoretical Quantiles'"),
      
      "ylab"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'Sample Quantiles'")))
  
  ggenericwidget(qqnorm.default.LIST,container=gwindow("Gráfico QQ-norm"))
  
}


graf_descrip=function(){ #Genera todos los gráficos
  
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"/Analisis grafico"),"Ubicación archivos")
  ifelse(file.exists("Analisis grafico")=="FALSE",dir.create("Analisis grafico",showWarnings=F),"Ya existe carpeta Analisis grafico")
  
  object=eval(parse(text=svalue(nom_val1)))
  object1=ts(object,start=min(object$year),frequency=365)
  
  station=names(object[-3:-1])
  
  if(svalue(nom_val1)=="tmax"){
    dir.create("Analisis grafico/tmax_plot",showWarnings=F)
    month1=recode(tmax$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Analisis grafico/tmax_plot/gral_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      m=matrix(c(1,1,2,3),2,2,byrow=T)
      layout(m)
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Temp_máx")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Temp_máx")
      hist(object[,i+3],main=paste(station[i]),xlab="Temp_máx")
      dev.off()
      
      jpeg(paste("Analisis grafico/tmax_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
           
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Temp_máx",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Temp_máx",station[i],sep="_"),xlab="Año")
      
      
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2,1)))
      print(mes, vp = vplayout(1, 1))
      print(year, vp = vplayout(2,1))
      dev.off()
      
    }
      }
  
  if(svalue(nom_val1)=="tmin"){
    dir.create("Analisis grafico/tmin_plot",showWarnings=F)
    month1=recode(tmin$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Analisis grafico/tmin_plot/tmin_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      m=matrix(c(1,1,2,3),2,2,byrow=T)
      layout(m)
      
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Temp_mín")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Temp_mín")
      hist(object[,i+3],main=paste(station[i]),xlab="Temp_mín")
      dev.off()
      
      jpeg(paste("Analisis grafico/tmin_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Temp_mín",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Temp_mín",station[i],sep="_"),xlab="Año")
      
      
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2,1)))
      print(mes, vp = vplayout(1, 1))
      print(year, vp = vplayout(2,1))
      dev.off()
      
    }
  }
  
  if(svalue(nom_val1)=="tmean"){
    dir.create("Analisis grafico/tmean_plot",showWarnings=F)
    month1=recode(tmean$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Analisis grafico/tmean_plot/tmean_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      m=matrix(c(1,1,2,3),2,2,byrow=T)
      layout(m)
      
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Temp_mean")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Temp_mean")
      hist(object[,i+3],main=paste(station[i]),xlab="Temp_mean")      
      dev.off()
      
      jpeg(paste("Analisis grafico/tmean_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Temp_mín",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Temp_mean",station[i],sep="_"),xlab="Año")
      
      
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2,1)))
      print(mes, vp = vplayout(1, 1))
      print(year, vp = vplayout(2,1))
      dev.off()
    }
  }
  
  
  if(svalue(nom_val1)=="precip"){
    dir.create("Analisis grafico/precip_plot",showWarnings=F)
    month1=recode(precip$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Analisis grafico/precip_plot/precip_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      m=matrix(c(1,1,2,3),2,2,byrow=T)
      layout(m)
      
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Precipitation")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Precipitation")
      hist(object[,i+3],main=paste(station[i]),xlab="Precipitation")     
      dev.off()
      
      jpeg(paste("Analisis grafico/precip_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Precip",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Precip",station[i],sep="_"),xlab="Año")
      
      
      vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(2,1)))
      print(mes, vp = vplayout(1, 1))
      print(year, vp = vplayout(2,1))
      dev.off()
    }
  }
}

graf_plot=function(){
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"/Analisis grafico"),"Ubicación archivos")
  
  if(svalue(tipo)=="Diaria"){
  tmax1=ts(tmax,start=min(tmax$year),frequency=365)
  tmin1=ts(tmin,start=min(tmin$year),frequency=365)
  precip1=ts(precip,start=min(precip$year),frequency=365)
  
  station=names( tmax[-3:-1])
  m=matrix(c(1,1,2,3,3,4,5,5,6),3,3,byrow=T)
    
  dir.create("Analisis grafico",showWarnings=F)
  dir.create("Analisis grafico/Gráficos plot e histogramas",showWarnings=F)
  for(i in 1:(ncol(tmax)-3)){
    
    jpeg(paste("Analisis grafico/Gráficos plot e histogramas/plot_",station[i],".jpeg",sep=""),width = 1200, height = 500)
    layout(m)
    plot(tmax1[,i+3],type="l",xlab="Años",ylab="Temp_máx",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
    title(main=paste(station[i]),outer = TRUE, line = -1)
    hist(tmax1[,i+3],main="",xlab="Temp_máx")
    plot(tmin1[,i+3],type="l",xlab="Años",ylab="Temp_mín",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
    hist(tmin1[,i+3],main="",xlab="Temp_mín")
    plot(precip1[,i+3],type="l",xlab="Años",ylab="Precip")
    hist(precip1[,i+3],main="",xlab="Precip")
    dev.off()
    
  }
  }
  
  if(svalue(tipo)=="Mensual"){
   
    tmax1=ts(aggregate(tmax[-3:-1],list(Mes=tmax$month,Año=tmax$year),mean2),frequency=12,start=min(tmax$year))
    tmin1=ts(aggregate(tmin[-3:-1],list(Mes=tmin$month,Año=tmin$year),mean2),start=min(tmax$year),frequency=12)
    precip1=ts(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum2),start=min(tmax$year),frequency=12)
    
    
    station=names(tmax[-3:-1])
    m=matrix(c(1,1,2,3,3,4,5,5,6),3,3,byrow=T)
    
    dir.create("Analisis grafico",showWarnings=F)
    dir.create("Analisis grafico/Gráficos plot e histogramas",showWarnings=F)
    i=1
    for(i in 1:(ncol(tmax)-3)){
     
      jpeg(paste("Analisis grafico/Gráficos plot e histogramas/plot_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      layout(m)
      plot(tmax1[,i+2],type="l",xlab="Años",ylab="Temp_máx",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
      title(main=paste(station[i]),outer = TRUE, line = -1)
      hist(tmax1[,i+2],main="",xlab="Temp_máx")
      plot(tmin1[,i+2],type="l",xlab="Años",ylab="Temp_mín",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
      hist(tmin1[,i+2],main="",xlab="Temp_mín")
      plot(precip1[,i+2],type="l",xlab="Años",ylab="Precip")
      hist(precip1[,i+2],main="",xlab="Precip")
      dev.off()
      
    
  }
  
}
   
}

graf_box=function(){
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"/Analisis grafico"),"Ubicación archivos")
  
  if(svalue(tipo)=="Mensual"){
    tmax=aggregate(tmax[-3:-1],list(month=tmax$month,year=tmax$year),mean2)
    tmin=aggregate(tmin[-3:-1],list(month=tmin$month,year=tmin$year),mean2)
    precip=aggregate(precip[-3:-1],list(month=precip$month,year=precip$year),sum2)
    
    station=names( tmax[-2:-1])
    month1=recode(tmax$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    levels(month1)<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
    
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    dir.create("Analisis grafico",showWarnings=F)
    dir.create("Analisis grafico/Gráficos boxplot",showWarnings=F)
    
    for(i in 1:(ncol(tmax)-2)){
      
      jpeg(paste("Analisis grafico/Gráficos boxplot/y_m_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+2], geom = "boxplot",group = month1, ylab=paste("Temp_máx",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      mes1=qplot(month1,tmin[,i+2], geom = "boxplot",group = month1, ylab=paste("Temp_mín",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      mes2=qplot(month1,precip[,i+2], geom = "boxplot",group = month1, ylab=paste("Precip",sep="_"),xlab="Mes", outlier.size =0.7)
      
      year=qplot(tmax$year ,tmax[,i+2], geom = "boxplot",  group =  tmax$year,ylab=paste("Temp_máx",sep="_"),xlab="Año",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      year1=qplot(tmin$year ,tmin[,i+2], geom = "boxplot", group =  tmin$year,ylab=paste("Temp_mín",sep="_"),xlab="Año",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      year2=qplot(precip$year ,precip[,i+2], geom = "boxplot", group =  precip$year,ylab=paste("Precip",sep="_"),xlab="Año", outlier.size =0.7)
      
      grid.newpage()
      
      pushViewport(viewport(layout = grid.layout(4,4, heights = unit(c(0.5, 4, 4,4), "null"))))
      grid.text(paste(station[i],"_anual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      grid.text(paste(station[i],"_mensual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 3:4))
      
      print(year, vp = vplayout(2, 1:2))
      print(mes, vp = vplayout(2, 3:4))
      
      print(year1, vp = vplayout(3,1:2))
      print(mes1, vp = vplayout(3,3:4))
      
      print(year2, vp = vplayout(4,1:2))
      print(mes2, vp = vplayout(4,3:4))
      dev.off()
      
    }
    x=melt(tmax[-2:-1])
    y=melt(tmin[-2:-1])
    z=melt(precip[-2:-1])
    
    gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Temp_máx",xlab="",ylim=c(min(tmin[-2:-1],na.rm=T),max(tmax[-2:-1],na.rm=T)), outlier.size =0.7)
    gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Temp_mín",xlab="",ylim=c(min(tmin[-2:-1],na.rm=T),max(tmax[-2:-1],na.rm=T)), outlier.size =0.7)
    gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precip",xlab="", outlier.size =0.7)
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    jpeg(paste("Analisis grafico/Gráficos boxplot/todos.jpeg",sep=""),width = 600, height = 500)
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4, 4,4), "null"))))
    grid.text("", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    print(gral, vp = vplayout(2, 1))
    print(gral1, vp = vplayout(3,1))
    print(gral2, vp = vplayout(4,1))
    dev.off()
  }
  
  if(svalue(tipo)=="Diaria"){
    tmax=tmax
    tmin=tmin
    precip=precip
    
    station=names( tmax[-3:-1])
    month1=recode(tmax$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    levels(month1)<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
    
    
 
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  dir.create("Analisis grafico",showWarnings=F)
  dir.create("Analisis grafico/Gráficos boxplot",showWarnings=F)
 
  for(i in 1:(ncol(tmax)-3)){
    
  jpeg(paste("Analisis grafico/Gráficos boxplot/y_m_",station[i],".jpeg",sep=""),width = 1200, height = 500)
  
  mes=qplot(month1,tmax[,i+3], geom = "boxplot",group = month1, ylab=paste("Temp_máx",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  mes1=qplot(month1,tmin[,i+3], geom = "boxplot",group = month1, ylab=paste("Temp_mín",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  mes2=qplot(month1,precip[,i+3], geom = "boxplot",group = month1, ylab=paste("Precip",sep="_"),xlab="Mes", outlier.size =0.7)
  
  year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",  group =  tmax$year,ylab=paste("Temp_máx",sep="_"),xlab="Año",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  year1=qplot(tmin$year ,tmin[,i+3], geom = "boxplot", group =  tmin$year,ylab=paste("Temp_mín",sep="_"),xlab="Año",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  year2=qplot(precip$year ,precip[,i+3], geom = "boxplot", group =  precip$year,ylab=paste("Precip",sep="_"),xlab="Año", outlier.size =0.7)
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(4,4, heights = unit(c(0.5, 4, 4,4), "null"))))
  grid.text(paste(station[i],"_anual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.text(paste(station[i],"_mensual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 3:4))
    
  print(year, vp = vplayout(2, 1:2))
  print(mes, vp = vplayout(2, 3:4))
  
  print(year1, vp = vplayout(3,1:2))
  print(mes1, vp = vplayout(3,3:4))
  
  print(year2, vp = vplayout(4,1:2))
  print(mes2, vp = vplayout(4,3:4))
  dev.off()
  
  }
  x=melt(tmax[-3:-1])
  y=melt(tmin[-3:-1])
  z=melt(precip[-3:-1])
  
  gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Temp_máx",xlab="",ylim=c(min(tmin[-3:-1],na.rm=T),max(tmax[-3:-1],na.rm=T)), outlier.size =0.7)
  gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Temp_mín",xlab="",ylim=c(min(tmin[-3:-1],na.rm=T),max(tmax[-3:-1],na.rm=T)), outlier.size =0.7)
  gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precip",xlab="", outlier.size =0.7)
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  jpeg(paste("Analisis grafico/Gráficos boxplot/todos.jpeg",sep=""),width = 600, height = 500)
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4, 4,4), "null"))))
  grid.text("", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  
  print(gral, vp = vplayout(2, 1))
  print(gral1, vp = vplayout(3,1))
  print(gral2, vp = vplayout(4,1))
  dev.off()
  }
}

graf_disp=function(){
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"/Analisis grafico"),"Ubicación archivos")
  
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2)
  }
  
  
  dir.create("Analisis grafico/Gráficos dispersión",showWarnings=F)
  jpeg(paste("Analisis grafico/Gráficos dispersión/tmax.jpeg",sep=""),width = 600, height = 600)
  datostmax=data.frame(na.omit(tmax[-3:-1]))
  pairs(datostmax,lower.panel=panel.cor, pch=19,col="black")
  
  dev.off()
  
  jpeg(paste("Analisis grafico/Gráficos dispersión/tmin.jpeg",sep=""),width = 600, height = 600)
  datostmin=data.frame(na.omit(tmin[-3:-1]))
  pairs(datostmin,lower.panel=panel.cor, pch=19,col="black")
  dev.off()
  
  jpeg(paste("Analisis grafico/Gráficos dispersión/precip.jpeg",sep=""),width = 600, height = 600)
  datosprecip=data.frame(na.omit(precip[-3:-1]))
  pairs(datosprecip,lower.panel=panel.cor, pch=19,col="black")
  dev.off()
  
}

#---------------------------------------------------------------------
####---------Funciones para generar graficos con ggplot2---------#####
#---------------------------------------------------------------------

hist_22=function(){ #genera grafico hist con ggplot2
  object=eval(parse(text=svalue(nom_val1)))
  qplot.LIST <- list(
    title = "qplot",
    help = "qplot",
    
    action = list(
      beginning = "x11();qplot(", 
      ending = ")"),
    arguments = list(
      
      "x"=list(
        type="gdroplist",
        items=ls(object),
        editable=T
      ),
      
      "geom"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'auto'"),
      
      "main"=list(
        type= "gedit",
        text="'Titulo del grafico'"),
      "xlab"=list(
        type= "gedit",
        text="'Etiqueta eje x'"),
      "ylab"=list(
        type= "gedit",
        text="'Etiqueta eje y'")))
  ggenericwidget(qplot.LIST,container=gwindow("Gráfico Histograma ggplot2"))
  
}

qplot2=function(){ #genera grafico plot con ggplot2
  object=eval(parse(text=svalue(nom_val1)))
  qplot.LIST <- list(
    title = "qplot",
    help = "qplot",
    
    action = list(
      beginning = "x11();qplot(", 
      ending = ")"),
    arguments = list(
      
      
      "x"=list(
        type="gdroplist",
        items=ls(object),
        editable=T
      ),
      "y"=list(
        type="gdroplist",
        items=ls(object),
        editable=T
      ), 
      "geom"=list(
        type= "gedit",
        coerce.with= as.character,
        text="c('point','line')"),
      
      "main"=list(
        type= "gedit",
        text="'Titulo del grafico'"),
      "xlab"=list(
        type= "gedit",
        text="'Etiqueta eje x'"),
      "ylab"=list(
        type= "gedit",
        text="'Etiqueta eje y'")))
  ggenericwidget(qplot.LIST,container=gwindow("Gráfico Plot ggplot2"))
  
}

boxplotg2=function(){
  object=eval(parse(text=svalue(nom_val1)))
geom_boxplot.LIST <- list(
  title = "geom_boxplot",
  help = "geom_boxplot",
  variableType = "",
  assignto = TRUE,
  action = list(
    beginning = "geom_boxplot(", 
    ending = ")"),
  arguments = list(
    "mapping"=list(
      type= "gedit",
      text="NULL"),
    "data"=list(
      type= "gedit",
      text="NULL"),
    "stat"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'boxplot'"),
    "position"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'dodge'"),
    "outlier.colour"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'black'"),
    "outlier.size"=list(
      type= "gedit",
      coerce.with= as.numeric,
      text="2"),
     "..."=list(
      type= "gedit",
      text="")))
ggenericwidget(geom_boxplot.LIST,container=gwindow("Gráfico BoxPlot ggplot2"))

}


#---------------------------------------------------------------------
####--------------Funciones para control de calidad---------------####
#---------------------------------------------------------------------

confirmDialog <- function(message,title, handler=NULL) { #Funcion para cuadro de diálogo
  
  window <- gwindow(title,width=100,height=100)
  group <- ggroup(container = window)
  gimage("info", dirname="stock", size="large_toolbar", container=group)
  
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group, expand=TRUE)
  
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("Ok", handler = function(h,...) dispose(window),
          container=button.group)
  
  return()
}

min2=function(a){
  return(min(a,na.rm=T))
}

max2=function(a){
  return(max(a,na.rm=T))
}

mean2=function(a){
  return(mean(a,na.rm=T))
}

sd2=function(a){
  return(sd(a,na.rm=T))
}

sum2=function(a){
  return(sum(a,na.rm=T))
}


grafcontrol<-function(object){
  
  station=names(object[-3:-1])
  ifelse(file.exists("Control de calidad")=="FALSE",dir.create("Control de calidad",showWarnings=F),"Ya existe carpeta Control de Calidad")
  
  
  object1=ts(object,start=min(object$year),frequency=365)
  
  if(svalue(variable)=="tmax"){
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmax_atip"),"Ubicación archivos")
    dir.create("Control de calidad/tmax_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Control de calidad/tmax_atip/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
      plot(object1[,i+3],ylab="tmax",xlab=paste(station[i]),type="b")
      abline(h=min[i],col="red",lwd=2)
      abline(h=max[i],col="red",lwd=2) 
      pos=which(object1[,i+3] < min[i] | object1[,i+3] >max[i])
      atipicos<-data.frame(pos,object1[pos,i+3])
     #points(atipicos,col="red",pch=16)
      dev.off()
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
                 
      }
    } 
  
  
  if(svalue(variable)=="tmin"){
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmin_atip"),"Ubicación archivos")
       dir.create("Control de calidad/tmin_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
   porcentajes=matrix(0,ncol(object)-3,2)
   
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Control de calidad/tmin_atip/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
      plot(object1[,i+3],ylab="tmin",xlab=paste(station[i]),type="b")
      abline(h=min[i],col="red",lwd=2)
      abline(h=max[i],col="red",lwd=2) 
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-data.frame(pos,object[pos,i+3])
      #points(atipicos,col="red",pch=16)
      dev.off()
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
    }
  } 
  
  if(svalue(variable)=="tmean"){
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmean_atip"))
      dir.create("Control de calidad/tmean_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Control de calidad/tmean_atip/tmean_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
      plot(object1[,i+3],ylab="tmean",xlab=paste(station[i]),type="b")
      abline(h=min[i],col="red",lwd=2)
      abline(h=max[i],col="red",lwd=2) 
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-data.frame(pos,object[pos,i+3])
      #points(atipicos,col="red",pch=16)
      dev.off()
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
    }
  } 
  
  if(svalue(variable)=="precip"){
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/precip_atip"),"Ubicación archivos")
    dir.create("Control de calidad/precip_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Control de calidad/precip_atip/precip_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
      plot(object1[,i+3],ylab="Precip",xlab=paste(station[i]),type="b")
      abline(h=min[i],col="red",lwd=2)
      abline(h=max[i],col="red",lwd=2) 
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-data.frame(pos,object[pos,i+3])
      #points(atipicos,col="red",pch=16)
      dev.off()
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
    }
  } 
  
}

validartemp<-function(object){
    
  station=names(object[-3:-1])
  
  
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(tmax[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-data.frame(pos.4,object[pos.4,i+3])
      
      z=with(rle(object[,i+3]),cbind(lengths,cumsum(lengths),values))
      if(sum(z[,1]<5)!=nrow(z)){ 
        posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
        
        consec=rbind(z[posc,])
        impares=c()
        for(j in 1:(nrow(consec))){
          if(j%%2!=0) impares<-c(impares,j)}
        
        unos=rep(1,length(object[,i+3]))
        for(k in 1:length(impares)){
          pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
          unos[pos]<-NA
        }
        
        posc1=which(is.na(unos)) 
        x=length(posc1)
      }else {posc1=0
             x=0}
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((dim(atipicos3)[1]/dim(tmax)[1])*100),round((dim(atipicos4)[1]/dim(tmax)[1])*100,2),round((x/dim(tmax)[1])*100,2),
                            round((sum(nrow(atipicos2),nrow(atipicos3),na,x)/dim(tmax)[1])*100,2)) 
      
      
      
    }
    
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
  porcentajes=as.table(porcentajes)
  return(porcentajes)
}
  
validarprec<-function(object){
  
  station=names(object[-3:-1])
  
  
  min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
  max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
  porcentajes=matrix(0,ncol(object)-3,3)
  
  
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(object)[1])*100,2),round((sum(nrow(atipicos2),na)/dim(object)[1])*100,2)) 
        
      
    }
     dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Total datos NA")))
    
    porcentajes=as.table(porcentajes)
  return(porcentajes)
}


validar2<-function(object){
  
  val=ifelse(sapply(object[-3:-1],min2)>=as.numeric(svalue(minim)) & sapply(object[-3:-1],max2)<=as.numeric(svalue(maxim)),"está dentro del rango permitido","contiene valores por fuera del rango permitido")
  val=cbind(val)
  
  confirmDialog(paste("La estación",names(object[-3:-1]),val,sep=" "),"Validación")
  
  station=names(object[-3:-1])
  ifelse(file.exists("Datos_faltantes")=="FALSE",dir.create("Datos_faltantes",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("Control de calidad")=="FALSE",dir.create("Control de calidad",showWarnings=F),"Ya existe carpeta Control de Calidad")
  
  if(svalue(variable)=="tmax"){
    dir.create("Control de calidad/tmax",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Control de calidad/tmax/","atipicos_",station[i],".csv",sep=""),row.names=F)
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(object[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-cbind(object[pos.4,1],object[pos.4,2],object[pos.4,3],object[pos.4,i+3])
      colnames(atipicos4)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos4,paste("Control de calidad/tmax/","TM_10_",station[i],".csv",sep=""),row.names=F)
      
      
      z=with(rle(object[,i+3]),cbind(lengths,cumsum(lengths),values))
      if(sum(z[,1]<5)!=nrow(z)){ 
        posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
        
        consec=rbind(z[posc,])
        impares=c()
        for(j in 1:(nrow(consec))){
        if(j%%2!=0) impares<-c(impares,j)}
        
        unos=rep(1,length(object[,i+3]))
        for(k in 1:length(impares)){
          pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
          unos[pos]<-NA
        }
         
        posc1=which(is.na(unos)) 
        x=length(posc1)
      }else {posc1=0
              x=0}
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((dim(atipicos3)[1]/dim(tmax)[1])*100),round((dim(atipicos4)[1]/dim(tmax)[1])*100,2),round((x/dim(tmax)[1])*100,2),
                            round((sum(nrow(atipicos2),nrow(atipicos3),na,x)/dim(tmax)[1])*100,2)) 
      
      
      
      post=c(pos2,pos3,posc1)
      
      tmax[post,i+3]<-NA
      
    }
    
    write.csv(tmax,"Datos_faltantes/tmax_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
  }
  
  if(svalue(variable)=="tmin"){
    dir.create("Control de calidad/tmin",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Control de calidad/tmin/","atipicos_",station[i],".csv",sep=""),row.names=F)
      
      
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(object[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-cbind(object[pos.4,1],object[pos.4,2],object[pos.4,3],object[pos.4,i+3])
      colnames(atipicos4)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos4,paste("Control de calidad/tmin/","TM_10_",station[i],".csv",sep=""),row.names=F)
      
      
      z=with(rle(object[,i+3]),cbind(lengths,cumsum(lengths),values))
      if(sum(z[,1]<5)!=nrow(z)){ 
        posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
        
        consec=rbind(z[posc,])
        impares=c()
        for(j in 1:(nrow(consec))){
          if(j%%2!=0) impares<-c(impares,j)}
        
        unos=rep(1,length(object[,i+3]))
        for(k in 1:length(impares)){
          pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
          unos[pos]<-NA
        }
        
        posc1=which(is.na(unos)) 
        x=length(posc1)
      }else {posc1=0
             x=0}
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((dim(atipicos3)[1]/dim(tmax)[1])*100),round((dim(atipicos4)[1]/dim(tmax)[1])*100,2),round((x/dim(tmax)[1])*100,2),
                            round((sum(nrow(atipicos2),nrow(atipicos3),na,x)/dim(tmax)[1])*100,2)) 
      
                 post=c(pos2,pos3,posc1)
                 tmax[post,i+3]<-NA
      
    }
    write.csv(tmin,"Datos_faltantes/tmin_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
  }
  
  if(svalue(variable)=="tmean"){
    dir.create("Control de calidad/tmean_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-data.frame(pos,object[pos,i+3])
     
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2)) 
      post=c(pos2)
      tmean[post,i+3]<-NA
      
    }
    write.csv(tmean,"Datos_faltantes/tmean.csv")
  }
  
  
  if(svalue(variable)=="precip"){
    dir.create("Control de calidad/precip_atip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,3)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Control de calidad/precip/","atipicos_",station[i],".csv",sep=""),row.names=F)
            
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
      post=pos2
      
      precip[post,i+3]<-NA
      
    }
    write.csv(precip,"Datos_faltantes/precip_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Total datos NA")))
    
  }
  porcentajes=as.table(porcentajes)
  names(dimnames(porcentajes)) <- c("", paste("Control de calidad para la variable",svalue(variable))) 
  return(porcentajes)
}

validar22<-function(object){
  
  val=ifelse(sapply(object[-3:-1],min2)>=as.numeric(svalue(minim)) & sapply(object[-3:-1],max2)<=as.numeric(svalue(maxim)),"está dentro del rango permitido","contiene valores por fuera del rango permitido")
  val=cbind(val)
  
 # confirmDialog(paste("La estación",names(object[-3:-1]),val,sep=" "))
  
  station=names(object[-3:-1])
 
  #ifelse(file.exists("Datos_faltantes")=="FALSE",dir.create("Datos_faltantes"),"Ya existe carpeta")
  #ifelse(file.exists("Control de calidad")=="FALSE",dir.create("Control de calidad"),"Ya existe carpeta Control de Calidad")
  
  if(svalue(variable)=="tmax"){
   # dir.create("Control de calidad/tmax_atip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
     # write.csv(atipicos,paste("Control de calidad/tmax_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(tmax[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-data.frame(pos.4,object[pos.4,i+3])
      
      
      z=with(rle(object[,i+3]),cbind(lengths,cumsum(lengths),values))
      if(sum(z[,1]<5)!=nrow(z)){ 
        posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
        
        consec=rbind(z[posc,])
        impares=c()
        for(j in 1:(nrow(consec))){
          if(j%%2!=0) impares<-c(impares,j)}
        
        unos=rep(1,length(object[,i+3]))
        for(k in 1:length(impares)){
          pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
          unos[pos]<-NA
        }
        
        posc1=which(is.na(unos)) 
        x=length(posc1)
      }else {posc1=0
             x=0}
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((dim(atipicos3)[1]/dim(tmax)[1])*100),round((dim(atipicos4)[1]/dim(tmax)[1])*100,2),round((x/dim(tmax)[1])*100,2),
                            round((sum(nrow(atipicos2),nrow(atipicos3),na,x)/dim(tmax)[1])*100,2)) 
      
      
      
      post=c(pos2,pos3,posc1)
      
      #tmax[post,i+3]<-NA
      
    }
    
   # write.csv(tmax,"Datos_faltantes/tmax_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
  }
  
  if(svalue(variable)=="tmin"){
   # dir.create("Control de calidad/tmin_atip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
     # write.csv(atipicos,paste("Control de calidad/tmin_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
      
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(tmax[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-data.frame(pos.4,object[pos.4,i+3])
      
      
      z=with(rle(object[,i+3]),cbind(lengths,cumsum(lengths),values))
      if(sum(z[,1]<5)!=nrow(z)){ 
        posc=sort(c(which(z[,1]>=5),which(z[,1]>=5)-1))
        
        consec=rbind(z[posc,])
        impares=c()
        for(j in 1:(nrow(consec))){
          if(j%%2!=0) impares<-c(impares,j)}
        
        unos=rep(1,length(object[,i+3]))
        for(k in 1:length(impares)){
          pos=as.numeric((consec[impares[k],2]+1)):as.numeric(consec[impares[k]+1,2])
          unos[pos]<-NA
        }
        
        posc1=which(is.na(unos)) 
        x=length(posc1)
      }else {posc1=0
             x=0}
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((dim(atipicos3)[1]/dim(tmax)[1])*100),round((dim(atipicos4)[1]/dim(tmax)[1])*100,2),round((x/dim(tmax)[1])*100,2),
                            round((sum(nrow(atipicos2),nrow(atipicos3),na,x)/dim(tmax)[1])*100,2)) 
      
      post=c(pos2,pos3,pos.4,posc1)
      #tmax[post,i+3]<-NA
      
    }
   # write.csv(tmin,"Datos_faltantes/tmin_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
  }
  
  if(svalue(variable)=="tmean"){
   # dir.create("Control de calidad/tmean_atip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-data.frame(pos,object[pos,i+3])
      
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2)) 
      post=c(pos2)
      tmean[post,i+3]<-NA
      
    }
   # write.csv(tmean,"Datos_faltantes/tmean.csv")
  }
  
 
  if(svalue(variable)=="precip"){
    #dir.create("Control de calidad/precip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,3)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      #write.csv(atipicos,paste("Control de calidad/precip_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
      post=c(pos,pos2)
      
      #precip[post,i+3]<-NA
      
    }
    #write.csv(precip,"Datos_faltantes/precip_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Datos atípicos","% Datos fuera del rango","% Total datos NA")))
    
  }
  porcentajes=as.table(porcentajes)
  names(dimnames(porcentajes)) <- c("", paste("Control de calidad para la variable",svalue(variable))) 
  return(porcentajes)
}

#---------------------------------------------------------------------
####--------------Funciones para datos faltantes------------------####
#---------------------------------------------------------------------

datos_falt=function(){
  confirmDialog("Este proceso puede tomar varios minutos, presione OK para continuar","Estado del Proceso ")
  
  year_max <-as.numeric(svalue(year_max)) 
  year_min <- as.numeric(svalue(year_min))
  origin <- paste(svalue(origin),svalue(origin1),svalue(origin2),sep="-")
  
  valmin <- 1.0
  
  n_GPCA_iter <- 10
  n_GPCA_iteration_residuals <- 10
  n_GPCA_iter_prec <- 20
  n_GPCA_iteration_residuals_prec <- 20
  
  
    tmax<- read.csv("Datos_faltantes/tmax_na.csv", header = TRUE, sep = ",",  dec=".")
    tmin<- read.csv("Datos_faltantes/tmin_na.csv", header = TRUE, sep = ",",  dec=".")
   precip <- read.csv("Datos_faltantes/precip_na.csv", header = TRUE, sep = ",",  dec=".")
    
  
  
  station <- names(tmax) [-3:-1]
  
  
  # generation of temperature max and min 
  generation00_temp <- ComprehensiveTemperatureGenerator(station=station,
                                                         Tx_all=tmax,
                                                         Tn_all=tmin,
                                                         year_min=year_min,
                                                         year_max=year_max,
                                                         p=1,n_GPCA_iteration=n_GPCA_iter,
                                                         n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
                                                         sample="monthly")
  
  # Use of measured and observed temperature as exogenous variables
  exogen_sim <- cbind(generation00_temp$output$Tx_gen,generation00_temp$output$Tn_gen)
  names(exogen_sim) <- cbind(paste(names(generation00_temp$output$Tx_gen),"_Tx",sep=""),paste(names(generation00_temp$output$Tn_gen),"_Tn",sep=""))
  exogen <- cbind(generation00_temp$input$Tx_mes,generation00_temp$input$Tn_mes)
  names(exogen) <- cbind(paste(names(generation00_temp$input$Tx_mes),"_Tx",sep=""),paste(names(generation00_temp$input$Tn_mes),"_Tn",sep=""))
  
  # Precipitation Generator (temperture enters as exogenous variable)
  valmin <- 1.0
  
  n_GPCA_iter_prec <- 20
  n_GPCA_iteration_residuals_prec <- 20
  
  
  generation00_prec <- ComprehensivePrecipitationGenerator(station=station,
                                                           prec_all=precip,
                                                           year_min=year_min,
                                                           year_max=year_max,
                                                           exogen=exogen,
                                                           exogen_sim=exogen_sim,
                                                           p=5,n_GPCA_iteration=n_GPCA_iter_prec,
                                                           n_GPCA_iteration_residuals=n_GPCA_iteration_residuals_prec,
                                                           sample="monthly",valmin=1,extremes=TRUE,no_spline = T)
#   names(generation00_prec)
#   generation00_prec$var
#   # Post-processing calcul
  
  
  #-----------------------Gerar Arquivos--------------------------------------------- 
  prec_mes <- generation00_prec$prec_mes
  prec_gen <- generation00_prec$prec_gen
  
  tmin_mes <-generation00_temp$input$Tn_mes
  tmin_gen <-generation00_temp$out$Tn_gen
  
  tmax_mes <-generation00_temp$input$Tx_mes
  tmax_gen <-generation00_temp$out$Tx_gen
  
  test = names(prec_gen)
  teste_min <- names(tmin_gen)
  teste_max <- names(tmax_gen)
  
  
  data_genPrec <-extractmonths(data=generation00_prec$prec_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  data_genTmin <- extractmonths(data=generation00_temp$out$Tn_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  data_genTmax <- extractmonths(data=generation00_temp$out$Tx_gen,when=c("Jan", "Feb", "Mar", "Apr","May","Jun","Jul","Aug","Sep", "Oct", "Nov", "Dec"),origin)
  
  
  #------------------------------Salvando Arquivo------------------------------------
  
  arquivo <- adddate(data_genPrec[test[1]], origin)
  arquivo$.INMET.00304_tmax = data_genTmax$.INMET.00304  
  arquivo$.INMET.00304_tmin = data_genTmin$.INMET.00304
  
  
  data_genTmin <- data.frame(tmin[,1:3],data_genTmin)
  data_genTmax <- data.frame(tmax[,1:3],data_genTmax)
  data_genPrec <- data.frame(precip[,1:3],data_genPrec)
  
  
  ifelse(file.exists("tmax_faltantes")=="FALSE",dir.create("Datos_faltantes/tmax_faltantes",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("tmin_faltantes")=="FALSE",dir.create("Datos_faltantes/tmin_faltantes",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("precip_faltantes")=="FALSE",dir.create("Datos_faltantes/precip_faltantes",showWarnings=F),"Ya existe carpeta")
  
#   write.csv(data_genTmin,"Datos_faltantes/data_genTmin.csv",row.names =F)
#   write.csv(data_genTmax,"Datos_faltantes/data_genTmax.csv",row.names =F)
#   write.csv(data_genPrec,"Datos_faltantes/data_genPrec.csv",row.names =F)
#   
#   doc=list.files("Datos_faltantes",pattern ="_na.csv")
#   nom_doc<-substring(doc,1,nchar(doc)-4)
#        
#   listArch=lapply(doc,function(x){read.csv(x,sep=",",header=TRUE)})
#   names(listArch)=nom_doc
  
  for(i in 1:(ncol(data_genTmax)-3))
  {
    jpeg(paste("Datos_faltantes/tmax_faltantes/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(tmax)[1],data_genTmax[,i+3],type="l",col="blue",lwd=1.5,main=paste("tmax_",station[i]))
    lines(1:dim(tmax)[1],tmax[,i+3],col="red",lwd=1.5)
    legend("topright",c("generada","original"),lwd=c(1.5,1.5),col=c("blue","red"))
    dev.off()
    
    jpeg(paste("Datos_faltantes/tmin_faltantes/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(tmin)[1],data_genTmin[,i+3],type="l",col="blue",lwd=1.5,main=paste("tmin_",station[i]))
    lines(1:dim(tmin)[1],tmin[,i+3],col="red",lwd=1.5)
    legend("topright",c("generada","original"),lwd=c(1.5,1.5),col=c("blue","red"))
    dev.off()
    
    jpeg(paste("Datos_faltantes/precip_faltantes/prec_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(precip)[1],data_genPrec[,i+3],type="l",col="blue",lwd=1.5,main=paste("prec_",station[i],sep=""))
    lines(1:dim(precip)[1],precip[,i+3],col="red",lwd=1.5)
    legend("topright",c("generada","original"),lwd=c(1.5,1.5),col=c("blue","red"))
    dev.off()
    
        
     tmax.na=which(is.na(tmax[,i+3]))
     tmax[tmax.na,i+3]<-data_genTmax[tmax.na,i+3]
    
      
    tmin.na=which(is.na(tmin[,i+3]))
    tmin[tmin.na,i+3]<-data_genTmin[tmin.na,i+3]
    
    
    precip.na=which(is.na(precip[,i+3]))
    precip[precip.na,i+3]<-data_genPrec[precip.na,i+3]
         
  }
  
  write.csv(tmax,"Datos_faltantes/data_genTmax.csv",row.names =F)
  write.csv(tmin,"Datos_faltantes/data_genTmin.csv",row.names =F)
  write.csv(precip,"Datos_faltantes/data_genPrec.csv",row.names =F)
  
  
}

graf_falt=function(){
 
    tmaxg<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    tming<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    precip<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    
    tmaxn<- read.csv("Datos_faltantes/tmax_na.csv", header = TRUE, sep = ",",  dec=".")
    tminn<- read.csv("Datos_faltantes/tmin_na.csv", header = TRUE, sep = ",",  dec=".")
    precipn<- read.csv("Datos_faltantes/precip_na.csv", header = TRUE, sep = ",",  dec=".")
    
    x11()
    plot(tmaxn[-3:-1],data_genTmax[-3:-1])
    r=summary(lm(tmaxn[,4]~data_genTmax[,4]))$r.squared
    legend("top",paste(expression(R^2),"=",round(r,3)),bty="n")
    
    x11()
    res=tmaxn[,7]-tmaxg[,7]
    plot(res)
    abline(h=0,col="red")
    
    plot(precip[,6],precin[,6])
    summary(lm(precip[,6]~precin[,6]))$r.squared
    
}

#---------------------------------------------------------------------
####------------Funciones para homogeneidad de series-------------####
#---------------------------------------------------------------------

#--Gráficos normalidad--#
graf_norm=function(){
  confirmDialog(paste("Los gráficos QQ-norm se guardarán en",getwd(),"/Homogeneidad"),"Ubicación archivos")
  
  ifelse(file.exists("Homogeneidad")=="FALSE",dir.create("Homogeneidad",showWarnings=F),"Ya existe carpeta Homogeneidad")
  
object=eval(parse(text=svalue(nom_val2)))

station=names(object[-3:-1])

if(svalue(nom_val2)=="tmax"){
dir.create("Homogeneidad/tmax_norm",showWarnings=F)
  
for(i in 1:(ncol(object)-3))
{
  jpeg(paste("Homogeneidad/tmax_norm/tmax_",station[i],".jpeg",sep=""),width = 800, height = 800)
  
  qqnorm(object[,i+3],main=paste(station[i]))
  qqline(object[,i+3],col="red",lwd=2)
  dev.off()
}}


if(svalue(nom_val2)=="tmin"){
  
  dir.create("Homogeneidad/tmin_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneidad/tmin_norm/tmin_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  
}

if(svalue(nom_val2)=="tmean"){
  
  dir.create("Homogeneidad/tmean_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneidad/tmean_norm/tmean_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  }

if(svalue(nom_val2)=="precip"){
  
  dir.create("Homogeneidad/precip_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneidad/precip_norm/precip_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  
}
  total=20
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = total, width = 300)
  
  for(i in 1:total){
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),
                                          "% done"))
  }
  close(pb)
  
  
}

#--Prueba de  normalidad: Shapiro--#
shapTest <- function(Estaciones){
  
  norm=shapiro.test(Estaciones)$p.value
  norms=ifelse(norm<svalue(obj),"NO Normal","NR")
  return(norms)
}
shap=function(x){
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Shapiro_Wilk=apply(x[,4:ncol(x)],2,shapTest)
  return(cbind(Shapiro_Wilk))
}

#--Prueba de  normalidad: KS--#
KS <- function(serie){
  #serie=na.omit(Estaciones)
  norm=ks.test(serie,"pnorm")$p.value
  ks=ifelse(norm<svalue(obj),"No Normalidad","NR")
#   result=rbind(norm,ks)
#   names(result)<-c("Valor-p","Decisión")
  return(paste(norm,ks,sep=" - "))
}
KS_test=function(object){

  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
    }else {data=object}
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmax)],2,KS)     
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmin)],2,KS)     
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(precip)],2,KS)     
  }
  
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
    }else{data1=x}

   
  Sin_datos_falt=apply(data1[,4:ncol(data1)],2,KS)
  result=as.table(cbind(Con_datos_falt,Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Kolmogorov_Smirnov (p-valor/ Desición)") 
  
  return(result)
}

#--Prueba de  normalidad: JB--#
JB<-function(Estaciones){
  #serie=na.omit(Estaciones)
  JBt=jarque.bera.test(Estaciones)$p.value
  jb=ifelse(JBt<svalue(obj),"No Normalidad","NR")
  return(paste(round(JBt,2),jb,sep=" - "))
}
JB2=function(x){
    
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    #Con_datos_falt=apply(tmax[,4:ncol(tmax)],2,JB)
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    #Con_datos_falt=apply(tmin[,4:ncol(tmin)],2,JB)
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    #Con_datos_falt=apply(precip[,4:ncol(precip)],2,JB)
  }
  
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {x=cultivoh(x)
  }else{x=x}
  
  
  Sin_datos_falt=apply(x[,4:ncol(x)],2,JB)
  result=as.table(cbind(Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Jarque_Bera (p-valor/ Desición)") 
  
  return(result)
}
## Siendo la hipótesis nula que la población está distribuida normalmente, si el p-valor es menor a alfa (nivel de confianza) 
## entonces la hipótesis nula es rechazada (se concluye que los datos no vienen de una distribución normal)


#--Estacionariedad de la Serie: Test de tendencia--#
#--Método del Rango correlación de Spearman--#

Spearman<-function(Estaciones){
  
  stat=Estaciones
  
  Rsp=round(cor(rank(stat),rank(sort(stat)),method="spearman"),4 ) ###Correlación de Rangos de Spearman
  T_t= abs(Rsp*((length(stat)-2)/(1-(Rsp^2)))^(1/2))      ###Estadistico T
  critico=qt(svalue(obj)/2,length(stat)-2,lower.tail=F)
  vp=pt(T_t,length(stat)-2,lower.tail=F)###Valor crítico  
  sp=ifelse(T_t>critico,"Tendencia","NR")
  
  return(paste(sp))
  #T_t > critico: Se rechaza Ho, es decir que hay tendencia en la serie
  
}
Rsp_Test<- function(x){
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {x=cultivoh(x)
  }else{x=x}
    
  Sin_datos_falt=apply(x[,4:ncol(x)],2,Spearman)
  result=as.table(cbind(Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Spearman (p-valor/ Desición)") 
    
return(result)}

#--Mann Kendall test--#

Kend_Test <- function(Estaciones){
  stat=Estaciones
  Vcal=MannKendall(stat)$sl[1]     ###Tendencia
  
  vp=ifelse(Vcal<svalue(obj),"Tendencia","NR")
  return(paste(round(Vcal,2),vp,sep=" - "))
  
}
Ken_T <- function(object){
 
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmax)],2,Kend_Test)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmin)],2,Kend_Test)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(precip)],2,Kend_Test)
    }
    
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Sin_datos_falt=apply(x[,4:ncol(x)],2,Kend_Test)
  result=as.table(cbind(Con_datos_falt,Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Mann_Kendall (p-valor/ Desición)") 
  
   return(result)
}

#si valor P < alpha  : rechazo hipotesis, es decir que existe tendencia

#--Estabilidad en Varianza--#
#--Test F--#
#Haciendo el test a dos subgrupos (mitad vs mitad)

par.f <- function(Estaciones){
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=var.test(part1,part2)$p.value
  
  f=ifelse(valp<svalue(obj),"Var. NO Estable","NR")
  return(paste(round(valp,2),f,sep=" - "))
  
}
F_test.indic <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmax)],2,par.f)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmin)],2,par.f)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(precip)],2,par.f)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Sin_datos_falt=apply(x[,4:ncol(x)],2,par.f)
  result=as.table(cbind(Con_datos_falt,Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Test_F (p-valor/ Desición)") 
  
 return(result)}

#si valor P < alpha  : rechazo igualdad de varianzas

#--Estabilidad de la media--#
#--Test t: requiere Estabilidad en varianza--#

PruebaT=function(Estaciones){
  
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=t.test(part1,part2)$p.value
  s=ifelse(valp<svalue(obj),"Medias Diferentes","NR")
  return(paste(round(valp,2),s,sep=" - "))
  }
T_Test <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmax)],2,PruebaT)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmin)],2,PruebaT)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(precip)],2,PruebaT)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Sin_datos_falt=apply(x[,4:ncol(x)],2,PruebaT)
  result=as.table(cbind(Con_datos_falt,Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Test_t (p-valor/ Desición)") 
  
  return(result)}

#--Test U Mann Whitney o Wilcoxon: version no paramétrica--#
#--de la prueba T (No requiere normalidad)--#

Umann <- function(Estaciones){
  
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  
  valp=wilcox.test(part1,part2)$p.value
  u=ifelse(valp<svalue(obj),"Medias Diferentes","NR")
  return(paste(round(valp,2),u,sep=" - "))
}
Umann_Test <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmax)],2,Umann)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(tmin)],2,Umann)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    Con_datos_falt=apply(data[,4:ncol(precip)],2,Umann)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Sin_datos_falt=apply(x[,4:ncol(x)],2,Umann)
  result=as.table(cbind(Con_datos_falt,Sin_datos_falt))
  names(dimnames(result)) <- c("Estación", "Test_U_Mann_Whitney (p-valor/ Desición)") 
  
  return(result)}

#---------------------------------------------------------------------
####-------------------Funciones para indicadores-----------------####
#---------------------------------------------------------------------

diasmayor=function(valor){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  
  d=data.frame(ifelse(object>valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year),sum)
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/Ind_mayor_",svalue(nom_val3),".csv"),row.names=F)
  return(x)
}

diasmenor=function(valor){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  d=data.frame(ifelse(object<valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year),sum)
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/Ind_menor_",svalue(nom_val3),".csv"),row.names=F)
  return(x)
}

tempmax=function(object){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
 result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),max2)
 
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Ind_max_",svalue(nom_val3),".csv"),row.names=F)
 return(result)
 }

tempmin=function(object){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
 result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),min2)
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Ind_min_",svalue(nom_val3),".csv"),row.names=F)
  return(result)
}


#---------------------------------------------------------------------
####-------------------Funciones para ENSO-----------------####
#---------------------------------------------------------------------

enso=function(){
  
  enso_mes=read.csv("Enso_mensual.csv",header=T,row.names=1)
  enso_mes_ok=melt(t(enso_mes),id=enso_mes$Year_Month)
  colnames(enso_mes_ok)<-c("Mes","Año","Enso")
  
  x=subset(enso_mes_ok,as.numeric(svalue(año))<=Año & Año<=as.numeric(svalue(año1)))
  todos=seq(0,12*(as.numeric(svalue(año1))-as.numeric(svalue(año))+1),12)
  mesfin=seq(todos[length(todos)]-11,todos[length(todos)],1)
  mesfin=mesfin[as.numeric(svalue(mes1))]
  Condicion_ENSO=x[as.numeric(svalue(mes)):mesfin,]
  
  return(Condicion_ENSO)
}

enso1=function(){
  
  enso_trim=read.csv("Enso_trim.csv",header=T,row.names=1)
  enso_trim_ok=melt(t(enso_trim),id=enso_trim$Year_trim)
  colnames(enso_trim_ok)<-c("Trim","Año","Enso")
  
  x=subset(enso_trim_ok,as.numeric(svalue(año2))<=Año & Año<=as.numeric(svalue(año3)))
  todos=seq(0,4*(as.numeric(svalue(año3))-as.numeric(svalue(año2))+1),4)
  trimfin=seq(todos[length(todos)]-3,todos[length(todos)],1)
  trimfin=trimfin[as.numeric(svalue(trim1))]
  Condicion_ENSO=x[as.numeric(svalue(trim)):trimfin,]
  
  return(Condicion_ENSO)
}

#---------------------------------------------------------------------
####---------------Funciones para generacion informe--------------####
#---------------------------------------------------------------------

inf=function(h,...){ #Crea y genera informe en word
  confirmDialog(paste("El informe final se guardará en",getwd()),"Ubicación archivos")
  
  
  if(svalue(nom_val2)=="tmax"){
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  info<-RTF(svalue(nom_arch),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  
 addHeader(info,title="Aplicativo para análisis de series climatológicas",subtitle=paste("Informe para la variable",svalue(nom_val2)),font.size=14)
  
  descriptiva=as.table(descript2(object))
  names(dimnames(descriptiva)) <- c("", "Estación") 
  años=paste(min(object[3]),max(object[3]),sep="-")
  addNewLine(info)
  addNewLine(info)
  
  addHeader(info,title="1. Análisis descriptivo",font.size=12)
  addNewLine(info)
  addParagraph(info, paste("El siguiente informe analiza la variable",svalue(nom_val2), "para el período comprendido entre",años))
  
  addNewLine(info)
  addParagraph(info, "Tabla 1.  Estadísticas Descriptivas") 
  addTable(info, descriptiva, font.size=8, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)

  addHeader(info,title="2. Análisis de homogeneidad",font.size=12)
  addNewLine(info)
  addParagraph(info,paste("A continuación se muestra el análisis de homogeneidad para las series de las diferentes estaciones. Para todas las pruebas se utilizó un nivel de significancia del",svalue(obj),"
Nota: Recuerde que NR indica que NO se Rechaza la hipótesis nula"))
  
  addNewLine(info)
  addHeader(info,title="2.1 Pruebas para detectar normalidad",font.size=11)
  addNewLine(info)
  addParagraph(info, "Hipótesis nula: Los datos de la estación siguen una distribución normal") 
  
  addNewLine(info)
  addParagraph(info, "Tabla 2.1.1  Test de Kolmogorov Smirnov") 
  kolm=KS_test(object)
 # names(dimnames(kolm)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, kolm, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Tabla 2.1.2  Test Jarque Bera") 
  jb=JB2(object)
  #names(dimnames(jb)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, jb, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addHeader(info,title="2.2Pruebas para detectar tendencia",font.size=11)
  addNewLine(info)
  
  addParagraph(info, "Hipótesis nula: Los datos de la estación NO siguen una tendencia") 
  
  addNewLine(info)
  
  addParagraph(info, "Tabla 2.2.1  Rango de correlación Spearman") 
  rsp=Rsp_Test(object)
 # names(dimnames(rsp)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, rsp, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Tabla 2.2.2  Test Mann Kendall") 
  mk=Ken_T(object)
  #names(dimnames(mk)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, mk, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  
  addHeader(info,title="2.3 Pruebas para detectar estabilidad en varianza",font.size=11)
  addNewLine(info)
  addParagraph(info, "Hipótesis nula: Los datos de la estación son estables en varianza") 
  
  addNewLine(info)
  
  
  addParagraph(info, "Tabla 2.3.1  Test F") 
  ft=F_test.indic(object)
  #names(dimnames(ft)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info,ft, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addHeader(info,title="2.4 Pruebas para detectar estabilidad en media",font.size=11)
  addNewLine(info)
  
  addParagraph(info, "Hipótesis nula: Los datos de la estación son estables en media") 
  
  addNewLine(info)
  
  addParagraph(info, "Tabla 2.4.1  Test t") 
  tt=T_Test(object)
  #names(dimnames(tt)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info,tt, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Tabla 2.4.2  Test U Mann Whitney") 
  umt=Umann_Test(object)
  #names(dimnames(umt)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info,umt, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  addNewLine(info)
  
  
  
   addNewLine(info)
  addNewLine(info)
  
  addNewLine(info)
  addNewLine(info) 
  addSessionInfo(info) 
  done(info)
  }

inf2=function(h,...){ #Crea y genera informe en word
  confirmDialog(paste("El pre-informe se guardará en",getwd()),"Ubicación archivos")
  
  info<-RTF(svalue(nom_arch1),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  
  addHeader(info,title="Aplicativo para análisis de series climatológicas",subtitle="Pre-informe",font.size=14)
  
  descriptiva=as.table(descript2(eval(parse(text=svalue(variable)))))
  names(dimnames(descriptiva)) <- c("", "Estación") 
  
  
  años=paste(min(tmax[,3]),max(tmax[,3]),sep="-")
  addNewLine(info)
  addNewLine(info)
  
  addHeader(info,title="1. Análisis descriptivo",font.size=12)
  addNewLine(info)
  addParagraph(info, paste("El siguiente informe analiza la variable", svalue(variable) ,"para el período comprendido entre",años))
  
  addNewLine(info)
  addParagraph(info, "Tabla 1.1  Estadísticas Descriptivas ") 
  addTable(info, descriptiva, font.size=8, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
   
  validacion=as.table(validar22(eval(parse(text=svalue(variable)))))
  names(dimnames(validacion)) <- c("Estación", "") 
  
  
  addNewLine(info)
  
  addHeader(info,title="2. Control de calidad ",font.size=12)
  addNewLine(info)
  
  startParagraph(info) 
  addParagraph(info,"Ahora se realiza una validación para las diferentes estaciones en estudio, la cual consta del calculo de varios criterios que ayudan a identificar datos atípicos y/o erróneos para su posterior corrección. \n")
  addParagraph(info,paste("Para la identificación de datos atípicos se utilizaron",svalue(criterio),"desviaciones estándar y se estableció un rango de valores permitidos ",svalue(minim),"<",svalue(variable),"<",svalue(maxim),".\n Nota: Los datos identificados en esta sección son reemplazados por NA's, a excepción de los identificados como datos atípicos y aquellas temperaturas con variación mayor a 10ºC. La última columna de las siguientes tablas indican el % total de datos faltantes que serán llenados en la sección Datos faltantes. Se recomienda que este % no supere el 40% en cada una de las estaciones."))
  endParagraph(info)     
  addNewLine(info)
  
  addParagraph(info, paste("Tabla 2.1  Validación para ",svalue(variable))) 
  addTable(info, validacion, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addNewLine(info)
  addNewLine(info) 
  addSessionInfo(info) 
  done(info)
  
  
}


