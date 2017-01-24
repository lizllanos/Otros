

#                ____   ____ _ _         _____           _ 
#               |  _ \ / ___| (_)_ __ __|_   _|__   ___ | |
#               | |_) | |   | | | '_ ` _ \| |/ _ \ / _ \| |
#               |  _ <| |___| | | | | | | | | (_) | (_) | |
#               |_| \_\\____|_|_|_| |_| |_|_|\___/ \___/|_|

#               __     __            _               _   ___  
#               \ \   / /__ _ __ ___(_) ___  _ __   / | / _ \ 
#                \ \ / / _ \ '__/ __| |/ _ \| '_ \  | || | | |
#                 \ V /  __/ |  \__ \ | (_) | | | | | || |_| |
#                  \_/ \___|_|  |___/_|\___/|_| |_| |_(_)___/ 
  


#IMPORTANTE LEER!!!!
#IMPORTANTE LEER!!!!
#IMPORTANTE LEER!!!!
#Escriba la ubicación de la carpeta con los archivos de la herramienta
dir="C:/Users/lllanos/Dropbox/Trabajo_CIAT/Logos" 
#---------------------------------------------------------------------
####--------Paquetes necesarios para correr la aplicación---------####
#---------------------------------------------------------------------



if(require(ggplot2)==FALSE){install.packages("ggplot2")} 
if(require(rtf)==FALSE){ install.packages("rtf")}
if(require(gWidgets)==FALSE){install.packages("gWidgets")} 
if(require(gWidgetsRGtk2)==FALSE){install.packages("gWidgetsRGtk2") } 
if(require(RMAWGEN)==FALSE){install.packages("RMAWGEN") } 
if(require(Kendall)==FALSE){install.packages("Kendall") } 
if(require(tseries)==FALSE){install.packages("tseries")}
if(require(grid)==FALSE){install.packages("grid")} 
if(require(car)==FALSE){ install.packages("car") }
if(require(reshape)==FALSE){ install.packages("reshape")}
if(require(maptools)==FALSE){install.packages("maptools")}
if(require(mapdata)==FALSE){install.packages("mapdata")}
if(require(RColorBrewer)==FALSE){install.packages("RColorBrewer")}
if(require(classInt)==FALSE){install.packages("classInt")}
if(require(gpclib)==FALSE){install.packages("gpclib")}

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


cargar_tmin2=function(){
  tmin=read.csv(gfile("Seleccione un archivo"),header=T)
  assign("tmin",tmin,.GlobalEnv)
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

cargar_tmax2=function(){
  tmax=read.csv(gfile("Seleccione un archivo"),header=T)
  assign("tmax",tmax,.GlobalEnv)
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

cargar_precip2=function(){
  precip=read.csv(gfile("Seleccione un archivo"),header=T)
  assign("precip",precip,.GlobalEnv)
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
  Media=round(mean2(object, na.rm=T),3);  Varianza=round(var(object, na.rm=T),3);  Desv.Est=round(sqrt(Varianza),3)
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
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Temp. Máxima")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Temp. Máxima")
      hist(object[,i+3],main=paste(station[i]),xlab="Temp. Máxima")
      dev.off()
      
      jpeg(paste("Analisis grafico/tmax_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
           
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Temp. Máxima",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Temp. Máxima",station[i],sep="_"),xlab="Año")
      
      
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
      
      plot(object1[,i+3],type="l",main=paste(station[i]),xlab="Años",ylab="Temp. Mínima")
      boxplot(object[,i+3],main=paste(station[i]),ylab="Temp. Mínima")
      hist(object[,i+3],main=paste(station[i]),xlab="Temp. Mínima")
      dev.off()
      
      jpeg(paste("Analisis grafico/tmin_plot/mes_anual_",station[i],".jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+3], geom = "boxplot",
                group = month1, ylab=paste("Temp. Mínima",station[i],sep="_"),xlab="Mes")
      
      year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",
                 group =  tmax$year,ylab=paste("Temp. Mínima",station[i],sep="_"),xlab="Año")
      
      
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
                group = month1, ylab=paste("Temp. Mínima",station[i],sep="_"),xlab="Mes")
      
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
  dir.create("Analisis grafico/Gráficos diarios",showWarnings=F)
  
  dir.create("Analisis grafico/Gráficos diarios/Gráficos plot e histogramas",showWarnings=F)
  
  for(i in 1:(ncol(tmax)-3)){
   
    tiff(paste("Analisis grafico/Gráficos diarios/Gráficos plot e histogramas/plot_",station[i],"_d.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    #jpeg(paste("Analisis grafico/Gráficos diarios/Gráficos plot e histogramas/plot_",station[i],"_d.jpeg",sep=""),width = 1000, height = 500)
    layout(m)
    plot(tmax1[,i+3],type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
    title(main=paste(station[i]),outer = TRUE, line = -1)
    hist(tmax1[,i+3],main="",xlab="Temp. Máxima")
    plot(tmin1[,i+3],type="l",xlab="Años",ylab="Temp. Mínima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
    hist(tmin1[,i+3],main="",xlab="Temp. Mínima")
    plot(precip1[,i+3],type="l",xlab="Años",ylab="Precipitación")
    hist(precip1[,i+3],main="",xlab="Precipitación")
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
    dir.create("Analisis grafico/Gráficos Mensuales",showWarnings=F)
    
    dir.create("Analisis grafico/Gráficos Mensuales/Gráficos plot e histogramas",showWarnings=F)
   
    for(i in 1:(ncol(tmax)-3)){
     
      tiff(paste("Analisis grafico/Gráficos Mensuales/Gráficos plot e histogramas/plot_",station[i],"_m.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
      #jpeg(paste("Analisis grafico/Gráficos Mensuales/Gráficos plot e histogramas/plot_",station[i],"_m.jpeg",sep=""),width = 1200, height = 500)
      layout(m)
      plot(tmax1[,i+2],type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
      title(main=paste(station[i]),outer = TRUE, line = -1)
      hist(tmax1[,i+2],main="",xlab="Temp. Máxima")
      plot(tmin1[,i+2],type="l",xlab="Años",ylab="Temp. Mínima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black")
      hist(tmin1[,i+2],main="",xlab="Temp. Mínima")
      plot(precip1[,i+2],type="l",xlab="Años",ylab="Precipitación")
      hist(precip1[,i+2],main="",xlab="Precipitación")
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
    dir.create("Analisis grafico/Gráficos mensuales",showWarnings=F)
    
    dir.create("Analisis grafico/Gráficos mensuales/Gráficos boxplot",showWarnings=F)
    
    for(i in 1:(ncol(tmax)-2)){
      
      tiff(paste("Analisis grafico/Gráficos mensuales/Gráficos boxplot/box_",station[i],"_m.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
      
      #jpeg(paste("Analisis grafico/Gráficos mensuales/Gráficos boxplot/box_",station[i],"_m.jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+2], geom = "boxplot",group = month1, ylab=paste("Temp. Máxima",sep="_"),xlab="Mes", outlier.size =0.7)
      mes1=qplot(month1,tmin[,i+2], geom = "boxplot",group = month1, ylab=paste("Temp. Mínima",sep="_"),xlab="Mes", outlier.size =0.7)
      mes2=qplot(month1,precip[,i+2], geom = "boxplot",group = month1, ylab=paste("Precip",sep="_"),xlab="Mes", outlier.size =0.7)
      
      year=qplot(tmax$year ,tmax[,i+2], geom = "boxplot",  group =  tmax$year,ylab=paste("Temp. Máxima",sep="_"),xlab="Año",outlier.size =0.7)
      year1=qplot(tmin$year ,tmin[,i+2], geom = "boxplot", group =  tmin$year,ylab=paste("Temp. Mínima",sep="_"),xlab="Año", outlier.size =0.7)
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
    
    gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Temp. Máxima",xlab="",outlier.size =0.7)
    gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Temp. Mínima",xlab="",outlier.size =0.7)
    gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precipitación",xlab="", outlier.size =0.7)
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    tiff(paste("Analisis grafico/Gráficos mensuales/Gráficos boxplot/todos_m.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
    #jpeg(paste("Analisis grafico/Gráficos mensuales/Gráficos boxplot/todos_m.jpeg",sep=""),width = 600, height = 500)
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
  dir.create("Analisis grafico/Gráficos Diarios",showWarnings=F)
    dir.create("Analisis grafico/Gráficos Diarios/Gráficos boxplot",showWarnings=F)
    
  for(i in 1:(ncol(tmax)-3)){
    
    tiff(paste("Analisis grafico/Gráficos diarios/Gráficos boxplot/box_",station[i],"_d.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
 # jpeg(paste("Analisis grafico/Gráficos diarios/Gráficos boxplot/box_",station[i],"_d.jpeg",sep=""),width = 1200, height = 500)
  
  mes=qplot(month1,tmax[,i+3], geom = "boxplot",group = month1, ylab=paste("Temp. Máxima",sep="_"),xlab="Mes",outlier.size =0.7)
  mes1=qplot(month1,tmin[,i+3], geom = "boxplot",group = month1, ylab=paste("Temp. Mínima",sep="_"),xlab="Mes", outlier.size =0.7)
  mes2=qplot(month1,precip[,i+3], geom = "boxplot",group = month1, ylab=paste("Precipitación",sep="_"),xlab="Mes", outlier.size =0.7)
  
  year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",  group =  tmax$year,ylab=paste("Temp. Máxima",sep="_"),xlab="Año",outlier.size =0.7)
  year1=qplot(tmin$year ,tmin[,i+3], geom = "boxplot", group =  tmin$year,ylab=paste("Temp. Mínima",sep="_"),xlab="Año",outlier.size =0.7)
  year2=qplot(precip$year ,precip[,i+3], geom = "boxplot", group =  precip$year,ylab=paste("Precipitación",sep="_"),xlab="Año", outlier.size =0.7)
  
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
  
  gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Temp. Máxima",xlab="", outlier.size =0.7)
  gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Temp. Mínima",xlab="", outlier.size =0.7)
  gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precipitación",xlab="", outlier.size =0.7)
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
    tiff(paste("Analisis grafico/Gráficos diarios/Gráficos boxplot/todos_d.tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
  #jpeg(paste("Analisis grafico/Gráficos diarios/Gráficos boxplot/todos_d.jpeg",sep=""),width = 600, height = 500)
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
    r <- abs(cor(x, y,na.rm=T))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2)
  }
  
  
  dir.create("Analisis grafico/Gráficos dispersión",showWarnings=F)
  
  tiff(paste("Analisis grafico/Gráficos dispersión/tmax.tiff",sep=""), width = 10, height = 10,units = 'in',res=200)
  
  #jpeg(paste("Analisis grafico/Gráficos dispersión/tmax.jpeg",sep=""),width = 600, height = 600)
  datostmax=data.frame(na.omit(tmax[-3:-1]))
  pairs(datostmax,lower.panel=panel.cor, pch=19,col="black")
  
  dev.off()
  
  tiff(paste("Analisis grafico/Gráficos dispersión/tmin.tiff",sep=""), width = 10, height = 10,units = 'in',res=200)
  
  #jpeg(paste("Analisis grafico/Gráficos dispersión/tmin.jpeg",sep=""),width = 600, height = 600)
  datostmin=data.frame(na.omit(tmin[-3:-1]))
  pairs(datostmin,lower.panel=panel.cor, pch=19,col="black")
  dev.off()
  
  tiff(paste("Analisis grafico/Gráficos dispersión/precip.tiff",sep=""), width = 10, height = 10,units = 'in',res=200)
  
 # jpeg(paste("Analisis grafico/Gráficos dispersión/precip.jpeg",sep=""),width = 600, height = 600)
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

sum2=function(a,na.rm=T){
  return(sum(a,na.rm=T))
}


grafcontrol<-function(object){
  
  station=names(object[-3:-1])
  ifelse(file.exists("Control de calidad")=="FALSE",dir.create("Control de calidad",showWarnings=F),"Ya existe carpeta Control de Calidad")
  
  
  object1=ts(object,start=min(object$year),frequency=365)
  
  if(svalue(variable)=="tmax"){
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmax"),"Ubicación archivos")
    dir.create("Control de calidad/tmax",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      tiff(paste("Control de calidad/tmax/tmax_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
      
     # jpeg(paste("Control de calidad/tmax/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmin"),"Ubicación archivos")
       dir.create("Control de calidad/tmin",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
   porcentajes=matrix(0,ncol(object)-3,2)
   
    for(i in 1:(ncol(object)-3))
    {
      tiff(paste("Control de calidad/tmin/tmin_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
      
      #jpeg(paste("Control de calidad/tmin/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/tmean"))
      dir.create("Control de calidad/tmean",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Control de calidad/tmean/tmean_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Los resultados gráficos del control de calidad se guardarán en",getwd(),"/Control de calidad/precip"),"Ubicación archivos")
    dir.create("Control de calidad/precip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      tiff(paste("Control de calidad/precip/precip_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
      
     # jpeg(paste("Control de calidad/precip/precip_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
      if(sum2(z[,1]<5)!=nrow(z)){ 
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
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(object)[1])*100,2),round((sum2(nrow(atipicos2),na)/dim(object)[1])*100,2)) 
        
      
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
      if(sum2(z[,1]<5)!=nrow(z)){ 
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
      if(sum2(z[,1]<5)!=nrow(z)){ 
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
    dir.create("Control de calidad/precip",showWarnings=F)
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
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum2(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
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
      if(sum2(z[,1]<5)!=nrow(z)){ 
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
      if(sum2(z[,1]<5)!=nrow(z)){ 
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
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum2(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
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
  origin <- paste(1,1,svalue(year_min),sep="-")
  
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
  
    
    
  for(i in 1:(ncol(data_genTmax)-3))
  {
    tiff(paste("Datos_faltantes/tmax_faltantes/tmax_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
    #jpeg(paste("Datos_faltantes/tmax_faltantes/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(tmax)[1],data_genTmax[,i+3],,main=paste("tmax_",station[i]),type="l",xlab="Años",ylab="Temp. Máxima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="blue",lwd=1.5)
    lines(1:dim(tmax)[1],tmax[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generada","Original"),lwd=c(1.5,1.5),col=c("blue","black"))
    dev.off()
    
    tiff(paste("Datos_faltantes/tmax_faltantes/tmin_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
    #jpeg(paste("Datos_faltantes/tmin_faltantes/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(tmin)[1],data_genTmin[,i+3],type="l",col="blue",lwd=1.5,main=paste("tmin_",station[i]),xlab="Años",ylab="Temp. Mínima",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)))
    lines(1:dim(tmin)[1],tmin[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generada","Original"),lwd=c(1.5,1.5),col=c("blue","red"))
    dev.off()
    
    tiff(paste("Datos_faltantes/tmax_faltantes/prec_",station[i],".tiff",sep=""), width = 10, height = 7,units = 'in',res=200)
    
    #jpeg(paste("Datos_faltantes/precip_faltantes/prec_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(1:dim(precip)[1],data_genPrec[,i+3],type="l",col="blue",lwd=1.5,main=paste("prec_",station[i],sep=""),xlab="Años",ylab="Precipitación")
    lines(1:dim(precip)[1],precip[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generada","Original"),lwd=c(1.5,1.5),col=c("blue","red"))
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
  
  confirmDialog(paste("El proceso ha finalizado. Los resultados se guardarán en",getwd(),"/Datos faltantes"),"Estado del Proceso ")
  
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
  
  
  
}

barra=function(){
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


####Climaticos
tempmax_ind=function(valor){
 
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  if(svalue(nom_valm1)=="mayor a "){
    d=data.frame(ifelse(object>=valor,1,0))
    
  }
  
  if(svalue(nom_valm1)=="menor a "){
    d=data.frame(ifelse(object<=valor,1,0))
    
  }
  
  x=aggregate(d[-3:-1],list(Año=object$year,Mes=object$month),sum2)
  x=rbind(x,c("","Total ",apply(x[-2:-1],2,sum2)))
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/tmax_",svalue(nom_valm),valor,".csv"),row.names=F)
  return(x)
}

tempmin_ind=function(valor){
  object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  if(svalue(nom_valm)=="mayor a "){
    d=data.frame(ifelse(object>=valor,1,0))
    
  }
  
  if(svalue(nom_valm)=="menor a "){
    d=data.frame(ifelse(object<=valor,1,0))
    
  }
  
  d=data.frame(ifelse(object<valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year,Mes=object$month),sum2)
  x=rbind(x,c("","Total ",apply(x[-2:-1],2,sum2)))
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/tmin_",svalue(nom_valm1),valor,".csv"),row.names=F)
  return(x)
}


lluvia=function(){

  object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
}else{object=object}

d=data.frame(ifelse(object>=0.5,1,0))
x=aggregate(d[-3:-1],list(Año=object$year,Mes=object$month),sum2)

  x=rbind(x,c("","Total Acum.",apply(x[-2:-1],2,sum2)))
dir.create("Indicadores",showWarnings=F)
write.csv(x,paste("Indicadores/Ind_precip.csv"),row.names=F)
return(x)
}

lluvia2=function(valor){
  
  object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  if(svalue(nom_valp)=="mayor a "){
    d=data.frame(ifelse(object>=valor,1,0))
    
  }
  
  if(svalue(nom_valp)=="menor a "){
    d=data.frame(ifelse(object<=valor,1,0))
    
  }
  
  x=aggregate(d[-3:-1],list(Año=object$year,Mes=object$month),sum2)
  x=rbind(x,c("","Total Acum.",apply(x[-2:-1],2,sum2)))
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/Ind_lluvia_",svalue(nom_valp),valor,".csv"),row.names=F)
  return(x)
}

cosc=function(d,valor2){
  z=with(rle(d),cbind(lengths,cumsum(lengths),values))
  consec=sum2(z[which(z[,3]==1),1]>=valor2)
  
  return(consec)
}

lluviaconsc=function(valor,valor2){
  
  object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
      
  d=data.frame(ifelse(object>=valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year,Mes=object$month),cosc,valor2)
  x=rbind(x,c("","Total Acum.",apply(x[-2:-1],2,sum2)))
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(x,paste("Indicadores/Ind_lluvia_consec.csv"),row.names=F)
  return(x)
}

cuantil=function(est){
  est=which(names(tmax)==svalue(nom_est_c))
  tmaxc<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  tminc<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  precipc<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
 # todo=list()
  
  #for(est in 1:(ncol(tmaxc)-3)){
  
  tmax1=tmaxc[,est]
   tmin1=tminc[,est]
   precip1=precipc[,est]
   
 

  if(svalue(nom_val_c)=="Cuartiles"){
    cuantiles=cbind(quantile(tmax1),quantile(tmin1),quantile(precip1))
  }
  
  if(svalue(nom_val_c)=="Terciles"){
    cuantiles=cbind(quantile(tmax1,probs = c(0.33,0.66,0.99)),quantile(tmin1,probs = c(0.33,0.66,0.99)),quantile(precip1,probs = c(0.33,0.66,0.99)))
  }
  
  if(svalue(nom_val_c)=="Deciles"){
    cuantiles=cbind(quantile(tmax1,probs = seq(0, 1, 0.1)),quantile(tmin1,probs = seq(0, 1, 0.1)),quantile(precip1,probs = seq(0, 1, 0.1)))
  }
  if(svalue(nom_val_c)=="Percentiles"){
    cuantiles=cbind(quantile(tmax1,probs = seq(0, 1, 0.01)),quantile(tmin1,probs = seq(0, 1, 0.01)),quantile(precip1,probs = seq(0, 1, 0.01)))
  }
  
  cuantiles=round(cuantiles,2)
  colnames(cuantiles)<-c("Temp. Máx.","Temp. Mín.","Precip.")
  
#   todo[[est]]=cuantiles
#   }
#   
  return(cuantiles)
}

###Agroclimaticos

gradosdias=function(base){
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {tmax1=cultivoi(object)
  }else{tmax1=object}
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {tmin1=cultivoi(object1)
  }else{tmin1=object1}
  
  tprom=((tmax1[-3:-1]+tmin1[-3:-1])/2)-20
  gd=apply(tprom,2,sum2)
  return(gd)
}

chu=function(){
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {tmax1=cultivoi(object)
  }else{tmax1=object}
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {tmin1=cultivoi(object1)
  }else{tmin1=object1}
  
  x=1.8*(tmin1-5)
  y=3.33*(tmax1-10)-(0.083*(tmax1-10)^2)
  
  
  chu=(x+y)/2
  chuf=apply(chu[-3:-1],2,sum2)
  return(chuf)
}


spi=function(){
  confirmDialog(paste("Los resultados de éste indicador se guardarán en",getwd(),"/Indicadores/SPI"),"Ubicación archivos")
  
  options(digits=3) 
  
  object=read.csv("Datos_faltantes/data_genPrec.csv",header=T)
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {prec_d=cultivoi(object)
  }else{prec_d=object}
  
   
  
  x=aggregate(prec_d[-3:-1],list(Año=prec_d$year,Mes=prec_d$month),sum2)
  x1=order(x$Año)
  pr=x[x1,]
  
  iniyr=min(pr[,1])
  
  stations<-colnames(pr[,3:ncol(pr)])
  Nt <- dim(pr)
  nmo<-as.numeric(Nt[1])
  pr<-pr[,3:ncol(pr)]
  
  for (i in stations) {
    #  pr[[i]][pr[[i]]==-9999] <- NA
    rm(pr2)
    rm(spi)
    rm(precipitation)
    rm(precipitation.detrend)
    pr2<-t(pr[[i]])
    precipitation<-pr2[1,]
    
    
    getPrecOnTimescale <- function(precipitation,k){
      # precipitation is a vector of monthly precipitation values
      # returns monthly precipation averaged over current month and prior k-1 months
      Nt <- length(precipitation)
      prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); mean2(as.vector(precipitation[tm:t]))}))
      return(prec.k)
    }
    
    
    getSPIfromPrec <- function(precipitation){
      
      #takes a vector of precipitation values 
      #and returns a vector of spi values
      
      
      Nt <- length(precipitation)
      #include full years data only
      years <- ceiling(Nt/12)
      full.years <- trunc(Nt/12)
      Nt.f <- full.years*12
      monthsInCurrentYear <- Nt %% 12
      #monthly analysis
      spi.o <- array(NA,c(years,12))
      for (m in 1:12)
      {
        mdata <- precipitation[seq(from=m, to=Nt, by=12)]
        # empirical cdf's for each month 
        fit.cdf <- ecdf(mdata)
        #locate each data point on cdf by applying fit.cdf function for each location
        #cdf probabilites
        # these will be unformly distributed on interval 1/years
        cdfs <- as.vector(sapply(mdata,fit.cdf))
        #invert normal
        spi.t <- qnorm(cdfs)
        spi.tp <- spi.t[ spi.t != Inf] #drop Inf
        ff <- function(x) (1-sd(c(x,spi.tp)))^2
        #replace Inf with the value that sets sd(spi)=1 or mean = 0 (minimises ff)
        spi.t[spi.t==Inf] <- optimize(ff,lower=0,upper=100)$minimum
        # ensure mean is zero. spi.t is normally distributed with mean zero and sd approx 1
        spi.t <- spi.t - mean2(spi.t)
        ifelse( !(monthsInCurrentYear==0),ifelse (m <=monthsInCurrentYear, spi.o[,m]<-spi.t, spi.o[,m]<- c(spi.t,0) ), spi.o[,m]<-spi.t)
      }
      
      spi <- array(0,c(Nt))
      
      for ( t in 1:Nt){
        month <- (t-1)%%12 + 1
        year <- 1 + trunc((t-1)/12)
        spi[t] <- spi.o[year,month]
        
      }
      return(spi)
    }
    
    
    #generate some sample recipitation values using a weibull distribution
    #precipitation <- rweibull(500,2,1)
    
    #create a plot of SPI index vs timescale
    options(warn=-1)
    Nt<- length(precipitation)
    date <- seq(from=iniyr,by=1/12,length.out=Nt)
    #detrend
    prec.fit <- lm(precipitation~date)
    precipitation.detrend <- precipitation- fitted(prec.fit)
    
    #spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(precipitation,i)))
    # if you want to detrend use this one
    spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(precipitation.detrend,i)))
    
    
    ##plot timeseries panel
    dir.create("Indicadores",showWarnings=F)
    dir.create("Indicadores/SPI",showWarnings=F)
    
    tiff(paste("Indicadores/SPI/",i,"_ts.tiff",sep=""), width = 8, height = 5,units = 'in',res=200)
    
    #pdf(paste("Indicadores/SPI/",i,"_ts.pdf",sep=""),paper="USr", pointsize = 12,
    #    bg = "white")
    #attach(mtcars)
    Fechas=c("Ene","Mar","May","Jul","Sep","Nov") # Crea una tabla con los nombres de los meses
    
  if(max(date)-iniyr<4){
    label_x=rep(Fechas,round(max(date)-iniyr))
    at=seq(1,nrow(spi),by=2)
    
    axis_lab=function(label_x,at,Fechas){ 
      axis(side=1,labels=label_x,at=seq(iniyr,max(date),1/6),las=2)
      axis(side=2,labels=Fechas,at=seq(1,12,2),las=2)
    } 
     }
    
    if(max(date)-iniyr>=4){
      at=seq(1,nrow(spi),by=36)
      label_x=seq(iniyr,max(date),3)
      
      axis_lab=function(label_x,at,Fechas){ 
        axis(side=1,labels=label_x,at=label_x,las=2)
        axis(side=2,labels=Fechas,at=seq(1,12,2),las=2)
      } 
      
    }
    
   
    m=matrix(c(1:4),2,2,byrow=T)
    layout(m)
    par(mar=c(2.5,2.5,2.5,2.5))
    
    plot(1:nrow(spi),spi[ ,1],type="h",ylab="SPI",xlab="",col="red",main="SPI-1",xaxt="n")
    axis(side=1,labels=label_x,at=at,las=2)
    plot(1:nrow(spi),spi[ ,3],type="h",ylab="SPI",xlab="",col="orange",main="SPI-3",xaxt="n")
    axis(side=1,labels=label_x,at=at,las=2)
    plot(1:nrow(spi),spi[ ,6],type="h",ylab="SPI",xlab="",col="yellow",main="SPI-6",xaxt="n")
    axis(side=1,labels=label_x,at=at,las=2)
    plot(1:nrow(spi),spi[ ,12],type="h",ylab="SPI",xlab="",col="purple",main="SPI-12",xaxt="n")
    axis(side=1,labels=label_x,at=at,las=2)
    
    title(main=i, cex.main=1,side = 3, line = -1.2, outer = TRUE)
    dev.off()
    
    
    #plot multi-scale figure
    spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4)
    spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
    
    tiff(paste("Indicadores/SPI/",i,".tiff",sep=""), width = 8, height = 5,units = 'in',res=200)
    
    #pdf(paste("Indicadores/SPI/",i,".tiff",sep=""),paper="USr",width = 10, height = 4, pointsize = 12,
     #   bg = "white")
    filled.contour(date,seq(1:12),spi,col=spi.cols(11),xlab="",ylab="Escala temporal (meses)",xaxt="n",cex.lab=1,font.axis=1,font.lab=1,levels=spi.breaks,key.title="SPI",axes=T,plot.axes=axis_lab(label_x,at,Fechas))
    title(main=i,cex.main=1)
    
    dev.off()
       }
  #print("El proceso ha finalizado")
}

####Generacion de nuevas variables
#A nivel mensual
tempmax=function(object){
  
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
#   }else{object=object}
#   
  result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),max2)
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Ind_max_mensual.csv"),row.names=F)
  return(result)
}

tempmin=function(object){
  
  
  
    object<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
    
  result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),min2)
  
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Ind_min_mensual.csv"),row.names=F)
  return(result)
}

tempmean=function(object){
  
    object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  
    result=(object[-3:-1]+object1[-3:-1])/2
    result=cbind(object[,1:3],result)
  
    result_m=aggregate(result[-3:-1],list(Mes=object$month,Año=object$year),mean2)
    
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Ind_mean_mensual.csv"),row.names=F)
  return(result)
}

precpcum=function(){
  
  object <- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
#   }else{object=object}
#   
  result=aggregate(object[-3:-1],list(Año=object$year,Mes=object$month),sum2)
  result=rbind(result,c("","Total Acum.",apply(result[-2:-1],2,sum2)))
  dir.create("Indicadores",showWarnings=F)
  write.csv(result,paste("Indicadores/Precip_cum.csv"),row.names=F)
  return(result)
}

#A nivel diario o mensual
hr=function(){
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
#   }else{object=object}
#   
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object1=cultivoi(object1)
#   }else{object1=object1}
  
  twstmin=0.61088*exp((17.27*object1)/(237.3+object1))
  twstmax=0.61088*exp((object*17.27)/(237.3+object))
  
  twd=0.61088*exp((17.27*(object1-2))/(237.3+(object1-2)))
  
  hr=(((twd/twstmin)+(twstmin/twstmax))/2)*100
}

vpd=function(){
  
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object1=cultivoi(object1)
  }else{object1=object1}
  
  twstmin=0.61088*exp((17.27*object1[-3:-1])/(237.3+object1[-3:-1]))
  twstmax=0.61088*exp((object[-3:-1]*17.27)/(237.3+object[-3:-1]))
  
  es=(twstmin+twstmax)/2
  
  twd=0.61088*exp((17.27*(object1[-3:-1]-2))/(237.3+(object1[-3:-1]-2)))
  vdpr=es-twd
  vdpr=cbind(object[,1:3],vdpr)
  return(vdpr)
  
}

osc_temp=function(){
  
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
#   }else{object=object}
#   
#   if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object1=cultivoi(object1)
#   }else{object1=object1}
#   
  result=object[-3:-1]-object1[-3:-1]
  result=cbind("Año"=object[,3],"Mes"=object[,2],round(result,2))
  return(result)
}


#---------------------------------------------------------------------
####-----------------------Funciones para ENSO--------------------####
#---------------------------------------------------------------------
datos_enso_m=function(){
  data_m_e=read.csv(gfile("Seleccione un archivo"),header=T)
  assign("data_m_e",data_m_e,.GlobalEnv)
}

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
  
  enso_trim=read.csv(paste(dir,"/ENSO/","Enso_trim.csv",sep=""),header=T,row.names=1)
  enso_trim_ok=melt(t(enso_trim),id=enso_trim$Year_trim)
  colnames(enso_trim_ok)<-c("Trim","Año","Enso")
  
  x=subset(enso_trim_ok,as.numeric(svalue(año2))<=Año & Año<=as.numeric(svalue(año3)))
  todos=seq(0,4*(as.numeric(svalue(año3))-as.numeric(svalue(año2))+1),4)
  trimfin=seq(todos[length(todos)]-3,todos[length(todos)],1)
  trimfin=trimfin[as.numeric(svalue(trim1))]
  Condicion_ENSO=x[as.numeric(svalue(trim)):trimfin,]
  
  return(Condicion_ENSO)
}

graficos_enso_anomalias=function(){
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  datos=read.csv("Enso_ONI.csv",header=T, sep = ",",  dec=".")
  
  
  if(exists("data_m_e")){
    
    data_m_e1=cbind(data_m_e,datos[which(datos[,1]==min(data_m_e$year))[1]:max(which(datos[,1]==max(data_m_e$year))),])
    
    promedios_tmax=aggregate(data_m_e1[which(names(data_m_e1)=="tmax")],list(Mes=data_m_e1$month),mean2)
    promedios_tmin=aggregate(data_m_e1[which(names(data_m_e1)=="tmin")],list(Mes=data_m_e1$month),mean2)
    
    promedios_precip=aggregate(data_m_e1[which(names(data_m_e1)=="precip")],list(Mes=data_m_e1$month),mean2)
    desv_precip=aggregate(data_m_e1[which(names(data_m_e1)=="precip")],list(Mes=data_m_e1$month),sd2)
    
    
    
    #station=names(object[-3:-1])
    dir.create("ENSO",showWarnings=F)
    dir.create("ENSO/Graficos_anomalias",showWarnings=F)
    
       
      an=data_m_e1[which(names(data_m_e1)=="tmax")]-promedios_tmax[,2]
      
      an1=data_m_e1[which(names(data_m_e1)=="tmin")]-promedios_tmin[,2]
      
      an2=(data_m_e1[which(names(data_m_e1)=="precip")]-promedios_precip[,2])/as.numeric(unlist(rep(desv_precip[,2],max(data_m_e1$year)-min(data_m_e1$year)+1)))
      
   
    
    an=as.data.frame(cbind(an,"year"=data_m_e1$year))
    an1=as.data.frame(cbind(an1,"year"=data_m_e1$year))
    an2=as.data.frame(cbind(an2,"year"=data_m_e1$year))
    
    
    assign("an", an, envir=.GlobalEnv)
    assign("an1", an1, envir=.GlobalEnv)
    assign("an2", an2, envir=.GlobalEnv)
       
      
      g3=ggplot(data_m_e1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(data_m_e1),by=36), labels=seq(min(data_m_e1$year),max(data_m_e1$year),3))+xlab("")+ 
        theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
        scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an[,1],colour="Anomalías obsv. tmáx"),data=an,size=0.5)+ylab("ONI")+
        scale_color_manual(values=c("black"))
      
      g31=ggplot(data_m_e1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(data_m_e1),by=36), labels=seq(min(data_m_e1$year),max(data_m_e1$year),3))+xlab("")+ 
        theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
        scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an1[,1],colour="Anomalías obsv. tmín"),data=an1,size=0.5)+ylab("ONI")+
        scale_color_manual(values=c("black"))
      
      g32=ggplot(data_m_e1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(data_m_e1),by=36), labels=seq(min(data_m_e1$year),max(data_m_e1$year),3))+xlab("")+ 
        theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
        scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an2[,1],colour="Anomalías obsv. precip"),data=an2,size=0.5)+ylab("ONI")+
        scale_color_manual(values=c("black"))
      
      tiff(paste("ENSO/Graficos_anomalias/anomalias.tiff",sep=""), width = 9, height = 7,units = 'in',res=200)
      
      #jpeg(paste("ENSO/Graficos/",station[j],"_b.jpeg",sep=""),width = 1000, height = 700)
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
      grid.text(paste(""), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(g3, vp = vplayout(2,1))
      print(g31, vp = vplayout(3,1))
      print(g32, vp = vplayout(4,1))
      dev.off()
      
    
    
      confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_anomalias"),"Ubicación archivos")
    
      
  }else{object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  object2<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  promedios_tmax=aggregate(object[-3:-1],list(Mes=object$month),mean2)
  promedios_tmin=aggregate(object1[-3:-1],list(Mes=object1$month),mean2)
  
  #maximos=aggregate(object[-3:-1],list(Mes=object$month),max)
  x=aggregate(object[-3:-1],list(Año=object1$year, Mes=object$month),mean2)
  tx=cbind(x[order(x$Año),],datos[which(datos[,1]==min(object$year))[1]:max(which(datos[,1]==max(object$year))),])
  
  x1=aggregate(object1[-3:-1],list(Año=object1$year,Mes=object1$month),mean2)
  tx1=cbind(x1[order(x1$Año),],datos[which(datos[,1]==min(object1$year))[1]:max(which(datos[,1]==max(object1$year))),])
  
  x2=aggregate(object2[-3:-1],list(Año=object2$year,Mes=object2$month),sum2)
  tx2=cbind(x2[order(x2$Año),],datos[which(datos[,1]==min(object2$year))[1]:max(which(datos[,1]==max(object2$year))),])
  
  promedios_precip=aggregate(x2[-2:-1],list(Mes=x2$Mes),mean2)
  desv_precip=aggregate(x2[-2:-1],list(Mes=x2$Mes),sd)
  
  
  
  station=names(object[-3:-1])
  dir.create("ENSO",showWarnings=F)
  dir.create("ENSO/Graficos_anomalias",showWarnings=F)
  
  an=matrix(0,nrow(tx),ncol(object[-3:-1]))
  an1=matrix(0,nrow(tx),ncol(object[-3:-1]))
  an2=matrix(0,nrow(tx2),ncol(object[-3:-1]))
  
  for(i in 1:(ncol(object)-3)){  
    an[,i]=tx[,i+2]-promedios_tmax[,i+1]
    
    an1[,i]=tx1[,i+2]-promedios_tmin[,i+1]
    
    an2[,i]=(tx2[,i+2]-promedios_precip[,i+1])/as.numeric(unlist(rep(desv_precip[i+1],max(tx2$year)-min(tx2$year)+1)))
    
  }
  
  an=as.data.frame(cbind(an,"year"=tx$year))
  an1=as.data.frame(cbind(an1,"year"=tx1$year))
  an2=as.data.frame(cbind(an2,"year"=tx2$year))
  
  
  assign("an", an, envir=.GlobalEnv)
  assign("an1", an1, envir=.GlobalEnv)
  assign("an2", an2, envir=.GlobalEnv)
  assign("tx", tx, envir=.GlobalEnv)
  assign("tx1", tx1, envir=.GlobalEnv)
  assign("tx2", tx2, envir=.GlobalEnv)
  assign("station", station, envir=.GlobalEnv)
  
  j=1
  #   
  assign("j", j, envir=.GlobalEnv)
  plots <- function(j,...){
    j=j
    assign("j",j, envir=.GlobalEnv)
    
    g3=ggplot(tx, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(tx),by=36), labels=seq(min(tx$year),max(tx$year),3))+xlab("")+ 
      theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
      scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an[,j],colour="Anomalías obsv. tmáx"),data=an,size=0.5)+ylab("ONI")+
      scale_color_manual(values=c("black"))
    
    g31=ggplot(tx1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(tx),by=36), labels=seq(min(tx$year),max(tx$year),3))+xlab("")+ 
      theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
      scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an1[,j],colour="Anomalías obsv. tmín"),data=an1,size=0.5)+ylab("ONI")+
      scale_color_manual(values=c("black"))
    
    g32=ggplot(tx2, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(tx2),by=36), labels=seq(min(tx$year),max(tx$year),3))+xlab("")+ 
      theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
      scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an2[,j],colour="Anomalías obsv. precip"),data=an2,size=0.5)+ylab("ONI")+
      scale_color_manual(values=c("black"))
    
    tiff(paste("ENSO/Graficos_anomalias/",station[j],".tiff",sep=""), width = 9, height = 7,units = 'in',res=200)
    
    #jpeg(paste("ENSO/Graficos/",station[j],"_b.jpeg",sep=""),width = 1000, height = 700)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
    grid.text(paste(station[j]), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(g3, vp = vplayout(2,1))
    print(g31, vp = vplayout(3,1))
    print(g32, vp = vplayout(4,1))
    dev.off()
    
  }
  
  lapply(1:length(station), function(x){plots(x)})  
  # assign("plots", plots, envir=.GlobalEnv)
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_anomalias"),"Ubicación archivos")
  }
}

graficos_enso_plot=function(){
   
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  datos=read.csv("Enso_ONI.csv",header=T, sep = ",",  dec=".")
  
  if(exists("data_m_e")){
  
  data_m_e2=cbind(data_m_e,datos[which(datos[,1]==min(data_m_e$year))[1]:max(which(datos[,1]==max(data_m_e$year))),])
  
   
  dir.create("ENSO",showWarnings=F)
  dir.create("ENSO/Graficos_plot",showWarnings=F)
  
  
  assign("data_m_e2", data_m_e2, envir=.GlobalEnv)
 
    
    Fechas=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") # Crea una tabla con los nombres de los meses
    
    g2=ggplot(data_m_e2,aes(y=precip,x=month))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Precipitación")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(0,mean2(data_m_e2$precip)+100)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    
    g21=ggplot(data_m_e2,aes(y=tmax,x=month))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Temperatura Máxima")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(mean2(data_m_e2$tmax)-2,mean2(data_m_e2$tmax)+2)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    g22=ggplot(data_m_e2,aes(y=tmin,x=month))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Temperatura Mínima")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(mean2(data_m_e2$tmin)-1,mean2(data_m_e2$tmin)+1)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    tiff(paste("ENSO/Graficos_plot/plot.tiff",sep=""), width = 5, height = 10,units = 'in',res=200)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
    grid.text(paste(""), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(g2, vp = vplayout(2,1))
    print(g21, vp = vplayout(3,1))
    print(g22, vp = vplayout(4,1))
    dev.off()
    
    confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_plot"),"Ubicación archivos")
  
  }else{
  
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  object2<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  
  x=aggregate(object[-3:-1],list(Año=object1$year, Mes=object$month),mean2)
  tx=cbind(x[order(x$Año),],datos[which(datos[,1]==min(object$year))[1]:max(which(datos[,1]==max(object$year))),])
  
  x1=aggregate(object1[-3:-1],list(Año=object1$year,Mes=object1$month),mean2)
  tx1=cbind(x1[order(x1$Año),],datos[which(datos[,1]==min(object1$year))[1]:max(which(datos[,1]==max(object1$year))),])
  
  x2=aggregate(object2[-3:-1],list(Año=object2$year,Mes=object2$month),sum2)
  tx2=cbind(x2[order(x2$Año),],datos[which(datos[,1]==min(object2$year))[1]:max(which(datos[,1]==max(object2$year))),])
  
  station=names(object[-3:-1])
  dir.create("ENSO",showWarnings=F)
  dir.create("ENSO/Graficos_plot",showWarnings=F)
  
  
  assign("tx", tx, envir=.GlobalEnv)
  assign("tx1", tx1, envir=.GlobalEnv)
  assign("tx2", tx2, envir=.GlobalEnv)
  assign("station", station, envir=.GlobalEnv)
  
  i=1
  
  assign("i", i, envir=.GlobalEnv)
  plots1 <- function(i,...){
    i=i
    assign("i",i, envir=.GlobalEnv)
    
    Fechas=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") # Crea una tabla con los nombres de los meses
    
    g2=ggplot(tx2,aes(y=tx2[,i+2],x=Mes))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Precipitación")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(0,mean2(tx2[,i+2])+100)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    
    g21=ggplot(tx,aes(y=tx[,i+2],x=Mes))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Temperatura Máxima")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(mean2(tx[,i+2])-2,mean2(tx[,i+2])+2)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    g22=ggplot(tx1,aes(y=tx1[,i+2],x=Mes))+stat_smooth(method=loess,fullrange=T,aes(colour=ENSO),size=1,level=0)+xlab("")+ylab("Temperatura Mínima")+scale_x_continuous(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylim(mean2(tx1[,i+2])-1,mean2(tx1[,i+2])+1)+
      # scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"),name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+
      scale_colour_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ",breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))#+facet_wrap(~ENSO)
    
    tiff(paste("ENSO/Graficos_plot/",station[i],".tiff",sep=""), width = 5, height = 10,units = 'in',res=200)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
    grid.text(paste(station[i]), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(g2, vp = vplayout(2,1))
    print(g21, vp = vplayout(3,1))
    print(g22, vp = vplayout(4,1))
    dev.off()
    
  }
  
  lapply(1:length(station), function(x){plots1(x)})  
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_plot"),"Ubicación archivos")
  }
}

graficos_enso_boxplot=function(){
  
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  datos=read.csv("Enso_ONI.csv",header=T, sep = ",",  dec=".")
  
  if(exists("data_m_e")){
    
    data_m_e2=cbind(data_m_e,datos[which(datos[,1]==min(data_m_e$year))[1]:max(which(datos[,1]==max(data_m_e$year))),])
    
    
    dir.create("ENSO",showWarnings=F)
    dir.create("ENSO/Graficos_boxplot",showWarnings=F)
    
    
    assign("data_m_e2", data_m_e2, envir=.GlobalEnv)
    
    
    Fechas=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") # Crea una tabla con los nombres de los meses
    
    g21a=ggplot(data_m_e2,aes(y=precip,x=factor(month)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Precipitación")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
    g21b=ggplot(data_m_e2,aes(y=tmax,x=factor(month)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Temperatura Máxima")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
    g21c=ggplot(data_m_e2,aes(y=tmin,x=factor(month)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Temperatura Mínima")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
   
    tiff(paste("ENSO/Graficos_boxplot/boxplot.tiff",sep=""), width = 8, height = 9,units = 'in',res=200)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
    grid.text(paste(""), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(g21a, vp = vplayout(2,1))
    print(g21b, vp = vplayout(3,1))
    print(g21c, vp = vplayout(4,1))
    dev.off()
    
    confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_plot"),"Ubicación archivos")
  }else{
  
  object<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
  object1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
  object2<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  
  
  x=aggregate(object[-3:-1],list(Año=object1$year, Mes=object$month),mean2)
  tx=cbind(x[order(x$Año),],datos[which(datos[,1]==min(object$year))[1]:max(which(datos[,1]==max(object$year))),])
  
  x1=aggregate(object1[-3:-1],list(Año=object1$year,Mes=object1$month),mean2)
  tx1=cbind(x1[order(x1$Año),],datos[which(datos[,1]==min(object1$year))[1]:max(which(datos[,1]==max(object1$year))),])
  
  x2=aggregate(object2[-3:-1],list(Año=object2$year,Mes=object2$month),sum2)
  tx2=cbind(x2[order(x2$Año),],datos[which(datos[,1]==min(object2$year))[1]:max(which(datos[,1]==max(object2$year))),])
  
  station=names(object[-3:-1])
  dir.create("ENSO",showWarnings=F)
  dir.create("ENSO/Graficos_boxplot",showWarnings=F)
  
  
  assign("tx", tx, envir=.GlobalEnv)
  assign("tx1", tx1, envir=.GlobalEnv)
  assign("tx2", tx2, envir=.GlobalEnv)
  assign("station", station, envir=.GlobalEnv)
  
  i=1
  
  assign("i", i, envir=.GlobalEnv)
  plots2 <- function(i,...){
    i=i
    assign("i",i, envir=.GlobalEnv)
    
    Fechas=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic") # Crea una tabla con los nombres de los meses
    
    g2a=ggplot(tx2,aes(y=tx2[,i+2],x=factor(Mes)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Precipitación")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
    g21a=ggplot(tx,aes(y=tx[,i+2],x=factor(Mes)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Temperatura Máxima")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
    
    
    g22a=ggplot(tx1,aes(y=tx1[,i+2],x=factor(Mes)))+geom_boxplot(aes(fill=factor(ENSO1)),outlier.size=0.8)+scale_x_discrete(breaks=1:12, labels=Fechas)+theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))+ylab("Temperatura Mínima")+
      scale_fill_manual(values=c("firebrick3","chartreuse3","dodgerblue2"), name=" ",breaks=c("1","2","3"),labels=c("Niño","Normal","Niña"))+xlab("")
    
    tiff(paste("ENSO/Graficos_boxplot/",station[i],".tiff",sep=""), width = 8, height = 9,units = 'in',res=200)
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4,4,4), "null"))))
    grid.text(paste(station[i]), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(g2a, vp = vplayout(2,1))
    print(g21a, vp = vplayout(3,1))
    print(g22a, vp = vplayout(4,1))
    dev.off()
    
  }
  
  lapply(1:length(station), function(x){plots2(x)})  
  confirmDialog(paste("Los resultados gráficos se guardarán en",getwd(),"ENSO/Graficos_boxplot"),"Ubicación archivos")
  }
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
  addParagraph(info,paste("Para la identificación de datos atípicos se utilizaron",svalue(criterio),"desviaciones estándar y se estableció un rango de valores permitidos ",svalue(minim),"<",svalue(variable),"<",svalue(maxim),". Nota: Los datos identificados en esta sección son reemplazados por NA's, a excepción de los identificados como datos atípicos y aquellas temperaturas con variación mayor a 10ºC. La última columna de las siguientes tablas indican el % total de datos faltantes que serán llenados en la sección Datos faltantes. Se recomienda que este % no supere el 20% en cada una de las estaciones."))
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



#---------------------------------------------------------------------
####-------------Funciones para generacion pronósticos------------####
#---------------------------------------------------------------------

cargar_diarios=function(){
  data_d=read.csv2(gfile("Seleccione un archivo"),header=T)
  assign("data_d",data_d,.GlobalEnv)
}

cargar_mensual=function(){
  data_m=read.csv2(gfile("Seleccione un archivo"),header=T)
  assign("data_m",data_m,.GlobalEnv)
}

cargar_prob=function(){
  probabilidades=read.csv2(gfile("Seleccione un archivo"),header=T)
  assign("probabilidades",probabilidades,.GlobalEnv)
}

pronosticos=function(){
   
  if(ncol(probabilidades)>6) stop("El número de meses a pronosticar es mayor a 6, por favor verifique el archivo con las probabilidades")
  
  
  confirmDialog(paste("Los resultados se guardarán en",getwd(),"/Pronosticos"),"Ubicación archivos")
  
  dir.create("Pronosticos",showWarnings=F)
  Fechas=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre") # Crea una tabla con los nombres de los meses
  
  if(exists("data_d") ){
    
    attach(data_d)
    
    if(svalue(val_p)=="tmax"){data=aggregate(tmax,list(data_d$year,data_d$month),mean2)}
    if(svalue(val_p)=="tmin"){data=aggregate(tmin,list(data_d$year,data_d$month),mean2)}
    if(svalue(val_p)=="precip"){data=aggregate(precip,list(data_d$year,data_d$month),sum2)}
    if(svalue(val_p)=="srad"){data=aggregate(srad,list(data_d$year,data_d$month),sum2)}
    
    names(data)=c("year","month","value")
  }
  
  if(svalue(est_p)!=""){
    
    tmax1<- read.csv("Datos_faltantes/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    tmin1<- read.csv("Datos_faltantes/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    precip1<- read.csv("Datos_faltantes/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    
    data_d=cbind(tmax1[,2:3],"tmax"=tmax1[,as.numeric(svalue(est_p))+3],"tmin"=tmin1[,as.numeric(svalue(est_p))+3],"precip"=precip1[,as.numeric(svalue(est_p))+3])
    names(data_d)=c("month","year","tmax","tmin","precip")
    
    if(svalue(val_p)=="tmax"){data=aggregate(data_d$tmax,list(year=data_d$year,month=data_d$month),mean2)}
    if(svalue(val_p)=="tmin"){data=aggregate(data_d$tmin,list(year=data_d$year,month=data_d$month),mean2)}
    if(svalue(val_p)=="precip"){data=aggregate(data_d$precip,list(year=data_d$year,month=data_d$month),sum2)}
  }
  
  
  if(exists("data_m")){
    
    attach(data_m)
    
       data=data_m[,c(2,1,which(names(data_m)==svalue(val_p)))]
    
  }
  
   
  Enero1=data[data$month==1,-2]
  Febrero1=data[data$month==2,-2]
  Marzo1=data[data$month==3,-2]
  Abril1=data[data$month==4,-2]
  Mayo1=data[data$month==5,-2]
  Junio1=data[data$month==6,-2]
  Julio1=data[data$month==7,-2]
  Agosto1=data[data$month==8,-2]
  Septiembre1=data[data$month==9,-2]
  Octubre1=data[data$month==10,-2]
  Noviembre1=data[data$month==11,-2]
  Diciembre1=data[data$month==12,-2]
  
   
  Enero1=Enero1[order(Enero1[,2]),]
  Febrero1=Febrero1[order(Febrero1[,2]),]
  Marzo1=Marzo1[order(Marzo1[,2]),]
  Abril1=Abril1[order(Abril1[,2]),]
  Mayo1=Mayo1[order(Mayo1[,2]),]
  Junio1=Junio1[order(Junio1[,2]),]
  Julio1=Julio1[order(Julio1[,2]),]
  Agosto1=Agosto1[order(Agosto1[,2]),]
  Septiembre1=Septiembre1[order(Septiembre1[,2]),]
  Octubre1=Octubre1[order(Octubre1[,2]),]
  Noviembre1=Noviembre1[order(Noviembre1[,2]),]
  Diciembre1=Diciembre1[order(Diciembre1[,2]),]
  
   
  var_org=cbind(Enero1[,2],Febrero1[,2],Marzo1[,2],Abril1[,2],Mayo1[,2],Junio1[,2],Julio1[,2],Agosto1[,2],Septiembre1[,2],Octubre1[,2],Noviembre1[,2],Diciembre1[,2])
  colnames(var_org)=Fechas
   
  Años_org=cbind(Enero1[,1],Febrero1[,1],Marzo1[,1],Abril1[,1],Mayo1[,1],Junio1[,1],Julio1[,1],Agosto1[,1],Septiembre1[,1],Octubre1[,1],Noviembre1[,1],Diciembre1[,1])
  colnames(Años_org)=Fechas
   
  percentiles=rbind(apply(var_org,2,FUN=quantile,0.3333),apply(var_org,2,FUN=quantile,0.6666),apply(var_org,2,FUN=quantile,0.9999))
  
  
  
  if(svalue(val_p)=="precip"){ nomb_prob=c("Déficit","Normal","Exceso")
  }else nomb_prob=c("Disminución","Normal","Aumento")
  
  rownames(percentiles)=nomb_prob
  colnames(percentiles)=Fechas
  
  #Primera salida
  write.csv(percentiles,file="Pronosticos/percentiles.csv")
  
  probabilidades=as.data.frame(probabilidades)
  
  
  Años_org2=Años_org[,match(names(probabilidades),colnames(Años_org))]
  var_org2=var_org[,match(names(probabilidades),colnames(var_org))]
  
   
  
  o=melt(probabilidades)
  o1=cbind(o,"cond"=rep(nomb_prob,ncol(probabilidades)))
  
  
  p=ggplot(o1,aes(x=variable,y=value))+geom_bar(stat="identity", position="dodge",aes(fill=cond,alpha=0.7))+
    scale_fill_manual(values=c( "firebrick3","dodgerblue2","chartreuse3"),name=paste(svalue(val_p)), breaks=nomb_prob,labels=nomb_prob)+
    ylab("Probabilidad de pronóstico (%)")+xlab("")+scale_alpha(guide = 'none')+ylim(0,100)
  
  ggsave("Pronosticos/probabilidades.tiff",p,width=7,height=3)
  
   insumo=function(data,prob,añoshistorico){
    matrizcombinaciones=0
    vectorprobabilidades=prob
    datas=0
    for(i in 1:length(data)){
      r=sample(prob,1,prob=prob)
      if(r==vectorprobabilidades[1]){
        datas=which(data<quantile(data,0.3333))
        
        matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
      }
      
      if(r==vectorprobabilidades[2]){
        datas=which(data>=quantile(data,0.3333) & data<quantile(data,0.6666))
        matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
        
      }
      
      if(r==vectorprobabilidades[3]){
        datas=which(data>=quantile(data,0.6666))
        matrizcombinaciones[i]=añoshistorico[sample(datas,1)]
        
      }
      
      
    }
    return(matrizcombinaciones)
    
    
  }
  
   masprobable=matrix(0,nrow=200,ncol=dim(probabilidades)[2])
  
  for(j in 1:200){
    esc1=sapply(1:dim(probabilidades)[2], function(i) insumo(var_org2[,i],probabilidades[,i],Años_org2[,i]))
    masprobable[j,]=sapply(1:dim(probabilidades)[2],function(j) as.numeric(rownames(cbind(which(table(esc1[,j])==max(table(esc1[,j]))))))[1])
    
  }
  
  
  
  valores=function(mes,var,Años){
    datos=0
    for(i in 1:length(mes))
      datos[i]=var[which(mes[i]==Años)]
    return(datos)
  }
  
  masprobable2=apply(masprobable,2,function(masprobable)sample(masprobable,12,rep=F))
  
  todo=sapply(1:dim(probabilidades)[2], function(i) valores(masprobable2[,i],var_org2[,i],Años_org2[,i]))
  todo2=as.data.frame(rbind(masprobable2,c("Datos análogos",rep("",dim(probabilidades)[2]-1)),todo))
   
  resumen=function(x) rbind(min2(x),max(x))
  resumen2=apply(todo,2,resumen)
  medias=apply(todo,2,mean2)
  
  dif=t(t(todo)-medias)
  
  valores2=function(masprobable2,todo,resumen2,dif){
    datos=0
        for(i in 1:2){
    pos<-which(todo==resumen2[i])
    n=length(pos)
    datos[i]=masprobable2[pos[sample(n,1)]]
        }
    
    pos2=which(abs(dif)==min2(abs(dif)))
    n2=length(pos2)
    datos2=masprobable2[pos2[sample(n2,1)]]
    
    datost=c(datos[1],datos2,datos[2])
    
    return(datost)
  }
  
  todo3=sapply(1:dim(probabilidades)[2], function(i) valores2(masprobable2[,i],todo[,i],resumen2[,i],dif[,i]))
 
  resumen3=rbind(resumen2[1,],round(medias,2),resumen2[2,])
  row.names(resumen3)=c("Mín","Promedio","Máx")
  
  resumenf=rbind(resumen3,c("Años",rep("",dim(probabilidades)[2]-1)),todo3)
  colnames(resumenf)=names(probabilidades)
   
  ###Segunda salida
  colnames(todo2)=names(probabilidades)
  write.csv(todo2,file="Pronosticos/añosanalogos.csv")
  write.csv(resumenf,file="Pronosticos/resumen_añosanalogos.csv")
  
  
   a=masprobable2
  a=as.data.frame(a)
  names(a)=names(probabilidades)
   if(any(names(a)=="Enero")){ escenario_Ene=list()}
  if(any(names(a)=="Febrero")){ escenario_Feb=list()}
  if(any(names(a)=="Marzo")){ escenario_Mar=list()}
  if(any(names(a)=="Abril")){ escenario_Abr=list()}
  if(any(names(a)=="Mayo")){ escenario_May=list()}
  if(any(names(a)=="Junio")){ escenario_Jun=list()}
  if(any(names(a)=="Julio")){ escenario_Jul=list()}
  if(any(names(a)=="Agosto")){ escenario_Ago=list()}
  if(any(names(a)=="Septiembre")){ escenario_Sep=list()}
  if(any(names(a)=="Octubre")){ escenario_Oct=list()}
  if(any(names(a)=="Noviembre")){ escenario_Nov=list()}
  if(any(names(a)=="Diciembre")){ escenario_Dic=list()}
  
  esc_consolidado=list()
 
  num_esc=as.numeric(svalue(num_esc))
  for(w in 1:num_esc){
    esc_consolidado[[w]]=cbind(
      if(any(names(a)=="Enero")){ escenario_Ene[[w]]=sample(a$Enero,1)},
      if(any(names(a)=="Febrero")){ escenario_Feb[[w]]=sample(a$Febrero,1)},
      if(any(names(a)=="Marzo")){ escenario_Mar[[w]]=sample(a$Marzo,1)},
      
      if(any(names(a)=="Abril")){ escenario_Abr[[w]]=sample(a$Abril,1)},
      if(any(names(a)=="Mayo")){escenario_May[[w]]=sample(a$Mayo,1)},
      if(any(names(a)=="Junio")){escenario_Jun[[w]]=sample(a$Junio,1)},
      if(any(names(a)=="Julio")){escenario_Jul[[w]]=sample(a$Julio,1)},
      if(any(names(a)=="Agosto")){ escenario_Ago[[w]]=sample(a$Agosto,1)},
      if(any(names(a)=="Septiembre")){escenario_Sep[[w]]=sample(a$Septiembre,1)},
      if(any(names(a)=="Octubre")){escenario_Oct[[w]]=sample(a$Octubre,1)},
      if(any(names(a)=="Noviembre")){escenario_Nov[[w]]=sample(a$Noviembre,1)},
      if(any(names(a)=="Diciembre")){escenario_Dic[[w]]=sample(a$Diciembre,1)})
    
    
    #esc_consolidado[[w]]=cbind(escenario_Jul[[w]],escenario_Ago[[w]],escenario_Sep[[w]],escenario_Oct[[w]],escenario_Nov[[w]],escenario_Dic[[w]])
    
  }
  
  escenarios_final=esc_consolidado[[1]]
  for(w in 2:num_esc){
    escenarios_final=rbind(escenarios_final,esc_consolidado[[w]])
  }
  
  escenarios_final=as.data.frame(escenarios_final)
  names(escenarios_final)=names(probabilidades)
  
  if(exists("data_d")){
    esc_final_diarios=list()
    
    if(any(names(a)=="Enero")){ esc_diario_Ene=list()}
    if(any(names(a)=="Febrero")){ esc_diario_Feb=list()}
    if(any(names(a)=="Marzo")){ esc_diario_Mar=list()}
    if(any(names(a)=="Abril")){ esc_diario_Abr=list()}
    if(any(names(a)=="Mayo")){ esc_diario_May=list()}
    if(any(names(a)=="Junio")){ esc_diario_Jun=list()}
    if(any(names(a)=="Julio")){ esc_diario_Jul=list()}
    if(any(names(a)=="Agosto")){ esc_diario_Ago=list()}
    if(any(names(a)=="Septiembre")){ esc_diario_Sep=list()}
    if(any(names(a)=="Octubre")){ esc_diario_Oct=list()}
    if(any(names(a)=="Noviembre")){ esc_diario_Nov=list()}
    if(any(names(a)=="Diciembre")){ esc_diario_Dic=list()}
    
    for (n in 1:nrow(escenarios_final)){
      
      esc_final_diarios[[n]]=rbind(
        if(any(names(a)=="Enero")){esc_diario_Ene[[n]]=data_d[data_d$month=="1"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Enero")],]},
        if(any(names(a)=="Febrero")){esc_diario_Feb[[n]]=data_d[data_d$month=="2"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Febrero")],]},
        if(any(names(a)=="Marzo")){esc_diario_Mar[[n]]=data_d[data_d$month=="1"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Marzo")],]},
        if(any(names(a)=="Abril")){esc_diario_Abr[[n]]=data_d[data_d$month=="4"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Abril")],]},
        if(any(names(a)=="Mayo")){esc_diario_May[[n]]=data_d[data_d$month=="5"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Mayo")],]},
        if(any(names(a)=="Junio")){esc_diario_Jun[[n]]=data_d[data_d$month=="6"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Junio")],]},
        if(any(names(a)=="Julio")){esc_diario_Jul[[n]]=data_d[data_d$month=="7"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Julio")],]},
        if(any(names(a)=="Agosto")){esc_diario_Ago[[n]]=data_d[data_d$month=="8"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Agosto")],]},
        if(any(names(a)=="Septiembre")){esc_diario_Sep[[n]]=data_d[data_d$month=="9"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Septiembre")],]},
        if(any(names(a)=="Octubre")){esc_diario_Oct[[n]]=data_d[data_d$month=="10"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Octubre")],]},
        if(any(names(a)=="Noviembre")){esc_diario_Nov[[n]]=data_d[data_d$month=="11"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Noviembre")],]},
        if(any(names(a)=="Diciembre")){ esc_diario_Dic[[n]]=data_d[data_d$month=="12"&data_d$year==escenarios_final[n,which(names(escenarios_final)=="Diciembre")],]})
      
      
    }
    
    
    
    ########################################
    ###Exportar escenarios diarios a excel###
    #######################################
    dir.create("Pronosticos/Escenarios",showWarnings=F)
    for(k in 1:num_esc){
      
      j=paste("Pronosticos/Escenarios/escenario_",k,".csv",sep="") 
      
      write.csv(esc_final_diarios[[k]],j,row.names=F)
    }
    
    
    ##########Graficos#################
    
    esc_final_mensual_Lluvia=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_Lluvia[z,]=t(aggregate(esc_final_diarios[[z]]$precip,list(Mes=esc_final_diarios[[z]]$month),sum2))[-1,]
    }
    esc_final_mensual_Lluvia=as.data.frame(esc_final_mensual_Lluvia)
    
    colnames(esc_final_mensual_Lluvia)=names(probabilidades)
    
    promedio_escenarios=apply(esc_final_mensual_Lluvia,2,mean2)
    
    Multianual_Libertad1=aggregate(data_d$precip,list(Mes=data_d$month,Año=data_d$year),sum2) #Se carga la precipitación mensual multianual 
    Multianual_Libertad=aggregate(Multianual_Libertad1[,3],list(Mes=Multianual_Libertad1$Mes),mean2)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/precip_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    #jpeg("Pronosticos/precip_pronos.jpg", width = 1000, height = 700)
    
    a=barplot(as.numeric(Multianual_Libertad[,2]),names.arg=names(probabilidades),col="Slate Gray 2",ylab="Precipitacion (mm)",,cex.lab=1,main="Pronóstico precipitación",cex.main=0.8,ylim=c(0,max(Multianual_Libertad[,2])+200))
    
    for(m in 1:nrow(escenarios_final)){
      lines(a,as.numeric(esc_final_mensual_Lluvia[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios),ty="l",col="red3",lwd=2)
    
    legend(5,max(Multianual_Libertad[,2])+150,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    
    dev.off()
    
     esc_final_mensual_tmax=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmax[z,]=t(aggregate(esc_final_diarios[[z]]$tmax,list(Mes=esc_final_diarios[[z]]$month),mean2))[-1,]
    }
    esc_final_mensual_Tmax=as.data.frame(esc_final_mensual_tmax)
    
    colnames(esc_final_mensual_Tmax)=names(probabilidades)
    
    promedio_escenarios_Tmax=apply(esc_final_mensual_Tmax,2,mean2)
    
    
    Tmax_Multianual_Libertad1=aggregate(data_d$tmax,list(Mes=data_d$month,Año=data_d$year),mean2) #Se carga la precipitación mensual multianual 
    Tmax_Multianual_Libertad=aggregate(Tmax_Multianual_Libertad1[,3],list(Mes=Tmax_Multianual_Libertad1$Mes),mean2)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmax_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(Tmax_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Temperatura Máxima (°C)",,cex.lab=1,,main="Pronóstico Temperatura Máxima ",cex.main=0.8,ylim=c(min2(data_d$tmax)+2,max(data_d$tmax)-1))
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_Tmax[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_Tmax),ty="l",col="red3",lwd=2)
    legend(5,max(data_d$tmax)-1,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    dev.off()
   
    
    esc_final_mensual_tmin=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmin[z,]=t(aggregate(esc_final_diarios[[z]]$tmin,list(Mes=esc_final_diarios[[z]]$month),mean2))[-1,]
    }
    esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_tmin)
    
    colnames(esc_final_mensual_Tmin)=names(probabilidades)
    
      
    promedio_escenarios_Tmin=apply(esc_final_mensual_Tmin,2,mean2)
  
    Tmin_Multianual_Libertad1=aggregate(data_d$tmin,list(Mes=data_d$month,Año=data_d$year),mean2) #Se carga la precipitación mensual multianual 
    Tmin_Multianual_Libertad=aggregate(Tmin_Multianual_Libertad1[,3],list(Mes=Tmin_Multianual_Libertad1$Mes),mean2)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmin_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    r=barplot(as.numeric(Tmin_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min2(data_d$tmin)+2,max(data_d$tmin)+3),xpd = FALSE,col="cornsilk1",ylab="Temperatura Mínima(°C)",cex.lab=1,main="Pronóstico Temperatura Mínima ",cex.main=0.8)
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_Tmin[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_Tmin),ty="l",col="red3",lwd=2)
    legend(5,max(data_d$tmin),c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    dev.off()
    
    
    if(exists("srad")){
      
      esc_final_mensual_srad=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_srad[z,]=t(aggregate(esc_final_diarios[[z]]$srad,list(Mes=esc_final_diarios[[z]]$month),sum2))[-1,]
    }
    esc_final_mensual_srad=as.data.frame(esc_final_mensual_srad)
    
    colnames(esc_final_mensual_srad)=names(probabilidades)
    
    promedio_escenarios_srad=apply(esc_final_mensual_srad,2,mean2)
    
    
    srad_Multianual_Libertad1=aggregate(data_d$srad,list(Mes=data_d$month,Año=data_d$year),sum2) #Se carga la precipitación mensual multianual 
    srad_Multianual_Libertad=aggregate(srad_Multianual_Libertad1[,3],list(Mes=srad_Multianual_Libertad1$Mes),mean2)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/srad_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(srad_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Radiación Solar (MJ*m2/mes)",,cex.lab=1,,main="Pronóstico Radiación Solar ",cex.main=0.8,ylim=c(min2(as.numeric(srad_Multianual_Libertad[,2]))-30,max(srad_Multianual_Libertad[,2])+80))
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_srad[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_srad),ty="l",col="red3",lwd=2)
    legend(5,max(srad_Multianual_Libertad[,2])+50,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
      
    box()
    dev.off()
    }
    
    
  }
  
  
  
 
  
  if(exists("data_m")){
    esc_final_mensual=list()
    
    if(any(names(a)=="Enero")){ esc_mensual_Ene=list()}
    if(any(names(a)=="Febrero")){ esc_mensual_Feb=list()}
    if(any(names(a)=="Marzo")){ esc_mensual_Mar=list()}
    if(any(names(a)=="Abril")){ esc_mensual_Abr=list()}
    if(any(names(a)=="Mayo")){ esc_mensual_May=list()}
    if(any(names(a)=="Junio")){ esc_mensual_Jun=list()}
    if(any(names(a)=="Julio")){ esc_mensual_Jul=list()}
    if(any(names(a)=="Agosto")){ esc_mensual_Ago=list()}
    if(any(names(a)=="Septiembre")){ esc_mensual_Sep=list()}
    if(any(names(a)=="Octubre")){ esc_mensual_Oct=list()}
    if(any(names(a)=="Noviembre")){ esc_mensual_Nov=list()}
    if(any(names(a)=="Diciembre")){ esc_mensual_Dic=list()}
    
    for (n in 1:nrow(escenarios_final)){
      
      esc_final_mensual[[n]]=rbind(
        if(any(names(a)=="Enero")){esc_mensual_Ene[[n]]=data_m[data_m$month=="1"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Enero")],]},
        if(any(names(a)=="Febrero")){esc_mensual_Feb[[n]]=data_m[data_m$month=="2"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Febrero")],]},
        if(any(names(a)=="Marzo")){esc_mensual_Mar[[n]]=data_m[data_m$month=="1"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Marzo")],]},
        if(any(names(a)=="Abril")){esc_mensual_Abr[[n]]=data_m[data_m$month=="4"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Abril")],]},
        if(any(names(a)=="Mayo")){esc_mensual_May[[n]]=data_m[data_m$month=="5"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Mayo")],]},
        if(any(names(a)=="Junio")){esc_mensual_Jun[[n]]=data_m[data_m$month=="6"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Junio")],]},
        if(any(names(a)=="Julio")){esc_mensual_Jul[[n]]=data_m[data_m$month=="7"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Julio")],]},
        if(any(names(a)=="Agosto")){esc_mensual_Ago[[n]]=data_m[data_m$month=="8"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Agosto")],]},
        if(any(names(a)=="Septiembre")){esc_mensual_Sep[[n]]=data_m[data_m$month=="9"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Septiembre")],]},
        if(any(names(a)=="Octubre")){esc_mensual_Oct[[n]]=data_m[data_m$month=="10"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Octubre")],]},
        if(any(names(a)=="Noviembre")){esc_mensual_Nov[[n]]=data_m[data_m$month=="11"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Noviembre")],]},
        if(any(names(a)=="Diciembre")){ esc_mensual_Dic[[n]]=data_m[data_m$month=="12"&data_m$year==escenarios_final[n,which(names(escenarios_final)=="Diciembre")],]})
      
      
    }
    
    
    
    ########################################
    ###Exportar escenarios mensual a excel###
    #######################################
    dir.create("Pronosticos/Escenarios",showWarnings=F)
    for(k in 1:num_esc){
      
      j=paste("Pronosticos/Escenarios/escenario_",k,".csv",sep="") 
      
      write.csv(esc_final_mensual[[k]],j,row.names=F)
    }
    
      
    esc_final_mensual_Lluvia=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_Lluvia[z,]=rbind(esc_final_mensual[[z]]$precip)
    }
    colnames(esc_final_mensual_Lluvia)=names(probabilidades)
    
      promedio_escenarios=apply(esc_final_mensual_Lluvia,2,mean2)
     
    Multianual_Libertad=aggregate(data_m$precip,list(Mes=data_m$month),mean2)[match(names(probabilidades),colnames(Años_org)),]#Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/precip_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    a=barplot(as.numeric(Multianual_Libertad[,2]),names.arg=names(probabilidades),col="Slate Gray 2",ylab="Precipitacion (mm)",,cex.lab=1,main="Pronóstico precipitación",cex.main=0.8,ylim=c(0,max(Multianual_Libertad[,2])+200))
    
    for(m in 1:nrow(escenarios_final)){
      lines(a,as.numeric(esc_final_mensual_Lluvia[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios),ty="l",col="red3",lwd=2)
    
    legend(5,max(Multianual_Libertad[,2])+150,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    
    dev.off()
     
    esc_final_mensual_tmax=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmax[z,]=rbind(esc_final_mensual[[z]]$tmax)  }
    esc_final_mensual_Tmax=as.data.frame(esc_final_mensual_tmax)
    
    colnames(esc_final_mensual_Tmax)=names(probabilidades)
    
      
    promedio_escenarios_Tmax=apply(esc_final_mensual_Tmax,2,mean2)
    
    
    Tmax_Multianual_Libertad=aggregate(data_m$tmax,list(Mes=data_m$month),mean2)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmax_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    r=barplot(as.numeric(Tmax_Multianual_Libertad[,2]),names.arg=names(probabilidades),xpd = FALSE,col="cornsilk1",ylab="Temperatura Máxima (°C)",,cex.lab=1,,main="Pronóstico Temperatura Máxima ",cex.main=0.8,ylim=c(min2(data_m$tmax)-2,max(data_m$tmax)+1))
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_Tmax[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_Tmax),ty="l",col="red3",lwd=2)
    legend(5,max(data_m$tmax)-1,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    dev.off()
    
      
    esc_final_mensual_tmin=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_tmin[z,]=rbind(esc_final_mensual[[z]]$tmin)
    }
    esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_tmin)
    
    colnames(esc_final_mensual_Tmin)=names(probabilidades)
    
    
        
    promedio_escenarios_Tmin=apply(esc_final_mensual_Tmin,2,mean2)
   
    Tmin_Multianual_Libertad=aggregate(data_m$tmin,list(Mes=data_m$month),mean2)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/tmin_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    r=barplot(as.numeric(Tmin_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min2(data_m$tmin)-2,max(data_m$tmin)+2),xpd = FALSE,col="cornsilk1",ylab="Temperatura Mínima (°C)",cex.lab=1,main="Pronóstico Temperatura Mínima ",cex.main=0.8)
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_Tmin[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_Tmin),ty="l",col="red3",lwd=2)
    legend(5,max(data_m$tmin)-1,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    dev.off()
    
    if(exists("srad")){
    
    esc_final_mensual_srad=matrix(0,nrow(escenarios_final),ncol(probabilidades))
    for(z in 1:nrow(escenarios_final)){
      esc_final_mensual_srad[z,]=rbind(esc_final_mensual[[z]]$srad)
    }
    esc_final_mensual_Tmin=as.data.frame(esc_final_mensual_srad)
    
    colnames(esc_final_mensual_srad)=names(probabilidades)
    
    
    
    promedio_escenarios_srad=apply(esc_final_mensual_srad,2,mean2)
    
    srad_Multianual_Libertad=aggregate(data_m$srad,list(Mes=data_m$month),mean2)[match(names(probabilidades),colnames(Años_org)),] #Se carga la precipitación mensual multianual 
    
    tiff("Pronosticos/srad_pronos.tiff", width = 10, height = 5,units = 'in',res=200)
    
    r=barplot(as.numeric(srad_Multianual_Libertad[,2]),names.arg=names(probabilidades),ylim=c(min2(data_m$srad)-2,max(data_m$srad)),xpd = FALSE,col="cornsilk1",ylab="Radiación Solar  (MJ*m2/mes)",cex.lab=1,main="Pronóstico Radiación Solar",cex.main=0.8)
    
    for(m in 1:num_esc){
      lines(r,as.numeric(esc_final_mensual_srad[m,]),ty="l",col="gray76")
    }
    
    lines(a,as.numeric(promedio_escenarios_srad),ty="l",col="red3",lwd=2)
    legend(5,max(data_m$srad)-5,c("Climatologica","Escenarios","Promedio escenarios"),lty=c(1,1,1), lwd=c(1.5,1.5,1.5),col=c("dodgerblue1","gray76","red3"),cex=0.8)
    
    box()
    dev.off()
    }
  }
}


#####################################################################################
#####################################################################################
#####--------------------------CUERPO INTERFAZ GRÁFICA------------------------#######
#####################################################################################
#####################################################################################


win <- gwindow("RClimTool 1.0", visible=F ,width = 600) #Crea ventana inicial
nb = gnotebook(cont=win,expand=T,tab.pos = 2)


#------------------------------------------------------------------
###----------------Lectura y validacion de datos----------------###
#------------------------------------------------------------------

lyt=glayout(homogeneous =T,cont=nb,spacing=1,label="1. Lectura de datos",expand=T) 

lyt[1,1:3]=(g=gframe("Lectura de datos",container=lyt,horizontal = T))

lytgb=glayout(homogeneous =T,cont=g,spacing=1,expand=T) 


lytgb[1,1]=(h=gbutton("Cambiar directorio",container=lytgb,handler = function(h,...)setwd(gfile(text="Seleccionar directorio",type="selectdir"))))

lytgb[2,1]=(glabel=(""))
lytgb[3,1]=(glabel=("Cargar datos:"))

lytgb[4,1]=gbutton("Temp. Máxima",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmax2()})


lytgb[5,1]=gbutton("Temp. Mínima",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmin2()})

# lytgb[5,1]=gbutton("Temp. Media",container=lytgb, expand=F, 
#                    handler =function(h,...) {cargar_tmean()})


lytgb[6,1]=gbutton("Precipitación",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_precip2()})


lytgb[8,1]=glabel("¿Desea realizar el llenado de datos?")

lytgb[9,1]=op=(gdroplist(c("","Si","No"),handler=function(...) opc() ))
opc=function(){
  if(svalue(op)=="No"){
    dir.create("Datos_faltantes",showWarnings=F)
    write.csv(tmax,"Datos_faltantes/data_genTmax.csv",row.names =F)
    write.csv(tmin,"Datos_faltantes/data_genTmin.csv",row.names =F)
    write.csv(precip,"Datos_faltantes/data_genPrec.csv",row.names =F)
    
  }
}
#----------------------------------------------------------------
####--------------------Analisis descriptivo----------------#####
#----------------------------------------------------------------
lyt1=glayout(homogeneous =F,cont=nb,spacing=1,label="2. Análisis gráfico y descriptivo",expand=T) 


lyt1[1,1:6]=(g1=gframe("Análisis descripivo",container=lyt,expand=T,horizontal=F))
lytg2=glayout(homogeneous =F,cont=g1,spacing=1,expand=T) 


lytg2[1,1]=glabel("-Período de análisis",container=lytg2)
lytg2[2,1]=glabel("Desde",container=lytg2)
lytg2[2,2]=(mesd=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[2,3]=(añod=gedit("",cont=lytg2,expand=F,width =10,initial.msg="AAAA"))

lytg2[3,1]=glabel("Hasta",container=lytg2)
lytg2[3,2]=(mesd1=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[3,3]=(añod1=gedit("",cont=lytg2,expand=F,width =10,initial.msg="AAAA"))

lytg2[4,1]=glabel("",container=lytg2)

lytg2[5,1]=glabel("-Variable a analizar  ",container=lytg2)
lytg2[5,2]=(nom_val1=gdroplist(c("tmax","tmin","precip"),selected = 0,cont=lytg2,expand=T,handler=function(h,...){attach(eval(parse(text=svalue(h$obj))))}))
lytg2[5,3]=gbutton("Descriptivas",container=lytg2,handler=function(h,...){print(descript2(eval(parse(text=svalue(nom_val1)))))})

lyt1[2,1:6]=glabel("")


#---------------------------------------------------------------------
###------------------------Analisis grafico------------------------###
#---------------------------------------------------------------------
lyt1[3,1]=(g.2=gframe("Gráficos automáticos: ",container=lyt1 ,horizontal=F,expand=T))
lytg.1=glayout(homogeneous =F,cont=g.2,spacing=1,expand=T) 

lytg.1[1,1]=glabel("Tipo de análisis")
lytg.1[1,2]=(tipo=gdroplist(c("Diaria","Mensual"),selected=1,cont=lytg.1))

lytg.1[2,1]=glabel("",cont=lytg.1)

lytg.1[3,1]=gbutton("Gráficos Plot",container=lytg.1,
                    handler = function(h,...){graf_plot() } )

lytg.1[4,1]=gbutton("Gráficos Boxplot",container=lytg.1,
                    handler = function(h,...){graf_box() } )


lytg.1[5,1]=gbutton("Gráficos de dispersión",container=lytg.1,
                    handler = function(h,...){graf_disp() } )

lyt1[5,1:6]=glabel("")


lyt1[6,1]=(g2=gframe("Gráficos Personalizados: ",container=lyt1 ,horizontal=F,expand=T))
lytg1=glayout(homogeneous =F,cont=g2,spacing=1,expand=T) 

lytg1[1,1]=glabel("Clásicos",container=lytg1)
lytg1[2,1]=gbutton("Gráfico Plot",container=lytg1,
                   handler = function(h,...){plot2() } )

lytg1[3,1]=gbutton("Histograma",container=lytg1,
                   handler = function(h,...){hist2()} )

lytg1[4,1]=gbutton("Gráfico Boxplot",container=lytg1,
                   handler = function(h,...){boxplot2()} )

lytg1[1,2]=glabel("      ",container=lytg1)
lytg1[1,3]=glabel("P. ggplot2",container=lytg1)
lytg1[2,3]=gbutton("Gráfico Plot",container=lytg1,
                   handler = function(h,...){qplot2()} )

lytg1[3,3]=gbutton("Histograma",container=lytg1,
                   handler = function(h,...){hist_22()} )




#---------------------------------------------------------------
###----------------Seccion control de calidad----------------###
#---------------------------------------------------------------

lyt4=glayout(homogeneous =T,cont=nb,spacing=2,label="3. Control de calidad ",expand=T) 

lyt4[1,1]=(gg=gframe("Validación",cont=lyt4,horizontal=F))
lytg=glayout(homogeneous =F,cont=gg,spacing=1,expand=T)

lytg[1,1]=glabel("",cont=lytg)

lytg[2,1]=glabel("Variable a validar",container=lytg)
lytg[2,2]=(variable<-gdroplist(c("tmax","tmin","precip"),selected=0,cont=lytg,expand=T))

lytg[3,1]=glabel("No. de desviaciones estándar: ",container=lytg)
lytg[3,2]=(criterio=gedit("3",container=lytg,width = 5,initial.msg="Desv.Est."))

lytg[4,1]=glabel("Rango de la variable:",container=lytg)
lytg[4,2]=(minim=gedit("",container=lytg,width = 7,initial.msg="Mín."))
lytg[4,3]=(maxim=gedit("",container=lytg,width = 10,initial.msg="Máx."))

lytg[5,1]=glabel("",cont=lytg)
lytg[6,2]=gbutton("Validar",container=lytg,handler=function(h,...){print(validar2(eval(parse(text=svalue(variable)))))})
lytg[6,3]=gbutton("Gráficos",container=lytg,handler=function(h,...){grafcontrol(eval(parse(text=svalue(variable))))})

lyt4[2,1]=(gg.3=gframe("Pre-informe",cont=lyt4,horizontal=F))
lytgg.3=glayout(homogeneous =F,cont=gg.3,spacing=1,expand=T)

lytgg.3[1,1]=glabel("Nombre del archivo: ",container=lytgg.3)
lytgg.3[1,2]=(nom_arch1 <-gedit("preinforme.doc",container=lytgg.3,coerce.with=as.character)) #Genera un label editable para el nombre del archivo del informe

lytgg.3[2,2]=gbutton("Generar pre-informe",cont=lytgg.3,handler=function(h,...)inf2())

#------------------------------------------------------------
###----------------Seccion datos faltantes----------------###
#------------------------------------------------------------

lyt3=glayout(homogeneous =F,cont=nb,spacing=1,label="4. Datos faltantes",expand=T) 

lyt3[1,1]=glabel("Año inicial: ",container=lyt3)
lyt3[1,2]=(year_min <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe

lyt3[2,1]=glabel("Año final: ",container=lyt3)
lyt3[2,2]=(year_max <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe


lyt3[7,1]=gbutton("Completar datos",container=lyt3,handler = function(h,...){datos_falt()},expand=T)

lyt3[8,1:4]=gtext("Nota: Para llevar a cabo esta función es indispensable tener los datos de temperatura mínima, máxima y precipitación",container=lyt3,coerce.with=as.character)

#-------------------------------------------------------------------
###----------------Seccion homogeneidad de series----------------###
#-------------------------------------------------------------------
lyt5=glayout(homogeneous =F,cont=nb,spacing=2,label="5. Análisis de homogeneidad",expand=T) 

lyt5[1,1]=(gg.1=gframe("Definición de parámetros:",cont=lyt5,horizontal=F))

lyt55=glayout(homogeneous =F,cont=gg.1,spacing=2,expand=T) 
lyt55[1,1]=glabel("-Período de análisis",container=lyt55)
lyt55[2,1]=glabel("Desde",container=lyt55)
lyt55[2,2]=(mesh=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[2,3]=(añoh=gedit("",cont=lyt55,expand=F,width =10,initial.msg="AAAA"))

lyt55[3,1]=glabel("Hasta",container=lyt55)
lyt55[3,2]=(mesh1=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[3,3]=(añoh1=gedit("",cont=lyt55,expand=F,width =10,initial.msg="AAAA"))

lyt55[4,1]=glabel("")
lyt55[5,1]=glabel("-Variable a analizar",container=lyt55)

lyt55[5,2]=(nom_val2=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt55,expand=T))

lyt55[6,1]=glabel("-Nivel de significancia",container=lyt55)

lyt55[6,2]=(obj <- gspinbutton(from=0, to = 0.3, by =0.01, value=0.05,
                               container=lyt55)) #Idea para seleccionar nivel de confianza




lyt5[2,1]=glabel("")
lyt5[3,1]=(gg2=gframe("Test para Normalidad:",cont=lyt5,horizontal=F))
norm_test1=gcheckbox("Shapiro-Wilk",cont=gg2,handler=function(h,...){print(shap(eval(parse(text=svalue(nom_val2)))))})
norm_test2=gcheckbox("Kolmogorov-Smirnov",cont=gg2,handler=function(h,...){print(KS_test(eval(parse(text=svalue(nom_val2)))))})
norm_test3=gcheckbox("Jarque-Bera ",cont=gg2,handler=function(h,...){print(JB2(eval(parse(text=svalue(nom_val2)))))})

gbutton("Gráficos QQ-norm",cont=gg2,handler=function(h,...){graf_norm()},width=5)

lyt5[4,1]=glabel("")
lyt5[5,1]=(gg3=gframe("Test para Tendencia:",cont=lyt5,horizontal=F))
tend_test=gcheckbox("Rango correlación de Spearman",cont=gg3,handler=function(h,...){print(Rsp_Test(eval(parse(text=svalue(nom_val2)))))})
tend_test1=gcheckbox("Mann-Kendall",cont=gg3,handler=function(h,...){print(Ken_T(eval(parse(text=svalue(nom_val2)))))})

lyt5[6,1]=glabel("")
lyt5[7,1]=(gg4=gframe("Test para Estabilidad en Varianza:",cont=lyt5,horizontal=F))
estv_test=gcheckbox("Test-F",cont=gg4,handler=function(h,...){print(F_test.indic(eval(parse(text=svalue(nom_val2)))))})

lyt5[8,1]=glabel("")
lyt5[9,1]=(gg5=gframe("Test para Estabilidad en Media:",cont=lyt5,horizontal=F))
estm_test=gcheckbox("Test-T",cont=gg5,handler=function(h,...){print(T_Test(eval(parse(text=svalue(nom_val2)))))})

estm_test1=gcheckbox("Test U Mann - Whitney",cont=gg5,handler=function(h,...){print(Umann_Test(eval(parse(text=svalue(nom_val2)))))})

lyt5[1,2]=glabel("   ")
lyt5[1,3]=(gg.2=gframe("Informe",cont=lyt5,horizontal=F))
lyt.1=glayout(homogeneous =F,cont=gg.2,spacing=2,expand=T) 


lyt.1[1,1]=glabel("Nombre del archivo: ",container=lyt.1)
lyt.1[1,2]=(nom_arch <-gedit("informe.doc",container=lyt.1,coerce.with=as.character,width = 15)) #Genera un label editable para el nombre del archivo del informe
# lyt.1[2,1]=glabel("Seleccione la variable:",container=lyt.1)
# lyt.1[2,2]=(nom_val=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt.1,expand=T))

lyt.1[2,2]=gbutton("Generar Informe",container=lyt.1,handler = function(h,...){inf()},expand=T)

#---------------------------------------------------------------------
###------------------------Seccion Indicadores---------------------###
#---------------------------------------------------------------------

lyt2=glayout(homogeneous =F,cont=nb,spacing=2,label="6. Cálculo de indicadores ",expand=T) 


lyt2[1,1]=(gg.3=gframe("Definición de parámetros:",cont=lyt2,horizontal=F))
lyt.2=glayout(homogeneous =F,cont=gg.3,spacing=2,expand=T) 
lyt.2[1,1]=glabel("")

lyt.2[2,1]=glabel("-Período de análisis",container=lyt55)
lyt.2[3,1]=glabel("Desde",container=lyt.2)
lyt.2[3,2]=(mesi=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[3,3]=(añoi=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="AAAA"))

lyt.2[4,1]=glabel("Hasta",container=lyt.2)
lyt.2[4,2]=(mesi1=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[4,3]=(añoi1=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="AAAA"))

lyt.2[5,1]=glabel("",container=lyt.2)

# lyt.2[6,1]=glabel("-Variable a analizar",container=lyt55)
# 
# lyt.2[6,2]=(nom_val3=gdroplist(c("tmax","tmin"),selected=0,cont=lyt.2,expand=F))
# lyt.2[6,3]=glabel("     ",container=lyt55)


lyt2[2,1]=(gg.33=gframe("Indicadores Climáticos:",cont=lyt2,horizontal=F))
lyt.22=glayout(homogeneous =F,cont=gg.33,spacing=2,expand=F) 

#lyt.22[6,1]=glabel("-Indicadores Climáticos:",container=gg.33)

lyt.22[7,1]=gcheckbox("No. días con Temp. mín.",container=lyt.22,handler = function(h,...){print(tempmin_ind(as.numeric(svalue(valor1))))})
lyt.22[7,2]=(nom_valm=gdroplist(c("mayor a ","menor a "),selected=0,cont=lyt.22,expand=F))
lyt.22[7,3]=(valor1=gedit("",container=lyt.22,width = 0.5,initial.msg="Umbral"))

lyt.22[8,1]=gcheckbox("No. días con Temp. máx.",container=lyt.22,handler = function(h,...){print(tempmax_ind(as.numeric(svalue(valor2))))})
lyt.22[8,2]=(nom_valm1=gdroplist(c("mayor a ","menor a "),selected=0,cont=lyt.22,expand=F))
lyt.22[8,3]=(valor2=gedit("",container=lyt.22,width = 0.5,initial.msg="Umbral"))

lyt.22[9,1]=gcheckbox("No. días con lluvia ",container=lyt.22,handler = function(h,...){print(lluvia2(as.numeric(svalue(valor3))))})
lyt.22[9,2]=(nom_valp=gdroplist(c("mayor a ","menor a "),selected=0,cont=lyt.22,expand=F))
lyt.22[9,3]=(valor3=gedit("",container=lyt.22,width = 0.5,initial.msg="Umbral"))

lyt.22[10,1]=gcheckbox("No. días con lluvia consec",container=lyt.22,handler = function(h,...){print(lluviaconsc(as.numeric(svalue(valor31)),as.numeric(svalue(valor32))))})
lyt.22[10,2]=(valor31=gedit("",container=lyt.22,width = 0.5,initial.msg="Umbral"))
lyt.22[10,3:4]=(valor32=gedit("",container=lyt.22,width = 15,initial.msg="Dias consec."))

 lyt.22[11,1]=gcheckbox("Cuantiles",container=lyt.22,handler = function(h,...){print(cuantil())})
 lyt.22[11,2]=(nom_val_c=gdroplist(c("...","Terciles","Cuartiles","Deciles","Percentiles"),selected=1,cont=lyt.22,expand=F,handler = function(h,...){
   lyt.22[11,3]=(nom_est_c=gdroplist(names(tmax[-3:-1]),selected=1,cont=lyt.22,expand=F))
   assign("nom_est_c",nom_est_c,.GlobalEnv)}
   ))
lyt.22[12,1]=gcheckbox("Índice Estandarizado de Precipitación",container=lyt.22,handler = function(h,...){print(spi())})




lyt2[3,1]=(gg.3.3=gframe("Indicadores Agroclimáticos:",cont=lyt2,horizontal=F))
lyt.2.2=glayout(homogeneous =F,cont=gg.3.3,spacing=2,expand=F) 
lyt.2.2[1,1]=glabel("")
lyt.2.2[2,1]=gcheckbox("Grados días del cultivo ",container=lyt.2.2,handler = function(h,...){print(gradosdias(as.numeric(svalue(valor22))))})
lyt.2.2[2,2]=(valor22=gedit("",container=lyt.2.2,width = 15,initial.msg="Temp. Base"))

lyt.2.2[3,1]=gcheckbox("Unidades de calor del cultivo",container=lyt.2.2,handler = function(h,...){print(chu())})


#---------------------------------------------------------------------
###--------------Seccion Cálculo de nuevas variables---------------###
#---------------------------------------------------------------------


lyt22=glayout(homogeneous =F,cont=nb,spacing=2,label="7. Cálculo de nuevas variables ",expand=T) 

lyt22[1,1]=(gg.4=gframe("Cálculo de nuevas variables",cont=lyt22,horizontal=F))
lyt.3=glayout(homogeneous =T,cont=gg.4,spacing=3,expand=T) 

lyt.3[1,1]=glabel("-Seleccione la variable a calcular",container=lyt.3)

lyt.3[2,1]=gcheckbox("Temp. Máxima Mensual",container=lyt.3,handler = function(h,...){print(tempmax())})
lyt.3[3,1]=gcheckbox("Temp. Mínima Mensual",container=lyt.3,handler = function(h,...){print(tempmin())})
lyt.3[5,1]=gcheckbox("Precip. Acumulada Mensual",container=lyt.3,handler = function(h,...){print(precpcum())})


lyt.3[4,1]=gcheckbox("Temp. Promedio Mensual",container=lyt.3,handler = function(h,...){print(tempmean())})

 lyt.3[6,1]=gcheckbox("Oscilación de temperatura",container=lyt.3,handler = function(h,...){print(osc_temp())})
# lyt.3[7,1]=gcheckbox("Déficit de presión de Vapor (VPN) Diaria",container=lyt.3,handler = function(h,...){print(tempmean())})
# lyt.3[8,1]=gcheckbox("Humedad Relativa Mensual",container=lyt.3,handler = function(h,...){print(tempmean())})
# lyt.3[9,1]=gcheckbox("Déficit de presión de Vapor Mensual",container=lyt.3,handler = function(h,...){print(tempmean())})
# 


#---------------------------------------------------------------------
###---------------------Seccion Condición ENSO---------------------###
#---------------------------------------------------------------------


lyt22=glayout(homogeneous =F,cont=nb,spacing=2,label="8. Condición ENSO ",expand=T) 

lyt22[1,1]=(gg.4=gframe("Condición ENSO",cont=lyt22,horizontal=F))
lyt.3=glayout(homogeneous =T,cont=gg.4,spacing=3,expand=T) 

lyt.3[1,1]=glabel("-Mensual",container=lyt.3)
lyt.3[2,1]=glabel("Desde:",container=lyt.3)
lyt.3[2,2]=(mes=gdroplist(c("Mes",1:12),selected=1,cont=lyt.1,expand=T))
lyt.3[2,3]=(año=gdroplist(c("Año",1950:2014),selected=1,cont=lyt.1,expand=T))

lyt.3[3,1]=glabel("Hasta:",container=lyt.3)
lyt.3[3,2]=(mes1=gdroplist(c("Mes",1:12),selected=1,cont=lyt.1,expand=T))
lyt.3[3,3]=(año1=gdroplist(c("Año",1950:2014),selected=1,cont=lyt.1,expand=T))
lyt.3[4,2]=gbutton("Consulta Mensual",container=lyt.3,handler = function(h,...){print(enso())},expand=T)
#lyt.3[5,2]=gbutton("Generar Gráficos",container=lyt.3,handler = function(h,...){print(graficos_enso())},expand=T)
#lyt.3[5,3]=gdroplist("Gráficos",container=lyt.3,handler = function(h,...){print(enso())},expand=T)


lyt22[2,1]=(gg.44=gframe("Generar gráficos ENSO",cont=lyt22,horizontal=F))
lyt.33=glayout(homogeneous =T,cont=gg.44,spacing=3,expand=T) 

lyt.33[1,1]=gbutton("Cargar datos mensuales",container=lyt.33,handler = function(h,...){datos_enso_m()},expand=T)
lyt.33[2,1]=glabel("")

lyt.33[3,1]=gbutton("Gráficos plot",container=lyt.33,handler = function(h,...){print(graficos_enso_plot())},expand=T)
lyt.33[4,1]=gbutton("Gráficos boxplot",container=lyt.33,handler = function(h,...){print(graficos_enso_boxplot())},expand=T)
lyt.33[5,1]=gbutton("Gráficos anomalías",container=lyt.33,handler = function(h,...){print(graficos_enso_anomalias())},expand=T)

# lyt.3[6,1]=glabel("-Trimestral",container=lyt.3)
# lyt.3[7,1]=glabel("Desde:",container=lyt.3)
# lyt.3[7,2]=(trim=gdroplist(c("Trim",1:4),selected=1,cont=lyt.1,expand=T))
# lyt.3[7,3:4]=(año2=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))

# lyt.3[8,1]=glabel("Hasta:",container=lyt.3)
# lyt.3[8,2]=(trim1=gdroplist(c("Trim",1:4),selected=1,cont=lyt.1,expand=T))
# lyt.3[8,3:4]=(año3=gdroplist(c("Año",1950:2013),selected=1,cont=lyt.1,expand=T))
# lyt.3[9,2:3]=gbutton("Consulta Trimestral",container=lyt.3,handler = function(h,...){print(enso1())},expand=T)



#---------------------------------------------------------------------
###---------------------Seccion Condición ENSO---------------------###
#---------------------------------------------------------------------
# lyt4=glayout(homogeneous =T,cont=nb,spacing=2,label="3. Control de calidad ",expand=T) 
# 
# lyt4[1,1]=(gg=gframe("Validación",cont=lyt4,horizontal=F))
# lytg=glayout(homogeneous =F,cont=gg,spacing=1,expand=T)


lyt6=glayout(homogeneous =T,cont=nb,spacing=2,label="9. Pronósticos Agroclimáticos",expand=T) 

lyt6[1,1]=(gg6=gframe("Parámetros de entrada",cont=lyt6,horizontal=F))
lty.33=glayout(homogeneous =F,cont=gg6,spacing=2,expand=T) 

lty.33[1,1]=glabel(" ",cont=lty.33)


lty.33[2,1]=glabel("-Estación No. :",cont=lty.33)
lty.33[2,2]=(est_p=gedit("",cont=lty.33,width =5))
                                           

#Fechas=c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre") # Crea una tabla con los nombres de los meses
lty.33[4,1]=glabel("-Cargar datos (opcional):",cont=lty.33)
lty.33[4,2]=gbutton("Diarios",cont=lty.33,handler=function(h,...) cargar_diarios())
lty.33[4,3]=gbutton("Mensuales",cont=lty.33,handler=function(h,...) cargar_mensual())

lty.33[5,1]=glabel(" ",cont=lty.33)

lty.33[6,1]=glabel("-Seleccione la variable:",cont=lty.33)
lty.33[6,2]=(val_p=gdroplist(c("tmax","tmin","precip","srad"),cont=lty.33))
#lty.33[3,3]=gdroplist(c("Hasta",Fechas),cont=lty.33)
lty.33[7,1]=glabel("-Cargar tabla probabilidades:",cont=lty.33)
lty.33[7,2]=gbutton("Probabilidades",cont=lty.33,handler=function(h,...) cargar_prob())


lty.33[8,1]=glabel("-No. de escenarios a simular:",cont=lty.33)
lty.33[8,2]=(num_esc=gedit(" ",cont=lty.33,width=3))


lty.33[9,1]=glabel("-Generar pronósticos:",cont=lty.33)
lty.33[9,2]=gbutton("Pronosticar",cont=lty.33,handler=function(h,...) pronosticos())



lyt6[2,1]=(gg.6=gframe("Enlaces de interés",cont=lyt6,horizontal=F))
lty.3.3=glayout(homogeneous =F,cont=gg.6,spacing=2,expand=T) 
lty.3.3[1,1]=glabel("-IDEAM: www.ideam.gov.co/",cont=lty.3.3)
lty.3.3[2,1]=glabel("-NOAA: www.noaa.gov/",cont=lty.3.3)
lty.3.3[3,1]=glabel("-ECMWF: www.ecmwf.int/",cont=lty.3.3)


#---------------------------------------------------------------------
###---------------------------Bienvenida---------------------------###
#---------------------------------------------------------------------

g5=ggroup(container=nb,horizontal = F,label="Bienvenid@",cont=nb)

#Para el logo modifique la ruta en "dirname="en la cual se encuentra la imagen
#gimage("logo", size="menu", container=g5,label="Bienvenid@",width=700,heigth=700) #Inserta imagen "ciat.png"en la ventana
gimage("logo.png",dirname=dir, size="menu", container=g5,label="Bienvenid@",width=700,heigth=700) #Inserta imagen "ciat.png"en la ventana

mensj=gtext("Bienvenid@ ",cont=g5)
insert(mensj, "a RClimTool, una aplicación diseñada para el análisis de series climatológicas (Temperatura Mínima, Temperatura Máxima, Temperatura Media y Precipitación)")

visible(win) = T
focus(win)
