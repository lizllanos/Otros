



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
  


#IMPORTANT READ IT!!!!
#IMPORTANT READ IT!!!!
#IMPORTANT READ IT!!!!

dir="C:/Users/lllanos/Desktop/english" #Enter the location of the folder containing the files of the tool

#---------------------------------------------------------------------
####--------Packages needed to run the application---------####
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
if(require(zoo)==FALSE){ install.packages("zoo")}


require(ggplot2) 
require(rtf) 
require(gWidgets) 
require(gWidgetsRGtk2)
require(RMAWGEN) 
require(Kendall) 
require(tseries) 
require(grid) 
require(car) 
require(reshape) 
require(zoo) 
options("guiToolkit"="RGtk2") 


#---------------------------------------------------------------------
####-----------------Load data-----------------#####
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

ggenericwidget(gfile.LIST,cont=gwindow("Load Min. Temp",width=300,height=100))

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
  
  ggenericwidget(gfile.LIST,cont=gwindow("Load Max. Temp",width=300,height=100))
  
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
  
  ggenericwidget(gfile.LIST,cont=gwindow("Load Precipitation",width=300,height=100))
  
}

#---------------------------------------------------------------------
####--------Descriptive analysis---------#####
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
  Mín=round(min(object,na.rm=T),0); Max=round(max(object,na.rm=T),0)
  Datos.NA=round(sum(is.na(object)),0)
  Porcentaje.NA=round((Datos.NA/length(object))*100,3)
  result=cbind(n,"Min"=Mín,"Max"=Max,"Mean"=Media,"Variance"=Varianza,"Stand. Desv."=Desv.Est,"Median"=Mediana,"CV"=Coef.Var,"NA data"=Datos.NA,"% NA"=Porcentaje.NA)
  }

descript2=function(object){
  if(svalue(mesd)!="" && svalue(mesd1)!="" && svalue(añod)!="" && svalue(añod1)!="") {object=cultivod(object)
  }else{object=object}
  
  d=sapply(object[-3:-1],descript)
  row.names(d) <-c("n","Min","Max","Mean","Variance","Stand. Desv.","Median","CV %","NA","NA %")
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
####----------Classic graphs-----------#####
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
        text="'Title'"),
      "xlab"=list(
        type= "gedit",
        text="'Label for the x axis'"),
      "ylab"=list(
        type= "gedit",
        text="'Label for the y axis'")))
  ggenericwidget(plot.default.LIST,cont=gwindow("  Plot"))
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
        text="'Title'"),
      "xlab"=list(
        type= "gedit",
        text="'Label for the x axis'"),
      "ylab"=list(
        type= "gedit",
        text="'Label for the y axis'")))
  
  ggenericwidget(hist.LIST,container=gwindow("  Histogram")) 
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
      text="'Title'"),
    "ylab"=list(
      type= "gedit",
      text="'Label for the y axis'")))
  ggenericwidget( boxplot.LIST ,container=gwindow("  Boxplot"))
  
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
  
  ggenericwidget(qqnorm.default.LIST,container=gwindow("  QQ-norm"))
  
}



graf_plot=function(){
  confirmDialog(paste("Graphical results will be saved in",getwd(),"/Graphic analysis"),"Location files")
  
  if(svalue(tipo)=="Daily"){
    
  tmax1=ts(tmax,start=min(tmax$year),frequency=365)
  tmin1=ts(tmin,start=min(tmin$year),frequency=365)
  precip1=ts(precip,start=min(precip$year),frequency=365)
  
  station=names( tmax[-3:-1])
  m=matrix(c(1,1,2,3,3,4,5,5,6),3,3,byrow=T)
    
  dir.create("Graphic analysis",showWarnings=F)
  dir.create("Graphic analysis/Daily graphs",showWarnings=F)
  
  dir.create("Graphic analysis/Daily graphs/Plots and histograms",showWarnings=F)
  
  for(i in 1:(ncol(tmax)-3)){
    
    jpeg(paste("Graphic analysis/Daily graphs/Plots and histograms/plot_",station[i],"_d.jpeg",sep=""),width = 1200, height = 500)
    layout(m)
    plot(as.zoo(tmax1[,i+3]),type="l",xlab="Years",ylab="Max. Temp",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black",cex.lab=1.5)
    title(main=paste(station[i]),outer = TRUE, line = -1)
    hist(tmax1[,i+3],main="",xlab="Max. Temp",cex.lab=1.5,xlim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),ylab="Density",freq=F)
    plot(as.zoo(tmin1[,i+3]),type="l",xlab="Years",ylab="Min. Temp",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black",cex.lab=1.5)
    hist(tmin1[,i+3],main="",xlab="Min. Temp",cex.lab=1.5,xlim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),ylab="Density",freq=F)
    plot(as.zoo(precip1[,i+3]),type="l",xlab="Years",ylab="Precipitation",cex.lab=1.5)
    hist(precip1[,i+3],main="",xlab="Precipitation",cex.lab=1.5,ylab="Density",freq=F)
    dev.off()
      #xlim=c(0,max(max(hist(tmax1[,i+3],plot=F)$density),max(hist(tmin1[,i+3],plot=F)$density)))
  }
  }
  
  if(svalue(tipo)=="Monthly"){
   
    tmax1=ts(aggregate(tmax[-3:-1],list(Mes=tmax$month,Año=tmax$year),mean2),frequency=12,start=min(tmax$year))
    tmin1=ts(aggregate(tmin[-3:-1],list(Mes=tmin$month,Año=tmin$year),mean2),start=min(tmax$year),frequency=12)
    precip1=ts(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum2),start=min(tmax$year),frequency=12)
    
    
    station=names(tmax[-3:-1])
    m=matrix(c(1,1,2,3,3,4,5,5,6),3,3,byrow=T)
    
    dir.create("Graphic analysis",showWarnings=F)
    dir.create("Graphic analysis/Monthly graphs",showWarnings=F)
    
    dir.create("Graphic analysis/Monthly graphs/Plots and histograms",showWarnings=F)
   
    for(i in 1:(ncol(tmax)-3)){
     
      jpeg(paste("Graphic analysis/Monthly graphs/Plots and histograms/plot_",station[i],"_m.jpeg",sep=""),width = 1200, height = 500)
      layout(m)
      plot(as.zoo(tmax1[,i+2]),type="l",xlab="Years",ylab="Max. Temp",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black",cex.lab=1.5)
      title(main=paste(station[i]),outer = TRUE, line = -1)
      hist(tmax1[,i+2],main="",xlab="Max. Temp",cex.lab=1.5,xlim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),ylab="Density",freq=F)
      plot(as.zoo(tmin1[,i+2]),type="l",xlab="Years",ylab="Min. Temp",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),col="black",cex.lab=1.5)
      hist(tmin1[,i+2],main="",xlab="Min. Temp",cex.lab=1.5,xlim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)),ylab="Density",freq=F)
      plot(as.zoo(precip1[,i+2]),type="l",xlab="Years",ylab="Precipitation",cex.lab=1.5)
      hist(precip1[,i+2],main="",xlab="Precipitation",cex.lab=1.5,ylab="Density",freq=F)
      dev.off()
    
  }
  
}
   
}

graf_box=function(){
  confirmDialog(paste("Graphical results will be saved in",getwd(),"/Graphic analysis"),"Location files")
  
  if(svalue(tipo)=="Monthly"){
    tmax=aggregate(tmax[-3:-1],list(month=tmax$month,year=tmax$year),mean2)
    tmin=aggregate(tmin[-3:-1],list(month=tmin$month,year=tmin$year),mean2)
    precip=aggregate(precip[-3:-1],list(month=precip$month,year=precip$year),sum2)
    
    station=names( tmax[-2:-1])
    month1=recode(tmax$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    levels(month1)<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
    
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    dir.create("Graphic analysis",showWarnings=F)
    dir.create("Graphic analysis/Monthly graphs",showWarnings=F)
    
    dir.create("Graphic analysis/Monthly graphs/Boxplot",showWarnings=F)
    
    for(i in 1:(ncol(tmax)-2)){
      
      jpeg(paste("Graphic analysis/Monthly graphs/Boxplot/box_",station[i],"_m.jpeg",sep=""),width = 1200, height = 500)
      
      mes=qplot(month1,tmax[,i+2], geom = "boxplot",group = month1, ylab=paste("Max. Temp",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      mes1=qplot(month1,tmin[,i+2], geom = "boxplot",group = month1, ylab=paste("Min. Temp",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      mes2=qplot(month1,precip[,i+2], geom = "boxplot",group = month1, ylab=paste("Precip",sep="_"),xlab="Mes", outlier.size =0.7)
      
      year=qplot(tmax$year ,tmax[,i+2], geom = "boxplot",  group =  tmax$year,ylab=paste("Max. Temp",sep="_"),xlab="Año",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      year1=qplot(tmin$year ,tmin[,i+2], geom = "boxplot", group =  tmin$year,ylab=paste("Min. Temp",sep="_"),xlab="Año",ylim=c(min(tmin[,i+2],na.rm=T),max(tmax[,i+2],na.rm=T)), outlier.size =0.7)
      year2=qplot(precip$year ,precip[,i+2], geom = "boxplot", group =  precip$year,ylab=paste("Precip",sep="_"),xlab="Año", outlier.size =0.7)
      
      grid.newpage()
      
      pushViewport(viewport(layout = grid.layout(4,4, heights = unit(c(0.5, 4, 4,4), "null"))))
      grid.text(paste(station[i],"_annual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
      grid.text(paste(station[i],"_monthly"), vp = viewport(layout.pos.row = 1, layout.pos.col = 3:4))
      
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
    
    gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Max. Temp",xlab="",ylim=c(min(tmin[-2:-1],na.rm=T),max(tmax[-2:-1],na.rm=T)), outlier.size =0.7)
    gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Min. Temp",xlab="",ylim=c(min(tmin[-2:-1],na.rm=T),max(tmax[-2:-1],na.rm=T)), outlier.size =0.7)
    gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precipitation",xlab="", outlier.size =0.7)
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    
    jpeg(paste("Graphic analysis/Monthly graphs/Boxplot/todos_m.jpeg",sep=""),width = 600, height = 500)
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(4,1, heights = unit(c(0.5, 4, 4,4), "null"))))
    grid.text("", vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    
    print(gral, vp = vplayout(2, 1))
    print(gral1, vp = vplayout(3,1))
    print(gral2, vp = vplayout(4,1))
    dev.off()
  }
  
  if(svalue(tipo)=="Daily"){
    tmax=tmax
    tmin=tmin
    precip=precip
    
    station=names( tmax[-3:-1])
    month1=recode(tmax$month,"1='Ene';2='Feb';3='Mar';4='Abr';5='May';6='Jun';7='Jul';8='Ago';9='Sep';10='Oct';11='Nov';12='Dec'",as.factor.result=TRUE)
    levels(month1)<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dec")
    
    
 
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  dir.create("Graphic analysis",showWarnings=F)
  dir.create("Graphic analysis/Daily graphs",showWarnings=F)
    dir.create("Graphic analysis/Daily graphs/Boxplot",showWarnings=F)
    
  for(i in 1:(ncol(tmax)-3)){
    
  jpeg(paste("Graphic analysis/Daily graphs/Boxplot/box_",station[i],"_d.jpeg",sep=""),width = 1200, height = 500)
  
  mes=qplot(month1,tmax[,i+3], geom = "boxplot",group = month1, ylab=paste("Max. Temp",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  mes1=qplot(month1,tmin[,i+3], geom = "boxplot",group = month1, ylab=paste("Min. Temp",sep="_"),xlab="Mes",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  mes2=qplot(month1,precip[,i+3], geom = "boxplot",group = month1, ylab=paste("Precipitation",sep="_"),xlab="Mes", outlier.size =0.7)
  
  year=qplot(tmax$year ,tmax[,i+3], geom = "boxplot",  group =  tmax$year,ylab=paste("Max. Temp",sep="_"),xlab="Año",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  year1=qplot(tmin$year ,tmin[,i+3], geom = "boxplot", group =  tmin$year,ylab=paste("Min. Temp",sep="_"),xlab="Año",ylim=c(min(tmin[,i+3],na.rm=T),max(tmax[,i+3],na.rm=T)), outlier.size =0.7)
  year2=qplot(precip$year ,precip[,i+3], geom = "boxplot", group =  precip$year,ylab=paste("Precipitation",sep="_"),xlab="Año", outlier.size =0.7)
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(4,4, heights = unit(c(0.5, 4, 4,4), "null"))))
  grid.text(paste(station[i],"_annual"), vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.text(paste(station[i],"_monthly"), vp = viewport(layout.pos.row = 1, layout.pos.col = 3:4))
    
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
  
  gral=qplot(x$variable,x$value,geom="boxplot",group=x$variable,ylab="Max. Temp",xlab="",ylim=c(min(tmin[-3:-1],na.rm=T),max(tmax[-3:-1],na.rm=T)), outlier.size =0.7)
  gral1=qplot(y$variable,y$value,geom="boxplot",group=y$variable,ylab="Min. Temp",xlab="",ylim=c(min(tmin[-3:-1],na.rm=T),max(tmax[-3:-1],na.rm=T)), outlier.size =0.7)
  gral2=qplot(x$variable,z$value,geom="boxplot",group=z$variable,ylab="Precipitation",xlab="", outlier.size =0.7)
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  jpeg(paste("Graphic analysis/Daily graphs/Boxplot/todos_d.jpeg",sep=""),width = 600, height = 500)
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
  confirmDialog(paste("Graphical results will be saved in",getwd(),"/Graphic analysis"),"Location files")
  
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = 2)
  }
  
  
  dir.create("Graphic analysis/Scatter graphs",showWarnings=F)
  jpeg(paste("Graphic analysis/Scatter graphs/tmax.jpeg",sep=""),width = 600, height = 600)
  datostmax=data.frame(na.omit(tmax[-3:-1]))
  pairs(datostmax,lower.panel=panel.cor, pch=19,col="black")
  
  dev.off()
  
  jpeg(paste("Graphic analysis/Scatter graphs/tmin.jpeg",sep=""),width = 600, height = 600)
  datostmin=data.frame(na.omit(tmin[-3:-1]))
  pairs(datostmin,lower.panel=panel.cor, pch=19,col="black")
  dev.off()
  
  jpeg(paste("Graphic analysis/Scatter graphs/precip.jpeg",sep=""),width = 600, height = 600)
  datosprecip=data.frame(na.omit(precip[-3:-1]))
  pairs(datosprecip,lower.panel=panel.cor, pch=19,col="black")
  dev.off()
  
}

#---------------------------------------------------------------------
####---------Graphs with ggplot2---------#####
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
        text="'Title'"),
      "xlab"=list(
        type= "gedit",
        text="'Label for the x axis'"),
      "ylab"=list(
        type= "gedit",
        text="'Label for the y axis'")))
  ggenericwidget(qplot.LIST,container=gwindow("Histogram ggplot2"))
  
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
        text="'Title'"),
      "xlab"=list(
        type= "gedit",
        text="'Label for the x axis'"),
      "ylab"=list(
        type= "gedit",
        text="'Label for the y axis'")))
  ggenericwidget(qplot.LIST,container=gwindow("Plot ggplot2"))
  
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
ggenericwidget(geom_boxplot.LIST,container=gwindow("BoxPlot ggplot2"))

}


#---------------------------------------------------------------------
####--------------Quality control---------------####
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
  focus(window)
  
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
  ifelse(file.exists("Quality control")=="FALSE",dir.create("Quality control",showWarnings=F),"Ya existe carpeta Quality control")
  
  
  object1=ts(object,start=min(object$year),frequency=365)
  
  if(svalue(variable)=="tmax"){
    confirmDialog(paste("Graphical results of quality control will be saved in",getwd(),"/Quality control/tmax"),"Location files")
    dir.create("Quality control/tmax",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+as.numeric(svalue(criterio))*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Quality control/tmax/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Graphical results of quality control will be saved in",getwd(),"/Quality control/tmin"),"Location files")
       dir.create("Quality control/tmin",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
   porcentajes=matrix(0,ncol(object)-3,2)
   
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Quality control/tmin/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Graphical results of quality control will be saved in",getwd(),"/Quality control/tmean"))
      dir.create("Quality control/tmean",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Quality control/tmean/tmean_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    confirmDialog(paste("Graphical results of quality control will be saved in",getwd(),"/Quality control/precip"),"Location files")
    dir.create("Quality control/precip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,2)
    
    for(i in 1:(ncol(object)-3))
    {
      jpeg(paste("Quality control/precip/precip_",station[i],".jpeg",sep=""),width = 1150, height = 500)
      
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
    
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Datos fuera del rango","% Datos tmax<tmin","% Datos variación> 10","% Datos consecutivos","% Total datos NA")))
    
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
  
  val=ifelse(sapply(object[-3:-1],min2)>=as.numeric(svalue(minim)) & sapply(object[-3:-1],max2)<=as.numeric(svalue(maxim)),"is within the allowable range","contains values outside the range allowed")
  val=cbind(val)
  
  confirmDialog(paste("The station",names(object[-3:-1]),val,sep=" "),"Validation")
  
  station=names(object[-3:-1])
  ifelse(file.exists("Missing_data")=="FALSE",dir.create("Missing_data",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("Quality control")=="FALSE",dir.create("Quality control",showWarnings=F),"Ya existe carpeta Quality control")
  
  if(svalue(variable)=="tmax"){
    dir.create("Quality control/tmax",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Quality control/tmax/","atypical_",station[i],".csv",sep=""),row.names=F)
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(object[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-cbind(object[pos.4,1],object[pos.4,2],object[pos.4,3],object[pos.4,i+3])
      colnames(atipicos4)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos4,paste("Quality control/tmax/","TM_10_",station[i],".csv",sep=""),row.names=F)
      
      
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
    
    write.csv(tmax,"Missing_data/tmax_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of range","% Data tmax<tmin","% Data variation> 10","% Consecutive","% Total NA")))
    
  }
  
  if(svalue(variable)=="tmin"){
    dir.create("Quality control/tmin",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Quality control/tmin/","atypical_",station[i],".csv",sep=""),row.names=F)
      
      
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      pos3=which(tmax[i+3]<tmin[i+3])
      atipicos3<-data.frame(pos3,object[pos3,i+3])
      
      pos4=which(abs(diff(object[,i+3]))>=10)
      pos.4=unique(sort(c(pos4,pos4+1)))
      atipicos4<-cbind(object[pos.4,1],object[pos.4,2],object[pos.4,3],object[pos.4,i+3])
      colnames(atipicos4)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos4,paste("Quality control/tmin/","TM_10_",station[i],".csv",sep=""),row.names=F)
      
      
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
    write.csv(tmin,"Missing_data/tmin_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of range","% Data tmax<tmin","% Data variation> 10","% Consecutive","% Total NA")))
    
  }
  
  if(svalue(variable)=="tmean"){
    dir.create("Quality control/tmean_atip",showWarnings=F)
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
    write.csv(tmean,"Missing_data/tmean.csv")
  }
  
  
  if(svalue(variable)=="precip"){
    dir.create("Quality control/precip",showWarnings=F)
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,3)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      write.csv(atipicos,paste("Quality control/precip/","atypical",station[i],".csv",sep=""),row.names=F)
            
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
      post=pos2
      
      precip[post,i+3]<-NA
      
    }
    write.csv(precip,"Missing_data/precip_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of the range","% Total NA")))
    
  }
  porcentajes=as.table(porcentajes)
  names(dimnames(porcentajes)) <- c("", paste("Quality control for the variable",svalue(variable))) 
  return(porcentajes)
}

validar22<-function(object){
  
  val=ifelse(sapply(object[-3:-1],min2)>=as.numeric(svalue(minim)) & sapply(object[-3:-1],max2)<=as.numeric(svalue(maxim)),"está dentro del rango permitido","contiene valores por fuera del rango permitido")
  val=cbind(val)
  
 # confirmDialog(paste("La estación",names(object[-3:-1]),val,sep=" "))
  
  station=names(object[-3:-1])
 
  #ifelse(file.exists("Missing_data")=="FALSE",dir.create("Missing_data"),"Ya existe carpeta")
  #ifelse(file.exists("Quality control")=="FALSE",dir.create("Quality control"),"Ya existe carpeta Quality control")
  
  if(svalue(variable)=="tmax"){
   # dir.create("Quality control/tmax_atip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
     # write.csv(atipicos,paste("Quality control/tmax_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
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
    
   # write.csv(tmax,"Missing_data/tmax_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of range","% Data tmax<tmin","% Data variation> 10","% Consecutive","% Total NA")))
    
  }
  
  if(svalue(variable)=="tmin"){
   # dir.create("Quality control/tmin_atip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,6)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
     # write.csv(atipicos,paste("Quality control/tmin_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
      
      
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
   # write.csv(tmin,"Missing_data/tmin_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of range","% Data tmax<tmin","% Data variation> 10","% Consecutive","% Total NA")))
    
  }
  
  if(svalue(variable)=="tmean"){
   # dir.create("Quality control/tmean_atip")
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
   # write.csv(tmean,"Missing_data/tmean.csv")
  }
  
 
  if(svalue(variable)=="precip"){
    #dir.create("Quality control/precip")
    min=sapply(object[-3:-1],mean2)-3*sapply(object[-3:-1],sd2)
    max=sapply(object[-3:-1],mean2)+3*sapply(object[-3:-1],sd2)
    porcentajes=matrix(0,ncol(object)-3,3)
    
    for(i in 1:(ncol(object)-3))
    {
      pos=which(object[,i+3] < min[i] | object[,i+3] >max[i])
      atipicos<-cbind(object[pos,1],object[pos,2],object[pos,3],object[pos,i+3])
      colnames(atipicos)<-c("Day","Month","Year","Value")
      
      #write.csv(atipicos,paste("Quality control/precip_atip/",station[i],"_atipicos.csv",sep=""),row.names=F)
      
      pos2=which(object[,i+3] < as.numeric(svalue(minim)) | object[,i+3] >as.numeric(svalue(maxim)))
      atipicos2<-data.frame(pos2,object[pos2,i+3])
      
      
      na=descriptna(object[,i+3])
      porcentajes[i,]=cbind(round((dim(atipicos)[1]/dim(tmax)[1])*100,2),round((dim(atipicos2)[1]/dim(tmax)[1])*100,2),round((sum(nrow(atipicos2),na)/dim(tmax)[1])*100,2)) 
      post=c(pos,pos2)
      
      #precip[post,i+3]<-NA
      
    }
    #write.csv(precip,"Missing_data/precip_na.csv",row.names=F)
    dimnames(porcentajes)<-c(list(station),list(c("% Atypical data","% Data out of range","% Total NA")))
    
  }
  porcentajes=as.table(porcentajes)
  names(dimnames(porcentajes)) <- c("", paste("Quality control para la variable",svalue(variable))) 
  return(porcentajes)
}

#---------------------------------------------------------------------
####--------------Missing data------------------####
#---------------------------------------------------------------------

datos_falt=function(){
  confirmDialog("This process may take several minutes, press OK to continue","PROCESS STATUS")
  
  year_max <-as.numeric(svalue(year_max)) 
  year_min <- as.numeric(svalue(year_min))
  origin <- paste(1,1,svalue(year_min),sep="-")
  
  valmin <- 1.0
  
  n_GPCA_iter <- 10
  n_GPCA_iteration_residuals <- 10
  n_GPCA_iter_prec <- 20
  n_GPCA_iteration_residuals_prec <- 20
  
  
    tmax<- read.csv("Missing_data/tmax_na.csv", header = TRUE, sep = ",",  dec=".")
    tmin<- read.csv("Missing_data/tmin_na.csv", header = TRUE, sep = ",",  dec=".")
   precip <- read.csv("Missing_data/precip_na.csv", header = TRUE, sep = ",",  dec=".")
    
  
  
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
  
  ComprehensivePrecipitationGenerator=
    function (station = c("T0001", "T0010", "T0099"), prec_all, 
              mean_climate_prec = NULL, year_max = 1990, year_min = 1961, 
              leap = TRUE, nmonth = 12, cpf = NULL, verbose = TRUE, p = 1, 
              type = "none", lag.max = NULL, ic = "AIC", activateVARselect = FALSE, 
              exogen = NULL, exogen_sim = NULL, is_exogen_gaussian = FALSE, 
              year_max_sim = year_max, year_min_sim = year_min, mean_climate_prec_sim = NULL, 
              onlygeneration = FALSE, varmodel = NULL, type_quantile = 3, 
              qnull = NULL, valmin = 0.5, step = 0, n_GPCA_iteration = 0, 
              n_GPCA_iteration_residuals = n_GPCA_iteration, sample = NULL, 
              extremes = TRUE, exogen_all = NULL, exogen_all_col = station, 
              no_spline = FALSE, nscenario = 1, seed = NULL, noise = NULL) 
    {
      useVAR = TRUE
      origin <- paste(year_min, "1", "1", sep = "/")
      origin_sim <- paste(year_min_sim, "1", "1", sep = "/")
      prec_mes <- as.data.frame(extractyears(prec_all, year_min = year_min, 
                                             year_max = year_max, station = station))
      nyear <- year_max - year_min + 1
      if (!is.monthly.climate(mean_climate_prec, nstation = length(station), 
                              nmonth = nmonth, verbose = verbose)) 
        mean_climate_prec <- getMonthlyMean(prec_all, year_min = year_min, 
                                            year_max = year_max, station = station)
      MEAN_CLIMATE_prec_SAVED <- mean_climate_prec
      prec_spline <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec, 
                                                                                  start_year = year_min, nyear = nyear, leap = leap, no_spline = no_spline))
      names(prec_spline) <- names(mean_climate_prec)
      #    prec_spline[prec_spline<=0] = 0.00001
      for(i in 1:dim(prec_spline)[2]){
        if((min(prec_spline[[i]])<=0)){
          prec_spline[[i]][which(prec_spline[[i]]<=0)]=0.001
        }  
      }
      data_prec <- normalizeGaussian_severalstations(x = prec_mes, 
                                                     data = prec_mes, sample = sample, cpf = cpf, step = step, 
                                                     origin_x = origin, origin_data = origin, extremes = extremes)
      if (!onlygeneration) {
        if (!is.null(exogen_all)) {
          exogen <- as.data.frame(extractyears(exogen_all, 
                                               year_min = year_min, year_max = year_max, station = exogen_all_col))
          is_exogen_gaussian = FALSE
        }
        if (is.null(exogen_sim)) 
          exogen_sim <- exogen
        if (!is_exogen_gaussian) {
          exogen0 <- exogen
          if (!is.null(exogen)) 
            exogen <- normalizeGaussian_severalstations(x = exogen0, 
                                                        data = exogen0, sample = sample, cpf = cpf, 
                                                        origin_x = origin, origin_data = origin)
        }
        var <- getVARmodel(data = data_prec, suffix = NULL, 
                           sep = "", p = p, type = type, exogen = exogen, lag.max = lag.max, 
                           ic = ic, activateVARselect = activateVARselect, 
                           n_GPCA_iteration_residuals = n_GPCA_iteration_residuals, 
                           n_GPCA_iteration = n_GPCA_iteration, extremes = extremes)
        if (activateVARselect) 
          return(var)
      }
      else {
        var <- varmodel
      }
      if (!is.null(noise)) {
        if (noise == "residuals") 
          noise <- residuals(var)
      }
      if (is.null(mean_climate_prec_sim)) 
        mean_climate_prec_sim <- mean_climate_prec
      nyear_sim <- year_max_sim - year_min_sim + 1
      nyear_max <- max(nyear_sim, nyear)
      prec_spline_sim <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec_sim, 
                                                                                      start_year = year_min_sim, nyear = nyear_max, leap = leap, 
                                                                                      no_spline = no_spline))
      for(i in 1:dim(prec_spline_sim)[2]){
        if((min(prec_spline_sim[[i]])<=0)){
          prec_spline_sim[[i]][which(prec_spline_sim[[i]]<=0)]=0.001
        }  
      }
      
      prec_spline_sim2 <- as.data.frame(splineInterpolateMonthlytoDailyforSeveralYears(val = mean_climate_prec_sim, 
                                                                                       start_year = year_min_sim, nyear = nyear_sim, leap = leap, 
                                                                                       no_spline = no_spline))
      
      for(i in 1:dim(prec_spline_sim2)[2]){
        if((min(prec_spline_sim2[[i]])<=0)){
          prec_spline_sim2[[i]][which(prec_spline_sim2[[i]]<=0)]=0.001
        }  
      }
      names(prec_spline_sim) <- colnames(mean_climate_prec_sim)
      names(prec_spline_sim2) <- colnames(mean_climate_prec_sim)
      if (is.null(exogen_sim)) 
        exogen_sim <- exogen
      if (!is.null(exogen_sim) & (!is_exogen_gaussian)) {
        exogen_sim0 <- exogen_sim
        exogen_sim <- normalizeGaussian_severalstations(x = exogen_sim0, 
                                                        data = exogen_sim0, sample = sample, cpf = cpf, 
                                                        origin_x = origin_sim, origin_data = origin_sim, 
                                                        extremes = extremes)
      }
      if (!is.null(seed)) 
        set.seed(seed)
      data_prec_gen <- newVARmultieventRealization(var, exogen = exogen_sim, 
                                                   nrealization = nrow(prec_spline_sim2), extremes = extremes, 
                                                   type = type_quantile, noise = noise)
      precrows <- 1:(min(c(nrow(prec_mes), nrow(prec_spline), 
                           nrow(prec_spline_sim))))
      prec_mes_rescaled <- prec_mes[precrows, ]/prec_spline[precrows, 
                                                            ] * prec_spline_sim[precrows, ]
      prec_gen <- as.data.frame(normalizeGaussian_severalstations(x = data_prec_gen, 
                                                                  data = prec_mes_rescaled, inverse = TRUE, type = type_quantile, 
                                                                  step = step, sample = sample, origin_x = origin_sim, 
                                                                  origin_data = origin, extremes = extremes))
      names(prec_gen) <- names(prec_spline_sim)
      colnames(data_prec_gen) <- names(prec_spline_sim)
      out <- NULL
      if (onlygeneration) {
        names_out <- c("prec_gen", "prec_spline_sim", "data_prec_gen", 
                       "mean_climate_prec_sim", "prec_mes", "prec_spline", 
                       "prec_mes_rescaled")
        for (it in names_out) {
          if (!exists(it)) 
            assign(it, NULL)
        }
        out <- list(prec_gen, prec_spline_sim, data_prec_gen, 
                    mean_climate_prec_sim, prec_mes, prec_spline, prec_mes_rescaled)
        names(out) <- names_out
      }
      else {
        names_out <- c("prec_mes", "prec_spline", "data_prec", 
                       "prec_gen", "prec_spline_sim", "data_prec_gen", 
                       "mean_climate_prec", "mean_climate_prec_sim", "var")
        for (it in names_out) {
          if (!exists(it)) 
            assign(it, NULL)
        }
        out <- list(prec_mes, prec_spline, data_prec, prec_gen, 
                    prec_spline_sim, data_prec_gen, mean_climate_prec, 
                    mean_climate_prec_sim, var)
        names(out) <- names_out
      }
      if (nscenario > 1) {
        for (kk in 1:nscenario) {
          data_prec_gen <- newVARmultieventRealization(var, 
                                                       exogen = exogen_sim, nrealization = nrow(prec_spline_sim), 
                                                       extremes = extremes, type = type_quantile)
          colnames(data_prec_gen) <- names(prec_spline_sim)
          precrows <- 1:(min(c(nrow(prec_mes), nrow(prec_spline), 
                               nrow(prec_spline_sim))))
          prec_mes_rescaled <- prec_mes[precrows, ]/prec_spline[precrows, 
                                                                ] * prec_spline_sim[precrows, ]
          prec_gen <- as.data.frame(normalizeGaussian_severalstations(x = data_prec_gen, 
                                                                      data = prec_mes_rescaled, inverse = TRUE, type = type_quantile, 
                                                                      step = step, sample = sample, origin_x = origin_sim, 
                                                                      origin_data = origin, extremes = extremes))
          names(prec_gen) <- names(prec_spline_sim)
          prec_index <- sprintf("prec_gen%05d", kk)
          out[[prec_index]] <- prec_gen
        }
      }
      return(out)
    }
  
  
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
  
  
  ifelse(file.exists("tmax_missing")=="FALSE",dir.create("Missing_data/tmax_missing",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("tmin_missing")=="FALSE",dir.create("Missing_data/tmin_missing",showWarnings=F),"Ya existe carpeta")
  ifelse(file.exists("precip_missing")=="FALSE",dir.create("Missing_data/precip_missing",showWarnings=F),"Ya existe carpeta")
  
  tmax1=ts(tmax,start=min(tmax$year),frequency=365)
  tmin1=ts(tmin,start=min(tmin$year),frequency=365)
  precip1=ts(precip,start=min(precip$year),frequency=365)
  
  data_genTmax1=ts(data_genTmax,start=min(tmax$year),frequency=365)
  data_genTmin1=ts(data_genTmin,start=min(tmin$year),frequency=365)
  data_genPrec1=ts(data_genPrec,start=min(precip$year),frequency=365)
  
    
  for(i in 1:(ncol(data_genTmax)-3))
  {
    jpeg(paste("Missing_data/tmax_missing/tmax_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(as.zoo(data_genTmax1[,i+3]),main=paste("tmax_",station[i]),type="l",xlab="Years",ylab="Max. Temp",col="blue",lwd=1.5,cex.lab=1.5)
    lines(tmax1[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generated","Original"),lwd=c(1.5,1.5),col=c("blue","red"),bty="n")
    dev.off()
    
      
    jpeg(paste("Missing_data/tmin_missing/tmin_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(as.zoo(data_genTmin1[,i+3]),main=paste("tmin_",station[i]),type="l",xlab="Years",ylab="Min. Temp",col="blue",lwd=1.5,cex.lab=1.5)
    lines(tmin1[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generated","Original"),lwd=c(1.5,1.5),col=c("blue","red"),bty="n")
    dev.off()
    
    jpeg(paste("Missing_data/precip_missing/prec_",station[i],".jpeg",sep=""),width = 1150, height = 500)
    plot(as.zoo(data_genPrec1[,i+3]),main=paste("prec_",station[i]),type="l",xlab="Years",ylab="Precipitation",col="blue",lwd=1.5,cex.lab=1.5)
    lines(precip1[,i+3],col="red",lwd=1.5)
    legend("topright",c("Generated","Original"),lwd=c(1.5,1.5),col=c("blue","red"),bty="n")
    dev.off()
    
        
     tmax.na=which(is.na(tmax[,i+3]))
     tmax[tmax.na,i+3]<-data_genTmax[tmax.na,i+3]
    
      
    tmin.na=which(is.na(tmin[,i+3]))
    tmin[tmin.na,i+3]<-data_genTmin[tmin.na,i+3]
    
    
    precip.na=which(is.na(precip[,i+3]))
    precip[precip.na,i+3]<-data_genPrec[precip.na,i+3]
         
  }
  
  write.csv(tmax,"Missing_data/data_genTmax.csv",row.names =F)
  write.csv(tmin,"Missing_data/data_genTmin.csv",row.names =F)
  write.csv(precip,"Missing_data/data_genPrec.csv",row.names =F)
  
  confirmDialog(paste("The process is complete. The results will be saved in",getwd(),"/Missing_data"),"PROCESS STATUS ")
  
}


#---------------------------------------------------------------------
####------------Homogeneity-------------####
#---------------------------------------------------------------------

#--Gráficos normalidad--#
graf_norm=function(){
  confirmDialog(paste("The QQ-norm graphs will be saved in",getwd(),"/Homogeneity"),"Location files")
  
  ifelse(file.exists("Homogeneity")=="FALSE",dir.create("Homogeneity",showWarnings=F),"Ya existe carpeta Homogeneity")
  
object=eval(parse(text=svalue(nom_val2)))

station=names(object[-3:-1])

if(svalue(nom_val2)=="tmax"){
dir.create("Homogeneity/tmax_norm",showWarnings=F)
  
for(i in 1:(ncol(object)-3))
{
  jpeg(paste("Homogeneity/tmax_norm/tmax_",station[i],".jpeg",sep=""),width = 800, height = 800)
  
  qqnorm(object[,i+3],main=paste(station[i]))
  qqline(object[,i+3],col="red",lwd=2)
  dev.off()
}}


if(svalue(nom_val2)=="tmin"){
  
  dir.create("Homogeneity/tmin_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneity/tmin_norm/tmin_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  
}

if(svalue(nom_val2)=="tmean"){
  
  dir.create("Homogeneity/tmean_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneity/tmean_norm/tmean_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  }

if(svalue(nom_val2)=="precip"){
  
  dir.create("Homogeneity/precip_norm",showWarnings=F)
  
  for(i in 1:(ncol(object)-3))
  {
    jpeg(paste("Homogeneity/precip_norm/precip_",station[i],".jpeg",sep=""),width = 800, height = 800)
    
    qqnorm(object[,i+3],main=paste(station[i]))
    qqline(object[,i+3],col="red")
    dev.off()
  }
  
}
  
  
  
}


#--Prueba de  normalidad: Shapiro--#
shapTest <- function(Estaciones){
  
  norm=shapiro.test(Estaciones)$p.value
  norms=ifelse(norm<svalue(obj),"NO Normal","NR")
  return(norms)
}
shap=function(x){
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
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
  ks=ifelse(norm<svalue(obj),"No Normality","NR")
#   result=rbind(norm,ks)
#   names(result)<-c("Valor-p","Decisión")
  return(paste(norm,ks,sep=" - "))
}
KS_test=function(object){

  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
    }else {data=object}
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmax)],2,KS)     
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmin)],2,KS)     
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(precip)],2,KS)     
  }
  
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
    }else{data1=x}

   
  Without_miss_data=apply(data1[,4:ncol(data1)],2,KS)
  result=as.table(cbind(With_miss_data,Without_miss_data))
  names(dimnames(result)) <- c("Station", "Kolmogorov_Smirnov (p-valor/ Desicion)") 
  
  return(result)
}

#--Prueba de  normalidad: JB--#
JB<-function(Estaciones){
  #serie=na.omit(Estaciones)
  JBt=jarque.bera.test(Estaciones)$p.value
  jb=ifelse(JBt<svalue(obj),"No Normality","NR")
  return(paste(round(JBt,2),jb,sep=" - "))
}
JB2=function(x){
    
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    #With_miss_data=apply(tmax[,4:ncol(tmax)],2,JB)
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    #With_miss_data=apply(tmin[,4:ncol(tmin)],2,JB)
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    #With_miss_data=apply(precip[,4:ncol(precip)],2,JB)
  }
  
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {x=cultivoh(x)
  }else{x=x}
  
  
  Without_miss_data=apply(x[,4:ncol(x)],2,JB)
  result=as.table(cbind(Without_miss_data))
  names(dimnames(result)) <- c("Station", "Jarque_Bera (p-valor/ Desicion)") 
  
  return(result)
}

Spearman<-function(Estaciones){
  
  stat=Estaciones
  
  Rsp=round(cor(rank(stat),rank(sort(stat)),method="spearman"),4 ) ###Correlación de Rangos de Spearman
  T_t= abs(Rsp*((length(stat)-2)/(1-(Rsp^2)))^(1/2))      ###Estadistico T
  critico=qt(svalue(obj)/2,length(stat)-2,lower.tail=F)
  vp=pt(T_t,length(stat)-2,lower.tail=F)###Valor crítico  
  sp=ifelse(T_t>critico,"Trend","NR")
  
  return(paste(sp))
  #T_t > critico: Se rechaza Ho, es decir que hay tendencia en la serie
  
}
Rsp_Test<- function(x){
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {x=cultivoh(x)
  }else{x=x}
    
  Without_miss_data=apply(x[,4:ncol(x)],2,Spearman)
  result=as.table(cbind(Without_miss_data))
  names(dimnames(result)) <- c("Station", "Spearman (p-valor/ Desicion)") 
    
return(result)}

#--Mann Kendall test--#

Kend_Test <- function(Estaciones){
  stat=Estaciones
  Vcal=MannKendall(stat)$sl[1]     ###Tendencia
  
  vp=ifelse(Vcal<svalue(obj),"Trend","NR")
  return(paste(round(Vcal,2),vp,sep=" - "))
  
}
Ken_T <- function(object){
 
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmax)],2,Kend_Test)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmin)],2,Kend_Test)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(precip)],2,Kend_Test)
    }
    
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Without_miss_data=apply(x[,4:ncol(x)],2,Kend_Test)
  result=as.table(cbind(With_miss_data,Without_miss_data))
  names(dimnames(result)) <- c("Station", "Mann_Kendall (p-valor/ Desicion)") 
  
   return(result)
}


par.f <- function(Estaciones){
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=var.test(part1,part2)$p.value
  
  f=ifelse(valp<svalue(obj),"Var. isn't stable","NR")
  return(paste(round(valp,2),f,sep=" - "))
  
}
F_test.indic <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmax)],2,par.f)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmin)],2,par.f)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(precip)],2,par.f)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Without_miss_data=apply(x[,4:ncol(x)],2,par.f)
  result=as.table(cbind(With_miss_data,Without_miss_data))
  names(dimnames(result)) <- c("Station", "Test_F (p-valor/ Desicion)") 
  
 return(result)}


PruebaT=function(Estaciones){
  
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=t.test(part1,part2)$p.value
  s=ifelse(valp<svalue(obj),"Different mean","NR")
  return(paste(round(valp,2),s,sep=" - "))
  }
T_Test <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmax)],2,PruebaT)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmin)],2,PruebaT)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(precip)],2,PruebaT)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Without_miss_data=apply(x[,4:ncol(x)],2,PruebaT)
  result=as.table(cbind(With_miss_data,Without_miss_data))
  names(dimnames(result)) <- c("Station", "Test_t (p-valor/ Desicion)") 
  
  return(result)}

#--Test U Mann Whitney o Wilcoxon: version no paramétrica--#
#--de la prueba T (No requiere normalidad)--#

Umann <- function(Estaciones){
  
  stat=Estaciones
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  
  valp=wilcox.test(part1,part2)$p.value
  u=ifelse(valp<svalue(obj),"Different mean","NR")
  return(paste(round(valp,2),u,sep=" - "))
}
Umann_Test <- function(object){
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data=cultivoh(object)
  }else {data=object}
  
  
  if(svalue(nom_val2)=="tmax"){
    x<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmax)],2,Umann)
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    x<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(tmin)],2,Umann)
    
  }
  
  if(svalue(nom_val2)=="precip"){
    x <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
    With_miss_data=apply(data[,4:ncol(precip)],2,Umann)
    
  }
  if(svalue(mesh)!="" && svalue(mesh1)!="" && svalue(añoh)!="" && svalue(añoh1)!="") {data1=cultivoh(x)
  }else{data1=x}
  
  Without_miss_data=apply(x[,4:ncol(x)],2,Umann)
  result=as.table(cbind(With_miss_data,Without_miss_data))
  names(dimnames(result)) <- c("Station", "Test_U_Mann_Whitney (p-valor/ Desicion)") 
  
  return(result)}

#---------------------------------------------------------------------
####-------------------Indicators-----------------####
#---------------------------------------------------------------------

diasmayor=function(valor){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  
  d=data.frame(ifelse(object>valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year),sum)
  
  dir.create("Indicators",showWarnings=F)
  write.csv(x,paste("Indicators/Ind_higher_",svalue(nom_val3),".csv"),row.names=F)
  return(x)
}

diasmenor=function(valor){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  d=data.frame(ifelse(object<valor,1,0))
  x=aggregate(d[-3:-1],list(Año=object$year),sum)
  
  dir.create("Indicators",showWarnings=F)
  write.csv(x,paste("Indicators/Ind_lower_",svalue(nom_val3),".csv"),row.names=F)
  return(x)
}

tempmax=function(object){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
 result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),max2)
 
  dir.create("Indicators",showWarnings=F)
  write.csv(result,paste("Indicators/Ind_max_",svalue(nom_val3),".csv"),row.names=F)
 return(result)
 }

tempmin=function(object){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
 result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),min2)
  
  dir.create("Indicators",showWarnings=F)
  write.csv(result,paste("Indicators/Ind_min_",svalue(nom_val3),".csv"),row.names=F)
  return(result)
}

tempmean=function(object){
  if(svalue(nom_val3)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val3)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  if(svalue(mesi)!="" && svalue(mesi1)!="" && svalue(añoi)!="" && svalue(añoi1)!="") {object=cultivoi(object)
  }else{object=object}
  
  result=aggregate(object[-3:-1],list(Mes=object$month,Año=object$year),mean2)
  
  dir.create("Indicators",showWarnings=F)
  write.csv(result,paste("Indicators/Ind_mean_",svalue(nom_val3),".csv"),row.names=F)
  return(result)
}

#---------------------------------------------------------------------
####-----------------------ENSO--------------------####
#---------------------------------------------------------------------

enso=function(){
  
  enso_mes=read.csv(paste(dir,"/ENSO/","Enso_monthly.csv",sep=""),header=T,row.names=1)
  enso_mes_ok=melt(t(enso_mes),id=enso_mes$Year_Month)
  colnames(enso_mes_ok)<-c("Month","Year","Enso")
  
  x=subset(enso_mes_ok,as.numeric(svalue(año))<=Year & Year<=as.numeric(svalue(año1)))
  todos=seq(0,12*(as.numeric(svalue(año1))-as.numeric(svalue(año))+1),12)
  mesfin=seq(todos[length(todos)]-11,todos[length(todos)],1)
  mesfin=mesfin[as.numeric(svalue(mes1))]
  Condicion_ENSO=x[as.numeric(svalue(mes)):mesfin,]
  
  return(Condicion_ENSO)
}

enso1=function(){
  
  enso_trim=read.csv(paste(dir,"/ENSO/","Enso_quart.csv",sep=""),header=T,row.names=1)
  enso_trim_ok=melt(t(enso_trim),id=enso_trim$Year_trim)
  colnames(enso_trim_ok)<-c("Quart","Year","Enso")
  
  x=subset(enso_trim_ok,as.numeric(svalue(año2))<=Year & Year<=as.numeric(svalue(año3)))
  todos=seq(0,4*(as.numeric(svalue(año3))-as.numeric(svalue(año2))+1),4)
  trimfin=seq(todos[length(todos)]-3,todos[length(todos)],1)
  trimfin=trimfin[as.numeric(svalue(trim1))]
  Condicion_ENSO=x[as.numeric(svalue(trim)):trimfin,]
  
  return(Condicion_ENSO)
}

#---------------------------------------------------------------------
####---------------Reports--------------####
#---------------------------------------------------------------------

inf=function(h,...){ #Crea y genera informe en word
  confirmDialog(paste("The final report will be saved in",getwd()),"Location files")
  
  
  if(svalue(nom_val2)=="tmax"){
    object<- read.csv("Missing_data/data_genTmax.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="tmin"){
    object<- read.csv("Missing_data/data_genTmin.csv", header = TRUE, sep = ",",  dec=".")
    
  }
  
  if(svalue(nom_val2)=="precip"){
    object <- read.csv("Missing_data/data_genPrec.csv", header = TRUE, sep = ",",  dec=".")
  }
  
  info<-RTF(svalue(nom_arch),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  
 addHeader(info,title="Application for analysis of climatic series",subtitle=paste("Report for the variable",svalue(nom_val2)),font.size=14)
  
  descriptiva=as.table(descript2(object))
  names(dimnames(descriptiva)) <- c("", "Station") 
  años=paste(min(object[3]),max(object[3]),sep="-")
  addNewLine(info)
  addNewLine(info)
  
  addHeader(info,title="1. Descriptive analysis",font.size=12)
  addNewLine(info)
  addParagraph(info, paste("The following report analyzes the",svalue(nom_val2), "variable for the period",años))
  
  addNewLine(info)
  addParagraph(info, "Table 1.  Descriptive Statistics") 
  addTable(info, descriptiva, font.size=8, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)

  addHeader(info,title="2. Homogeneity analysis",font.size=12)
  addNewLine(info)
  addParagraph(info,paste("An analysis of homogeneity for the series of different seasons is shown. For all tests the level of significance of ",svalue(obj)," was used 
NOTE: Remember that NR indicates that the null hypothesis is NOT Rejected "))
  
  
  
  addNewLine(info)
  addHeader(info,title="2.1 Testing for normality",font.size=11)
  addNewLine(info)
  addParagraph(info, "Null hypothesis: The station data follow a normal distribution") 
  
  addNewLine(info)
  addParagraph(info, "Table 2.1.1  Kolmogorov Smirnov-test") 
  kolm=KS_test(object)
 # names(dimnames(kolm)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, kolm, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Table 2.1.2 Jarque Bera-test") 
  jb=JB2(object)
  #names(dimnames(jb)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, jb, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addHeader(info,title="2.2 Testing for trend",font.size=11)
  addNewLine(info)
  
  addParagraph(info, "Null hypothesis: The station data don't follow a trend  ") 
  
  addNewLine(info)
  
  addParagraph(info, "Table 2.2.1  Spearman's rank correlation") 
  rsp=Rsp_Test(object)
 # names(dimnames(rsp)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, rsp, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Table 2.2.2  Mann Kendall-test") 
  mk=Ken_T(object)
  #names(dimnames(mk)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info, mk, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  
  addHeader(info,title="2.3 Testing for variance stability",font.size=11)
  addNewLine(info)
  addParagraph(info, "Null hypothesis: The station data are stable in variance") 
  
  addNewLine(info)
  
  
  addParagraph(info, "Table 2.3.1 F-test") 
  ft=F_test.indic(object)
  #names(dimnames(ft)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info,ft, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addHeader(info,title="2.4 Testing for mean stability",font.size=11)
  addNewLine(info)
  
  addParagraph(info, "Null hypothesis: The station data are stable in mean") 
  
  addNewLine(info)
  
  addParagraph(info, "Table 2.4.1 T-test") 
  tt=T_Test(object)
  #names(dimnames(tt)) <- c("Estación", "p-valor/Decisión") 
  
  addTable(info,tt, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addParagraph(info, "Table 2.4.2 U Mann Whitney-test") 
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
  confirmDialog(paste("The pre-report will be saved in",getwd()),"Location files")
  
  info<-RTF(svalue(nom_arch1),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  
  addHeader(info,title="Application for analysis of climatic series",subtitle="Pre-report",font.size=14)
  
  descriptiva=as.table(descript2(eval(parse(text=svalue(variable)))))
  names(dimnames(descriptiva)) <- c("", "Station") 
  
  
  años=paste(min(tmax[,3]),max(tmax[,3]),sep="-")
  addNewLine(info)
  addNewLine(info)
  
  addHeader(info,title="1. Descriptive analysis",font.size=12)
  addNewLine(info)
  
  addParagraph(info, paste("The following report analyzes the",svalue(variable), "variable for the period",años))
  
  addNewLine(info)
  addParagraph(info, "Tabla 1.1  Descriptive Statistics") 
  addTable(info, descriptiva, font.size=8, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
   
  validacion=as.table(validar22(eval(parse(text=svalue(variable)))))
  names(dimnames(validacion)) <- c("Station", "") 
  
  
  addNewLine(info)
  
  addHeader(info,title="2. Quality control ",font.size=12)
  addNewLine(info)
  
  startParagraph(info) 
  addParagraph(info,"Now a validation for different seasons under study is performed, which consists of calculating several criteria that help identify outliers and/or erroneous data for later correction. \n")
  addParagraph(info,paste("For the identification of outliers ",svalue(criterio)," standard deviations were used and a range of values established ",svalue (minim)," <", svalue (variable)," <", svalue (maxim),". Note: The data identified in this section are replaced by NA's, except for those identified as outliers and those with higher temperature variation at 10°C. The last column of the following tables show the total of missing data to be filled in the missing data section. It is recommended that this % does not exceed 20% in each of the stations."))
  endParagraph(info)     
  addNewLine(info)
  
   addParagraph(info, paste("Table 2.1  Validation for ",svalue(variable))) 
  addTable(info, validacion, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addNewLine(info)
  
  addNewLine(info)
  addNewLine(info) 
  addSessionInfo(info) 
  done(info)
  
  
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

lyt=glayout(homogeneous =T,cont=nb,spacing=1,label="1. Data reader",expand=T) 

lyt[1,1:3]=(g=gframe("Data reader",container=lyt,horizontal = T))

lytgb=glayout(homogeneous =T,cont=g,spacing=1,expand=T) 


lytgb[1,1]=(h=gbutton("Change directory",container=lytgb,handler = function(h,...)setwd(gfile(text="Select directory",type="selectdir"))))

lytgb[2,1]=(glabel=(""))
lytgb[3,1]=(glabel=("Load data:"))

lytgb[4,1]=gbutton("Max. Temp",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmax()})


lytgb[5,1]=gbutton("Min. Temp",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_tmin()})

# lytgb[5,1]=gbutton("Temp. Media",container=lytgb, expand=F, 
#                    handler =function(h,...) {cargar_tmean()})


lytgb[6,1]=gbutton("Precipitation",container=lytgb, expand=F, 
                   handler =function(h,...) {cargar_precip()})


#----------------------------------------------------------------
####--------------------Analisis descriptivo----------------#####
#----------------------------------------------------------------
lyt1=glayout(homogeneous =F,cont=nb,spacing=1,label="2. Graphical and descriptive analysis",expand=T) 


lyt1[1,1:6]=(g1=gframe("Descriptive analysis",container=lyt,expand=T,horizontal=F))
lytg2=glayout(homogeneous =F,cont=g1,spacing=1,expand=T) 


lytg2[1,1]=glabel("-Period of analysis",container=lytg2)
lytg2[2,1]=glabel("From",container=lytg2)
lytg2[2,2]=(mesd=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[2,3]=(añod=gedit("",cont=lytg2,expand=F,width =10,initial.msg="YYYY"))

lytg2[3,1]=glabel("To",container=lytg2)
lytg2[3,2]=(mesd1=gedit("",cont=lytg2,expand=F,width =7,initial.msg="M"))
lytg2[3,3]=(añod1=gedit("",cont=lytg2,expand=F,width =10,initial.msg="YYYY"))

lytg2[4,1]=glabel("",container=lytg2)

lytg2[5,1]=glabel("-Variable to be analyzed  ",container=lytg2)
lytg2[5,2]=(nom_val1=gdroplist(c("tmax","tmin","precip"),selected = 0,cont=lytg2,expand=T,handler=function(h,...){attach(eval(parse(text=svalue(h$obj))))}))
lytg2[5,3]=gbutton("Descriptive",container=lytg2,handler=function(h,...){print(descript2(eval(parse(text=svalue(nom_val1)))))})

lyt1[2,1:6]=glabel("")


#---------------------------------------------------------------------
###------------------------Analisis grafico------------------------###
#---------------------------------------------------------------------
lyt1[3,1]=(g.2=gframe("Automated graphics: ",container=lyt1 ,horizontal=F,expand=T))
lytg.1=glayout(homogeneous =F,cont=g.2,spacing=1,expand=T) 

lytg.1[1,1]=glabel("Type of analysis")
lytg.1[1,2]=(tipo=gdroplist(c("Daily","Monthly"),selected=1,cont=lytg.1))

lytg.1[2,1]=glabel("",cont=lytg.1)

lytg.1[3,1]=gbutton(" Plot ",container=lytg.1,
                    handler = function(h,...){graf_plot() } )

lytg.1[4,1]=gbutton(" Boxplot",container=lytg.1,
                    handler = function(h,...){graf_box() } )


lytg.1[5,1]=gbutton("Scatter plot",container=lytg.1,
                    handler = function(h,...){graf_disp() } )

lyt1[5,1:6]=glabel("")


lyt1[6,1]=(g2=gframe("Custom graphics: ",container=lyt1 ,horizontal=F,expand=T))
lytg1=glayout(homogeneous =F,cont=g2,spacing=1,expand=T) 

lytg1[1,1]=glabel("Classic",container=lytg1)
lytg1[2,1]=gbutton("  Plot",container=lytg1,
                   handler = function(h,...){plot2() } )

lytg1[3,1]=gbutton("Histogram",container=lytg1,
                   handler = function(h,...){hist2()} )

lytg1[4,1]=gbutton(" Boxplot",container=lytg1,
                   handler = function(h,...){boxplot2()} )

lytg1[1,2]=glabel("      ",container=lytg1)
lytg1[1,3]=glabel("P. ggplot2",container=lytg1)
lytg1[2,3]=gbutton("  Plot",container=lytg1,
                   handler = function(h,...){qplot2()} )

lytg1[3,3]=gbutton("Histogram",container=lytg1,
                   handler = function(h,...){hist_22()} )




#---------------------------------------------------------------
###----------------Seccion Quality control----------------###
#---------------------------------------------------------------

lyt4=glayout(homogeneous =T,cont=nb,spacing=2,label="3. Quality control ",expand=T) 

lyt4[1,1]=(gg=gframe("Validation",cont=lyt4,horizontal=F))
lytg=glayout(homogeneous =F,cont=gg,spacing=1,expand=T)

lytg[1,1]=glabel("",cont=lytg)

lytg[2,1]=glabel("Variable to be analyzed",container=lytg)
lytg[2,2]=(variable<-gdroplist(c("tmax","tmin","precip"),selected=0,cont=lytg,expand=T))

lytg[3,1]=glabel("Number of standard deviations: ",container=lytg)
lytg[3,2]=(criterio=gedit("3",container=lytg,width = 5,initial.msg="Desv.Est."))

lytg[4,1]=glabel("Variable range:",container=lytg)
lytg[4,2]=(minim=gedit("",container=lytg,width = 7,initial.msg="Min."))
lytg[4,3]=(maxim=gedit("",container=lytg,width = 10,initial.msg="Max."))

lytg[5,1]=glabel("",cont=lytg)
lytg[6,2]=gbutton("Validate",container=lytg,handler=function(h,...){print(validar2(eval(parse(text=svalue(variable)))))})
lytg[6,3]=gbutton("Graphics",container=lytg,handler=function(h,...){grafcontrol(eval(parse(text=svalue(variable))))})

lyt4[2,1]=(gg.3=gframe("Pre-Report",cont=lyt4,horizontal=F))
lytgg.3=glayout(homogeneous =F,cont=gg.3,spacing=1,expand=T)

lytgg.3[1,1]=glabel("Filename: ",container=lytgg.3)
lytgg.3[1,2]=(nom_arch1 <-gedit("pre-report.doc",container=lytgg.3,coerce.with=as.character)) #Genera un label editable para el nombre del archivo del informe

lytgg.3[2,2]=gbutton("Generate pre-report",cont=lytgg.3,handler=function(h,...)inf2())

#------------------------------------------------------------
###----------------Missing data---------------###
#------------------------------------------------------------

lyt3=glayout(homogeneous =F,cont=nb,spacing=1,label="4. Missing data",expand=T) 

lyt3[1,1]=glabel("Initial year: ",container=lyt3)
lyt3[1,2]=(year_min <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe

lyt3[2,1]=glabel("Final year: ",container=lyt3)
lyt3[2,2]=(year_max <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe


lyt3[7,1]=gbutton("Complete data",container=lyt3,handler = function(h,...){datos_falt()},expand=T)

lyt3[8,1:4]=gtext("Note: To perform this function, you must have minimum temperature, maximum and precipitation data",container=lyt3,coerce.with=as.character)

#-------------------------------------------------------------------
###----------------Seccion homogeneidad de series----------------###
#-------------------------------------------------------------------
lyt5=glayout(homogeneous =F,cont=nb,spacing=2,label="5. Homogeneity analysis",expand=T) 

lyt5[1,1]=(gg.1=gframe("Definition of parameters:",cont=lyt5,horizontal=F))

lyt55=glayout(homogeneous =F,cont=gg.1,spacing=2,expand=T) 
lyt55[1,1]=glabel("-Period of analysis",container=lyt55)
lyt55[2,1]=glabel("From",container=lyt55)
lyt55[2,2]=(mesh=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[2,3]=(añoh=gedit("",cont=lyt55,expand=F,width =10,initial.msg="YYYY"))

lyt55[3,1]=glabel("To",container=lyt55)
lyt55[3,2]=(mesh1=gedit("",cont=lyt55,expand=F,width =7,initial.msg="M"))
lyt55[3,3]=(añoh1=gedit("",cont=lyt55,expand=F,width =10,initial.msg="YYYY"))

lyt55[4,1]=glabel("")
lyt55[5,1]=glabel("-Variable to be analyzed",container=lyt55)

lyt55[5,2]=(nom_val2=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt55,expand=T))

lyt55[6,1]=glabel("-Level of significance",container=lyt55)

lyt55[6,2]=(obj <- gspinbutton(from=0, to = 0.3, by =0.01, value=0.05,
                               container=lyt55)) #Idea para seleccionar nivel de confianza




lyt5[2,1]=glabel("")
lyt5[3,1]=(gg2=gframe("Test for normality:",cont=lyt5,horizontal=F))
norm_test1=gcheckbox("Shapiro-Wilk",cont=gg2,handler=function(h,...){print(shap(eval(parse(text=svalue(nom_val2)))))})
norm_test2=gcheckbox("Kolmogorov-Smirnov",cont=gg2,handler=function(h,...){print(KS_test(eval(parse(text=svalue(nom_val2)))))})
norm_test3=gcheckbox("Jarque-Bera ",cont=gg2,handler=function(h,...){print(JB2(eval(parse(text=svalue(nom_val2)))))})

gbutton("QQ-norm graphics",cont=gg2,handler=function(h,...){graf_norm()},width=5)

lyt5[4,1]=glabel("")
lyt5[5,1]=(gg3=gframe("Test for trend:",cont=lyt5,horizontal=F))
tend_test=gcheckbox("Spearman's rank correlation",cont=gg3,handler=function(h,...){print(Rsp_Test(eval(parse(text=svalue(nom_val2)))))})
tend_test1=gcheckbox("Mann-Kendall",cont=gg3,handler=function(h,...){print(Ken_T(eval(parse(text=svalue(nom_val2)))))})

lyt5[6,1]=glabel("")
lyt5[7,1]=(gg4=gframe("Test for variance stability:",cont=lyt5,horizontal=F))
estv_test=gcheckbox("F-test",cont=gg4,handler=function(h,...){print(F_test.indic(eval(parse(text=svalue(nom_val2)))))})

lyt5[8,1]=glabel("")
lyt5[9,1]=(gg5=gframe("Test for mean stability:",cont=lyt5,horizontal=F))
estm_test=gcheckbox("T-test",cont=gg5,handler=function(h,...){print(T_Test(eval(parse(text=svalue(nom_val2)))))})

estm_test1=gcheckbox("U Mann - Whitney test ",cont=gg5,handler=function(h,...){print(Umann_Test(eval(parse(text=svalue(nom_val2)))))})

lyt5[1,2]=glabel("   ")
lyt5[1,3]=(gg.2=gframe("Report",cont=lyt5,horizontal=F))
lyt.1=glayout(homogeneous =F,cont=gg.2,spacing=2,expand=T) 


lyt.1[1,1]=glabel("Filename: ",container=lyt.1)
lyt.1[1,2]=(nom_arch <-gedit("report.doc",container=lyt.1,coerce.with=as.character,width = 15)) #Genera un label editable para el nombre del archivo del informe
# lyt.1[2,1]=glabel("Seleccione la variable:",container=lyt.1)
# lyt.1[2,2]=(nom_val=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt.1,expand=T))

lyt.1[2,2]=gbutton("Generate Report",container=lyt.1,handler = function(h,...){inf()},expand=T)

#---------------------------------------------------------------------
###------------------------Seccion Indicadores---------------------###
#---------------------------------------------------------------------

lyt2=glayout(homogeneous =F,cont=nb,spacing=2,label="6. Indicator calculations",expand=T) 


lyt2[1,1]=(gg.3=gframe("Indicators:",cont=lyt2,horizontal=F))
lyt.2=glayout(homogeneous =F,cont=gg.3,spacing=2,expand=T) 

lyt.2[1,1]=glabel("-Period of analysis",container=lyt55)
lyt.2[2,1]=glabel("From",container=lyt.2)
lyt.2[2,2]=(mesi=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[2,3]=(añoi=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="YYYY"))

lyt.2[3,1]=glabel("To",container=lyt.2)
lyt.2[3,2]=(mesi1=gedit("",cont=lyt.2,expand=F,width =7,initial.msg="M"))
lyt.2[3,3]=(añoi1=gedit("",cont=lyt.2,expand=F,width =10,initial.msg="YYYY"))

lyt.2[4,1]=glabel("",container=lyt.2)

lyt.2[5,1]=glabel("-Variable to be analyzed",container=lyt55)

lyt.2[5,2]=(nom_val3=gdroplist(c("tmax","tmin", "precip"),selected=0,cont=lyt.2,expand=F))
lyt.2[5,3]=glabel("     ",container=lyt55)

lyt.2[6,1]=glabel("-Annual indicators:",container=lyt.2)

lyt.2[7,1]=gcheckbox("# days higher than",container=lyt.2,handler = function(h,...){print(diasmayor(as.numeric(svalue(valor1))))})
lyt.2[7,2]=(valor1=gedit("",container=lyt.2,width = 1))

lyt.2[8,1]=gcheckbox("# days lower than",container=lyt.2,handler = function(h,...){print(diasmenor(as.numeric(svalue(valor2))))})
lyt.2[8,2]=(valor2=gedit("",container=lyt.2,width = 1))


lyt.2[9,1]=glabel("",container=lyt.2)

lyt.2[10,1]=glabel("-Monthly indicators:",container=lyt.2)


lyt.2[11,1]=gcheckbox("Maximun",container=lyt.2,handler = function(h,...){print(tempmax())})
lyt.2[12,1]=gcheckbox("Minimum",container=lyt.2,handler = function(h,...){print(tempmin())})
lyt.2[13,1]=gcheckbox("Average",container=lyt.2,handler = function(h,...){print(tempmean())})

#---------------------------------------------------------------------
###---------------------Seccion Condición ENSO---------------------###
#---------------------------------------------------------------------


lyt22=glayout(homogeneous =F,cont=nb,spacing=2,label="7. ENSO condition",expand=T) 

lyt22[1,1]=(gg.4=gframe("ENSO condition",cont=lyt22,horizontal=F))
lyt.3=glayout(homogeneous =T,cont=gg.4,spacing=3,expand=T) 

lyt.3[1,1]=glabel("-Monthly",container=lyt.3)
lyt.3[2,1]=glabel("From:",container=lyt.3)
lyt.3[2,2]=(mes=gdroplist(c("Month",1:12),selected=1,cont=lyt.1,expand=T))
lyt.3[2,3:4]=(año=gdroplist(c("Year",1950:2013),selected=1,cont=lyt.1,expand=T))

lyt.3[3,1]=glabel("To:",container=lyt.3)
lyt.3[3,2]=(mes1=gdroplist(c("Month",1:12),selected=1,cont=lyt.1,expand=T))
lyt.3[3,3:4]=(año1=gdroplist(c("Year",1950:2013),selected=1,cont=lyt.1,expand=T))
lyt.3[4,2:3]=gbutton("Monthly consultation",container=lyt.3,handler = function(h,...){print(enso())},expand=T)

#lyt.3[5,1]=glabel(" ",container=lyt.3)
lyt.3[6,1]=glabel("-Quarterly",container=lyt.3)
lyt.3[7,1]=glabel("From:",container=lyt.3)
lyt.3[7,2]=(trim=gdroplist(c("Quart",1:4),selected=1,cont=lyt.1,expand=T))
lyt.3[7,3:4]=(año2=gdroplist(c("Year",1950:2013),selected=1,cont=lyt.1,expand=T))

lyt.3[8,1]=glabel("To:",container=lyt.3)
lyt.3[8,2]=(trim1=gdroplist(c("Quart",1:4),selected=1,cont=lyt.1,expand=T))
lyt.3[8,3:4]=(año3=gdroplist(c("Year",1950:2013),selected=1,cont=lyt.1,expand=T))
lyt.3[9,2:3]=gbutton("Quarterly consultation",container=lyt.3,handler = function(h,...){print(enso1())},expand=T)


#---------------------------------------------------------------------
###---------------------------Welcolme---------------------------###
#---------------------------------------------------------------------

g5=ggroup(container=nb,horizontal = F,label="Welcome",cont=nb)

#Para el logo modifique la ruta en "dirname="en la cual se encuentra la imagen
#gimage("logo", size="menu", container=g5,label="Bienvenid@",width=700,heigth=700) #Inserta imagen "ciat.png"en la ventana
gimage("logo_en.png",dirname=dir, size="menu", container=g5,label="Welcome",width=700,heigth=700) #Inserta imagen "ciat.png"en la ventana

mensj=gtext("Welcome",cont=g5)
insert(mensj, " to RClimTool, an application designed for analyzing climatic series ( Minimun Temperature, Maximun Temperature, Temperature and Precipitation )")

visible(win) = T
focus(win)
