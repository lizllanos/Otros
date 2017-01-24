
##################################################################
#--------------Interfaz avance 1.2 -analisis descriptivo-----------#
##################################################################

##################################################################
###Paquetes necesarios para correr la aplicación####
##################################################################

require(ggplot2)
require(rtf)
require(gdata)
require(gWidgets)
require(gWidgetsRGtk2)
require(RMAWGEN)
options("guiToolkit"="RGtk2")

##################################################################
####Funciones para validar datos#####
##################################################################
fun1 <-function(h,...){ #Función que imprime la accion que se le haya indicado en la funcion
  print(do.call(h$action, list(get(svalue(h$obj)))))
} 


cargar=function(){
datos=read.table(gfile(),header=T,sep="")
attach(datos)
}
#-----------------------------------------------#




##################################################################
####Funciones para realizar Analisis descriptivo#####
##################################################################

leer<-function(){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  gdroplist(items,selected = 0,action="descript",handler=fun1,cont=g1,expand=T)
}


descript <- function(object){ #Función para descriptivas de las variables
  Media=mean(object, na.rm=T);  Varianza=var(object, na.rm=T);  Desv.Est=sqrt(Varianza)
  Mediana=as.numeric(quantile(object,probs=0.5, na.rm=T));  Coef.Var=sqrt(Varianza)/Media
  Mín=min(object,na.rm=T); Máx=max(object,na.rm=T)
  Datos.NA=sum(is.na(object))
  result=round(cbind(Mín,Máx,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA),3)
    return(result)
}


##################################################################
###Funciones para generar graficos clásicos#####
##################################################################


plot2=function(){ #Genera gráfico plot
  plot.default.LIST <- list(
    title = "plot.default",
    help = "plot.default",
    action = list(
      beginning = "x11();plot.default(", 
      ending = ")"),
    arguments = list(
      "x"=list(
        type="gdroplist",
        items=ls(datos),
        editable =T
      ),
      "y"=list(
        type="gdroplist",
        items=ls(datos),
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
  hist.LIST <- list(
    title = "hist",
    help = "hist",
    action = list(
      beginning = "x11();hist(", 
      ending = ")"),
    arguments = list(
      "x"=list(
        type="gdroplist",
        items=ls(datos),
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

boxplot2=function(){
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
      items=ls(datos),
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
        items=ls(datos),
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

##################################################################
###Funciones para generar graficos con ggplot2#####
##################################################################

hist_22=function(){ #genera grafico hist con ggplot2
  qplot.LIST <- list(
    title = "qplot",
    help = "qplot",
    
    action = list(
      beginning = "x11();qplot(", 
      ending = ")"),
    arguments = list(
      
      "x"=list(
        type="gdroplist",
        items=ls(datos),
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
  qplot.LIST <- list(
    title = "qplot",
    help = "qplot",
    
    action = list(
      beginning = "x11();qplot(", 
      ending = ")"),
    arguments = list(
      
      
      "x"=list(
        type="gdroplist",
        items=ls(datos),
        editable=T
      ),
      "y"=list(
        type="gdroplist",
        items=ls(datos),
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
# myresiduals=rnorm(100)
# myresiduals=as.data.frame(qqnorm( myresiduals , plot=F))
# qplot(x,y,data=myresiduals,geom=c("point","smooth"))


##################################################################
##Funciones para control de calidad######
##################################################################
leer2<-function(h,...){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  lytg[6,2]=(nom_val=gdroplist(items,selected = 0,handler=fun1,action="validar",cont=lytg,expand=T))
}


confirmDialog <- function(message, handler=NULL) {
  
  window <- gwindow("Validación",width=100,height=100)
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

validar<-function(object){
  val=ifelse(svalue(tipo)=="Precipitación",ifelse(min(object,na.rm=T)>=svalue(min) & max(object,na.rm=T)<=svalue(max),"está dentro del rango permitido","contiene valores por fuera del rango permitido"),ifelse(min(object,na.rm=T)>=svalue(min) & max(object,na.rm=T)<=svalue(max),"está dentro del rango permitido","contiene valores por fuera del rango permitido"))
  confirmDialog(paste("La variable",svalue(tipo),val,sep=" "))
  
  if(svalue(tipo)!="Precipitación"){
  min=mean(object,na.rm=T)-as.numeric(svalue(criterio))*sd(object,na.rm=T)
  max=mean(object,na.rm=T)+as.numeric(svalue(criterio))*sd(object,na.rm=T)
  x11()
  
  datos=data.frame(1:length(object),object)
  plot(datos,ylab="Days",xlab=svalue(tipo))
  abline(h=min,col="red")
  abline(h=max,col="red") 
  atipicos <- with(datos, datos[datos$object < min | datos$object >max, ])
  points(atipicos,col="red",pch=16)
  
    } 
  
}


# write.csv(data_genPrec,"data_genPrec.csv")
# ifelse(file.exists("tmax")=="FALSE",dir.create("tmax"),"Ya existe carpeta")


##################################################################
##Funciones para generacion informe######
##################################################################

inf=function(h,...){ #Crea y genera informe en word
  
  info<-RTF(svalue(nom_arch),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  #info<-RTF("infor.doc",width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  
  names_var=ls(datos)
  datos.new=as.data.frame(sapply(parse(text=names_var), eval))
  names(datos.new)<-names_var
  data1=as.matrix(datos.new)
  
  addHeader(info,title="Aplicativo para análisis de series climatológicas",subtitle="Análisis Descriptivo")
  
  descriptiva=as.table(summary(datos.new))
  
  
  addTable(info, descriptiva, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
  addParagraph(info, "Table 1.  Estadísticas Descriptivas") 
  p1=which(names(datos.new)==names_var[5])
  p2=which(names(datos.new)=="month")
  
  
  addPlot(info, plot.fun=plot.default, width=5, height=5, res=300,data1[,p1],main="") #Inserta gráfico
  addNewLine(info) 
  addParagraph(info, "Gráfico 1.  Plot ")
  
  addPlot(info, plot.fun=hist.default , width=5, height=5, res=300,data1[,p1],main="") #Inserta gráfico
  addNewLine(info) 
  addParagraph(info, "Gráfico 2.  Histograma ")
  
  mimedia<-function(x) mean(x, na.rm=TRUE) 
  promedio=cbind(tapply(data1[,p1],data1[,p2],FUN=mimedia))
  
  
  Mes=1:length(promedio)
  histo=qplot(Mes,promedio,geom=c("point", "line"),xlim = c(1, 12))
  print(histo) 
  
  addPlot(info, plot.fun=print , width=5, height=5, res=300,histo) #Inserta gráfico
  addNewLine(info) 
  addParagraph(info, "Gráfico 3.  Serie promedio mensual con ggplot2")
  
  addNewLine(info) 
  addSessionInfo(info) 
  done(info)
  
}


#####################################################################################
#####---------CUERPO INTERFAZ GRÁFICA-----------#######
#####################################################################################


win <- gwindow("Aplicacion CIAT Version 1.1.0", visible=T) #Crea ventana inicial
nb = gnotebook(cont=win,expand=T,tab.pos = 2)
g5=ggroup(container=nb,horizontal = F,label="Bienvenid@",cont=nb)
gimage("ciat3.png",dirname="C:/Users/lllanos/Desktop/Dctos Lizeth/Logos", size="dialog", container=g5,label="Bienvenid@") #Inserta imagen "ciat.png"en la ventana
mensj=gtext("Bienvenid@",cont=g5,height=550)
insert(mensj, "esta es una aplicación diseñada para el análisis de series climatológicas")



##################################################################
###Lectura y validacion de datos ###
##################################################################

lyt=glayout(homogeneous =F,cont=nb,spacing=1,label="1. Lectura y validación de datos ",expand=T) 

lyt[1,1:3]=(g=gframe("Lectura de datos",container=lyt,horizontal = T))

lytgb=glayout(homogeneous =F,cont=g,spacing=1,expand=T) 


lytgb[1,1]=glabel("Cambiar directorio",container=lytgb)
lytgb[1,2]=(h=gbutton(". . .",container=lytgb,handler = function(h,...)setwd(gfile(type="selectdir"))))

lytgb[2,1]=glabel("Abrir archivo",container=lytgb)

lytgb[2,2]=gimage("carpeta.png",dirname="C:/Users/lllanos/Desktop",size="button",handler = function(h,...)cargar(),cont=lytgb)

# gbutton("Seleccionar archivo",container=g, expand=T, #Crea botón anidado a la función "fun"
#         handler =function(h,...) {cargar()})

lyt[2,1:5]=glabel("",cont=lyt)
                  



##################################################################
####Analisis descriptivo#####
##################################################################
lyt1=glayout(homogeneous =F,cont=nb,spacing=1,label="2. Análisis gráfico y descriptivo",expand=T) 


lyt1[1,1:6]=(g1=gframe("Análisis descripivo",container=lyt,expand=T))
gbutton("Seleccionar variable",container=g1,
        handler = function(h,...) leer())

lyt1[2,1:6]=glabel("")


##################################################################
###Analisis grafico#####
##################################################################

lyt1[3,1:3]=(g2=gframe("Gráficos: ",container=lyt1 ,horizontal=F,expand=T))
lytg1=glayout(homogeneous =F,cont=g2,spacing=1,expand=T) 

lytg1[1,1]=gbutton("Gráfico Plot",container=lytg1,
        handler = function(h,...){plot2() } )

lytg1[1,2]=gbutton("Histograma",container=lytg1,
        handler = function(h,...){hist2()} )

lytg1[2,1]=gbutton("QQ-norm",container=lytg1,
        handler = function(h,...){qqnorm2()} )

lytg1[2,2]=gbutton("Gráfico Boxplot",container=lytg1,
                   handler = function(h,...){boxplot2()} )


lyt1[4,1]=glabel("")


##################################################################
####Graficos prueba de ggplot2
##################################################################

lyt1[5,1:3]=(g2=gframe("Gráficos ggplot2: ",container=lyt1 ,horizontal=F,expand=T))
lytg2=glayout(homogeneous =F,cont=g2,spacing=1,expand=T) 

lytg2[1,1]=gbutton("Gráfico Plot",container=lytg2,
        handler = function(h,...){qplot2()} )

lytg2[1,2]=gbutton("Histograma",container=lytg2,
        handler = function(h,...){hist_22()} )

##################################################################
##Seccion control de calidad######
##################################################################
lyt4=glayout(homogeneous =T,cont=nb,spacing=2,label="3. Control de calidad ",expand=T) 

lyt4[1,1]=(gg=gframe("Validación",cont=lyt4,horizontal=F))
lytg=glayout(homogeneous =F,cont=gg,spacing=1,expand=T)

lytg[1,1]=glabel("",cont=lytg)

lytg[2,1]=glabel("Seleccione tipo de variable: ",container=lytg)
lytg[2,2]=(tipo=gdroplist(c("Precipitación","Temp. Media","Temp. Máx","Temp. Mín"),selected = 0,cont=lytg,expand=T,coerce.with=as.character))

lytg[3,1]=glabel("Rango de la variable:",container=lytg)
lytg[3,2]=(min=gedit("",container=lytg,width = 7,initial.msg="Mín."))
lytg[3,3]=(max=gedit("",container=lytg,width = 15,initial.msg="Máx."))

lytg[5,1]=glabel("No. de desviaciones estándar:",container=lytg)
lytg[5,2]=(criterio=gedit("3",container=lytg,width = 5,initial.msg="Desv.Est."))


lytg[6,1]=gbutton("Variable a validar",container=lytg,handler=leer2)

##################################################################
##Seccion homogeneidad de series######
##################################################################
lyt5=glayout(homogeneous =F,cont=nb,spacing=2,label="4. Homogeneidad",expand=T) 


##########################################################
##Seccion datos faltantes######
##################################################################

lyt3=glayout(homogeneous =F,cont=nb,spacing=1,label="5. Datos faltantes",expand=T) 

lyt3[1,1]=glabel("Año inicial: ",container=lyt3)
lyt3[1,2]=(year_min <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe

lyt3[2,1]=glabel("Año final: ",container=lyt3)
lyt3[2,2]=(year_max <-gedit("",container=lyt3,width = 10,initial.msg="AAAA")) #Genera un label editable para el nombre del archivo del informe


lyt3[3,1]=glabel("Fecha inicial",container=lyt3,coerce.with=as.character)
lyt3[3,2]=(origin <-gedit("",initial.msg="AAAA",container=lyt3,width = 10)) 
lyt3[3,3]=(origin1 <-gedit("",initial.msg="M",container=lyt3,width = 10)) 
lyt3[3,4]=(origin2 <-gedit("",initial.msg="D",container=lyt3,width = 10)) 


 lyt3[4,1]=gbutton("Seleccionar archivo tmax",container=lyt3, expand=F, #Crea botón anidado a la función "fun"
         handler =function(h,...) {leerdatos()})

 
 lyt3[5,1]=gbutton("Seleccionar archivo tmin",container=lyt3, expand=F, #Crea botón anidado a la función "fun"
                     handler =function(h,...) {leerdatos2()})
 
 lyt3[6,1]=gbutton("Seleccionar archivo prec",container=lyt3, expand=F, #Crea botón anidado a la función "fun"
                     handler =function(h,...) {leerdatos3()})
 

lyt3[7,1]=gbutton("Completar datos",container=lyt3,handler = function(h,...){datos_falt()},expand=T)


##################################################################
##Seccion generacion informe######
##################################################################
lyt2=glayout(homogeneous =F,cont=nb,spacing=2,label="6. Generar Informe ",expand=T) 


lyt2[1,1:3]=glabel("Nombre del archivo:",container=lyt2)
lyt2[1,4]=(nom_arch <-gedit("informe.doc",container=lyt2,coerce.with=as.character)) #Genera un label editable para el nombre del archivo del informe

lyt2[2,1:3]=glabel("",container=lyt2)

lyt2[3:5,1:4]=gbutton("Generar informe",container=lyt2,handler = function(h,...){inf()},expand=T)


visible(win) = T
focus(win)

#cat(gWidgets:::autogenerategeneric(geom_boxplot))
