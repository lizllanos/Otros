
##################################################################
#--------------Interfaz avance 1.2 -analisis descriptivo-----------#
##################################################################

##################################################################
###Paquetes necesarios para correr la aplicaci�n####
##################################################################

 require(ggplot2)
 require(rtf)
 require(gdata)
 require(gWidgets)
 require(gWidgetsRGtk2)
 options("guiToolkit"="RGtk2")

##################################################################
####Funciones para validar datos#####
##################################################################
fun1 <-function(h,...){ #Funci�n que imprime la accion que se le haya indicado en la funcion
  print(do.call(h$action, list(get(svalue(h$obj)))))
} 

validar1<-function(){
  val=ifelse(svalue(tipo)=="Precipitaci�n",ifelse(min(sapply(parse(text=svalue(object)),eval),na.rm=T)>=0 & max(sapply(parse(text=svalue(nom_val)),eval),na.rm=T)<=3500,"Correcto","Datos at�picos"),ifelse(min(sapply(parse(text=svalue(nom_val)),eval),na.rm=T)>=0 & max(sapply(parse(text=svalue(nom_val)),eval),na.rm=T)<=50,"Correcto","Datos at�picos"))
print(val)}

validar<-function(object){
  val=ifelse(svalue(tipo)=="Precipitaci�n",ifelse(min(object,na.rm=T)>=0 & max(object,na.rm=T)<=3500,"Correcto","Datos at�picos"),ifelse(min(object,na.rm=T)>=0 & max(object,na.rm=T)<=50,"Correcto","Datos at�picos"))
  }


##################################################################
####Funciones para realizar Analisis descriptivo#####
##################################################################

leer<-function(){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  gdroplist(items,selected = 0,action="descript",handler=fun1,cont=g1,expand=T)
    }

leer2<-function(h,...){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  lytg[3,2]=(nom_val=gdroplist(items,selected = 0,handler=fun1,action="validar",cont=lytg,expand=T))
    }

leer3<-function(h,...){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  lyt2[4,1:3]=(nom_var2=gdroplist(items,selected = 0,cont=lyt2,expand=F,coerce.with=as.character,handler=function(h,...)print(nom_var2)))
    }

leer4<-function(h,...){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  lyt2[4,4:6]=(nom_var3=gdroplist(items,selected = 0,cont=lyt2,expand=T,coerce.with=as.character))
}


descript <- function(object){ #Funci�n para descriptivas de las variables
    Media=mean(object, na.rm=T);  Varianza=var(object, na.rm=T);  Desv.Est=sqrt(Varianza)
    Mediana=as.numeric(quantile(object,probs=0.5, na.rm=T));  Coef.Var=sqrt(Varianza)/Media
    M�n=min(object,na.rm=T); M�x=max(object,na.rm=T)
    Datos.NA=sum(is.na(object))
    result=round(rbind(M�n,M�x,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA),3)
    return(result)
              }


##################################################################
###Funciones para generar graficos cl�sicos#####
##################################################################


plot2=function(){ #Genera gr�fico plot
plot.default.LIST <- list(
  title = "plot.default",
  help = "plot.default",
  action = list(
    beginning = "x11();plot.default(", 
    ending = ")"),
  arguments = list(
    "x"=list(
      type="gdroplist",
      items=ls(datos)
    ),
    "y"=list(
      type="gdroplist",
      items=ls(datos)
    ),
    "type"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'p'"),
    "xlab"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'NULL'"),
    "ylab"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'NULL'"),
    
    "xlim"=list(
      type= "gedit",
      text="NULL"),
    "ylim"=list(
      type= "gedit",
      text="NULL"),
    
    "main"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'Grafico'"),
    
    "..."=list(
      type= "gedit",
      text="")))
ggenericwidget(plot.default.LIST,cont=gwindow("Gr�fico Plot"))
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
      items=ls(datos)
    ),
    "xlab"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'NULL'"),
    "ylab"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'Frecuencia'"),
    
    "main"=list(
      type= "gedit",
      coerce.with= as.character,
      text="'Grafico'"),
    
    "..."=list(
      type= "gedit",
      text="")))

ggenericwidget(hist.LIST,container=gwindow("Gr�fico Histograma")) 
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
      items=ls(datos)
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
      text="'Sample Quantiles'"),
    
    "..."=list(
      type= "gedit",
      text="")))

ggenericwidget(qqnorm.default.LIST,container=gwindow("Gr�fico QQ-norm"))

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
        items=ls(datos)
      ),
       
      "geom"=list(
        type= "gedit",
        coerce.with= as.character,
        text="'auto'"),
      
      "main"=list(
        type= "gedit",
        text="NULL"),
      "xlab"=list(
        type= "gedit",
        text=""),
      "ylab"=list(
        type= "gedit"),
      
      "..."=list(
        type= "gedit",
        text="")))
  ggenericwidget(qplot.LIST,container=gwindow("Gr�fico Histograma ggplot2"))
  
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
      items=ls(datos)
    ),
    "y"=list(
      type="gdroplist",
      items=ls(datos)
    ), 
    "geom"=list(
      type= "gedit",
      coerce.with= as.character,
      text="c('point','line')"),
    
    "main"=list(
      type= "gedit",
      text="NULL"),
    "xlab"=list(
      type= "gedit",
      text=""),
    "ylab"=list(
      type= "gedit"),
    
    "..."=list(
      type= "gedit",
      text="")))
ggenericwidget(qplot.LIST,container=gwindow("Gr�fico Plot ggplot2"))

}

##################################################################
##Funciones para generacion informe######
##################################################################

inf=function(h,...){ #Crea y genera informe en word
  
 info<-RTF(svalue(nom_arch),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))

 names_var=ls(datos)
 datos.new=as.data.frame(sapply(parse(text=names_var), eval))
 names(datos.new)<-names_var
 data1=as.matrix(datos.new)
 
 addHeader(info,title="Aplicativo para an�lisis de series climatol�gicas",subtitle="An�lisis Descriptivo")
 
 descriptiva=as.table(summary(datos.new))
 
 
 addTable(info, descriptiva, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
 addParagraph(info, "Table 1.  Estad�sticas Descriptivas") 
  p1=which(names(datos.new)==svalue(nom_var2))
  p2=which(names(datos.new)==svalue(nom_var3))
  
  
 addPlot(info, plot.fun=plot.default, width=5, height=5, res=300,data1[,p1],main="") #Inserta gr�fico
 addNewLine(info) 
 addParagraph(info, "Gr�fico 1.  Plot ")
 
 addPlot(info, plot.fun=hist.default , width=5, height=5, res=300,data1[,p1],main="") #Inserta gr�fico
 addNewLine(info) 
 addParagraph(info, "Gr�fico 2.  Histograma ")
 
  mimedia<-function(x) mean(x, na.rm=TRUE) 
  promedio=cbind(tapply(data1[,p1],data1[,p2],FUN=mimedia))
  
   
  Mes=1:length(promedio)
  histo=qplot(Mes,promedio,geom=c("point", "line"),xlim = c(1, 12))
  print(histo) 
 
  addPlot(info, plot.fun=print , width=5, height=5, res=300,histo) #Inserta gr�fico
  addNewLine(info) 
  addParagraph(info, "Gr�fico 3.  Serie promedio mensual con ggplot2")

 addSessionInfo(info) 
 done(info)
 
    }

 
#####################################################################################
#####---------CUERPO INTERFAZ GR�FICA-----------#######
#####################################################################################
# 
# win <- gwindow("Aplicacion CIAT Version 1.1.0", visible=T,width=200,height=500) #Crea ventana inicial
# group <- ggroup(horizontal = FALSE, container=win) #Es �til para organizar el contenido de la ventana
# nb = gnotebook(cont=win)
# gimage("ciat.png", dirname="C:/Users/lllanos/Desktop/Dctos Lizeth", size="small_toolbar", container=nb) #Inserta imagen "ciat.png"en la ventana
# 
# lyt=glayout(homogeneous =F,cont=nb,spacing=1,label="") #Crea un arreglo layout para ubicar botones y labels
# 
# 
# ##################################################################
# ###Lectura de datos###
# ##################################################################
# 
# 
# lyt[1,1:3]=(g=gframe("1. Lectura de datos",container=lyt))
# gbutton("Seleccionar archivo",container=g, expand=T, #Crea bot�n anidado a la funci�n "fun"
#         handler =function(h,...) {datos=read.table(gfile(),header=T,sep="")
#                                   attach(datos)})
# 
# lyt[2,1:3]=glabel("")
# 
# ##################################################################
# ####Analisis descriptivo#####
# ##################################################################
# 
# 
# lyt[3,1:3]=(g1=gframe("2. An�lisis descripivo",container=lyt))
# gbutton("Seleccionar variable",container=g1,
#         handler = function(h,...) leer())
# 
# lyt[4,1:3]=glabel("")
# 
# ##################################################################
# ###Analisis grafico#####
# ##################################################################
# 
# 
# lyt[5,1:3]=(g2=gframe("3. Gr�ficos ",container=lyt ,horizontal=F))
# 
# gbutton("Gr�fico Plot",container=g2,
#         handler = function(h,...){plot2() } )
# 
# gbutton("Histograma",container=g2,
#         handler = function(h,...){hist2()} )
# 
# gbutton("QQ-norm",container=g2,
#         handler = function(h,...){qqnorm2()} )
# 
# 
# lyt[6,1:3]=glabel("")
# 
# 
# ##################################################################
# ####Graficos prueba de ggplot2
# ##################################################################
# 
# lyt[7,1:3]=(g2=gframe("4. Gr�ficos ggplot2",container=lyt ,horizontal=F))
# 
# gbutton("Gr�fico Plot",container=g2,
#         handler = function(h,...){qplot2()} )
# 
# gbutton("Histograma",container=g2,
#         handler = function(h,...){hist_22()} )
# lyt[8,1:3]=glabel("")
# 
# 
# ##################################################################
# ##Seccion generacion informe######
# ##################################################################
# 
# 
# lyt[9,1:3]=(g3=gframe("5. Informe general",container=lyt,horizontal=F))
# 
# nom_arch <-gedit("informe.doc",container=g3,coerce.with=as.character) #Genera un label editable para el nombre del archivo del informe
# nom_var <-gedit("X35015050",container=g3,coerce.with=as.character)
# gbutton("Generar informe",container=g3,handler = function(h,...){inf()})
# nom_var2=gdroplist(leer2(),selected = 0,cont=g3,expand=T,coerce.with=as.character)
# 
# visible(win) = T


#####################################################################################
##FIN###
#####################################################################################

#cat(gWidgets:::autogenerategeneric(qplot))

