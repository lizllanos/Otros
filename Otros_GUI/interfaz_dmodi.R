

##################################################################
###Paquetes necesarios para correr la aplicación####
##################################################################

require(ggplot2)
 require(rtf)
 require(gdata)
 require(gWidgets)
 require(gWidgetsRGtk2)
 options("guiToolkit"="RGtk2")


##################################################################
####Interfaz avance 1.2 -analisis descriptivo
##################################################################


win <- gwindow("Aplicacion CIAT Version 1.1.0", visible=T) #Crea ventana inicial
group <- ggroup(horizontal = FALSE, container=win) #Es útil para organizar el contenido de la ventana
gimage("ciat.png", dirname="C:/Users/lllanos/Desktop/Dctos Lizeth", size="small_toolbar", container=group) #Inserta imagen "ciat.png"en la ventana

lyt=glayout(homogeneous =F,cont=group,spacing=1) #Crea un arreglo layout para ubicar botones y labels

##################################################################
###Lectura de datos###
##################################################################


lyt[1,1:3]=(g=gframe("1. Lectura de datos",container=lyt))

gbutton("Seleccionar archivo",container=g, expand=T, #Crea botón anidado a la función "fun"
                              handler =function(h,...) {datos=read.table(gfile(),header=T,sep="")
                                                        attach(datos)})




##################################################################
####Analisis descriptivo#####
##################################################################

fun1 <-function(h,...){ #Función que lista las variables del conjunto de datos
      print(do.call(h$action, list(get(svalue(h$obj)))))
          } 

leer<-function(){  #Funcion para crear lista de variables para el analisis descriptivo
  items=ls(datos)
  gdroplist(items,selected = 0,action="descript",handler=fun1,cont=g1,expand=T)
}

descript <- function(object){ #Función para descriptivas de las variables
    Media=mean(object, na.rm=T);  Varianza=var(object, na.rm=T);  Desv.Est=sqrt(Varianza)
    Mediana=as.numeric(quantile(object,probs=0.5, na.rm=T));  Coef.Var=sqrt(Varianza)/Media
    Mín=min(object,na.rm=T); Máx=max(object,na.rm=T)
    Datos.NA=length(object)-nobs(object)
    result=rbind(Mín,Máx,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA)
    return(result)
              }

#  listavar <- function() { #Aquí se da la opción que se desee realizar al dar doble click, para este caso "descript"
#      listOfObjects <- ls(datos)
#       gtable(listOfObjects, container = gwindow("Seleccione la variable"),action="descript",handler=fun1)
#              }

 lyt[3,1:3]=(g1=gframe("2. Análisis descripivo",container=lyt))
  gbutton("Seleccionar variable",container=g1,
         handler = function(h,...) leer())


##################################################################
###Analisis grafico#####
##################################################################


#####Funciones para generar graficos###

plot2=function(){
plot.default.LIST <- list(
  title = "plot.default",
  help = "plot.default",
  #variableType = "bivariate",
  #assignto = FALSE,
  action = list(
    beginning = "plot.default(", 
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
ggenericwidget(plot.default.LIST,cont=gwindo("Gráfico Plot"))

}

hist2=function(){
hist.LIST <- list(
  title = "hist",
  help = "hist",
  #variableType = "univariate",
  #assignto = FALSE,
  action = list(
    beginning = "hist(", 
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

ggenericwidget(hist.LIST,container=gwindow("Gráfico Histograma")) 
}

qqnorm2=function(){
qqnorm.default.LIST <- list(
  title = "qqnorm.default",
  help = "qqnorm.default",
  variableType = "",
  # assignto = FALSE,
  action = list(
    beginning = "qqnorm.default(", 
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

ggenericwidget(qqnorm.default.LIST,container=gwindow("Gráfico QQ-norm"))

}

####################################################################

lyt[5,1:3]=(g2=gframe("3. Gráficos ",container=lyt ,horizontal=F))
            
gbutton("Gráfico Plot",container=g2,
                 handler = function(h,...){plot2() } )

gbutton("Histograma",container=g2,
        handler = function(h,...){hist2()} )

gbutton("QQ-norm",container=g2,
        handler = function(h,...){qqnorm2()} )


##########################funciones para ggplot2#################################

qplot2=function{
qplot.LIST <- list(
  title = "qplot",
  help = "qplot",
  
  action = list(
    beginning = "qplot(", 
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
      text="")
  ))
ggenericwidget(qplot.LIST,container=gwindow("Gráfico Histograma ggplot2")) } )

}

##################################################################
##Seccion generacion informe######
##################################################################
inf=function(h,...){
  
  
 info<-RTF(svalue(nom_arch),width=8.5,height=11,font.size=10,omi=c(1,1,1,1))

 names_var=ls(datos)
 datos.new=as.data.frame(sapply(parse(text=names_var), eval))
 names(datos.new)<-names_var
 data1=as.matrix(datos.new)
 
 addHeader(info,title="Aplicativo para análisis de series climatológicas",subtitle="Análisis Descriptivo")
 
 descriptiva=as.table(summary(datos.new))
 
 
addTable(info, descriptiva, font.size=10, row.names=TRUE, NA.string="-") #Inserta la tabla creada anteriormente)
 addParagraph(info, "Table 1.  Estadísticas Descriptivas") 
  p1=which(names(datos.new)=="Value")
  p2=which(names(datos.new)=="Month")
  
  
 addPlot(info, plot.fun=plot.default, width=5, height=5, res=300,data1[,p1]) #Inserta gráfico
 addNewLine(info) 
 addParagraph(info, "Gráfico 1.  Plot ")
 
 addPlot(info, plot.fun=hist.default , width=5, height=5, res=300,data1[,p1]) #Inserta gráfico
 addNewLine(info) 
 addParagraph(info, "Gráfico 2.  Histograma ")
 
  mimedia<-function(x) mean(x, na.rm=TRUE) 
  promedio=cbind(tapply(data1[,p1],data1[,p2],FUN=mimedia))
  
   
  ejex=seq(1,length(promedio),1)
  histo=qplot(promedio,ejex,geom=c("point", "line"))
 print(histo) 
 
  addPlot(info, plot.fun=print , width=5, height=5, res=300,histo) #Inserta gráfico
  addNewLine(info) 
  addParagraph(info, "Gráfico 3.  Histograma con ggplot2")

 addSessionInfo(info) 
 done(info)
 
    }

lyt[7,1:3]=(g3=gframe("4. Informe general",container=lyt))
 
nom_arch <-gedit("informe.doc",container=g3,coerce.with=as.character) #Genera un label editable para el nombre del archivo del informe
gbutton("Generar informe",container=g3,handler = function(h,...){inf()})
 

##################################################################
####Graficos prueba de ggplot2
##################################################################

lyt[8,1:3]=(g2=gframe("3. Gráficos ggplot2",container=lyt ,horizontal=F))

gbutton("Histograma",container=g2,
        handler = function(h,...){qplot2() } )

visible(win) = T


#####################################################################################
#####Cuerpo de la interfaz#######
#####################################################################################





#####################################################################################
##FIN###
#####################################################################################

cat(gWidgets:::autogenerategeneric(qplot))

