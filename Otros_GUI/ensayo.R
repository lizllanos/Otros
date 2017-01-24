
require(gWidgets)
require(gWidgetstcltk)

a=gwindow()

grp_name <- ggroup(container = a) #Crea un container

lbl_data_frame_name <- glabel( #Le da nombre al container creado anteriormente (grp_name)
  "Ubicación del archivo ",
  container = grp_name
)

txt_data_frame_name <- gedit("", container = grp_name) #Crea cuadro de texto en el container, "texto inicial"

grp_upload <- ggroup(container = grp_upload) #Agrupar los containers


b=gbutton("Cargar datos",container=a,handler=function(h, ...) 
  {
  # TODO!
  }
)



read.table("C:\Users\darango\Desktop\Dctos lizeth\station-example\13055030_prec.txt",header=T,sep=".")

datos <- read.delim("C:/Users/darango/Desktop/Dctos lizeth/station-example/13055030_tmax.txt")
View(datos)
attach(datos)

ensayo= function(x){
  prom=mean(Value,na.rm=T)
  var_1=var(Value,na.rm=T)
  desv_1=sqrt(var_1)
  qtile=as.numeric(quantile(Value,na.rm=T,0.5))
  
  return(cbind(prom,var_1,desv_1,qtile))
}


###################
##NUEVO LIBRO##
###################


require(gWidgets)
require(gWidgetsRGtk2)
options(guiToolkit="RGtk2")



a=gwindow(visible=T)  ##Inicializa la ventana pero no la despliaga
paned=gpanedgroup(cont=a) #construye un panel
g=ggroup(container=paned,horizontal=F) #Se indica el orden para agrupar los containers
#size(a)=c(200,150) #Sirve para cambiar el tamano de la ventana inicial

gl1=glabel("Buscar en:",container=g) #Crea linea de texto
buscador=gfilebrowse("Seleccione el directorio...",type="selectdir",cont=g,quote=F) #Abre una ventana para buscar un archivo

b=gbutton("Cargar datos",container=g)
addSpring(g) #Dividir paneles?

panel=gframe("Salida:",cont=g,horizontal=F)
resultados=gtext("",cont=panel,expan=T)
size(resultados)=c(350,200)



new=ggroup(container=a,horizontal=T)

f1= function(h,...){
  svalue(respuesta)=as.numeric(mean(Value,na.rm=T)) #crea funcion para sumar dos valores
}


addSpace(new,2,horizontal=F)
c=gbutton("Realizar descriptivas",container=new,handler=f1) #Crea boton anidado a la función creada anteriormente f1
addSpace(g,2,horizontal=F)

respuesta=glabel("",container=new)

font(respuesta)=list(size=10)
#visible(a) = T
#focus(a) = T

#######

fix(lmwidget <- ggenericwidget(summary) #sirve para crear una interfaz de una función ya establecida
    
