##########################
##Ejemplo 1 - Hola Mundo##
##########################

require(gWidgets)
require(gWidgetstcltk)

a=gwindow()
2
f= function(h,...){print("Hola Mundo")}
b=gbutton("No presionar este boton",container=a,handler=f)


###################
##Ejemplo 2- Suma##
###################

a=gwindow(visible=F)  ##Inicializa la ventana pero no la despliaga
g=ggroup(container=a,horizontal=F) #Se indica el orden para agrupar los containers
size(a)=c(200,150) #Sirve para cambiar el tamano de la ventana inicial

f= function(h,...){
  svalue(respuesta)=as.numeric(svalue(s1))+as.numeric(svalue(s2)) #crea funcion para sumar dos valores
}

gl1=glabel("Ingresa Primer Valor",container=g) #Crea linea de texto
font(gl1)=list(size=10) #Tamano de la letra del texto

s1=gtext(container=g,height=2,width=30) #Agrega cuadro de texto
addSpace(g,5,horizontal=F) #Agrega espacio entre los containers

gl2=glabel("Ingresa Segundo Valor",container=g)
font(gl2)=list(size=10)

s2=gtext(container=g,height=2,width=30)
addSpace(g,5,horizontal=F)

b=gbutton("Realizar Suma",container=g,handler=f) #Crea boton anidado a la función creada anteriormente
addSpace(g,2,horizontal=F)

respuesta=glabel("",container=g)

font(respuesta)=list(size=10)
visible(a) = T
focus(a) = T

#######

lmwidget <- ggenericwidget(lm) #sirve para crear una interfaz de una función ya establecida

