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

a=gwindow(visible=FALSE)  ##Inicializa la ventana pero no la despliaga
g=ggroup(container=a,horizontal=F)
size(a)=c(200,150)
f= function(h,...){
  svalue(respuesta)=as.numeric(svalue(s1))+as.numeric(svalue(s2))
}
gl1=glabel("Ingresa Primer Valor",container=g)
font(gl1)=list(size=10)
s1=gtext(container=g,height=2,width=30)
addSpace(g,5,horizontal=F)
gl2=glabel("Ingresa Segundo Valor",container=g)
font(gl2)=list(size=10)
s2=gtext(container=g,height=2,width=30)
addSpace(g,5,horizontal=F)
b=gbutton("Realizar Suma",container=g,handler=f)
addSpace(g,2,horizontal=F)
respuesta=glabel("",container=g)
font(respuesta)=list(size=10)
visible(a) = T
focus(a) = T


