
####Interfaz avance 1.0 -analisis descriptivo

require(gWidgets)
require(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")

win <- gwindow("Aplicacion CIAT Version 1.0.0", visible=TRUE) #Crea ventana inicial

group <- ggroup(horizontal = FALSE, container=win) #Es útil para organizar el contenido de la ventana
gimage("ciat.png", dirname="C:/Users/darango/Desktop", size="small_toolbar", container=group) #Inserta imagen "ciat.png"en la ventana

gl1=glabel("Buscar en:",container=group) #Crea linea de texto

fileChoose <- function(action="print", text = "Select a file...",type="open", ...) { #Función para seleccionar directorio de trabajo
                      gfile(text=text, type=type, ..., action = action, handler =
                      function(h,...) {
                      do.call(h$action, list(h$file))
                        })}


obj <- gbutton("Seleccionar Directorio",container=group, #Crea botón anidado a la función "fun"
               handler =function(h,...) {
                 fileChoose(action="setwd", type="selectdir", text="Select a directory...")})


nom_arch <-gedit("",initial.msg="Nombre del archivo",container=group,coerce.with=as.character) #Crea cuadro de texto editable para colocar el nombre del archivo que contiene los datos

fun <-function(h,...) { #Función para leer y cargar datos
      base=svalue(nom_arch)
      datos=read.table(base,header=T,sep="")
      attach(datos)
        }

obj <- gbutton("Cargar Datos",container=group, #Crea botón anidado a la función "fun"
               handler =fun )

fun1 <-function(h,...){
      print(do.call(h$action, list(get(svalue(h$obj)))))} #Función que lista las variables del conjunto de datos

listavar <- function(envir = .GlobalEnv, pattern) { #Aquí se da la opción que se desee realizar al dar doble click, para este caso "summary"
          listOfObjects <- ls(datos)
          gtable(listOfObjects, container = gwindow("Seleccione la variable"),action="summary", 
                 handler=fun1)
              }

obj <- gbutton("Realizar descriptivas",container=group,
               handler =function(h,...) {listavar()})

obj <- gbutton("Gráficos....falta",container=group,
              handler = function(h,...){print(summary(Value))})
              # handler = function(h,...){svalue(respuesta)=summary(datos[-1])})


######################


####Interfaz avance 1.1 -analisis descriptivo


require(gWidgets)
require(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")

win <- gwindow("Aplicacion CIAT Version 1.1.0", visible=FALSE) #Crea ventana inicial

group <- ggroup(horizontal = FALSE, container=win) #Es útil para organizar el contenido de la ventana
gimage("ciat.png", dirname="C:/Users/darango/Desktop", size="small_toolbar", container=group) #Inserta imagen "ciat.png"en la ventana

gl1=glabel("Buscar en:",container=group) #Crea linea de texto

fileChoose <- function(action="print", text = "Select a file...",type="open", ...) { #Función para seleccionar directorio de trabajo
  gfile(text=text, type=type, ..., action = action, handler =
          function(h,...) {
            do.call(h$action, list(h$file))
          })}


obj <- gbutton("Seleccionar conjunto de datos",container=group, #Crea botón anidado a la función "fun"
               handler =function(h,...) {
                 fileChoose(action="read.table", type="open", text="Seleccione un archivo...")})



fun1 <-function(h,...){
  print(do.call(h$action, list(get(svalue(h$obj)))))} #Función que lista las variables del conjunto de datos

listavar <- function(envir = .GlobalEnv, pattern) { #Aquí se da la opción que se desee realizar al dar doble click, para este caso "summary"
  listOfObjects <- ls(datos)
  gtable(listOfObjects, container = gwindow("Seleccione la variable"),action="summary", 
         handler=fun1)
}

obj <- gbutton("Realizar descriptivas",container=group,
               handler =function(h,...) {listavar()})

obj <- gbutton("Gráficos....falta",container=group,
               handler = function(h,...){print(summary(Value))})

visible(win) = T


