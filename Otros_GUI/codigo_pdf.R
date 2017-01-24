
obj <- gslider(from=0, to = 7734, by =100, value=0,
               container=gwindow()) #Genera una barra movil para seleccionar un numero

obj <- gspinbutton(from=0, to = 0.3, by =0.01, value=0,
                   container=gwindow()) #Idea para seleccionar nivel de confianza



############################


 win <- gwindow("Hello World, ad nauseum", visible=TRUE)
 group <- ggroup(horizontal = FALSE, container=win)
 obj <- gbutton("Hello...",container=group,
                 + handler = function(h,...) gmessage("world"))
 obj <- glabel("Hello...", container =group,
                + handler = function(h,...) gmessage("world"))
 obj <- gcombobox(c("Hello","world"), container=group)
 obj <- gedit("Hello world", container=group)
 obj <- gtext("Hello world", container=group, font.attr=list(style="bold"))





#############################

 confirmDialog <- function(message, handler=NULL) {
  window <- gwindow("Confirm")
   group <- ggroup(container = window)
  gimage("info", dirname="stock", size="dialog", container=group)
  
   ## A group for the message and buttons
   inner.group <- ggroup(horizontal=FALSE, container = group)
   glabel(message, container=inner.group, expand=TRUE)
  
   ## A group to organize the buttons
   button.group <- ggroup(container = inner.group)
   ## Push buttons to right
  addSpring(button.group)
   gbutton("ok", handler=handler, container=button.group)
   gbutton("cancel", handler = function(h,...) dispose(window),
             container=button.group)
  
     return()
   }



#######################

 w <- gwindow("Two widgets")
 g <- ggroup(container = w)
 widget1 <- gbutton("Click me to update the counter", container=g,
                      handler = function(h,...) {
                        oldVal <- svalue(widget2)
                        svalue(widget2) <- as.numeric(oldVal) + 1
                        })
 widget2 <- glabel(0, container=g)

##################
source(file.choose()) #Abre una ventana para escoger la ubicacion del archivo
source(gfile()) #devuelve el nombre de la ubicacion del archivo



########
 fileChoose <- function(action="print", text = "Select a file...",
                          type="open", ...) {
   gfile(text=text, type=type, ..., action = action, handler =
             function(h,...) {
               do.call(h$action, list(h$file))
               })
   }

fileChoose(action="source")
fileChoose(action="setwd", type="selectdir", text="Select a directory...") ###Selecciona el directorio


#################

lstObjects <- function(envir= .GlobalEnv, pattern) {
   objlist <- ls(envir=envir, pattern=pattern)
   objclass <- sapply(objlist, function(objName) {
     obj <- get(objName, envir=envir)
     class(obj)[1]
     })
   data.frame(Name = I(objlist), Class = I(objclass))
   }
lstObjects() #funcion que lista los objetos que hay actualmente en la consola

 browseEnv1 <- function(envir = .GlobalEnv, pattern) {
   listOfObjects <- lstObjects(envir=envir, pattern)
   gtable(listOfObjects, container = gwindow("browseEnv1"))
   }
browseEnv1() #funcion que lista los objetos que hay actualmente en una ventana

 browseEnv2 <- function(envir = .GlobalEnv, pattern, action="summary") {
   listOfObjects <- lstObjects(envir=envir, pattern)
   gtable(listOfObjects, container = gwindow("browseEnv2"),
            action = action,
            handler = function(h,...) {
              print(do.call(h$action, list(get(svalue(h$obj)))))
              
              })
   }
browseEnv2() #funcion que lista los objetos que hay actualmente en una ventana y al hacer doble click te arroja un resumen del obj en la consola


#############ejemplos en R


obj <- gtext("First line", container=gwindow())
insert(obj,"second line", font.attr=c(family="monospace"))
insert(obj,"third line", font.attr=c(foreground.colors="red"))


w <- gwindow("Delete and add", visible=FALSE)
g <- ggroup(container=w)
b <- gcheckbox("hide", checked=FALSE, container=g)
l <- gedit("click checkbox to hide me", container=g)
addHandlerClicked(b, handler=function(h,...) {
  if(svalue(b))
    delete(g, l)
  else
    add(g, l)
})
visible(w) <- TRUE


datos=as.data.frame(c(Date,Value1))

#####################

 doPlot <- function() {
   ## Set up main group
     mainGroup <- ggroup(container=gwindow("doPlot example"))
  
     ## The variable browser widget
     gvarbrowser(container = mainGroup)
  
     rightGroup <- ggroup(horizontal=FALSE, container=mainGroup)
  
     ## The graphics device
     ggraphics(container=rightGroup)
   entry <- gedit("drop item here to be plotted", container=rightGroup)
  
   adddroptarget(entry,handler = function(h,...) {
     do.call("plot",list(svalue(h$dropdata),main=id(h$dropdata)))
     })
   }
 doPlot()


##########################

makeDynamicWidget <- function() {
  
     win <- gwindow("Draw a boxplot")
   gd <- ggraphics(container = win)
  
     adddroptarget(gd, targetType="object", handler=function(h,...) {
       tag(gd,"data") <- h$dropdata
       plotWidget(gd)
      
         ## this makes the dynamic part:
         ## - we put a change handler of the column that we get the data from
         ## - we store the handler id, so that we can clean up the handler when this
         ## window is closed
        
         ## The is.gdataframecolumn function checks if the drop value
         ## comes from the data frame editor (gdf)
         if(gWidgetsRGtk2:::is.gdataframecolumn(h$dropdata)) {
           view.col <- h$dropdata
          
             ## Put change handler on column to update plotting widget
             ## (use lower case, to fix oversight)
             id <- addhandlerchanged(view.col, handler=function(h,...) plotWidget(gd))
           ## Save drop handler id so that it can be removed when
             ## widget is closed
             dropHandlers <- tag(gd,"dropHandlers")
           dropHandlers[[length(dropHandlers)+1]] <- list(view.col = view.col,
                                                             id = id
                                                             )
           tag(gd,"dropHandlers") <- dropHandlers
           }
      })
  
     ## Remove drop handlers if widget is unrealized.
     addHandlerUnrealize(gd, handler = function(h,...) {
       dropHandlers <- tag(gd,"dropHandlers")
       if(length(dropHandlers) > 0) {
         for(i in 1:length(dropHandlers)) {
           removehandler(dropHandlers[[i]]$view.col,dropHandlers[[i]]$id)
           }
         }
       })
   }

 plotWidget <- function(widget) {
   data <- tag(widget, "data")
   theName <- id(data)
   values <- svalue(data)
   boxplot(values, xlab=theName, horizontal=TRUE, col=gray(.75))
   }

 gdf(mtcars, container=TRUE)
 makeDynamicWidget()\



##################

 win <- gwindow("Plot notebook")
 group <- ggroup(horizontal = FALSE, container=win)
 nb <- gnotebook(container = group, expand=TRUE)
ggraphics(container = nb, label="plot")

################

w <- gwindow("Click on button to change")
g <- ggroup(cont = w) # abbreviate container
glabel("Hello ", cont=g)
guiWidget of type: gLabelRGtk for toolkit: guiWidgetsToolkitRGtk2
world <- gbutton("world", cont=g)
lst <- list()
lst$world$handler <- function(h,...) svalue(world) <- "world"
lst$continent$handler <- function(h,...) svalue(world) <- "continent"
lst$country$handler <- function(h,...) svalue(world) <- "country"
lst$state$handler <- function(h,...) svalue(world) <- "state"
addPopupmenu(world, lst)

##################

w=gwindow()
gr=ggroup(horizontal=F,cont=w)
glabel("...",cont=w)
grb=ggroup(cont=gr)
addSpring(grb)
parb=gbutton("...par",con=grb)

addHandlerClicked(parb,handler=function(h,...){
  child=gwindow("set",parent=w)
  ly=glayout(cont=child)
  ly[1,1,align=c(-1,0)]="mfrow"
  ly[2,1]=(nr=gedit(1,cont=ly))
  ly[2,2]=(nc=gedit(1,cont=ly))
  ly[3,2]=gbutton("ok",cont=ly,handler=function(h,...){
    x=as.numeric(c(svalue(nr),svalue(nc)))
    par(mfrow=x)
    dispose(child)
  })
  
})


######################
