require(utils)

summary(women$height)   # refers to variable 'height' in the data frame
attach(women)
summary(height)         # The same variable now available by name
height <- height*2.54   # Don't do this. It creates a new variable
# in the user's workspace
find("height")
summary(height)         # The new variable in the workspace
rm(height)
summary(height)         # The original variable.
height <<- height*25.4  # Change the copy in the attached environment
find("height")
summary(height)         # The changed copy
detach("women")
summary(women$height)   # unchanged

sys.source("otro.R", envir=attach(NULL, name="myfuns"))

as.character(x)=gfile()
datos_new=read.table(gfile(),header=T,sep="")
x=source(source())

object=rnorm(100)
descript <- function(object){ #Función para descriptivas de las variables
  Media=mean(object, na.rm=T);  Varianza=var(object, na.rm=T);  Desv.Est=sqrt(Varianza)
  Mediana=as.numeric(quantile(object,probs=0.5, na.rm=T));  Coef.Var=sqrt(Varianza)/Media
  Mín=min(object,na.rm=T); Máx=max(object,na.rm=T)
  Datos.NA=length(object)-nobs(object)
  
  descriptivas=rbind(Mín,Máx,Media,Varianza,Desv.Est,Mediana,Coef.Var,Datos.NA)
  descriptivas=as.table(round(descriptivas,3))
  
  
  info<-RTF("pruebatabla.doc",width=8.5,height=11,font.size=10,omi=c(1,1,1,1))
  addHeader(info,title="Aplicativo para análisis de series climatológicas",subtitle="Análisis Descriptivo")
 
  addTable(info, descriptivas, font.size=10, row.names=TRUE, NA.string="-", #Inserta la tabla creada anteriormente
           col.widths=c( 1, 1) )
  done(info)
  
  return(descripivas)
}