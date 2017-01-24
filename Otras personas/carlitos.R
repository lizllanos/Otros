
rutOrigen=""
files=list.files(rutOrigen,pattern="\\.txt$")


x=seq(as.Date("1980/1/1"), as.Date("2014/12/31"), "days") #Definir periodo que se desea analizar

fechas=format(x,"%Y%m%d")
fechas=cbind.data.frame("Date"=fechas,"NA")

Datos <- lapply(paste(rutOrigen,"\\",files,sep=""),function(x){read.table(x,header=T,sep="\t")})

datostmax=as.data.frame(matrix(NA,nrow(fechas),length(Datos)))

for(j in 1:length(Datos)) {  
  
  final=merge(fechas,Datos[[j]],by="Date",all.x=T)
  datostmax[,j]=final[,3]
}

sapply(1:length(Datos_gcm),function(x){bc(x,Datos_gcm)})