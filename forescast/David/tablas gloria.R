#########################
##Tabla con Indicadores##
#########################

tabla_indicadores=function(i){
name = strsplit(names[i],".txt")[[1]]
datos=read.table(paste(ruta,"/",name,".txt",sep=""),skip=3,colClasses=c("character","numeric","numeric","numeric"))
mes=strsplit(strsplit(names[i],".txt")[[1]],"_")[[1]][length(strsplit(strsplit(names[i],".txt")[[1]],"_")[[1]])]
datos=cbind.data.frame(datos,rep(mes,dim(datos)[1]))
names(datos)=c("Estacion","Lat","Long","Indicador","Mes")
return(datos)}

ruta="C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/Resultados Feb15"
metrica=c("Pearson","Spearman","ROCA","ROCB","HitSS","LEPS")

names=list.files(ruta,pattern=metrica[1])
resultados=tabla_indicadores(1)
for(j in 2:length(names)){
  resultados=rbind.data.frame(resultados,tabla_indicadores(j))}
resultados=cbind.data.frame(rep(metrica[1],dim(resultados)[1]),resultados[,c(1,4,5)])
names(resultados)[1]="Metrica"
resultados_finales=resultados

for(k in 2:length(metrica)){
names=list.files(ruta,pattern=metrica[k])
resultados=tabla_indicadores(1)
for(j in 2:length(names)){
  resultados=rbind.data.frame(resultados,tabla_indicadores(j))}
resultados=cbind.data.frame(rep(metrica[k],dim(resultados)[1]),resultados[,c(1,4,5)])
names(resultados)[1]="Metrica"
resultados_finales=rbind.data.frame(resultados_finales,resultados)
print(k)}

write.table(resultados_finales,"C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/tabla_indicadores_feb2015.xls")

#############################
##Tabla con los Pronosticos##
#############################

tabla_pronosticos=function(name,name1,name2){

datos=read.table(paste(ruta,"/",name,".txt",sep=""),skip=2,colClasses=c("character","numeric","numeric","numeric"))
data=data.frame(names(datos),as.numeric(t(datos[3,])))
names(data)=c("estacion","pronostico (mm)")

datos=readLines(paste(ruta,"/",name1,".txt",sep=""),skip=5)
data2=cbind.data.frame(as.numeric(strsplit(datos[7],"\t")[[1]][-1]),
as.numeric(strsplit(datos[12],"\t")[[1]][-1]))
names(data2)=c("umbral_inferior","umbral_superior")

datos=readLines(paste(ruta,"/",name2,".txt",sep=""),skip=5)
data3=cbind.data.frame(as.numeric(strsplit(datos[7],"\t")[[1]][-1]),
                       as.numeric(strsplit(datos[12],"\t")[[1]][-1]),
                       as.numeric(strsplit(datos[17],"\t")[[1]][-1]))
names(data3)=c("prob_inferior","prob_normal","prob_superior")

data_all=data.frame(data,data2,data3)

return(data_all)}

ruta="C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/Resultados Feb15"
name=unlist(strsplit(list.files(ruta,pattern="Forecast_"),".txt"))
name1=unlist(strsplit(list.files(ruta,pattern="ForecastL_"),".txt"))
name2=unlist(strsplit(list.files(ruta,pattern="ForecastP_"),".txt"))


mes=strsplit(name,"_")[[1]][length(strsplit(name,"_")[[1]])]
resultado=tabla_pronosticos(name[1],name1[1],name2[1])
resultado=data.frame(rep(mes,dim(resultado)[1]),resultado)
names(resultado)[1]="mes"
resultados_total=resultado

for(j in 2:length(name)){

mes=strsplit(name,"_")[[j]][length(strsplit(name,"_")[[j]])]
resultado=tabla_pronosticos(name[j],name1[j],name2[j])
resultado=data.frame(rep(mes,dim(resultado)[1]),resultado)
names(resultado)[1]="mes"

resultados_total=rbind.data.frame(resultados_total,resultado)}

write.table(resultados_total,"C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/tabla_pronosticos_feb2015.xls")

############################################
##Tabla Goodness y Correlaciones Canonicas##
############################################

ruta="C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/Resultados Feb15/"
nombresG=list.files(ruta,pattern="Good")
nombresC=list.files(ruta,pattern="Canonical")

datosG=read.table(paste(ruta,nombresG[1],sep=""),skip=8)
datosC=read.table(paste(ruta,nombresC[1],sep=""),skip=3)
resultados=data.frame(c("Goodness",paste("Mode_",datosC$V1,sep="")),
                      c(datosG[dim(datosG)[1],8],datosC$V2))
mes=strsplit(strsplit(nombresG[1],".txt")[[1]],"_")[[1]][length(strsplit(strsplit(nombresG[1],".txt")[[1]],"_")[[1]])]
names(resultados)=c("indicador",mes)

resultados_final=resultados


for(j in 2:length(nombresG)){
  datosG=read.table(paste(ruta,nombresG[j],sep=""),skip=8)
  datosC=read.table(paste(ruta,nombresC[j],sep=""),skip=3)
  resultados=data.frame(c(datosG[dim(datosG)[1],8],datosC$V2))
  mes=strsplit(strsplit(nombresG[j],".txt")[[1]],"_")[[1]][length(strsplit(strsplit(nombresG[j],".txt")[[1]],"_")[[1]])]
  names(resultados)=mes
  
  resultados_final=cbind.data.frame(resultados_final,resultados)}

write.table(resultados_final,"C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/tabla_Goodness_Feb2015.xls")


