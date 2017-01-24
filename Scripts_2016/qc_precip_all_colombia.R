rutOrigen="S:/observed/weather_station/col-ideam/daily-raw/prec-per-station" #Ruta donde se encuentran los archivos .txt


files <-list.files(rutOrigen,pattern="\\.txt$")
nom.files<-substring(files,1,nchar(files)-13)
#nom.files=as.numeric(nom.files[-length(nom.files)])

# [1] "13067020" "16015130" "16017020" "21015060" "21055070" "21206790" "21206940" "21206980" "22025040" "23125160" "23197690" "24035380" "24035410"
# [14] "24055080" "25015020" "25017010" "26085170" "26135290" "27015280" "27015290" "28025130" "32067030" "35035110" "35095120" "51025070" "51039010"
# [27] "52015050" "52055150"


x=seq(as.Date("1965/1/1"), as.Date("2014/12/31"), "days") #Definir periodo que se desea analizar
#x=seq(as.Date("1980/1/1"), as.Date("2014/12/31"), "days") #Definir periodo que se desea analizar

fechas=format(x,"%Y%m%d")
fechas=cbind.data.frame("Date"=fechas,"NA")

Datos <- lapply(paste(rutOrigen,"/",nom.files,"_raw_prec.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

convert=function(x){y=as.numeric(sub(",", ".", x, fixed = TRUE))
return(y)}

Datos_n=lapply(Datos,convert)
data_na=function(x){na=sum(is.na(x[,2]))/length(x[,2])
return(na)}


qc=function(x){
  pos=which(x[,2]>350 | x[,2]<0)
  if(length(pos)!=0){
  x[pos,2]=NA}
return(x)}

Data_qc=lapply(Datos,qc)

qc2=function(x){
  pos=which(x[,2]>350 | x[,2]<0)
  ps2=length(pos)
  return(pos2)}
 qc_all=unlist(lapply(Data_qc,qc))

# aut=which(is.na(qc_all) | qc_all>0)
# nom.files2=nom.files[-aut]
# Datos2=Datos[-aut]



na_all=round(unlist(lapply(Data_qc,data_na)),3)
na_all2=cbind(nom.files,na_all)
datosprecip=as.data.frame(matrix(NA,nrow(fechas),length(Data_qc)))

for(j in 1:length(Data_qc)) {  
  
  final=merge(fechas,Data_qc[[j]],by="Date",all.x=T)
  datosprecip[,j]=final[,3]
}

year=as.numeric(substr(fechas[,1],1,4))
month=as.numeric(substr(fechas[,1],5,6))
day=as.numeric(substr(fechas[,1],7,8))


precipfin=cbind(day,month,year,datosprecip)

names(precipfin)=c("day","month","year",nom.files)

write.csv(precipfin,"daily_all.csv",row.names = F)



#########Daily to Monthly
data_na2=function(x){na=sum(is.na(x))/length(x)
return(na)}

precipfin=read.csv("C:/Users/lllanos/Desktop/daily_all.csv",header=T)

data=sapply(precipfin, as.numeric)

qc=function(x){
  pos=which(x>350 | x<0)
  if(length(pos)!=0){
    x[pos]=NA}
  return(x)}

data_qc=apply(data[,-3:-1],2,qc)

write.csv(data_qc,"daily_all_qc.csv",row.names = F)
nas=apply(precipfin,2,data_na2)


sum22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.05){
    x=NA
  }else{x=sum(a,na.rm=any(!is.na(a)))}
  
  return(x)
}

monthly_precip=aggregate(data_qc,list(Mes=precipfin$month,Año=precipfin$year),sum22)
write.csv(monthly_precip,"monthly_all.csv",row.names=F)
which(apply(monthly_precip,2,min,na.rm=T)==Inf)
