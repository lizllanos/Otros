##########################################################################################
## Purpose: Merge daily IDEAM weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

var <- "tmax"
bDir <- "S:/observed/weather_station/col-ideam/daily-raw"
oDir <- "D:/cenavarro/workspace/Request_risidro"

#Ruta donde se encuentran los archivos .txt
rutOrigen=paste0(bDir, "/", var, "-per-station") 

files <- list.files(rutOrigen, pattern="\\.txt$")

if (var == "tmean"){
  nom.files <- substring(files, 1, nchar(files)-14)  
} else { 
  nom.files <- substring(files, 1, nchar(files)-13)
  #nom.files=as.numeric(nom.files[-length(nom.files)])
}


#Definir periodo que se desea analizar
x = seq(as.Date("1965/1/1"), as.Date("2014/12/31"), "days") 
#x=seq(as.Date("1980/1/1"), as.Date("2014/12/31"), "days")

fechas= format(x,"%Y%m%d")
fechas= cbind.data.frame("Date"=fechas,"NA")

Datos <- lapply(paste(rutOrigen,"/", nom.files, "_raw_", var, ".txt",sep=""), function(x){read.table(x, header=T, sep="\t")})

convert=function(x){y=as.numeric(sub(",", ".", x, fixed = TRUE))
return(y)}

Datos_n=lapply(Datos,convert)
data_na=function(x){na=sum(is.na(x[,2]))/length(x[,2])
return(na)}

if(var == "prec"){
  
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
  
} else {
  
  qc=function(x){
    pos=which(x[,2]>50 | x[,2]<(-20))
    if(length(pos)!=0){
      x[pos,2]=NA}
    return(x)}
  
  Data_qc=lapply(Datos,qc)
  
  qc2=function(x){
    pos=which(x[,2]>50 | x[,2]<(-20))
    ps2=length(pos)
    return(pos2)}
  qc_all=unlist(lapply(Data_qc,qc))
  
  
}




pos4=which(abs(diff(object[,i+3]))>=10)
pos.4=unique(sort(c(pos4,pos4+1)))


# aut=which(is.na(qc_all) | qc_all>0)
# nom.files2=nom.files[-aut]
# Datos2=Datos[-aut]

na_all = round(unlist(lapply(Data_qc,data_na)),3)
na_all2 = cbind(nom.files,na_all)
datosprecip = as.data.frame(matrix(NA,nrow(fechas),length(Data_qc)))

for(j in 1:length(Data_qc)) {  
  
  final=merge(fechas,Data_qc[[j]],by="Date",all.x=T)
  datosprecip[,j]=final[,3]
}

year=as.numeric(substr(fechas[,1],1,4))
month=as.numeric(substr(fechas[,1],5,6))
day=as.numeric(substr(fechas[,1],7,8))


varfin=cbind(day,month,year,datosprecip)

names(varfin)=c("day","month","year",nom.files)

write.csv(varfin, paste0(oDir, "/", var, "_daily_all.csv"), row.names = F)



## Convert Daily to Monthly

data_na2 = function(x){na=sum(is.na(x)) / length(x)
return(na)}

varfin = read.csv(paste0(oDir, "/", var, "_daily_all.csv"), header=T)

data = sapply(varfin, as.numeric)

if (var == "prec"){

  qc=function(x){
    pos=which(x>350 | x<0)
    if(length(pos)!=0){
      x[pos]=NA}
    return(x)}
  
} else { 

  qc=function(x){
    pos=which(x>50 | x<(-20))
    if(length(pos)!=0){
      x[pos]=NA}
    return(x)}

}

data_qc=apply(data[,-3:-1],2,qc)

write.csv(data_qc, paste0(oDir, "/", var, "_daily_all_qc.csv"), row.names = F)

nas = apply(varfin,2,data_na2)


if (var == "prec"){
    
  sum22=function(a,na.rm=T){
    na.x=sum(is.na(a))/length(a)
    if(na.x>=0.20){
      x=NA
    }else{x=sum(a,na.rm=any(!is.na(a)))}
    
    return(x)
  }

} else {
  
  
  sum22=function(a,na.rm=T){
    na.x=mean(is.na(a))/length(a)
    if(na.x>=0.20){
      x=NA
    }else{x=mean(a,na.rm=any(!is.na(a)))}
    
    return(x)
  }
  
}

monthly_var = aggregate(data_qc, list(Month=varfin$month,Year=varfin$year),sum22)
write.csv(monthly_var, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
# which(apply(monthly_precip, 2, min,na.rm=T) == Inf)


### To remove stations uncomment the following lines
# 
# var <- "prec"
# varfin = read.csv(paste0(oDir, "/", var, "_monthly_all.csv"), header=T)
# 
# choco = read.csv(paste0(oDir, "/choco/", var, "_monthly_all.csv"), header=T)
# 
# varfin = varfin[ , -which(names(varfin) %in% c("X11010010", "X11020010", "X11020020", "X11020050", "X11030010", "X11030030", "X11030040", "X11035010", "X11035020", "X11040010", "X11045010", "X11050010", "X11050020", "X11050030", "X11050040", "X11050060", "X11080010", "X11085010", "X11090010", "X11100010", "X11100020", "X11105020", "X11120040", "X11125010", "X11130010", "X11130020", "X11135010", "X11135020", "X11135030", "X11145010", "X11150010", "X11150020", "X11150030", "X11159010", "X54010010", "X54020010", "X54020020", "X54020040", "X54020050", "X54020060", "X54020080", "X54020090", "X54025010", "X54025020", "X54085010", "X54090010", "X55010010", "X55010020", "X55015010", "X56010020", "X56010030", "X56010040", "X56010050", "X56015010", "X56015030", "X56019010"))]
# varfin <- cbind(varfin, choco[,3:length(choco)])
# 
# write.csv(varfin, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
