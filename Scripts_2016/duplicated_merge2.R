variables=c("prec","tmax","tmin","tmean","rhum","sbright")

dir.create("S:\\observed\\weather_station\\col-ideam\\daily-raw-new")
rutfinal="S:\\observed\\weather_station\\col-ideam\\daily-raw-new"

dup_merge=function(j){
  
var=variables[j]
dir.create(paste(rutfinal,"/",var,"-per-station",sep=""))

rutOrigen_new=paste("S:\\observed\\weather_station\\col-ideam\\daily-raw-new_temp\\",var,"-per-station",sep="")
files_new <-list.files(rutOrigen_new,pattern="\\.txt$")
pos.files_new<-which(substring(files_new,1,nchar(files_new)-(9+nchar(var)))=="")
nom.files_new<-substring(files_new,1,nchar(files_new)-(9+nchar(var)))[-pos.files_new]
pos.files_new2<-grep("sbright",nom.files_new)
nom.files_new<-nom.files_new[-pos.files_new2]

Datos<- lapply(paste(rutOrigen_new,"\\",nom.files_new,"_",var,"_raw.txt",sep=""),function(x){read.table(x,header=T,sep="\t")})

merge_all=function(i){
Datos_old1=Datos[[i]]
  
old=na.omit(Datos_old1)
if(nrow(old)!=0){
combnew=old[!duplicated(old[,1]),]

x=seq(as.Date(paste(min(combnew[,1])),format="%Y%m%d"), as.Date(paste(max(combnew[,1])),format="%Y%m%d"), "days")
fechas=format(x,"%Y%m%d")

datesall=cbind.data.frame("Date"=fechas,"Value"=rep("NA",length(fechas)))

final=merge(datesall,combnew,by="Date",all.x=T)
final=final[,-2]
names(final)=c("Date","Value")
write.table(final,paste(rutfinal,"/",var,"-per-station/",nom.files_new[i],"_",var,"_raw.txt",sep=""),row.names=F,quote=F,sep ="\t")
}

}
sapply(1:length(nom.files_new),merge_all)

}


sapply(1:length(variables),dup_merge)


for(i in 1:5){
  var=variables[i]
  rutOrigen_new=paste("S:\\observed\\weather_station\\col-ideam\\daily-raw-new\\",var,"-per-station",sep="")
  
  
  files_new <-list.files(rutOrigen_new,pattern="\\.txt$")
  nom.files_new<-substring(files_new,1,nchar(files_new)-(9+nchar(var)))
  
  
  file.rename(paste0(rutOrigen_new,"/",files_new), paste(rutOrigen_new,"\\",nom.files_new,"_raw_",var,".txt",sep=""))
  
}
