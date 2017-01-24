# 
#  _____        _ _         _          __  __             _   _     _       
# |  __ \      (_) |       | |        |  \/  |           | | | |   | |      
# | |  | | __ _ _| |_   _  | |_ ___   | \  / | ___  _ __ | |_| |__ | |_   _ 
# | |  | |/ _` | | | | | | | __/ _ \  | |\/| |/ _ \| '_ \| __| '_ \| | | | |
# | |__| | (_| | | | |_| | | || (_) | | |  | | (_) | | | | |_| | | | | |_| |
# |_____/ \__,_|_|_|\__, |  \__\___/  |_|  |_|\___/|_| |_|\__|_| |_|_|\__, |
#                    __/ |                                             __/ |
#                   |___/                                             |___/ 
#                

mean22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.5){
    x=NA
  }else{x=mean(a,na.rm=T)}
  
  return(x)
} 

sum22=function(a,na.rm=T){
  na.x=sum(is.na(a))/length(a)
  if(na.x>=0.2){
    x=NA
  }else{x=sum(a,na.rm=T)}
  
  return(x)
}


# hr=read.table("clipboard",header = T)
# precip=read.table("clipboard",header = T)
# tmax=read.table("clipboard",header = T)
# tmin=read.table("clipboard",header = T)

tmax=read.table("clipboard",header = T)
tmax=read.csv(file.choose(),header = T)
tmin=read.csv(file.choose(),header = T)


monthly_precip=round(aggregate(precip[-3:-1],list(Mes=precip$month,Año=precip$year),sum22),2)
monthly_tmax=round(aggregate(tmax[-3:-1],list(Mes=tmax$month,Año=tmax$year),mean22),2)
monthly_tmin=round(aggregate(tmin[-3:-1],list(Mes=tmin$month,Año=tmin$year),mean22),2)
monthly_hr=round(aggregate(hr[-3:-1],list(Mes=hr$month,Año=hr$year),mean22),2)

aggregate(monthly_precip,list(monthly_precip$Mes),mean,na.rm=T)

write.csv(monthly_tmax,"monthly_tmax.csv",row.names = F)
write.csv(monthly_tmin,"monthly_tmin.csv",row.names = F)
write.csv(monthly_precip,"monthly_precip.csv",row.names = F)
write.csv(monthly_hr,"monthly_hr.csv",row.names = F)

hr=read.table("clipboard",header = T)
