jefer=read.table("clipboard",header=T)

data_final=rbind("********************",paste("IYEAR = ",jefer[1,1]),paste("STTIME = ",jefer[1,2],".",sep=""),paste("EMD =  ",jefer[1,2]),paste("EMYR = ",jefer[1,1]))

for(i in 2:nrow(jefer)){
  data_final=rbind(data_final,"********************",paste("IYEAR = ",jefer[i,1]),paste("STTIME = ",jefer[i,2],".",sep=""),paste("EMD =  ",jefer[i,2]),paste("EMYR = ",jefer[i,1]))
}

write.table(data_final,"jefer.txt",row.names=F,col.names=F,quote =F)
