data_cordoba=read.table("clipboard",header=T)
labels_c=read.table("clipboard",header=F) ## nombreestacion-codigo 

x=seq(as.Date("1980/1/1"), as.Date("2014/12/31"), "month")
fechas=format(x,"%Y-%m")

dir.create("C:/Users/lllanos/Desktop/Jaziku")
for (i in 3:ncol(data_cordoba)){
  new_data=cbind.data.frame(fechas,data_cordoba[,i])
  label=as.character(labels_c[i-2,])
 write.table(new_data,paste("C:/Users/lllanos/Desktop/Jaziku/",substring(label,1,nchar(label)-4),".txt",sep=""),na="nan",col.names = F,row.names = F,quote = F)
}



