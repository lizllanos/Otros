#########################
##Crear Graficos Scores##
#########################

ruta="C:/Users/darango/Desktop/Pronosticos/Gloria feb2015/Resultados Ene15/"
nombresX=list.files(ruta,pattern="XCCAMS")
nombresY=list.files(ruta,pattern="YCCAMS")

for(i in 1:length(nombresX)){
  
dataX=read.table(paste(ruta,nombresX[i],sep=""),skip=3,colClasses=c("character","numeric"))
dataY=read.table(paste(ruta,nombresY[i],sep=""),skip=3,colClasses=c("character","numeric"))

dataX[dataX==-999]=NA
dataY[dataY==99999]=NA
dataX[,2]=dataX[,2]*100 ##Cambiar para mas nodos
dataY[,2]=dataY[,2]*100


##Graficos

mes=strsplit(strsplit(nombresX[i],".txt")[[1]],"_")[[1]][length(strsplit(strsplit(nombresX[i],".txt")[[1]],"_")[[1]])]
name=paste("temporal_score_",mes,sep="")

tiff(paste(ruta,name,".tif",sep=""), height=900,width=1200,res=250,
     pointsize=4,compression="lzw")

par(mfrow=c(1,1))
plot(dataX$V2,type="l",ylim=c(min(c(dataX$V2,dataY$V2),na.rm=T),max(c(dataX$V2,dataY$V2),na.rm=T)),xaxt="n",ylab="Scores (X=black , Y=red) *100",xlab="",main="Temporal Scores (Mode 1)")
axis(1, at=1:length(dataX$V2),labels=dataX$V1, las=2,cex.axis=0.7)
lines(dataY$V2,col="red")
abline(h=0,col="gray",lwd=2)
grid()

# plot(dataX$V3,type="l",ylim=c(min(c(dataX$V3,dataY$V3),na.rm=T),max(c(dataX$V3,dataY$V3),na.rm=T)),xaxt="n",ylab="Scores (X=black , Y=red) *100",xlab="",main="Temporal Scores (Mode 2)")
# axis(1, at=1:length(dataX$V2),labels=dataX$V1, las=2,cex.axis=0.7)
# lines(dataY$V3,col="red")
# abline(h=0,col="gray",lwd=2)
# grid()

dev.off()

}
