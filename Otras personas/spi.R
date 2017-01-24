spi=function(){
  confirmDialog(paste("Los resultados de éste indicador se guardarán en",getwd(),"/Indicadores/SPI"),"Ubicación archivos")
  
  options(digits=3) 


prec_d=read.csv("Datos_faltantes/data_genPrec.csv",header=T)
x=aggregate(prec_d[-3:-1],list(Año=prec_d$year,Mes=prec_d$month),sum)
x1=order(x$Año)
pr=x[x1,]

iniyr=min(pr[,1])

stations<-colnames(pr[,3:ncol(pr)])
Nt <- dim(pr)
nmo<-as.numeric(Nt[1])
pr<-pr[,3:ncol(pr)]

for (i in stations) {
  #  pr[[i]][pr[[i]]==-9999] <- NA
  rm(pr2)
  rm(spi)
  rm(precipitation)
  rm(precipitation.detrend)
  pr2<-t(pr[[i]])
  precipitation<-pr2[1,]
  
  
  getPrecOnTimescale <- function(precipitation,k){
    # precipitation is a vector of monthly precipitation values
    # returns monthly precipation averaged over current month and prior k-1 months
    Nt <- length(precipitation)
    prec.k <- as.vector(sapply(seq(from=1, to=Nt),function(t) {tm <- max(t-k+1,1); mean(as.vector(precipitation[tm:t]))}))
    return(prec.k)
  }
  
  
  getSPIfromPrec <- function(precipitation){
    
    #takes a vector of precipitation values 
    #and returns a vector of spi values
    
    
    Nt <- length(precipitation)
    #include full years data only
    years <- ceiling(Nt/12)
    full.years <- trunc(Nt/12)
    Nt.f <- full.years*12
    monthsInCurrentYear <- Nt %% 12
    #monthly analysis
    spi.o <- array(NA,c(years,12))
    for (m in 1:12)
    {
      mdata <- precipitation[seq(from=m, to=Nt, by=12)]
      # empirical cdf's for each month 
      fit.cdf <- ecdf(mdata)
      #locate each data point on cdf by applying fit.cdf function for each location
      #cdf probabilites
      # these will be unformly distributed on interval 1/years
      cdfs <- as.vector(sapply(mdata,fit.cdf))
      #invert normal
      spi.t <- qnorm(cdfs)
      spi.tp <- spi.t[ spi.t != Inf] #drop Inf
      ff <- function(x) (1-sd(c(x,spi.tp)))^2
      #replace Inf with the value that sets sd(spi)=1 or mean = 0 (minimises ff)
      spi.t[spi.t==Inf] <- optimize(ff,lower=0,upper=100)$minimum
      # ensure mean is zero. spi.t is normally distributed with mean zero and sd approx 1
      spi.t <- spi.t - mean(spi.t)
      ifelse( !(monthsInCurrentYear==0),ifelse (m <=monthsInCurrentYear, spi.o[,m]<-spi.t, spi.o[,m]<- c(spi.t,0) ), spi.o[,m]<-spi.t)
    }
    
    spi <- array(0,c(Nt))
    
    for ( t in 1:Nt){
      month <- (t-1)%%12 + 1
      year <- 1 + trunc((t-1)/12)
      spi[t] <- spi.o[year,month]
      
    }
    return(spi)
  }
  
  
  #generate some sample recipitation values using a weibull distribution
  #precipitation <- rweibull(500,2,1)
  
  #create a plot of SPI index vs timescale
  options(warn=-1)
  Nt<- length(precipitation)
  date <- seq(from=iniyr,by=1/12,length.out=Nt)
  #detrend
  prec.fit <- lm(precipitation~date)
  precipitation.detrend <- precipitation- fitted(prec.fit)
  
  #spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(precipitation,i)))
  # if you want to detrend use this one
  spi <- sapply(1:12, function(i) getSPIfromPrec(getPrecOnTimescale(precipitation.detrend,i)))
  
  
  ##plot timeseries panel
  ifelse(file.exists("Indicadores")=="FALSE",dir.create("Indicadores",showWarnings=F),"Ya existe carpeta Indicadores")
  ifelse(file.exists("Indicadores/SPI")=="FALSE",dir.create("Indicadores/SPI",showWarnings=F),"Ya existe carpeta SPI")
  
  pdf(paste("Indicadores/SPI/",i,"_ts.pdf",sep=""),paper="USr", pointsize = 12,
      bg = "white")
  #attach(mtcars)
  
  m=matrix(c(1:4),2,2,byrow=T)
  layout(m)
  par(mar=c(2.5,2.5,2.5,2.5))
  
  plot(1:nrow(spi),spi[ ,1],type="h",ylab="SPI",xlab="",col="red",main="SPI-1",xaxt="n")
  axis(side=1,labels=seq(iniyr,max(date),5),at=seq(1,nrow(spi),by=60))
  plot(1:nrow(spi),spi[ ,3],type="h",ylab="SPI",xlab="",col="orange",main="SPI-3",xaxt="n")
  axis(side=1,labels=seq(iniyr,max(date),5),at=seq(1,nrow(spi),by=60))
  plot(1:nrow(spi),spi[ ,6],type="h",ylab="SPI",xlab="",col="yellow",main="SPI-6",xaxt="n")
  axis(side=1,labels=seq(iniyr,max(date),5),at=seq(1,nrow(spi),by=60))
  plot(1:nrow(spi),spi[ ,12],type="h",ylab="SPI",xlab="",col="purple",main="SPI-12",xaxt="n")
  axis(side=1,labels=seq(iniyr,max(date),5),at=seq(1,nrow(spi),by=60))
  
  title(main=i, cex.main=2,side = 3, line = -1.2, outer = TRUE)
  dev.off()
  
  
  #plot multi-scale figure
  spi.breaks <- c(-2.4,-2,-1.6,-1.3,-0.8,-0.5,0.5,0.8,1.3,1.6,2,2.4)
  spi.cols <- colorRampPalette(c("darkred","red","yellow","white","green","blue","darkblue"),space="rgb")
  pdf(paste("Indicadores/SPI/",i,".pdf",sep=""),paper="USr",width = 10, height = 4, pointsize = 12,
      bg = "white")
  filled.contour(date,seq(1:12),spi,col=spi.cols(11),xlab="",ylab="escala temporal (meses)",cex.lab=1.7,font.axis=2,font.lab=2,levels=spi.breaks,key.title="SPI",xaxt="n")
  title(main=i,cex.main=2)
  
  dev.off()
  
  #write output to file
#   dateh<-strftime(seq(as.Date(paste(iniyr,"/1/1",sep="")), by = "month", length.out = nmo), "%Y %m")
#   combined <- data.frame(dateh, spi)
#   write.table(combined,paste(i,"_SPI.txt",sep=""),row.names=F,col.names=F,sep="\t")
}
  #print("El proceso ha finalizado")
}