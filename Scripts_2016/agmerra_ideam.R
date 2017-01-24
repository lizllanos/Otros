require(ncdf4)
require(raster)

all_files=list.files("D:/AgMerra/Tmax")

tmax_files=all_files[grep(all_files,pattern = "tmax")]

tmax_raster=list()

for (j in 1:length(tmax_files)){
  tmax_raster[[j]]=list()
  
  tmax_raster[[j]][[1]]=raster(paste("D:/AgMerra/Tmax/",tmax_files[j],sep=""),band=1)
  tmax_raster[[j]][[1]]=shift(tmax_raster[[j]][[1]],-360,0)
  
  for (i in 2:nbands(tmax_raster[[j]][[1]])){
    tmax_raster[[j]][[i]]=raster(paste("D:/AgMerra/Tmax/",tmax_files[j],sep=""),band=i)
    tmax_raster[[j]][[i]]=shift(tmax_raster[[j]][[i]],-360,0)
  }
}

coord=read.table("clipboard",header=T)

data=list()
for(i in 1:nrow(coord)){
  data[[i]]=list()
  for(j in 22:length(tmax_raster)){
    
    year_1=stack(tmax_raster[[j]])
    data[[i]][[j]]=as.numeric(extract(year_1,cbind(coord[i,1],coord[i,2])))
    
  }
}




dates=seq(as.Date("1980/1/1"), as.Date("2010/12/31"), "days") #Definir periodo que se desea analizar

all=matrix(NA,length(dates),7)
for(i in 1:7){
  all[,i]=do.call(what = "rbind.fill.matrix",data[[i]])[,1]
  
}



plot(as.numeric(data),type="l")
