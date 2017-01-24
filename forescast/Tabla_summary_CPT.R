#Load and concatenate a bunch of forecast data by station

#Set static variables & load libraries
root = 'C:/Users/lllanos/Desktop/Analisis Pronosticos CPT/colombia_deps/All/'
months = c('Ene','Feb','Mar','Abr','May','Jun','Jul','Ago','Sep','Oct','Nov','Dic')

#Load observations (pull out 2009 to 2011)
obsv = read.csv('C:/Users/lllanos/Desktop/Analisis Pronosticos CPT/Data All/CPT/Datos Estaciones CPT/data/PreCiatMod.csv',header=T,stringsAsFactors=F)
num.forec=3

library('gplots')
library('abind')
library('xlsx')

tabla_summary<-function(root,months,obsv,num.forec){

num.f=length(months)*num.forec
prec.cpt=obsv 
#Load monthly deterministic forecasts & concatenate
for (m in 1:length(months))  {
  test = read.table(paste(root,months[m],'/Deterministic.forecasts',m,'.txt',sep=''),header=T,as.is=T,skip=2)
  test = data.matrix(test)  #convert from character to numeric
  test[test=='99999']=NA  #convert 99999 to NA
  year.i=substring(rownames(test)[3],1,4)
  year.i=as.numeric(year.i)
  if (m==1)  {
    stations = test[1:2,]  #extract station data
    p = dim(stations)[2]  #total number of stations
    forecasts = array(NA,dim=c(p,num.f))  #initialize matrix for 3 years of monthly forecasts
    
  }
  forecasts[,seq(m,num.f,length(months))] = t(test[3:5,])  #3 years by month
  print(m)
}

#Load prediction limits & concatenate
for (m in 1:length(months))  {
  test.lower = read.table(paste(root,months[m],'/PredictionLimits',m,'.txt',sep=''),header=T,as.is=T,skip=3,nrows=5)
  test.lower = data.matrix(test.lower)  #convert from character to numeric
  test.lower[test.lower=='99999']=NA  #convert 99999 to NA
  
  test.upper = read.table(paste(root,months[m],'/PredictionLimits',m,'.txt',sep=''),header=T,as.is=T,skip=10,nrows=5)
  test.upper = data.matrix(test.upper)  #convert from character to numeric
  test.upper[test.upper=='99999']=NA  #convert 99999 to NA
  
  if (m==1)  {
    stations = test.lower[1:2,]  #extract station data
    p = dim(stations)[2]  #total number of stations
    forecast.limits = array(NA,dim=c(p,num.f,2))  #initialize matrix for 3 years of monthly forecasts
  }
  forecast.limits[,seq(m,num.f,length(months)),1] = t(test.lower[3:5,])  #lower limits
  forecast.limits[,seq(m,num.f,length(months)),2] = t(test.upper[3:5,])  #upper limits
  
  print(m)
}

#Load average climatology & repeat by year
for (m in 1:length(months))  {
  test = read.table(paste(root,months[m],'/ClimatologyAverages',m,'.txt',sep=''),header=T,as.is=T,skip=2)
  test = data.matrix(test)  #convert from character to numeric
  test[test=='99999']=NA  #convert 99999 to NA
  if (m==1)  {
    stations = test[1:2,]  #extract station data
    p = dim(stations)[2]  #total number of stations
    climatology = array(NA,dim=c(p,length(months)))  #initialize matrix for 3 years of monthly forecasts
  }
  climatology[,m] = t(test[3,])  #12 months of data
  print(m)
}
climatology=do.call("cbind", replicate(num.forec, climatology, simplify = FALSE))

#Load upper & lower climatology & repeat by year
for (m in 1:length(months))  {
  test.lower = read.table(paste(root,months[m],'/ClimatologyThresholds',m,'.txt',sep=''),header=T,as.is=T,skip=2,nrows=3)
  test.lower = data.matrix(test.lower)  #convert from character to numeric
  test.lower[test.lower=='99999']=NA  #convert 99999 to NA
  
  test.upper = read.table(paste(root,months[m],'/ClimatologyThresholds',m,'.txt',sep=''),header=T,as.is=T,skip=7,nrows=3)
  test.upper = data.matrix(test.upper)  #convert from character to numeric
  test.upper[test.upper=='99999']=NA  #convert 99999 to NA
  
  if (m==1)  {
    stations = test.lower[1:2,]  #extract station data
    p = dim(stations)[2]  #total number of stations
    climatology.terciles = array(NA,dim=c(p,length(months),2))  #initialize matrix for 3 years of monthly forecasts
  }
  climatology.terciles[,m,1] = t(test.lower[3,])  #12 months of data
  climatology.terciles[,m,2] = t(test.upper[3,])  #12 months of data
  print(m)
}

for(i in 1:(num.forec-1)){climatology.terciles = abind(climatology.terciles,climatology.terciles,along=2)  #concatenate for 3 years
}
# climatology.terciles3 = abind(climatology.terciles2,climatology.terciles,along=2)  #concatenate for 3 years
# abind2=function(x)abind(x,along=2)
# do.call("abind2", climatology.terciles)


ind_obs = match(colnames(stations),colnames(prec.cpt))  #find matching stations
dates.obs = as.Date(paste(as.character(prec.cpt[,1]),'-01',sep=''),format='%Y-%m-%d')  #extract dates of historical obs
ind_dates = which(format(dates.obs,'%Y')>=year.i)
prec.hist = array(NA,dim=c(dim(stations)[2],num.f))
prec.hist[which(is.na(ind_obs)==F),] = dim(t(prec.cpt[ind_dates,ind_obs[is.na(ind_obs)==F]]))

#Look at goodness index by month for fixed region X, but varying regions Y
GI = array(NA,dim=c(length(months),1))
#colnames(GI) = regiones
  for (m in 1:length(months))  {
    test = read.table(paste(root,months[m],'/GoodnessIndex',m,'.txt',sep=''),skip=7)
    GI[m] = max(test$V8)
  }


## Calculate statistics
#Set first month to NA if it looks weird!
#get rid of negative forecasts
forecasts[,1] = NA
forecast.limits[,1,]=NA

#How often is forecast within "normal" terciles from climatology?
SK_pred_normal=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  pred_normal = (forecasts[,seq(j,num.f,length(months))]>=climatology.terciles[,seq(j,num.f,length(months)),1])&(forecasts[,seq(j,num.f,length(months))]<=climatology.terciles[,seq(j,num.f,length(months)),2])
#calculate % of cases where prediction is "normal"
SK_pred_normal[j]=sum(pred_normal==T,na.rm=T)/(dim(pred_normal)[1]*dim(pred_normal)[2]-sum(is.na(pred_normal)))*100
}

#Look to see if climatological average between prediction limits 
SK_clim_CI=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  
clim_CI = (climatology[,seq(j,num.f,length(months))]>=forecast.limits[,seq(j,num.f,length(months)),1])&(climatology[,seq(j,num.f,length(months))]<=forecast.limits[,seq(j,num.f,length(months)),2])
#calculate % of cases where prediction limits include climatological average
SK_clim_CI[j]=sum(clim_CI==T,na.rm=T)/(dim(clim_CI)[1]*dim(clim_CI)[2]-sum(is.na(clim_CI)))*100
}

#How often are observations within prediction limits?
skill=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  
obs_CI = (prec.hist[,seq(j,num.f,length(months))]>=forecast.limits[,seq(j,num.f,length(months)),1]&prec.hist[,seq(j,num.f,length(months))]<=forecast.limits[,seq(j,num.f,length(months)),2])
#calculate % of obs that fall within prediction limits
skill[j] = sum(obs_CI==T,na.rm=T)/(dim(obs_CI)[1]*dim(obs_CI)[2]-sum(is.na(obs_CI)))*100
}

#calculate skill by month & plot
pct_good=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  obs_CI = (prec.hist[,seq(j,num.f,length(months))]>=forecast.limits[,seq(j,num.f,length(months)),1]&prec.hist[,seq(j,num.f,length(months))]<=forecast.limits[,seq(j,num.f,length(months)),2])
  pct_good[j] = sum(obs_CI==T,na.rm=T)/(length(obs_CI)-sum(is.na(obs_CI)))*100  
}

#calculate average width of confidence intervals by month?
conf.int = (forecast.limits[,,2] - forecast.limits[,,1])
conf.int_std = (forecast.limits[,,2] - forecast.limits[,,1])/climatology

# quantile(conf.int,na.rm=T)
# mean(conf.int,na.rm=T)

LPI=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
    LPI[j] =  mean(conf.int[,seq(j,num.f,length(months))],na.rm=T)
}

LPI_std=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  LPI_std[j] =  mean(conf.int_std[,seq(j,num.f,length(months))],na.rm=T)
}



#Load Pearson's correlations & repeat by year
for (m in 1:length(months))  {
  test = read.table(paste(root,months[m],'/Pearsons_correlation',m,'.txt',sep=''),header=T,as.is=T,skip=2)
  #test = data.matrix(test)  #convert from character to numeric
  test$X1 = as.numeric(test$X1)
  test$X1[test$X1=='99999']=NA  #convert 99999 to NA
  if (m==1)  {
    stations = test[,1]  #extract station data
    p=length(stations)
    pearson = array(NA,dim=c(p,length(months)))  #initialize matrix for 3 years of monthly forecasts
  }
  pearson[,m] = test$X1 
  print(m)
}

pearson_ind=apply(pearson,2,function(x)median(x,na.rm=T))


#Load ROC below & repeat by year
for (m in 1:length(months))  {
  test = read.table(paste(root,months[m],'/ROC_below',m,'.txt',sep=''),header=T,as.is=T,skip=2)
  #test = data.matrix(test)  #convert from character to numeric
  test$X1 = as.numeric(test$X1)
  test$X1[test$X1=='99999']=NA  #convert 99999 to NA
  if (m==1)  {
    stations = test[,1]  #extract station data
    p=length(stations)
    ROC_b = array(NA,dim=c(p,length(months)))  #initialize matrix for 3 years of monthly forecasts
  }
  ROC_b[,m] = test$X1 
  print(m)
}

ROC_ind=apply(ROC_b,2,function(x)mean(x,na.rm=T))


#Load Hit Skill & repeat by year
for (m in 1:length(months))  {
  test = read.table(paste(root,months[m],'/HitSkillScore',m,'.txt',sep=''),header=T,as.is=T,skip=2)
  #test = data.matrix(test)  #convert from character to numeric
  test$X1 = as.numeric(test$X1)
  test$X1[test$X1=='99999']=NA  #convert 99999 to NA
  if (m==1)  {
    stations = test[,1]  #extract station data
    p=length(stations)
    Hit_skill = array(NA,dim=c(p,length(months)))  #initialize matrix for 3 years of monthly forecasts
  }
  Hit_skill[,m] = test$X1 
  print(m)
}

HitSk_ind=apply(Hit_skill,2,function(x)mean(x,na.rm=T))

#Calculate the RSME
RMSE=mat.or.vec(length(months),1)
for (j in 1:length(months))  {
  dif = (prec.hist[,seq(j,num.f,length(months))]-forecasts[,seq(j,num.f,length(months))])^2
  RMSE[j]=mean(sqrt(apply(dif,2,function(x)mean(x,na.rm=T))))
  }


#####Generate final summary on a table
all_ind=cbind("Good_In"=as.vector(GI),ROC_ind,HitSk_ind,pearson_ind,LPI,LPI_std,"% Cobertura"=pct_good,"% Clim all"=SK_clim_CI,"% Clim Norm"=SK_pred_normal,RMSE)
all_ind1=rbind(all_ind,apply(all_ind,2,mean)) #Indicadores promedios de todos los meses

tabla_resumen=cbind(c(months,"All"),round(all_ind1,2))

write.csv(tabla_resumen,paste(root,"summary_CPT.csv",sep=""),quote = F,row.names=F)
}

tabla_summary(root,months,obsv,num.forec)
