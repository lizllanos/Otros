########################################################################################################################
########################################################################################################################
########################################Pruebas Estadisticas para series de clima#######################################
###################################################Control de Calidad###################################################
###################Victor Hugo Patino Bravo
####Librerias

library(Kendall)
library(tseries) ###Para la prueba JB

########################################Lectura de los archivos con las series a analizar
#dir=setwd("//dapadfs/workspace_cluster_3/Aeps/2013/MADR/Clima/pruebas-rmawagen/prueba-llanos/estaciones_fedearroz/estaciones_desde_2011/QC_series")
dir=setwd("D:/formatoDavid")


doc=list.files()
nom_doc<-substring(doc,1,nchar(doc)-4)

listArch=lapply(paste(dir,"/",doc,sep="",dec="."),function(x){read.csv(x,sep=",",header=TRUE)})
names(listArch)=nom_doc

#lapply(listArch,head)
###################Algunos descriptivos

summary <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,summary)})

#box <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,boxplot)})

#########################################Análisis confirmatorio

###################Prueba de  normalidad: Shapiro

shapTest <- function(Estaciones){
    serie=na.omit(Estaciones)
    norm=shapiro.test(serie)$p.value
    if(norm<0.05){
      Normalidad="NO Normal"
    }else{Normalidad="NR"}
}
  

shap<- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,shapTest)})

###################Prueba de  normalidad: KS

KS <- function(Estaciones){
  serie=na.omit(Estaciones)
  norm=ks.test(serie,"pnorm")$p.value
  if(norm<0.05){
    Normalidad="No Normalidad"
  }else{Normalidad="NR"}
}

KS_test<- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,KS)})

###################Prueba de  normalidad: JB
JB<-function(Estaciones){
  serie=na.omit(Estaciones)
  JBt=jarque.bera.test(serie)$p.value
  if(JBt<0.05){
    Normalidad="No Normalidad"
  }else{Normalidad="NR"}
  
}
JB<- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,JB)})
## Siendo la hipótesis nula que la población está distribuida normalmente, si el p-valor es menor a alfa (nivel de confianza) 
## entonces la hipótesis nula es rechazada (se concluye que los datos no vienen de una distribución normal)

#qqnorm(na.omit(part1))
#qqline(na.omit(part1))

###################Estacionariedad de la Serie
###################Test de tendencia

##Método del Rango correlación de Spearman

Spearman<-function(Estaciones){

  stat=na.omit(Estaciones)
    
            Rsp=round(cor(rank(stat),rank(sort(stat)),method="spearman"),4 ) ###Correlación de Rangos de Spearman
            T_t= abs(Rsp*((length(stat)-2)/(1-(Rsp^2)))^(1/2))      ###Estadistico T
            critico=qt(0.05/2,length(stat)-2,lower.tail=F)          ###Valor crítico  
            if(T_t>critico){
              Decision="Tendencia"
            }else{Decision="NR"}
  
            resul=as.matrix(Decision)
  
  
  #T_t > critico: Se rechaza Ho, es decir que hay tendencia en la serie
  
}

Rsp_Test<- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,Spearman)})

##Mann Kendall test

Kend_Test <- function(Estaciones){
  stat=na.omit(Estaciones)
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  Vcal=MannKendall(stat)$sl[1]     ###Tendencia
  
  if(Vcal<0.05){
    Decision="NO Tendencia"
  }else{Decision="NR" }
}

Ken_T <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,Kend_Test)})

#si valor P < alpha  : rechazo hipotesis, es decir que existe tendencia

###################Estabilidad en Varianza
###################Test F

#Haciendo el test a dos subgrupos (mitad vs mitad)

par.f <- function(Estaciones)
{
  stat=na.omit(Estaciones)
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=var.test(part1,part2)$p.value
  
  if(valp<0.05){Decision="Var. NO Estable"
  }else{Decision="NR"}
}

F_test.indic <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,par.f)})


#si valor P < alpha  : rechazo igualdad de varianzas


###################Estabilidad de la media
###################Test t: requiere Estabilidad en varianza

PruebaT=function(Estaciones){
  
  stat=na.omit(Estaciones)
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  valp=t.test(part1,part2)$p.value
  if(valp<0.05){EstabMedia="Medias Diferentes"
  }else{EstabMedia="NR"}
  
}
T_Test <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,PruebaT)})


###################Test U Mann Whitney o Wilcoxon: version no paramétrica de la prueba T

Umann <- function(Estaciones){
  
  stat=na.omit(Estaciones)
  m <- length(stat)/2
  part1=stat[1:m]
  part2=stat[(m+1):length(stat)]
  
  valp=wilcox.test(part1,part2)$p.value
  if(valp<0.05){EstabMedia="Medias Diferentes"
  }else{EstabMedia="NR"}
}

Umann_Test <- lapply(listArch,function(x){apply(x[,4:ncol(x)],2,Umann)})

