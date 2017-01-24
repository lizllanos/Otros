################### LIZETH LLANOS HERRERA ###########################
##### Script para hacer control de calidad a archivos de CPT ########
######################### CIAT ######################################


require(lubridate)
require(Kendall)
require(DescTools)
require(ggplot2)
require(grid)
require(reshape)
library(scales)

setwd("C:\\Users\\lllanos\\Desktop\\Datos Estaciones CPT\\data")

#Load data
tmax_cpt=read.csv("tmax.csv",header=T)
tmin_cpt=read.csv("tmin.csv",header=T)
precip_cpt=read.csv("precip.csv",header=T)

############ Analysis TMAX #############################


tmax_dpto=read.table("clipboard",header=T)
tmax_all=melt(tmax_cpt_2,id="date")
tmax_all$date2=as.Date(as.character(paste(tmax_all$date,"01",sep="-")),format ="%Y-%m-%d")
tmax_all$dpto=melt(matrix(0,nrow(tmax_cpt),ncol(tmax_cpt_2)-1,dimnames=list(NULL,c(as.character(catalog[,3])))))[,2]
  
#Plot all stations simultaneous
x11()
a<-ggplot(tmax_all[which(tmax_all$variable!="TOTORO" & tmax_all$variable!="POPAYAN"),], aes(x = date2, y = as.numeric(value)))+geom_line()+ stat_smooth(color="red",se = FALSE)+facet_wrap( ~ variable)+xlab("")+ylab("Temperatura máxima (°C)")+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

print(a)
ggsave("ALL_STATIONS.jpg", width=18, height=13)


#Plot stations by department

nom_dpt=unique(tmax_dpto[,2])

for(i in 1:length(nom_dpt)){
  ggplot(tmax_all[which(tmax_all$dpto==nom_dpt[i]),], aes(x = date2, y = as.numeric(value)))+geom_line()+ stat_smooth(color="red",se = FALSE)+facet_wrap( ~ variable)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Temperatura máxima (°C)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}

#BoxPlot stations by department


for(i in 1:length(nom_dpt)){
  ggplot(tmax_all[which(tmax_all$dpto==nom_dpt[i]),], aes(x = variable, y = as.numeric(value)))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Temperatura máxima (°C)")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"))}




############ Analysis TMIN #############################
tmin_dpto=read.table("clipboard",header=T)
tmin_all=melt(tmin_cpt,id="date")
tmin_all$date2=as.Date(as.character(paste(tmin_all$date,"01",sep="-")),format ="%Y-%m-%d")
tmin_all$dpto=melt(matrix(0,nrow(tmin_cpt),ncol(tmin_cpt)-1,dimnames=list(NULL,c(as.character(tmin_dpto[,2])))))[,2]

#Plot all stations simultaneous
x11()
a<-ggplot(tmin_all[which(tmin_all$variable!="TOTORO" & tmin_all$variable!="POPAYAN"),], aes(x = date2, y = as.numeric(value)))+geom_line()+facet_wrap( ~ variable)+xlab("")+ylab("Temperatura máxima (°C)")+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

print(a)
ggsave("ALL_STATIONS.jpg", width=18, height=13)

#Plot stations by department
nom_dpt=unique(tmin_dpto[,2])

for(i in 1:length(nom_dpt)){
  ggplot(tmin_all[which(tmin_all$dpto==nom_dpt[i]),], aes(x = date2, y = as.numeric(value)))+geom_line()+ stat_smooth(color="red",se = FALSE)+facet_wrap( ~ variable)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Temperatura mínimo (°C)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}

#BoxPlot stations by department

for(i in 1:length(nom_dpt)){
  ggplot(tmin_all[which(tmin_all$dpto==nom_dpt[i]),], aes(x = variable, y = as.numeric(value)))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Temperatura mínimo (°C)")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"))}



############ Analysis PRECIP ######################

precip_dpto=read.table("clipboard",header=T)
precip_all=melt(precip_cpt,id="date")
precip_all$date2=as.Date(as.character(paste(precip_all$date,"01",sep="-")),format ="%Y-%m-%d")
precip_all$dpto=melt(matrix(0,nrow(precip_cpt),ncol(precip_cpt)-1,dimnames=list(NULL,c(as.character(precip_dpto[,2])))))[,2]

#Plot all stations simultaneous
x11()
a<-ggplot(precip_all, aes(x = date2, y = as.numeric(value)))+geom_line()+facet_wrap( ~ variable)+xlab("")+ylab("Precipitación (mm)")+ 
  theme(axis.text.x = element_text(angle = 60, hjust = 1,size=11),legend.position="none")

print(a)
ggsave("ALL_STATIONS.jpg", width=18, height=13)

#Plot stations by department
nom_dpt=unique(precip_dpto[,2])

for(i in 1:length(nom_dpt)){
  ggplot(precip_all[which(precip_all$dpto==nom_dpt[i]),], aes(x = date2, y = as.numeric(value)))+geom_line()+ stat_smooth(color="red",se = FALSE)+facet_wrap( ~ variable)+labs(title=paste("DEPARTAMENTO DE ",nom_dpt[i]))+xlab("")+ylab("Precipitación (mm)")+scale_x_date(breaks = "4 years",labels=date_format("%Y"))+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=13),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],".jpg"))}

#BoxPlot stations by department
for(i in 1:length(nom_dpt)){
  ggplot(precip_all[which(precip_all$dpto==nom_dpt[i]),], aes(x = variable, y = as.numeric(value)))+geom_boxplot()+facet_wrap( ~ dpto,scales="free")+xlab("")+ylab("Precipitación (mm)")+
    theme(axis.text.x = element_text(angle = 60, hjust = 1,size=8),panel.background = element_rect(fill = 'grey85'),strip.background = element_rect(fill = "lightsteelblue1"),panel.margin = unit(0.8, "lines"))
  ggsave(paste(nom_dpt[i],"_box.jpg"))}


