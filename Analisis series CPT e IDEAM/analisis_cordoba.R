#CLIMATOLOGIAS CORDOBA LIZETH LLANOS

library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
data_cordoba=read.csv("precip_cordoba_zona.csv",header=T)
#data_cordoba=read.csv("precip_cordoba_zona.csv",col_types = eval(parse(text=list(paste(names(data_cordoba),"=col_double()",sep="")))))
apply(data_cordoba,2,mean)
problems(data_cordoba)

monthly <- data_cordoba %>%
  gather(station, precip, CampoBello_AltoSinu: PlanetaRica_SanJorge) %>%
  separate(station, c("station", "zone")) %>%
  group_by(year,month,station,zone)%>%
  summarise(precip=sum(precip,na.rm=T))%>%
  group_by(month,station,zone)%>%
  summarise(precip=mean(precip,na.rm=T))

clim=monthly%>%
  group_by(month,zone)%>%
  summarise(precip=mean(precip,na.rm=T))
 
clim2=clim[order(clim$zone),]
x11()
qplot(data=clim2,factor(month,labels=month.abb),geom="bar",weight=precip,fill=zone, color="black")+facet_wrap(~zone,scales="free_x")+ylab("Precipitation (mm)")+
  theme_bw()+theme(strip.text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=1,size=12),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  scale_y_continuous(breaks=seq(0,300,30),labels=seq(0,300,30))+
  xlab(" ")+scale_fill_brewer(type="seq", palette=3, guide = "none")+scale_color_manual(values = "black", guide = "none")



#GRAFICAS ENSO CORDOBA LIZETH LLANOS
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

cpt_cordoba=read_delim("precip_final_merge.txt",delim="\t",col_types=list(date=col_character(),Cerete=col_double(),Rabolargo=col_double()),na="99999")

clim=cpt_cordoba%>%
  separate(date,c("year","month"),sep="-")%>%
  gather(station,precip,Ayapel:AptoLosGarzones)%>%
  group_by(station,month)%>%
  summarise(precip=mean(precip,na.rm=T))%>%
  spread(station, precip)

write.table(clim,"clim_cpt_merge.txt",row.names=F)

library(ggplot2)

clim_all=read_delim("clim_all.txt",delim="\t")

clim2=clim_all%>%
  gather(station,precip,-month)%>%
  separate(station,c("station","fuente"))

x11()
qplot(data=clim2,factor(month,levels=month.abb),weight=precip,fill=fuente,geom="bar",color="black", position="dodge",binwidth =0.001)+facet_wrap(~station,scales="free_x")+
  theme_bw()+theme(strip.text = element_text(size=15),axis.text.x = element_text(angle=90, vjust=0.5,size=12),
                   axis.line = element_line(colour = "black"),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank())+
  scale_y_continuous(breaks=seq(0,400,50),labels=seq(0,400,50))+
  scale_fill_brewer(type="seq", palette="Blues")+ylab("Precipitation (mm)")+xlab(" ")+scale_color_manual(values = "black", guide = "none")

###Graficos ENSO
climsd=cpt_cordoba%>%
  separate(date,c("year","month"),sep="-")%>%
  gather(station,precip,Ayapel:AptoLosGarzones)%>%
  group_by(station,month)%>%
  summarise(precip=sd(precip,na.rm=T))%>%
  spread(station, precip)


vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

datos=read.table("C:\\Users\\lllanos\\Desktop\\RClimTool V2.0\\Enso_ONI.txt",header=T)
datos2=subset(datos,datos[,1]>=1980 & datos[,1]<=2014)

anom=as.data.frame(matrix("NA",420,20))
for(i in 2:ncol(cpt_cordoba)){
  anom[,i]=(cpt_cordoba[,i]-do.call("cbind",rep(clim[,i],35)))/as.numeric(unlist(rep(climsd[,i],35)))
  
}
datos2=cbind.data.frame(datos2,anom)


p=ggplot(data=datos2, aes(x=1:length(year),y=ONI,fill=ENSO))+geom_bar(stat="identity",position="dodge",binwidth=1)+
#scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
  theme(legend.title =element_text(color="white") ,legend.position="right")+ 
  scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))

p+geom_line(data=datos2,aes(1:length(year),V2,colour="Anomalías obsv."),size=0.5)+ylab("ONI")+
  scale_color_manual(values=c("black"))+ggtitle(paste(stationp[i]))
print(g3, vp = vplayout(i+1,1))  

for(i in 1:4)    { 
  g3=qplot(data=datos2, x=factor(year), weight=ONI,fill=ENSO,geom="bar",position="dodge",binwidth=0.005,colour="black") +  geom_bar(stat = "identity")+scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
    theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
    scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))
  +geom_line(aes(1:length(year),an2f[,i],colour="Anomalías obsv. tmin"),data=anf,size=0.5)+ylab("ONI")+
    scale_color_manual(values=c("black"))+ggtitle(paste(stationp[i]))
  print(g3, vp = vplayout(i+1,1))    }

g31=ggplot(data_m_e1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(data_m_e1),by=24), labels=seq(min(data_m_e1$year),max(data_m_e1$year),2))+xlab("")+ 
  theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
  scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an1[,1],colour="Anomalías obsv. tmín"),data=an1,size=0.5)+ylab("ONI")+
  scale_color_manual(values=c("black"))

g32=ggplot(data_m_e1, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(data_m_e1),by=24), labels=seq(min(data_m_e1$year),max(data_m_e1$year),2))+xlab("")+ 
  theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
  scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),an2[,1],colour="Anomalías obsv. precip"),data=an2,size=0.5)+ylab("ONI")+
  scale_color_manual(values=c("black"))

jpeg(paste("ENSO/Graficos_anomalias/anomalias.jpeg",sep=""), width = 9, height = 7,units = 'in',res=200)

#jpeg(paste("ENSO/Graficos/",station[j],"_b.jpeg",sep=""),width = 1000, height = 700)

x11()
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,1, heights = unit(c(0.5, 4,4,4,4), "null"))))
grid.text(paste(" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(g3, vp = vplayout(2,1))
print(g31, vp = vplayout(3,1))
print(g32, vp = vplayout(4,1))
dev.off()

write.csv(datos2,"anom_cordoba_cpt.csv",row.names=F)
