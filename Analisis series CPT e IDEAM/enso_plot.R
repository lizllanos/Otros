datos2=read.csv("anom_cordoba_cpt.csv",header=T)
station=names(clim)[-1]

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
x11()
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,2, heights = unit(c(4, 4,4,4,4), "null"))))

for(i in 7:11){
  g3=ggplot(datos2, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
  theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
  scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),datos2[,i],colour=paste("Anomalías",station[i-6])),data=datos2,size=0.5)+ylab("ONI")+
  scale_color_manual(values=c("black"))
  print(g3, vp = vplayout(i-6,1)) 
}

for(i in 11:15){
  g3=ggplot(datos2, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
    theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
    scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),datos2[,i],colour=paste("Anomalías",station[i-6])),data=datos2,size=0.5)+ylab("ONI")+
    scale_color_manual(values=c("black"))
  print(g3, vp = vplayout(i-10,2)) 
}



vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
x11()
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,2, heights = unit(c(4, 4,4,4,4), "null"))))


for(i in 16:20){
  g3=ggplot(datos2, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
    theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
    scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),datos2[,i],colour=paste("Anomalías",station[i-6])),data=datos2,size=0.5)+ylab("ONI")+
    scale_color_manual(values=c("black"))
  print(g3, vp = vplayout(i-15,1)) 
}

for(i in 21:25){
  g3=ggplot(datos2, aes(x=1:length(year), y=ONI )) +  geom_bar(stat = "identity",   aes(fill = ENSO))+scale_x_continuous(breaks=seq(1,nrow(datos2),by=24), labels=seq(min(datos2$year),max(datos2$year),2))+xlab("")+ 
    theme(legend.title =element_text(color="white") ,legend.position="right")+ #scale_fill_discrete(breaks = c("Niño","Normal","Niña"))
    scale_fill_manual(values=c( "dodgerblue2","firebrick3","chartreuse3"), name=" ", breaks=c("Niño","Normal","Niña"),labels=c("Niño","Normal","Niña"))+geom_line(aes(1:length(year),datos2[,i],colour=paste("Anomalías",station[i-6])),data=datos2,size=0.5)+ylab("ONI")+
    scale_color_manual(values=c("black"))
  print(g3, vp = vplayout(i-20,2)) 
}




vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
x11()
grid.newpage()
pushViewport(viewport(layout = grid.layout(5,2, heights = unit(c(4, 4,4,4,4), "null"))))
#grid.text(paste(" "), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(g3, vp = vplayout(2,1))
print(g31, vp = vplayout(3,1))
print(g32, vp = vplayout(4,1))
dev.off()