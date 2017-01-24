require(refund)
data=read.table("clipboard",header=T)

Y=data.frame(data[1:365,2])
names(Y)[1]=data[1,1]
X=data.frame(data[1:365,3])
names(X)[1]=data[1,1]

for(i in 1:8){
ini=(i*365)+1
fin=ini+364
Y=cbind.data.frame(Y,data[ini:fin,2])
names(Y)[i+1]=data[ini,1]
X=cbind.data.frame(X,data[ini:fin,3])
names(X)[i+1]=data[ini,1]}

X=t(X)
Y=t(Y)

t <- seq(0,364)/364
s <- seq(0,364)/364

m1 <- pffr(Y ~ ff(X,xind=s,splinepars=list(bs="ps", k=10, m=c(2,1))), yind=t,
           ,bs.yindex=list(bs="ps", k=10, m=c(2,1)))
summary(m1)
plot(m1, pers=TRUE, pages=1)
preddata=list(Y=Y,X=X)
res=predict(m1, newdata=preddata, se.fit = T)

windows()
par(mfrow=c(3,3))
for(i in 1:9){
plot(preddata$Y[i,],type='l',ylim=c(0,max(preddata$Y[i,],res$fit[i,])))
lines(res$fit[i,],col="red")
lines(res$fit[i,]+(1.96*res$se.fit[i,]),col="blue")
lines(res$fit[i,]-(1.96*res$se.fit[i,]),col="blue")}

windows()
plot(preddata$Y[1,],type='l')
lines(res[1,],col="red")
lines(res_k20[1,],col="blue")


