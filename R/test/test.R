m<-idpoly(A=c(1,-1.5,0.7),B=c(1,0.5),ioDelay=1)
u<-rnorm(1000)
y<-sim(m,u,sigma=0.1)
z<-idframe(y,u)
outputNames(z)<-"y"
inputNames(z)<-"u"
plot(z)