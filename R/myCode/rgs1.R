gen.rgs<-function(n,band,levels){
  require(signal)
  mu<-(levels[1]+levels[2])/2
  sigma<-(levels[2]-levels[1])/2
  v<-rnorm(n,mu,sigma)
  v<-sapply(v, function(x) {if(x==0) rnorm(1) else x})
  gfilt<-butter(8,band,type ='pass',plane ='z')
  v1<-filter(gfilt,v)
  return(v1)
}