#' @export
idinput<-function(n,type='rgs',band=c(0,1),levels=c(-1,1)){
  if(type=="rbs"){
    v1<-gen.rbs(n,band,levels)
  } 
  else if(type=="rgs"){
    v1<-gen.rgs(n,band,levels)
  }
  return(v1)
}

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

gen.rbs<-function(n,band,levels){
  require(signal)
  v<-rnorm(n)
  
  v<-sapply(v, function(x) {if(x==0) rnorm(1) else x}, simplify = 'vector')
  #if we do not specify else case, it assigns it as NULL
  bfilt<-butter(8,band,type = 'pass',plane = 'z')
  v1<-filter(bfilt,v)
  v1<-sapply(v1, function(x) {if(x>0) levels[2] else levels[1]})
  return(v1)
}