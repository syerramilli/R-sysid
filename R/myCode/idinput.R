idinput<-function(n,type='rgs',band=c(0,1),levels=c(-1,1)){
  if(type=="rbs"){
    v1<-gen.rbs(n,band,levels)
  } 
  else if(type=="rgs"){
    v1<-gen.rgs(n,band,levels)
  }
  return(v1)
}