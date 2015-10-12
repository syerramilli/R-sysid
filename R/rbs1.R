gen.rbs<-function(n,band,type){
  require(signal)
  v<-rnorm(n)
  for(i in 1:n){
    if (v[i]==0){
      
      v[i]=rnorm(1)
    }
  }
  a<-butter(8,band,type = type,plane = 'z')
  v1=filter(a,v)
  for(i in 1:n){
    
    if(v1[i]>0){
      v1[i]=1
    }
    if(v1[i]<0){
      v1[i]=-1
    }
  }
    return(v1)
}