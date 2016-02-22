#' @export
 library(bitops)
require(signal)

idin.prbs<-function(n,band=c(0,1),levels=c(0,1)){
  u=vector()
  for(i in 1:18){
    if(n / (2^i) < 1){
      u=idin.prbs12(i,band,levels)
      break
    }
  }
  return(u[1:n])
}

idin.prbs12 <- function(N,band=c(0,1),levels=c(0,1)){
  first=ceiling(abs(rnorm(1)*10))  #some non-zero initial state 
  x= first
  v = vector()  
  n=2^N-1
  i=1
  clock=floor(1/band[2])
  k=1
  M=rbind(c(0,0,0,0),c(1,2,0,0),c(1,3,0,0),c(1,4,0,0),c(2,5,0,0),c(1,6,0,0),
          c(1,7,0,0),c(1,2,7,8),c(4,9,0,0),c(3,10,0,0),c(9,11,0,0),
          c(6,8,11,12),c(9,10,12,13),c(4,8,13,14),c(14,15,0,0),c(4,13,15,16),
          c(14,17,0,0),c(11,18,0,0))
  repeat{
    a=M[N,1]
    b=M[N,2]
    c=M[N,3]
    d=M[N,4]
    four=c(8,12,13,14,16)
    if(N %in% four){
    e=bitwXor(bitwShiftR(x,N-a),bitwShiftR(x,N-b))
    f=bitwXor(bitwShiftR(x,N-c),bitwShiftR(x,N-d))
    newbit=bitwAnd(bitwXor(e,f),1)
    }else{
      newbit=bitwAnd(bitwXor(bitwShiftR(x,N-a),bitwShiftR(x,N-b)),1)
    }
     if(k>=clock || i==1){
       # newbit=bitwAnd(bitwXor(bitwShiftR(x,0),bitwShiftR(x,1)),1)
       v[i]=newbit
       i=i+1
       # x=bitwOr(bitwShiftR(x,1),bitwShiftL(newbit,6))
       k=1
     }else{
       
       v[i]=v[i-1]
       i=i+1
       k=k+1
     }
     
    x=bitwOr(bitwShiftR(x,1),bitwShiftL(newbit,N-1))
    
    #checking if it exceeds the repetition period or reaches
    #required no. of bits first
    if(x==first){
      cat("Repetition period is ",i-1)
      break
    }
    # else if(i-1==n){
    #   break
    # }
    
  }

  v=sapply(v, function(x){if (x==0) levels[1] else levels[2]})
  return(v)
}