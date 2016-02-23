#' function to generate input singals (rgs/rbs/prbs/sine)
#' 
#' \code{idinput} is a function for generating input signals (rgs/rbs/prbs/sine) for identification purposes
#' 
#' @param n integer length of the input singal to be generated
#' @param type the type of input signal to be generated. 
#' 'rgs' - generates random gaussian signal
#' 'rbs' - generates random binary signal
#' 'prbs' - generates pseudorandom binary signal
#' 'sine' - generates a signal that is a sum of sinusoids
#' 
#' Default value is type='rgs'
#' @param band determines the frequency content of the signal. 
#' For type='rbs'/'sine'/,  band = [wlow,whigh]
#' which specifies the lower and the upper bound of the passband frequencies(expressed as fractions of Nyquist frequency). Default is c(0,1)
#' For type='prbs', band=[0,B]
#' where B is such that the singal is constant over 1/B (clock period). Default is c(0,1)
#' @param levels row vector defining the input level. It is of the form 
#' levels=c(minu, maxu)
#' For 'rbs','prbs', 'sine', the generated signal always between minu and maxu.
#' For 'rgs', minu=mean value of signal minus one standard deviation and maxu=mean value of signal plus one standard deviation
#' 
#' Default value is levels=c(-1,1)
#' 
#' @export
idinput<-function(n,type='rgs',band=c(0,1),levels=c(-1,1)){
  if(type=="rbs"){
    rbs(n,band,levels)
  } else if(type=="rgs"){
    rgs(n,band,levels)
  } else if(type=="sine"){
    multisine(n,1,band,levels)
  }else if(type=="prbs"){
    idin.prbs(n,band,levels)
  }
}

rgs <- function(n,band,levels){
  u <- butter_filt(rnorm(n),band)
  mu<-(levels[1]+levels[2])/2
  sigma<-(levels[2]-levels[1])/2
  u*sigma+mu
}

rbs <- function(n,band,levels){
  u <- butter_filt(rnorm(n),band)
  sapply(u,function(x) if(x>0) levels[2] else levels[1])
}

butter_filt <- function(x,band){
  filt <- T; type <- "pass"
  if(band[1]<=2e-3){
    if(band[2]==1){
      filt <- F
    } else{
      type <- "low"
    }
  } else{
    if(band[2]==1){
      type <- "high"  
    }
  }
  if(filt==T){
    if(type=="low"){
      bf <- signal::butter(8,band[2],type,"z")
    } else if(type=="pass"){
      bf <- signal::butter(8,band,type,"z")
    }else{
      bf <- signal::butter(8,band[1],type,"z")
    }
    x <- as.numeric(signal::filter(bf,x))
  }
  return(matrix(x,ncol=1))
}

multisine <- function(N,nin=1,band,levels){
  sinedata <- list(nSin=10,nTrial=10,gridSkip=1)
  freq <- 2*pi*seq(1,floor(N/2),by=sinedata$gridSkip)/N
  band <- band*pi
  freq <- freq[freq>=band[1] & freq<=band[2]]
  nl <- length(freq)
  freqInd <- seq(from=1,to=nl,length.out = nin*sinedata$nSin)
  
  # Begin Trials
  trials <- function(x){
    freqs <- freq[freqInd[seq(from=x,to=nin*sinedata$nSin,by=nin)]]
    for(k in 1:sinedata$nTrial){
      utrial <- rowSums(cos(sapply(freqs,
                                   function(x) (1:N-1)*x+2*pi*rnorm(1))))
      if(k==1) u <- utrial; temp <- diff(range(utrial))
      if(diff(range(utrial))< temp) {
        u <- utrial; temp <- diff(range(utrial))
      }
    }
    clevel <- max(abs(u))
    diff(levels)/2/clevel*(u+clevel)+levels[1]
  }
  u <- sapply(1:nin,trials) 
  return(u)
}

#' @import bitops signal
idin.prbs<-function(n,band=c(0,1),levels=c(0,1)){
  u <- numeric(0)
  for(i in 1:18){
    if(n / (2^i) < 1){
      u <- idin.prbs12(i,band,levels)
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
      v[i]=newbit
      i=i+1
      k=1
    }else{
      v[i]=v[i-1]
      i=i+1
      k=k+1
    }
    x=bitwOr(bitwShiftR(x,1),bitwShiftL(newbit,N-1))
    if(x==first){
      break
    }
  }
  
  v=sapply(v, function(x){if (x==0) levels[1] else levels[2]})
  return(v)
}