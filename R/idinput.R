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
  } 
  else if(type=="rgs"){
    rgs(n,band,levels)
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