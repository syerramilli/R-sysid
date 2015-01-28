# class idframe
idframe <- function(output=numeric(0),input=numeric(0),
                     type=c("time","freq")[1],Ts = 1,
                     t.start=0,t.end=NA, tUnit = "seconds", 
                     frequencies = NA, fUnit= "Hz"){
  
  ## Input Validation
  if(!(type %in% c("time","freq"))) # type validation
    stop("Unknown domain type")
  
  if(length(output)!=0 && length(input)!=0){
    if(dim(output)[1]!=dim(input)[1]) # observation validation
      stop("Dimensions don't matach")
  }
  
  # Object Constructor
  dat <- list(output=data.frame(output),input=data.frame(input),type=type,Ts=Ts)
  n <- dim(output)[1]
  p <- dim(output)[2];m <- dim(input)[2]
  
  if(type=="freq"){

    if(is.na(frequencies)){
      frequncies <- seq(0,2*pi,length=n)
    }
    
    dat$frequencies <- frequencies
    dat$fUnit <- fUnit
    
  } else {
    
    if(is.na(t.end)) {
      t.end <- t.start + Ts*(n-1)
    } else {
      dat$Ts <- (t.end-t.start)/(n-1)
    }
    
    dat$tStart <- t.start; dat$tEnd <- t.end
    dat$tUnit <- tUnit
  }
      
  class(dat) <- "idframe"
  return(dat)
}

# plot method for idframe object
plot.idframe <- function(object,...){
  
  p <- dim(object$output)[2];m <- dim(object$input)[2]
  
  if(p!=1 && m!=1){
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    
    for(i in seq(m)){
      for(j in seq(p)){
        par(mfrow=c(2,1),mar=c(3,4,3,2))
        plot(.index(object),object$output[,j],xlab=object$type,
             ylab=colnames(object$output)[j],type="l",...)
        plot(.index(object),object$input[,i],xlab=object$type,
             ylab=colnames(object$input)[i],type="l",...)
      }
    }
  } else {
    par(mfrow=c(2,1))
    plot(.index(object),object$output[,1],xlab=object$type,
         ylab=colnames(object$output),type="l",...)
    plot(.index(object),object$input[,1],xlab=object$type,
         ylab=colnames(object$input),type="l",...)
  }  
}

.index <- function(object){
  if(object$type=="time"){
    return(seq(from=object$tStart,to=object$tEnd,by=object$Ts)) 
  } else {
    return(object$frequencies) 
  }
}

# summary method for idframe object
summary.idframe <- function(object,...){
  out_sum <- summary(object$output)
  in_sum <- summary(object$input)
  
  out <- list(outputs=out_sum,inputs=in_sum,Ts=object$Ts,type=object$type,
              tUnit=object$tUnit,no_of_samples = dim(outputs)[1])
  if(object$type=="time"){
    out$tStart <- object$tStart;out$tEnd <- object$tEnd
  } else{
    out$frequencies <- object$frequencies;out$fUnit <- object$fUnit
  }
  
  return(out)
}