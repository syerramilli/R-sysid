# class idframe
idframe <- function(output=data.frame(numeric(0)),input=data.frame(numeric(0)),
                     type=c("time","freq")[1],Ts = 1,
                     outputnames = colnames(output),inputnames = colnames(input),
                     t.start=0,t.end=NA, timeUnit = "seconds", 
                     frequencies = NA, freqUnit= "Hz"){
  
  ## Input Validation
  if(!(type %in% c("time","freq"))) # type validation
    stop("Unknown domain type")
  
  if(dim(output)[1]!=dim(input)[1]) # observation validation
    stop("Dimensions don't matach")
  
  # Object Constructor
  dat <- list(output=data.frame(output),input=data.frame(input),type=type,Ts=Ts)
  n <- dim(output)[1]
  p <- dim(output)[2];m <- dim(input)[2]
  
  if(outputnames==NULL)
    outputnames <- sapply(1:p,FUN=function(x){paste("y",as.character(x),sep="")}})
  
  if(inputnames==NULL)
    inputnames <- sapply(1:m,FUN=function(x){paste("u",as.character(x),sep="")}})
  
  colnames(dat$output) <- outputnames
  colnames(dat$input) <- inputnames
  
  if(type=="freq"){

    if(is.na(frequencies)){
      frequncies <- seq(0,2*pi,length=n)
    }
    
    dat$frequencies <- frequencies
    dat$freqUnit <- freqUnit
    
  } else {
    
    if(is.na(t.end)) {
      t.end <- t.start + Ts*(n-1)
    } else {
      out$Ts <- (t.end-t.start)/(n-1)
    }
    
    out$tStart <- t.start; out$tEnd <- t.end
    out$timeUnit <- timeUnit
  }
      
  class(dat) <- "idframe"
  return(dat)
}

# print method for idframe class
print.idframe <- function(object,...){
  
}

# plot method for idframe object
plot.idframe <- function(object,...){
  
}

# summary method for idframe object
summary.idframe <- function(object,...){
  
}