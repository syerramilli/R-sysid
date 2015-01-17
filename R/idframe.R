# class idframe
idframe <- function(output=data.frame(numeric(0)),input=data.frame(numeric(0)),
                     type=c("time","freq")[1],Ts = 1,
                     outputnames = colnames(output),inputnames = colnames(input),
                     t.start=0,t.end=NA, timeUnit = "seconds", 
                     frequencies = NA, freqUnit= NA){
  out <- list(output=output,input=input,type=type,Ts=Ts)
  class(out) <- "idframe"
  return(out)
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

