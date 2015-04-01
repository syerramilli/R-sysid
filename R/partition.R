#' Subset an idframe data
#' 
#' Subsetting method for objects of class \code{idframe}
#' 
#' @param data an object of class \code{idframe}
#' @param start the start
#' @export
dataSlice <- function(data,start=NULL,end=NULL,freq=NULL){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  nin <- dim(data$input)[2]; nout <- dim(data$output)[2]
  dataMatrix <- cbind(data$input,data$output)
  if(type=="freq"){
    dataMatrix <- cbind(dataMatrix,data$frequencies)
  } else {
    timeSeq <- seq(from=data$t.start,to=data$t.end,by=data$Ts)
    dataMatrix <- cbind(dataMatrix,timeSeq)
  }
  
  l <- as.list(dataMatrix)
  trimData <- as.data.frame(sapply(l,window,start=start,end=end,deltat=freq))
  
  trim <- idframe(output=trimData[,(nin+1):(nin+nout+1)],input=trimData[,1:nin],
                  type=data$type,Ts=data$tTs,tUnit=data$tUnit)
  
  if(trim$type=="freq"){
    trim$frequncies <- trimData[,ncol(trimData)]
  } else {
    trim$t.start <- trimData[1,ncol(trimData)]
    trim$t.end <- trimData[nrow(trimData),ncol(trimData)]
  }
  
  return(trim)
}