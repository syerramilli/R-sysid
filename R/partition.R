#' Subset an idframe data
#' 
#' \code{dataSlice} is a Subsetting method for objects of class \code{idframe}. It 
#' extracts the subset of the object \code{data} observed between indices \code{start}
#' and \code{end}.  If a frequency is specified, the series is then re-sampled at the 
#' new frequency.
#' 
#' @param data an object of class \code{idframe}
#' @param start the start index
#' @param end the end index
#' @param freq the new sampling frequency
#' 
#' @details
#' The dataSlice function extends the \code{\link[stats]{window}} function for idframe
#' objects
#' 
#' @return an idframe object
#' 
#' @examples
#' data(cstr)
#' cstrsub <- dataSlice(cstr,start=200,end=400) # extract between indices 200 and 400
#' cstrTrain <- dataSlice(cstr,end=4500) # extract upto index 4500
#' cstrTest <- dataSlice(cstr,start=6501) # extract from index 6501 till the end
#' cstr_new <- dataSlice(cstr,freq=3) # resample data at thrice the frequency  
#' 
#' @seealso \code{\link[stats]{window}}
#' @export
dataSlice <- function(data,start=NULL,end=NULL,freq=NULL){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  nin <- dim(data$input)[2]; nout <- dim(data$output)[2]
  dataMatrix <- cbind(data$input,data$output)
  if(data$type=="freq"){
    dataMatrix <- cbind(dataMatrix,data$frequencies)
  } else {
    timeSeq <- seq(from=data$t.start,to=data$t.end,by=data$Ts)
    dataMatrix <- cbind(dataMatrix,timeSeq)
  }
  
  l <- as.list(dataMatrix)
  trimData <- as.data.frame(sapply(l,window,start=start,end=end,deltat=freq))
  
  trim <- idframe(output=trimData[,(nin+1):(nin+nout),drop=F],
                  input=trimData[,1:nin,drop=F],type=data$type,Ts=data$Ts,
                  tUnit=data$tUnit)
  
  if(trim$type=="freq"){
    trim$frequncies <- trimData[,ncol(trimData)]
  } else {
    trim$t.start <- trimData[1,ncol(trimData)]
    trim$t.end <- trimData[nrow(trimData),ncol(trimData)]
  }
  
  return(trim)
}