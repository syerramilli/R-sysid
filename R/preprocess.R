#' Remove offsets and linear trends
#' 
#' Removes the offsets or linear trends in each of the input and output matrices. 
#' 
#' @param data an object of class \code{idframe}
#' @param type trend type - "constant" or "linear". (Default: \code{"linear"})
#' 
#' @return 
#' A list containing the following elements
#' 
#' \tabular{ll}{
#'    \code{fitted.values} \tab \code{idframe} object with detrended variables \cr
#'    \code{output.trend} \tab \code{list} containing trend fits for each output 
#'    variable \cr
#'    \code{input.trend} \tab \code{list} containing trend fits for each input 
#'    variable
#'  }
#' 
#' @examples
#' data(cstr)
#' fit <- detrend(cstr) # remove linear trends 
#' Zdetrend <- predict(fit) # get the detrended data
#' 
#' demean <- detrend(cstr,type="constant") # remove offsets
#' Zcent <- predict(demean) # get the centered data
#' 
#' @seealso \code{\link{predict.detrend}}, \code{\link[stats]{lm}}
#' @export
detrend <- function(data,type=c("constant","linear")[2]){ 
  
  if(!(type %in% c("constant","linear"))){
    stop("Error: Invalid trend type")
  }
  
  reg <- time(data$output[,1])
  
  if(type=="linear"){
    formula <- X ~ reg
  } else {
    formula <- X ~ 1 + offset(0*reg)  
  }
  
  data_detrend <- data
  out <- data$output;output_trend <- list()
  for(i in 1:ncol(out)){
    output_trend[[i]] <- lm(formula,data=data.frame(X=out[,i],reg=reg))
    out[,i] <- fitted(output_trend[[i]])
  }
  
  input <- data$input;input_trend <- list()
  
  for(i in 1:ncol(input)){
    input_trend[[i]] <- lm(formula,data=data.frame(X=input[,i],reg=reg))
    input[,i] <- fitted(input_trend[[i]])
  }
  
  data_detrend$output <- data$output - out;data_detrend$input <- data$input - input
    
  est <- list(fitted.values=data_detrend,output.trend = output_trend,
              input.trend = input_trend)
  
  class(est) <- "detrend"
  return(est)
}

#' Predict method for trend fits on idframe objects
#' 
#' Returns detrended \code{idframe} object based on linear trend fit
#' 
#' @param object an object of class \code{idframe}
#' @param newdata An optional idframe object in which to look for variables with 
#' which to predict. If ommited, the original detrended idframe object is used
#' 
#' @return an \code{idframe} object
#' 
#' @examples
#' data(cstr)
#' train <- dataSlice(cstr,end=5000) # subset the first 5000 indices
#' test <- dataSlice(cstr,start=6001) # subset from index 6001 till the end
#' fit <- detrend(train)
#' Ztrain <- predict(fit)
#' Ztest <- predict(fit,test)
#' 
#' @export
predict.detrend <- function(object,newdata=NULL,...){
  
  if(is.null(newdata)){
    data <- fitted(object)
  } else{
     data <- newdata
     out <- detrend.predict(object$output.trend,data$output)
     input <- detrend.predict(object$input.trend,data$input)
     data$output <- data$output - out
     data$input <- data$input - input
  }
  return(data)
}

detrend.predict <- function(object,data){
  pred_list <- list()
  for(i in 1:ncol(data)){
    pred_list[[i]] <- predict(object[[i]],newdata=data.frame(reg = time(data[,i])))
  }
  pred <- data.frame(matrix(unlist(pred_list),ncol=ncol(data),byrow=T))
  colnames(pred) <- colnames(data)
  return(pred)
}

#' Replace Missing Data by Interpolation
#' 
#' Function for replacing missing values with interpolated ones. This is an
#' extension of the \code{na.approx} function from the \code{zoo} package.
#' The missing data is indicated using the value \emph{NA}.
#' 
#' @param data an object of class \code{idframe}
#' @return 
#' data (an idframe object) with missing data replaced.
#' 
#' @seealso \code{\link[zoo]{na.approx}}
#' 
#' @examples
#' data(cstr_mis)
#' summary(cstr_mis) # finding out the number of NAs
#' cstr <- misdata(cstr_mis)
#' 
#' @export
misdata <- function(data){
  require(zoo)
  
  f <- function(var,start,end,Ts){
    var <- ts(data=var,start=start,end=end,frequency=floor(1/Ts))
    out <- na.approx(var,na.rm=F)
    return(as.numeric(out))
  }
  
  dataout <- data
  dataout$output <- data.frame(apply(data$output,2,f,start=data$t.start,
                     end=data$t.end,Ts= data$Ts))
  dataout$input <- data.frame(apply(data$input,2,f,start=data$t.start,
                     end=data$t.end,Ts= data$Ts))
  
  return(dataout)
}

#' Subset or Resample idframe data
#' 
#' \code{dataSlice} is a subsetting method for objects of class \code{idframe}. It 
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