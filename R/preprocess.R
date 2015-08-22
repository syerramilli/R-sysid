#' Remove offsets and linear trends
#' 
#' Removes the offsets or linear trends in each of the input and output matrices. 
#' 
#' @param x an object of class \code{idframe}
#' @param type trend type - "constant" or "linear". (Default: \code{"constant"})
#' 
#' @return 
#' A list containing the following elements
#' 
#' \tabular{ll}{
#'    \code{fitted.values} \tab \code{idframe} object with detrended variables \cr
#'    \code{output_trend} \tab \code{list} containing trend fits for each output 
#'    variable \cr
#'    \code{input_trend} \tab \code{list} containing trend fits for each input 
#'    variable
#'  }
#' 
#' @examples
#' data(cstr)
#' fit <- detrend(cstr,type="linear") # remove linear trends 
#' Zdetrend <- predict(fit) # get the detrended data
#' 
#' demean <- detrend(cstr) # remove offsets
#' Zcent <- predict(demean) # get the centered data
#' 
#' @seealso \code{\link{predict.detrend}}, \code{\link[stats]{lm}}
#' @export
detrend <- function(x,type=c("constant","linear")[1]){ 
  
  if(!(type %in% c("constant","linear"))){
    stop("Error: Invalid trend type")
  }
  
  reg <- time(x)
  
  if(type=="linear"){
    formula <- X ~ reg
  } else {
    formula <- X ~ 1 + offset(0*reg)  
  }
  
  # Return Variables
  Z <- x # Detrended object
  output_trend <- NULL # object containing the output trend fits/offsets
  input_trend <- NULL # object containing the input trend fits/offsets
  
  # Function which performs linear regression across every column
  multilm <- function(x,formula,reg){
    l <- lapply(as.list(x),function(x) data.frame(X=x,reg=reg))
    trend <- lapply(l,function(x) lm(formula,data=x))
    trend
  }
  
  if(nOutputSeries(x)!=0){
    output_trend <- multilm(outputData(x),formula,reg)
    outputData(Z) <- ts(sapply(output_trend,resid),start=reg[1],
                        end=tail(reg,n=1),deltat=deltat(x))
  }
  
  if(nInputSeries(x)!=0){
    input_trend <- multilm(inputData(x),formula,reg)
    inputData(Z) <- ts(sapply(input_trend,resid),start=reg[1],
                        end=tail(reg,n=1),deltat=deltat(x))
  }
  
  est <- list(fitted.values=data_detrend,output_trend = output_trend,
              input_trend = input_trend)
  
  class(est) <- "detrend"
  return(est)
}

#' Detrend data based on linear trend fits
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
#' train <- dataSlice(cstr,end=5000)
#' test <- dataSlice(cstr,start=6001)
#' fit <- detrend(train)
#' Ztrain <- predict(fit)
#' Ztest <- predict(fit,test)
#' 
#' @export
predict.detrend <- function(model,newdata=NULL,...){
  
  if(is.null(newdata)){
    x <- fitted(model)
  } else{
    x <- newdata; reg <- time(x)
    
    # checking if the original data has outputs
    if(!is.null(model$output_trend)){
      y <- ts(sapply(output_trend,predict,newdata=data.frame(reg=reg)),
              start=reg[1],end=tail(reg,n=1),deltat = deltat(x))
      outputData(x) <- outputData(x) - y
    }
    
    if(!is.null(model$input_trend)){
      y <- ts(sapply(in_trend,predict,newdata=data.frame(reg=reg)),
              start=reg[1],end=tail(reg,n=1),deltat = deltat(x))
      inputData(x) <- inputData(x) - y
    }
  }
  return(x)
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
#' @importFrom zoo na.approx
#' @export
misdata <- function(data){
  require(zoo)
  
  f <- function(var,start,end,Ts){
    time_range <- range(time(var))
    start <- time_range[1];end <- time_range[2]
    Ts <- deltat(var)
    var <- ts(data=var,start=start,end=end,deltat=Ts)
    out <- na.approx(var,na.rm=F)
    return(as.numeric(out))
  }
  
  Z <- data
  outputData(Z) <- apply(outputData(data),2,f)
  inputData(Z) <- apply(inputData(data),2,f)
  Z
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
#' @param freq fraction of the original frequency at which the series
#' to be sampled.
#' 
#' @details
#' The dataSlice function extends the \code{\link[stats]{window}} 
#' function for idframe objects
#' 
#' @return an idframe object
#' 
#' @examples
#' data(cstr)
#' cstrsub <- dataSlice(cstr,start=200,end=400) # extract between indices 200 and 400
#' cstrTrain <- dataSlice(cstr,end=4500) # extract upto index 4500
#' cstrTest <- dataSlice(cstr,start=6501) # extract from index 6501 till the end
#' cstr_new <- dataSlice(cstr,freq=0.5) # resample data at half the original frequency  
#' 
#' @seealso \code{\link[stats]{window}}
#' @export
dataSlice <- function(data,start=NULL,end=NULL,freq=NULL){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  indexWindow <- function(y,start,end,freq){
    Y <- as.matrix(y); z <- as.vector(time(y))
    Y <- window(Y,start=start,end=end,frequency=freq)
    zw <- window(z,start=start,end=end,frequency=freq)
    y <- ts(Y,start=zw[1],end=tail(zw,n=1),deltat=diff(zw)[1])
  }
  if(nOutputSeries(data)!=0)
    outputData(data) <- indexWindow(outputData(data),start,end,freq)
  
  if(nInputSeries(data)!=0)
    inputData(data) <- indexWindow(inputData(data),start,end,freq)
  
  return(data)
}