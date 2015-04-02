#' Remove linear trends
#' 
#' Removes the mean value or linear trends in each of the input and output matrices. 
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
#' demean <- detrend(cstr,type="constant") # remove mean values
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
#' Detrended \code{idframe} object based on linear trend fit
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