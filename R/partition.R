#' Subset an idframe object
#' 
#' Subsetting method for objects of class \code{idframe}
#' 
#' @param object an object of class \code{idframe}
#' @param indices the indices that need to be subsetted
#' @export
dataSlice <- function(object,indices){
  # check if the class is correct
  if(class(object)!='idframe')
    stop("Not an idframe object")
  
  if(!all(indices %in% seq(to=dim(object$output)[1],by=1)))
    stop("Invalid indices")
  
  trim <- object
  trim$output <- trim$output[indices,]
  trim$input <- trim$input[indices,]
  
  if(trim$type=="freq"){
    trim$frequncies <- trim$frequencies[indices] 
  } else {
    trim$t.start <- trim$t.start + trim$Ts*(indices[1]-1)
    trim$t.end <- trim$t.start + trim$Ts*(length(indices)-1)
  }
  
  return(trim)
}

#' Split data into training and validation sets
#' 
#' The function splits the data into training and validation sets and returns them bundled
#' as a list. The size of the sets are determined by the parameter \code{p}.
#' 
#' @param object an object of class \code{idframe}
#' @param p the percentage of the data that goes to training (Default : \code{0.6})
#' @return list containing estimation and validation idframe objects
#' @export
dataPartition <- function(object,p=0.6){
  # check if the class is correct
  if(class(object)!='idframe')
    stop("Not an idframe object")
  
  index <- seq_along(object$output[,1])
  
  trainIndex <- index[1:round(p*length(index))]
  testIndex <- index[!(index %in% trainIndex)]
  
  train <- dataSlice(object,trainIndex)
  test <- dataSlice(object,testIndex)
  
  return(list(estimation=train,validation=test))
}