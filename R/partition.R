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
  
  if(trim$type=="freq")
    trim$frequncies <- trim$frequencies[indices]
}