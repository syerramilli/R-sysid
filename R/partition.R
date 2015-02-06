#' Subset an idframe data
#' 
#' Subsetting method for datas of class \code{idframe}
#' 
#' @param data an object of class \code{idframe}
#' @param indices the indices that need to be subsetted
#' @export
dataSlice <- function(data,indices){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  if(!all(indices %in% seq(to=dim(data$output)[1],by=1)))
    stop("Invalid indices")
  
  trim <- data
  trim$output <- trim$output[indices,,drop=F]
  trim$input <- trim$input[indices,,drop=F]
  
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
#' @param data an object of class \code{idframe}
#' @param p the percentage of the data that goes to training (Default : \code{0.6})
#' @return list containing estimation and validation idframe datas
#' 
#' @examples
#' data(cstr)
#' splitList <- dataPartition(cstr,p=0.6)
#' train <- splitList$estimation # training set 
#' test <- splitList$validation # testing set
#' 
#' @export
dataPartition <- function(data,p=0.6){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  index <- seq_along(data$output[,1])
  
  trainIndex <- index[1:round(p*length(index))]
  testIndex <- index[!(index %in% trainIndex)]
  
  train <- dataSlice(data,trainIndex)
  test <- dataSlice(data,testIndex)
  
  return(list(estimation=train,validation=test))
}