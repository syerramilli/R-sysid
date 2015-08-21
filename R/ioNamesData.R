require(tframe)

#' @export
nInputSeries <- function(data) ncol(data$input)

#' @export
nOutputSeries <- function(data) ncol(data$output)

#' @export
inputData <- function(x,series) UseMethod("inputData")

#' @export
inputData.default <- function(x,series=NULL){
  print("Not defined for this datatype")
}

#' @export
inputData.idframe <- function(x,series=seq(nInputSeries(x))){
  if (is.null(x$input))
    NULL
  else selectSeries(x$input,series=series)
}

"inputData<-" <- function(x,value) UseMethod("inputData<-")

"inputData<-.idframe" <- function(x,value){
  x$input <- value
  x
}

#' @export
inputNames <- function(x) UseMethod("inputNames")

#' @export
inputNames.default <- function(x){
  print("Not defined for this datatype")
}

#' @export
inputNames.idframe <- function(x){
  seriesNames(inputData(x))
}

#' @export
"inputNames<-" <- function(x,value) UseMethod("inputNames<-")

#' @export
"inputNames<-.idframe" <- function(x,value){
  seriesNames(inputData(x)) <- value
  x
}

#' @export
outputData <- function(x,series) UseMethod("outputData")

#' @export
outputData.default <- function(x,series=NULL){
  print("Not defined for this datatype")
}

#' @export
outputData.idframe <- function(x,series=seq(nOutputSeries(x))){
  if (is.null(x$output))
    NULL
  else selectSeries(x$output,series=series)
}

"outputData<-" <- function(x,value) UseMethod("outputData<-")

"outputData<-.idframe" <- function(x,value){
  x$output <- value
  x
}

#' @export
outputNames <- function(x) UseMethod("outputNames")

#' @export
outputNames.default <- function(x){
  print("Not defined for this datatype")
}

#' @export
outputNames.idframe <- function(x){
  seriesNames(outputData(x))
}

#' @export 
"outputNames<-" <- function(x,value) UseMethod("outputNames<-")

#' @export
"outputNames<-.idframe" <- function(x,value){
  seriesNames(outputData(x)) <- value
  x
}
