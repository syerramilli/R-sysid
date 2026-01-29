#' Data input into a idframe object
#' 
#' Read the contents of a data.frame/matrix into a \code{idframe} object.
#' 
#' @param data a \code{data.frame} object
#' @param ninputs the number of input columns. (Default: 0)
#' @param Ts sampling interval (Default: 1)
#' @param unit Time Unit (Default: "seconds")
#' 
#' 
#' @return an idframe object
#' @examples
#' data(cstrData)
#' data <- read.idframe(cstrData,ninputs=1,Ts= 1,unit="minutes")
#' 
#' @export 
read.idframe <- function(data,ninputs=NULL,Ts = 1,
                         unit=c("seconds","minutes","hours",
                                "days")[1]){
    outIndex <- 1:dim(data)[2]; inputs <- NULL
    if(ninputs!=0){
      inputs <- data[,1:ninputs,drop=F]
      outIndex <- seq(ninputs+1,dim(data)[2],by=1)
    }
    outputs <- data[,outIndex,drop=F]
    
    out <- idframe(output=outputs,input=inputs,Ts=Ts,unit=unit)
  
  return(out)
}

#' Read the contents of a table-formatted file
#' 
#' Read the contents of an file in table format into a \code{idframe} object.
#' 
#' @param file the path to the file to read
#' @param sep the field separator character. Values on each line of the file are 
#' separated by this character. (Default: \code{","})
#' @param header a logical value indicating whether the first row corresponding to 
#' the first element of the rowIndex vector contains the names of the variables. 
#' (Default: \code{TRUE})
#' @param ninputs the number of input columns. (Default: 0)
#' @param Ts sampling interval (Default: 1)
#' @param unit Time Unit (Default: "seconds")
#' @param ... additional arguments to be passed to the \code{\link[utils]{read.table}} function
#' 
#' @details
#' 
#' The \code{read.table.idframe} function uses the \code{\link[utils]{read.table}} function, 
#' provided by the \pkg{utils} package, to read data from a table-formatted file and then calls the 
#' \code{\link{read.idframe}} function to read the data into a idframe object
#' 
#' @return an idframe object
#' @examples
#' dataMatrix <- data.frame(matrix(rnorm(1000),ncol=5))
#' colnames(dataMatrix) <- c("u1","u2","y1","y2","y3")
#' tmpfile <- file.path(tempdir(),"test.csv")
#' write.csv(dataMatrix,file=tmpfile,row.names=FALSE)
#'
#' data <- read.table.idframe(tmpfile,ninputs=2,unit="minutes")
#' 
#' @seealso  \code{\link[utils]{read.table}}
#' @export 
read.table.idframe <- function(file,header=TRUE,sep=",",ninputs=0,
             Ts = 1,unit=c("seconds","minutes","hours",
                           "days")[1],...){
  
  # Read from file (default: csv file)
  dat <- read.table(file=file,header=header,sep=sep,...)
  
  # read from dataframe and return idframe object
  out <- read.idframe(dat,ninputs=ninputs,Ts = Ts,unit=unit)
  return(out)
}