#' Data input into a idframe object
#' 
#' Read the contents of a \code{data.frame} object into a \code{idframe} object.
#' 
#' @param data a \code{data.frame} object
#' @param freqData a logical value indicating whether the file contains the list of
#' frequencies. If \code{TRUE}, they need to be present in the first column. 
#' (Default: \code{idframe})
#' @param ninputs the number of input columns. (Default: NULL)
#' @param type indicates the domain of the data (Default:\code{"time"})
#' @param Ts sampling interval (Default: 1)
#' @param tUnit Time Unit (Default: "seconds")
#' 
#' @details
#' 
#' If \code{type="freq"} and \code{freqData = TRUE}, then the first column in the file 
#' should contain the frequencies.
#' 
#' @return an idframe object
#' @examples
#' data(cstr)
#' data <- read.idframe(cstrData,ninputs=1,type="time",Ts= 1,tUnit="min")
#' 
#' @export 
read.idframe <- function(data,freqData=FALSE,ninputs=NULL,
                    type=c("time","freq")[1],Ts = 1,tUnit="sec"){
  
  if((type=="freq") && (freqData)){
    
    frequencies <- data[,1] # the first column must contain frequencies
    inputs <- data[,seq(2,length.out=ninputs,by=1),drop=F]
    outputs <- data[,seq((ninputs+2),dim(data)[2],by=1),drop=F]
    
    out <- idframe(output=outputs,input=inputs,type=type,Ts=Ts,
                   frequencies=frequencies,tUnit=tUnit)
    
  } else{
    outIndex <- 1:dim(data)[2]; inputs <- NULL
    if(!is.null(ninputs)){
      inputs <- data[,1:ninputs,drop=F]
      outIndex <- seq(ninputs+1,dim(data)[2]
    }
    outputs <- data[,outIndex,by=1),drop=F]
    
    out <- idframe(output=outputs,input=inputs,type=type,Ts=Ts,tUnit=tUnit)
  }
  
  return(out)
}

#' Data input into a idframe object
#' 
#' Read the contents of an file in table format into a \code{idframe} object.
#' 
#' @param file the path to the file to read
#' @param sep the field separator character. Values on each line of the file are 
#' separated by this character. (Default: \code{","})
#' @param header a logical value indicating whether the first row corresponding to 
#' the first element of the rowIndex vector contains the names of the variables. 
#' (Default: \code{TRUE})
#' @param ninputs the number of input columns. (Default: 1)
#' @param type indicates the domain of the data (Default:\code{"time"})
#' @param Ts sampling interval (Default: 1)
#' @param freqData a logical value indicating whether the file contains the list of
#' frequencies. If \code{TRUE}, they need to be present in the first column. 
#' (Default: \code{idframe})
#' @param tUnit Time Unit (Default: "seconds")
#' @param ... additional arguments to be passed to the \code{\link[utils]{read.table}} function
#' 
#' @details
#' 
#' The \code{readxlsx.idframe} function uses the \code{\link[utils]{read.table}} function, 
#' provided by the \pkg{xlsx} package, to read data from an excel file and then calls the 
#' \code{\link{read.idframe}} function to read the data into a idframe object
#' 
#' 
#' If \code{type="freq"} and \code{freqData = TRUE}, then the first column in the file 
#' should contain the frequencies.
#' 
#' @return an idframe object
#' @examples
#' dataMatrix <- data.frame(matrix(rnorm(1000),ncol=5))
#' colnames(dataMatrix) <- c("u1","u2","y1","y2","y3")
#' write.csv(dataMatrix,file="test.csv",row.names=FALSE)
#'  
#' data <- read.table.idframe("test.csv",ninputs=2,tUnit="min")
#' 
#' @seealso  \code{\link[utils]{read.table}}
#' @export 
read.table.idframe <- function(file,header=TRUE,sep=",",ninputs=1,
             type=c("time","freq")[1],Ts = 1,freqData=FALSE,
             tUnit="sec",...){
  
  # Read from file (default: csv file)
  dat <- read.table(file=file,header=header,sep=sep,...)
  
  # read from dataframe and return idframe object
  out <- read.idframe(dat,ninputs=ninputs,type=type,Ts = Ts,
                      freqData=freqData,tUnit=tUnit)
  return(out)
}

#' Read the contents of a worksheet into a idframe object
#' 
#' Read the contents of an excel worksheet into a \code{idframe} object.
#' 
#' @param file the path to the file to read
#' @param sheetName a character string with the sheet name
#' @param header a logical value indicating whether the first row corresponding to 
#' the first element of the rowIndex vector contains the names of the variables. 
#' (Default: \code{TRUE})
#' @param ninputs the number of input columns. (Default: 1)
#' @param type indicates the domain of the data (Default:\code{"time"})
#' @param Ts sampling interval (Default: 1)
#' @param freqData a logical value indicating whether the file contains the list of
#' frequencies. If \code{TRUE}, they need to be present in the first column. 
#' (Default: \code{idframe})
#' @param tUnit Time Unit (Default: "seconds")
#' @param ... additional arguments to be passed to the \code{\link[xlsx]{read.xlsx2}} function
#' 
#' @details
#' 
#' The \code{read.xlsx.idframe} function uses the \code{\link[xlsx]{read.xlsx2}} function, 
#' provided by the \pkg{xlsx} package, to read data from an excel file and then calls the 
#' \code{\link{read.idframe}} function to read the data into a idframe object
#' 
#' 
#' If \code{type="freq"} and \code{freqData = TRUE}, then the first column in the file 
#' should contain the frequencies.
#' 
#' 
#' The function requires the java runtime to be installed on the system (Requirement of 
#' the \pkg{xlsx} package).
#' 
#' @return an idframe object
#' @examples
#' library(xlsx)
#' dataMatrix <- data.frame(matrix(rnorm(1000),ncol=5))
#' colnames(dataMatrix) <- c("u1","u2","y1","y2","y3")
#' write.xlsx2(dataMatrix,file="test.xlsx",row.names=FALSE)
#'  
#' data <- read.xls.idframe("test.xlsx","Sheet1",ninputs=2,tUnit="min")
#' 
#' @seealso  \code{\link[xlsx]{read.xlsx2}}
#' @export
read.xls.idframe <- function(file,sheetName,header=TRUE,ninputs=1,
                type=c("time","freq")[1],Ts = 1,freqData=FALSE,tUnit="time",
                ...){
  
  require(xlsx)
  
  # Read from file into an R data.frame
  dat <- read.xlsx2(file=file,sheetName=sheetName,header=header,...)
  l <- as.list(dat)
  dat <- as.data.frame(sapply(l,as.numeric))
  
  # read from dataframe and return idframe object
  out <- read.idframe(dat,ninputs=ninputs,type=type,Ts = Ts,
                      freqData=freqData,tUnit=tUnit)
  return(out)
}