#' Data input into a idframe object
#' 
#' Read the contents of an file in table format into a \code{idframe} object.
#' 
#' @param data a \code{data.frame} object
#' @param freqData a logical value indicating whether the file contains the list of
#' frequencies. If \code{TRUE}, they need to be present in the first column. 
#' (Default: \code{idframe})
#' @param ninputs the number of input columns. (Default: 1)
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
#' @export 
read.idframe <- function(data,freqData=FALSE,ninputs=1,
                    type=c("time","freq")[1],Ts = 1,tUnit="time"){
  
  if((type=="freq") && (freqData)){
    
    frequencies <- dat[,1] # the first column must contain frequencies
    inputs <- dat[,seq(2,length.out=ninputs,by=1)]
    outputs <- dat[,seq((ninputs+1),dim(dat)[2],by=1)]
    
    out <- idframe(output=outputs,input=inputs,type=type,Ts=Ts,
                   frequencies=frequencies,tUnit=tUnit)
    
  } else{
    
    inputs <- dat[,1:ninputs]
    outputs <- dat[,seq(ninputs,dim(dat)[2],by=1)]
    
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
#' @export 
#' @seealso  \code{\link[utils]{read.table}}
read.table.idframe <- function(file,header=TRUE,sep=",",ninputs=1,
             type=c("time","freq")[1],Ts = 1,freqData=FALSE,
             tUnit="time",...){
  
  # Read from file (default: csv file)
  dat <- read.table(file=file,header=header,sep=sep,...)
  
  # read from dataframe and return idframe object
  out <- read.idframe(data,ninputs=ninputs,type=type,Ts = Ts,
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
#' @export 
#' @seealso  \code{\link[xlsx]{read.xlsx2}}
read.xls.idframe <- function(file,sheetName,header=TRUE,ninputs=1,
                type=c("time","freq")[1],Ts = 1,freqData=FALSE,tUnit="time",
                ...){
  
  require(xlsx)
  
  # Read from file into an R data.frame
  dat <- read.xlsx2(file=file,sheetName=sheetName,header=header,...)
  
  # read from dataframe and return idframe object
  out <- read.idframe(data,ninputs=ninputs,type=type,Ts = Ts,
                      freqData=freqData,tUnit=tUnit)
  return(out)
}