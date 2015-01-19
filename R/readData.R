# Reading data from dataframes
readData.idframe <- function(data,ninputs=1,type=c("time","freq")[1],
                 Ts = 1,freqData=FALSE){
  
  if((type=="freq") && (freqData)){
    
    frequencies <- dat[,1] # the first column must contain frequencies
    inputs <- dat[,seq(2,length.out=ninputs,by=1)]
    outputs <- dat[,seq((ninputs+1),dim(dat)[2],by=1)]
    
    out <- idframe(output=outputs,input=inputs,type=type,Ts=Ts,
                   frequencies=frequencies)
    
  } else{
    
    inputs <- dat[,1:ninputs]
    outputs <- dat[,seq(ninputs,dim(dat)[2],by=1)]
    
    out <- idframe(output=outputs,input=inputs,type=type,Ts=Ts)
  }
  
  return(out)
}

# Reading from table-formatted files
read.idframe <- function(file,header=TRUE,sep=",",ninputs=1,
             type=c("time","freq")[1],Ts = 1,freqData=FALSE,...){
  
  # Read from file (default: csv file)
  dat <- read.table(file=file,header=header,sep=sep,...)
  
  # read from dataframe and return idframe object
  out <- readData.idframe(data,ninputs=ninputs,type=type,Ts = Ts,freqData=freqData)
  return(out)
}

# Reading from excel files
readxls.idframe <- function(file,sheetName,header=TRUE,ninputs=1,
                type=c("time","freq")[1],Ts = 1,freqData=FALSE,...){
  
  require(xlsx)
  
  # Read from file (default: csv file)
  dat <- read.xlsx2(file=file,sheetName=sheetName,header=header,...)
  
  # read from dataframe and return idframe object
  out <- readData.idframe(data,ninputs=ninputs,type=type,Ts = Ts,freqData=freqData)
  return(out)
}