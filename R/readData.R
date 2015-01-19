# Reading from table-formatted files
read.idframe(file,header=TRUE,sep=",",ninputs=1,
             type=c("time","freq")[1],Ts = 1,freqData=FALSE...){
  # Read from file (default: csv file)
  dat <- read.table(file=file,header=header,sep=sep,...)
  
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
}