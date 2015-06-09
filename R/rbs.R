idin.rbs <- function(n,band,levels){
  # Function to generate a random binary 
  # signal of given frequency band and levels
  require(signal)
  uk1 = rnorm(n,mean = 0,sd = 1)
  uk = rep(0,n)
  for(i in 1:n){
    #Checking for zeros
    
    if(uk1[i] == 0){
      uk1[i] <- rnorm(1,mean = 0,sd = 1)
    }
  }
  # Getting the filter coefficients
  bfilt <- butter(8,c(band[1],band[2]),type = "pass",plane = "z")
  
  # Filtering the signal
  ukf <- filter(bfilt,uk1)
  
  # Getting the binary signal
  for(i in 1:n){
  if(ukf[i] < 0){
    uk[i] = levels[1]
  }
  }
  for(i in 1:n){
    if(ukf[i] > 0){
      uk[i] = levels[2]
    }
  }
  return(uk)
}
  
