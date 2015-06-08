idin.rbs <- function(n,levels.vec){
  # Function to generate a full band random binary 
  # signal of given frequency band and levels
  
  uk1 = rnorm(n,mean = 0,sd = 1)
  uk = rep(0,n)
  for(i in 1:n){
    #Checking for zeros
    
    if(uk1[i] == 0){
      uk1[i] <- rnorm(1,mean = 0,sd = 1)
    }
  }
  
  # Getting the binary signal
  for(i in 1:n){
  if(uk1[i] < 0){
    uk[i] = levels.vec[1]
  }
  }
  for(i in 1:n){
    if(uk1[i] > 0){
      uk[i] = levels.vec[2]
    }
  }
  return(uk)
}
  
