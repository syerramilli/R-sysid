#' @export
idin.rgs <- function(n,band,var){
  # Function to generate a random Gaussian 
  # signal of given frequency band and variance
  require(signal)
  uk1 <- rnorm(n,mean = 0,sd = 1)
 
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
  
  # Adjusting for required variance
  uk <- sqrt(var)*ukf
  return(uk)
}