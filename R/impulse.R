# Estimate Impulse Response Models
impulseest <- function(data,lags=30,conf=0.95){
  require(vars)
  Z <- cbind(data$output,data$input)
  
  fit.var <- VAR(Z,p=10)
  ir <- irf(fit.var,impulse=colnames(data$input),response=colnames(data$output),
            n.ahead = lags,ci=conf)
  
  out <- list()
  class(out) <- "impulseest"
  return(out)
}

# Impulse Response Plots
impulse <- function(model){
  
}


# Step Response Plots
step <- function(model){
  
}
