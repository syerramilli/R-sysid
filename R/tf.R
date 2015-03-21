# S3 class for defining trasnfer functions
tf <- function(num=c(1),den=c(1),Ts=1){
  out <- list(num=num,den=den,Ts=Ts)
  class(out) <- "tf"
  return(out)
}

# Display the Transfer Function
print.tf <- function(G){
  
}

