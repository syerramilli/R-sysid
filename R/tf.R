# S3 class for defining transfer functions
tf <- function(num=c(1),den=c(1),Ts=1){
  out <- list(num=num,den=den,Ts=Ts)
  class(out) <- "tf"
  return(out)
}

# Display the Transfer Function
print.tf <- function(G){ 
  cat("Transfer Function \nG(q^{-1}) = B(q^{-1})/A(q^{-1}) \n\n")
  cat("A(q^{-1}) = ")
  for(i in seq_along(G$den)){
    if(i-1==0){
      cat(G$den[i])
    } else{
      if(G$den[i]>0)
        cat("+")
      cat(G$den[i],"q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  cat("B(q^{-1}) = ")
  for(i in seq_along(G$num)){
    if(i-1==0){
      cat(G$num[i])
    } else{
      if(G$num[i]>0)
        cat("+")
      cat(G$num[i],"q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
}

