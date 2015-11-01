#' @export
idpoly <- function(A=1,B=1,C=1,D=1,F1=1,ioDelay=0,Ts=1){
  out <- list(A= A,B=B,C=C,D=D,F1=F1,ioDelay = ioDelay,Ts=Ts)
  out$type <- typecheck(out)
  class(out) <- "idpoly"
  return(out)
}

typecheck <- function(x){
  y <- lapply(x[1:5],checkUnity)
  if(y$A){
    out <- if(y$C||y$F1) "oe" else "bj" 
  } else{
    if(y$D && y$F1){
      out <- if(y$C) "arx" else "armax"
    } else{
      out <- "idpoly"
    }
  }
}

checkUnity <- function(x){
  out <- if(length(x)==1 && x==1) TRUE else FALSE
}

#' @export
print.idpoly <- function(x){
  if(x$type=="arx"){
    print_arx(x)
  } else if(x$type=="armax"){
    print_armax(x)
  }
}

print_arx <- function(obj){
  cat("Discrete-time ARX model: A(q^{-1})y[k] = B(q^{-1})u[k] + e[k] \n\n")
  cat("A(q^{-1}) = ")
  for(i in seq_along(obj$A)){
    if(i-1==0){
      cat(obj$A[i])
    } else{
      if(obj$A[i]>0) cat(" + ") else cat("- ")
      
      if(!(abs(obj$A[i])==1)) cat(abs(obj$A[i]))
      cat("q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  cat("B(q^{-1}) = ")
  for(i in seq_along(obj$B)){
    if(i+obj$ioDelay-1==0){
      cat(obj$B[i])
    } else{
      
      if(!((obj$ioDelay!=0) && (i==1))){
        if(obj$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(obj$B[i]<0) cat("-")
      }
      
      if(!(abs(obj$B[i])==1)) cat(abs(obj$B[i]))
      cat("q^{-",i+obj$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
}

print_armax <- function(obj){
  cat("Discrete-time ARX model: A(q^{-1})y[k] = B(q^{-1})u[k] + C(q^{-1})e[k] \n\n")
  cat("A(q^{-1}) = ")
  for(i in seq_along(obj$A)){
    if(i-1==0){
      cat(obj$A[i])
    } else{
      if(obj$A[i]>0) cat(" + ") else cat("- ")
      
      if(!(abs(obj$A[i])==1)) cat(abs(obj$A[i]))
      cat("q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  cat("B(q^{-1}) = ")
  for(i in seq_along(obj$B)){
    if(i+obj$ioDelay-1==0){
      cat(obj$B[i])
    } else{
      
      if(!((obj$ioDelay!=0) && (i==1))){
        if(obj$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(obj$B[i]<0) cat("-")
      }
      
      if(!(abs(obj$B[i])==1)) cat(abs(obj$B[i]))
      cat("q^{-",i+obj$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  cat("C(q^{-1}) = ")
  for(i in seq_along(obj$C)){
    if(i-1==0){
      cat(obj$C[i])
    } else{
      if(obj$C[i]>0) cat(" + ") else cat("- ")
      
      if(!(abs(obj$C[i])==1)) cat(abs(obj$C[i]))
      cat("q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
}

