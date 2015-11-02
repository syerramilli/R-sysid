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
print.idpoly <- function(mod){
  
  if(mod$type=="arx"){
    cat("Discrete-time ARX mod: A(q^{-1})y[k] = B(q^{-1})u[k] + e[k] \n\n")
  } else if(mod$type=="armax"){
    cat("Discrete-time ARMAX mod: A(q^{-1})y[k] = B(q^{-1})u[k] + C(q^{-1})e[k] \n\n")
  } else if(mod$type=="oe"){
    cat("Discrete-time OE mod: y[k] = B(q^{-1})/F(q^{-1}) u[k] + e[k] \n\n")
  } else if(mod$type=="bj"){
    cat("Discrete-time BJ mod: y[k] = B(q^{-1})/F(q^{-1}) u[k] + C(q^{-1})/D(q^{-1}) e[k] \n\n")
  } else{
    cat("Discrete-time Polynomial mod: A(q^{-1}) y[k] = B(q^{-1})/F(q^{-1}) u[k] + C(q^{-1})/D(q^{-1}) e[k] \n\n")
  }
  
  
  if(length(mod$A)>1){
    cat("A(q^{-1}) = ")
    for(i in seq_along(mod$A)){
      if(i-1==0){
        cat(mod$A[i])
      } else{
        if(mod$A[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$A[i])==1)) cat(abs(mod$A[i]))
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  cat("B(q^{-1}) = ")
  for(i in seq_along(mod$B)){
    if(i+mod$ioDelay-1==0){
      cat(mod$B[i])
    } else{
      
      if(!((mod$ioDelay!=0) && (i==1))){
        if(mod$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(mod$B[i]<0) cat("-")
      }
      
      if(!(abs(mod$B[i])==1)) cat(abs(mod$B[i]))
      cat("q^{-",i+mod$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  
  if(length(mod$C)>1){
    cat("C(q^{-1}) = ")
    for(i in seq_along(mod$C)){
      if(i-1==0){
        cat(mod$C[i])
      } else{
        if(mod$C[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$C[i])==1)) cat(abs(mod$C[i]))
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  if(length(mod$D)>1){
    cat("D(q^{-1}) = ")
    for(i in seq_along(mod$D)){
      if(i-1==0){
        cat(mod$D[i])
      } else{
        if(mod$D[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$D[i])==1)) cat(abs(mod$D[i]))
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  if(length(mod$F1)>1){
    cat("F(q^{-1}) = ")
    for(i in seq_along(mod$F1)){
      if(i-1==0){
        cat(mod$F1[i])
      } else{
        if(mod$F1[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$F1[i])==1)) cat(abs(mod$F1[i]))
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
  }
}