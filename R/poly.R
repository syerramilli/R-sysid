#' Polynomial model with identifiable parameters
#' 
#' Creates a polynomial model with identifiable coefficients
#' 
#' @param A Autoregressive coefficients
#' @param B,F1 Coefficients of the numerator and denominator respectively
#' of the deterministic model between the input and output 
#' @param C,D Coefficients of the numerator and denominator respectively
#' of the stochastic model 
#' @param ioDelay the delay in the input-output channel
#' @param Ts sampling interval
#' 
#' @details
#' Discrete-time polynomials are of the form
#' \deqn{
#'  A(q^{-1}) y[k] = \frac{B(q^{-1})}{F1(q^{-1})} u[k] + 
#'  \frac{C(q^{-1})}{D(q^{-1})} e[k] 
#' }
#' 
#' @examples
#' # define output-error model
#' mod_oe <- idpoly(B=c(0.6,-0.2),F1=c(1,-0.5),ioDelay = 2,Ts=0.1)
#' 
#' # define box-jenkins model
#' B <- c(0.6,-0.2)
#' C <- c(1,-0.3)
#' D <- c(1,1.5,0.7)
#' F1 <- c(1,-0.5)
#' mod_bj <- idpoly(1,B,C,D,F1,ioDelay=1)
#' 
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
print.idpoly <- function(mod,se=NULL,dig=3){
  
  if(mod$type=="arx"){
    cat("Discrete-time ARX mod: A(z)y[k] = B(z)u[k] + e[k] \n\n")
  } else if(mod$type=="armax"){
    cat("Discrete-time ARMAX mod: A(z)y[k] = B(z)u[k] + C(z)e[k] \n\n")
  } else if(mod$type=="oe"){
    cat("Discrete-time OE mod: y[k] = B(z)/F(z) u[k] + e[k] \n\n")
  } else if(mod$type=="bj"){
    cat("Discrete-time BJ mod: y[k] = B(z)/F(z) u[k] + C(z)/D(z) e[k] \n\n")
  } else{
    cat("Discrete-time Polynomial mod: A(z) y[k] = B(z)/F(z) u[k] + C(z)/D(z) e[k] \n\n")
  }
  
  # Printing Standard error sequence
  j=1
  print_se <- function(se){
    if(!is.null(se)){
      cat(" (+/- ",round(se[j],dig),") ",sep = "")
      j <<- j+1
    }
  }
  
  if(length(mod$A)>1){
    cat("A(q^{-1}) = ")
    for(i in seq_along(mod$A)){
      if(i-1==0){
        cat(round(mod$A[i],dig))
      } else{
        if(mod$A[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$A[i])==1)) cat(abs(round(mod$A[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  cat("B(q^{-1}) = ")
  for(i in seq_along(mod$B)){
    if(i+mod$ioDelay-1==0){
      cat(round(mod$B[i],dig))
    } else{
      
      if(!((mod$ioDelay!=0) && (i==1))){
        if(mod$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(mod$B[i]<0) cat("-")
      }
      
      if(!(abs(mod$B[i])==1)) cat(abs(round(mod$B[i],dig)))
      print_se(se)
      cat("q^{-",i+mod$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  
  if(length(mod$C)>1){
    cat("C(q^{-1}) = ")
    for(i in seq_along(mod$C)){
      if(i-1==0){
        cat(round(mod$C[i],dig))
      } else{
        if(mod$C[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$C[i])==1)) cat(abs(round(mod$C[i],dig)))
        print_se(se)
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
        cat(round(mod$D[i],dig))
      } else{
        if(mod$D[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$D[i])==1)) cat(abs(round(mod$D[i],dig)))
        print_se(se)
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
        cat(round(mod$F1[i],dig))
      } else{
        if(mod$F1[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(mod$F1[i])==1)) cat(abs(round(mod$F1[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
  }
  cat("\n")
}

#' @export
predict.idpoly <- function(x,data,nahead=1){
  y <- outputData(z); u<- inputData(z)
  G <- signal::Arma(b=c(rep(0,x$ioDelay),x$B),
                    a= as.numeric(polynom::polynomial(x$A)*
                                    polynom::polynomial(x$F1)))
  
  Hden <- as.numeric(polynom::polynomial(x$A)*polynom::polynomial(x$D))
  Hinv <- signal::Arma(b=Hden,a=x$C)
  
  return(ypred)
}