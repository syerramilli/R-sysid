#' Polynomial model with identifiable parameters
#' 
#' Creates a polynomial model with identifiable coefficients
#' 
#' @param A autoregressive coefficients
#' @param B,F1 coefficients of the numerator and denominator respectively
#' of the deterministic model between the input and output 
#' @param C,D coefficients of the numerator and denominator respectively
#' of the stochastic model 
#' @param ioDelay the delay in the input-output channel
#' @param Ts sampling interval
#' @param noiseVar variance of the white noise source (Default=\code{1})
#' @param intNoise Logical variable indicating presence or absence of integrator
#' in the noise channel (Default=\code{FALSE})
#' @param unit time unit (Default=\code{"seconds"})
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
#' mod_oe <- idpoly(B=c(0.6,-0.2),F1=c(1,-0.5),ioDelay = 2,Ts=0.1,
#' noiseVar = 0.1)
#' 
#' # define box-jenkins model with unit variance
#' B <- c(0.6,-0.2)
#' C <- c(1,-0.3)
#' D <- c(1,1.5,0.7)
#' F1 <- c(1,-0.5)
#' mod_bj <- idpoly(1,B,C,D,F1,ioDelay=1)
#' 
#' @export
idpoly <- function(A=1,B=1,C=1,D=1,F1=1,ioDelay=0,Ts=1,
                   noiseVar=1,intNoise = F,unit = c("seconds","minutes",
                                       "hours","days")[1]){
  Bindex <- which.max(B!=0)
  ioDelay <- ifelse(Bindex-1==0,ioDelay,Bindex-1)
  B <- B[Bindex:length(B)]
  out <- list(A= A,B=B,C=C,D=D,F1=F1,ioDelay = ioDelay,Ts=Ts,
              noiseVar=noiseVar,unit=unit,intNoise = intNoise)
  out$type <- typecheck(out)
  class(out) <- "idpoly"
  return(out)
}

typecheck <- function(x){
  y <- lapply(x[1:5],checkUnity)
  if(y$A){
    out <- if(y$C && y$D) if(y$F1) "fir" else "oe" else "bj" 
  } else{
    if(y$D && y$F1){
      if(x$intNoise){
        out <- if(y$C) "ari" else "arima"
      } else{
        out <- if(y$C) "ar" else "arma"
      }
      if(!y$B) out <- paste(out,"x",sep="")
    } else{
      out <- "polynomial"
    }
  }
  out
}

checkUnity <- function(x){
  out <- if(length(x)==1 && x==1) TRUE else FALSE
}

#' @export
print.idpoly <- function(x,se=NULL,dig=3,...){
  main <- paste("Discrete-time",toupper(x$type),"model:")
  if(x$type=="oe" || x$type=="bj"){
    main <- paste(main,"y[k] = B(z)/F(z) u[k] +")
    if(x$type=="bj"){
      polyExp <- if(!checkUnity(x$C)) "C(z)/D(z)" else "1/D(z)"
      if(x$intNoise==T) polyExp <- paste(polyExp,"1/(1-z^{-1})")
      main <- paste(main,polyExp,"e[k] \n\n")
    }
  } else{
    main <- paste(main,"A(z)y[k] =")
    if(!checkUnity(x$B)) main <- paste(main,"B(z)u[k] +")
    if(checkUnity(x$C)){
      Cexp <- if(!x$intNoise) "" else "1/(1-z^{-1})"
    }else{
      Cexp <- "C(z)"
      if(x$intNoise) Cexp <- paste(Cexp,"/(1-z^{-1})",sep="")
    }
    main <- paste(main,Cexp,"e[k] \n\n")
  }
  cat(main)
  
  # Printing Standard error sequence
  j=1
  print_se <- function(se){
    if(!is.null(se)){
      cat(" (+/- ",round(se[j],dig),") ",sep = "")
      j <<- j+1
    }
  }
  
  if(length(x$A)>1){
    cat("A(q^{-1}) = ")
    for(i in seq_along(x$A)){
      if(i-1==0){
        cat(round(x$A[i],dig))
      } else{
        if(x$A[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(x$A[i])==1)) cat(abs(round(x$A[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  cat("B(q^{-1}) = ")
  for(i in seq_along(x$B)){
    if(i+x$ioDelay-1==0){
      cat(round(x$B[i],dig))
    } else{
      
      if(!((x$ioDelay!=0) && (i==1))){
        if(x$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(x$B[i]<0) cat("-")
      }
      
      if(!(abs(x$B[i])==1)) cat(abs(round(x$B[i],dig)))
      print_se(se)
      cat("q^{-",i+x$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  
  if(length(x$C)>1){
    cat("C(q^{-1}) = ")
    for(i in seq_along(x$C)){
      if(i-1==0){
        cat(round(x$C[i],dig))
      } else{
        if(x$C[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(x$C[i])==1)) cat(abs(round(x$C[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  if(length(x$D)>1){
    cat("D(q^{-1}) = ")
    for(i in seq_along(x$D)){
      if(i-1==0){
        cat(round(x$D[i],dig))
      } else{
        if(x$D[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(x$D[i])==1)) cat(abs(round(x$D[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
    cat("\n")
  }
  
  if(length(x$F1)>1){
    cat("F(q^{-1}) = ")
    for(i in seq_along(x$F1)){
      if(i-1==0){
        cat(round(x$F1[i],dig))
      } else{
        if(x$F1[i]>0) cat(" + ") else cat("- ")
        
        if(!(abs(x$F1[i])==1)) cat(abs(round(x$F1[i],dig)))
        print_se(se)
        cat("q^{-",i-1,"}",sep="")
      }
      cat("\t")
    }
  }
  cat("\n")
}

params <- function(x){
  c(x$A[-1],x$B,x$C[-1],x$D[-1],x$F1[-1])
} 