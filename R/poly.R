#' @export
idpoly <- function(A,B,C,D,E,F1,ioDelay=0){
  out <- list(A= A,B=B,C=C,D=D,E=E,F1=F1,ioDelay = ioDelay)
  class(out) <- "idpoly"
  return(out)
}

#' @export
arx <- function(A,B,ioDelay=0){
  out <- idpoly(A=A,B=B,C=1,D=1,E=1,F1=1,ioDelay = ioDelay)
  class(out) <- c("arx","idpoly")
  return(out)
}

#' @export
armax <- function(A,B,C,ioDelay=0){
  out <- idpoly(A=A,B=B,C=C,D=1,E=1,F1=1,ioDelay = ioDelay)
  class(out) <- c("armax","idpoly")
  return(out)
}