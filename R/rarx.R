rarx <- function(x,order=c(1,1,1),lambda=0.95){
  y <- outputData(x); u <- inputData(x)
  N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  yout <- apply(y,2,padZeros,n=n)
  uout <- apply(u,2,padZeros,n=n)
  
  fixedflag = is.null(fixed)
  uindex <- nk:nb1
  yindex <- 1:na
}