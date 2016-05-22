predict.idpoly <- function(x,data,nahead=1){
  y <- outputData(data); u<- inputData(data)
  G <- signal::Arma(b=c(rep(0,x$ioDelay),x$B),
                    a= as.numeric(polynom::polynomial(x$A)*
                                    polynom::polynomial(x$F1)))
  det_sys <- as.numeric(signal::filter(G,u))
  if(x$type=="oe" || nahead==Inf){
    ypred <- det_sys
  } else{
    Hden <- as.numeric(polynom::polynomial(x$A)*polynom::polynomial(x$D))
    Hinv <- signal::Arma(b=Hden,a=x$C)
    filtered <- as.numeric(signal::filter(Hinv,as.numeric(y)-det_sys))
    if(nahead!=1){
      H <- as.numeric(polynom::polynomial(x$C)*polyinv(Hden,nahead))
      Hl <- signal::Ma(H[1:nahead])
      filtered <- as.numeric(signal::filter(Hl,filtered))
    }
    ypred <- as.numeric(y) - filtered
  }
  
  matrix(ts(ypred,start=start(data),deltat=deltat(data)))
}

polyinv <- function(x,k){
  gamma <- 1/Re(polyroot(x))
  
  inverse <- function(y,k){
    sapply(1:k-1,function(i) y^i)
  }
  z <- lapply(lapply(gamma,inverse,k=2),polynom::polynomial)
  temp = z[[1]]
  if(length(z)>1){
    for(i in 2:length(z)){
      temp = temp*z[[i]]
    }
  }
  temp
}

#' Predictions of identified model
#' 
#' Predicts the output of an identified model (\code{estpoly}) object K steps ahead. 
#' 
#' @param x \code{estpoly} object containing the identified model
#' @param newdata optional dataset to be used for predictions. If not supplied, 
#' predictions are made on the training set.
#' @param nahead number of steps ahead at which to predict (Default:1). For infinite-
#' step ahead predictions or pure simulation, supply \code{Inf}.
#' 
#' @return 
#' Time-series containing the predictions
#' 
#' @examples 
#' data(arxsim)
#' mod1 <- oe(arxsim,c(2,1,1))
#' Yhat <- predict(mod1,arxsim) #  1-step ahead predictions 
#' Yhat_2 <- predict(mod1,arxsim,nahead=2) # 2-step ahead predictions
#' Yhat_inf <- predict(mod1,arxsim,nahead=Inf) # Infinite-step ahead predictions
#' 
#' @references 
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: Theory 
#' and Practice}, CRC Press, Boca Raton. Chapter 18
#' 
#' @export
predict.estpoly <- function(x,newdata=NULL,nahead=1){
  if(is.null(newdata)&& nahead==1){
    return(matrix(fitted(x)))
  } else{
    model <- x$sys
    if(is.null(newdata)){
      y <- fitted(x)+resid(x)
      u <- x$input
      z <- idframe(y,u,Ts = deltat(y),start=start(y))
    } else{
      z <- newdata
    }
    predict(model,z,nahead)
  } 
}

#' Compare the measured output and the predicted output(s)
#' 
#' Plots the output predictions of model(s) superimposed over validation data, 
#' data, for comparison. 
#' 
#' @param data validation data in the form of an \code{idframe} object
#' @param nahead number of steps ahead at which to predict (Default:1). For infinite-
#' step ahead predictions, supply \code{Inf}.
#' @param \ldots models whose predictions are to be compared
#' 
#' @examples 
#' data(arxsim)
#' mod1 <- arx(arxsim,c(1,2,2))
#' mod2 <- oe(arxsim,c(2,1,1))
#' compare(arxsim,nahead=Inf,mod1,mod2)
#' 
#' @seealso \code{\link{predict.estpoly}} for obtaining model predictions
#' @import ggplot2 reshape2
#' @export
compare <- function(data,nahead=1,...){
  loadNamespace("ggplot2")
  # Input Validation
  input_list <- as.list(substitute(list(...)))[-1]
  dots <- list(...)
  if(is.null(dots)) stop("No model supplied")
  
  Y <- sapply(dots,predict,newdata=data,nahead=nahead)
  nrmse <- sapply(dots,FUN = function(x) fitch(x)$FitPer)
  temp <- paste(as.character(input_list),paste(round(nrmse,2),"%",sep=""),
                       sep=": ")
  df <- data.frame(Time = as.numeric(time(data)),
                   Actual=as.numeric(outputData(data)[,1]),Y)
  meltdf <- reshape2::melt(df,id="Time")
  
  ggplot(meltdf,aes(x=Time,y=value,color=variable,group=variable))+geom_line(size=1)+
    ggtitle(paste("Comparison with model predictions",nahead,"step(s) ahead"))+
    theme_bw()+ylab(outputNames(data)) + labs(colour="") + 
    scale_colour_hue(labels=c("Measured",temp),l=50)
}