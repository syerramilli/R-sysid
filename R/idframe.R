#' S3 class for storing input-output data. 
#'
#' \code{idframe} is an S3 class for storing and manipulating input-ouput data. It supports discrete time and frequency domain data.
#'
#' @param output dataframe/matrix/vector containing the outputs
#' @param input dataframe/matrix/vector containing the inputs
#' @param type indicates the domain of the data (Default:"time")
#' @param Ts sampling interval (Default: 1)
#' @param t.start Starting time (Valid only if type="time")
#' @param t.end End time. Optional Argument (Valid only if type="time")
#' @param tUnit Time Unit (Default: "seconds")
#' @param frequencies Vector containing the list of frequencies at which the data was 
#' recorded (Valid only if type="frequency")
#' @param fUnit Frequency Unit (Valid only if type="frequency")
#' @return an idframe object
#' 
#' @seealso  \code{\link{plot.idframe}}, the plot method for idframe objects, 
#' \code{\link{summary.idframe}}, the summary method for idrame objects
#' 
#' @examples
#' 
#' dataMatrix <- matrix(rnorm(1000),ncol=5) 
#' data <- idframe(output=dataMatrix[,3:5],input=dataMatrix[,1:2],Ts=1)
#' 
#' @export
idframe <- function(output=numeric(0),input=numeric(0),
                     type=c("time","freq")[1],Ts = 1,
                     t.start=0,t.end=NA, tUnit = "seconds", 
                     frequencies = NA, fUnit= "Hz"){
  
  ## Input Validation
  if(!(type %in% c("time","freq"))) # type validation
    stop("Unknown domain type")
  
  if(length(output)!=0 && length(input)!=0){
    if(dim(output)[1]!=dim(input)[1]) # observation validation
      stop("Dimensions don't match")
  }
  
  # Object Constructor
  dat <- list(output=data.frame(output),input=data.frame(input),type=type,Ts=Ts)
  n <- dim(output)[1]
  p <- dim(output)[2];m <- dim(input)[2]
  
  if(type=="freq"){

    if(is.na(frequencies)){
      frequncies <- seq(0,2*pi,length=n)
    }
    
    dat$frequencies <- frequencies
    dat$fUnit <- fUnit
    
  } else {
    
    if(is.na(t.end)) {
      t.end <- t.start + Ts*(n-1)
    } else {
      dat$Ts <- (t.end-t.start)/(n-1)
    }
    
    dat$t.start <- t.start; dat$t.end <- t.end
    dat$tUnit <- tUnit
  }
      
  class(dat) <- "idframe"
  return(dat)
}

#' Plotting idframe objects
#' 
#' Plotting method for objects inherting from class \code{idframe}
#' 
#' @param object an object of class \code{idframe}
#' @param ... additional arguments to be passed to the \code{plot} function
#' 
#' @examples
#' data(distill)
#' plot(distill,col="blue")
#' 
#' @export
plot.idframe <- function(object,...){
  
  p <- dim(object$output)[2];m <- dim(object$input)[2]
  
  if(p!=1 || m!=1){
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    
    for(i in seq(m)){
      for(j in seq(p)){
        par(mfrow=c(2,1),mar=c(3,4,2,2))
        plot(index(object),object$output[,j],xlab=object$type,
             ylab=colnames(object$output)[j],type="l",...)
        plot(index(object),object$input[,i],xlab=object$type,
             ylab=colnames(object$input)[i],type="l",...)
      }
    }
  } else {
    par(mfrow=c(2,1),mar=c(3,4,3,2))
    plot(index(object),object$output[,1],xlab=object$type,
         ylab=colnames(object$output),type="l",...)
    plot(index(object),object$input[,1],xlab=object$type,
         ylab=colnames(object$input),type="l",...)
  }  
}

index <- function(object){
  if(object$type=="time"){
    return(seq(from=object$t.start,to=object$t.end,by=object$Ts)) 
  } else {
    return(object$frequencies) 
  }
}

#' idframe-object summaries
#' 
#' Generates a summary of objects inherting from class \code{idframe}
#' 
#' @param object an object of class \code{idframe}
#' 
#' @examples
#' data(cstr)
#' summary(cstr)
#' 
#' @export
summary.idframe <- function(object){
  out_sum <- summary(object$output)
  in_sum <- summary(object$input)
  
  out <- list(outputs=out_sum,inputs=in_sum,Ts=object$Ts,type=object$type,
              tUnit=object$tUnit,no_of_samples = dim(object$output)[1])
  if(object$type=="time"){
    out$t.start <- object$t.start;out$t.end <- object$t.end
  } else{
    out$frequencies <- summary(object$frequencies);out$fUnit <- object$fUnit
  }
  
  class(out) <- "summary.idframe"
  return(out)
}

#' @export
print.summary.idframe <- function(object,...){
  cat("Domain: ");cat(object$type)
  cat("\t\t Number of samples:");cat(object$no_of_samples)
  cat("\nSampling time: ")
  cat(object$Ts);cat(" ");cat(object$tUnit)
  
  if(object$type=="frequency"){
    cat("\t Frequency Unit: ");print(object$fUnit)
    cat("\n\n Frequeny Summary:")
    print(object$frequencies)
  }
  
  cat("\n\n")
  cat("Outputs \n")
  print(object$outputs)
  cat("\n")
  
  cat("Inputs \n")
  print(object$inputs)
}

#' S3 class for storing frequency response data
#'
#' @export
idfrd <- function(response,freq,Ts){
  out <- list(response=response,freq=freq,Ts=Ts)
  class(out) <- "idfrd"
  return(out)
}

#' @export
plot.idfrd <- function(object){
  require(ggplot2);require(reshape2);require(signal)

  mag <- 20*log10(Mod(object$resp))
  phase <- -360/2/pi*unwrap(Arg(object$resp))
  sys_df <- data.frame(Frequency = object$freq,Gain = mag,Phase = phase)
  melted_sys_df <- melt(sys_df, id.var = c("Frequency"))
  
  bode <-  ggplot(sys_df, aes(x = Frequency)) + 
    geom_line(colour="steelblue") + scale_x_log10() + theme_bw() + 
    geom_vline(xintercept=max(object$freq),size=1.2)
  bode_gain <- bode + aes(y = Gain)
  bode_phase <- bode + aes(y = Phase)
  
  multiplot(bode_gain,bode_phase)
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
