#' S3 class for storing input-output data. 
#'
#' \code{idframe} is an S3 class for storing and manipulating input-ouput data. It supports discrete time and frequency domain data.
#'
#' @param output dataframe/matrix/vector containing the outputs
#' @param input dataframe/matrix/vector containing the inputs
#' @param Ts sampling interval (Default: 1)
#' @param start Time of the first observation
#' @param end Time of the last observation Optional Argument
#' @param unit Time Unit (Default: "seconds")
#'
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
idframe <- function(output=NULL,input=NULL,Ts = 1,start=0,end=NULL, 
                    unit = c("seconds","minutes","hours",
                             "days")[1]){
  
  l <- list(output,input)
  l2 <- lapply(l,data.frame)
  n <- dim(l2[[1]])
  
  if(!is.null(end)){
    start <- end - Ts*(n-1)
  } 
  
  l3 <- lapply(l,ts,start=start,deltat=Ts)
  
  # Object Constructor
  dat <- list(output=l3[[1]],input=l3[[1]],unit=unit)
  class(dat) <- "idframe"
  return(dat)
}

#' Plotting idframe objects
#' 
#' Plotting method for objects inherting from class \code{idframe}
#' 
#' @param x an object of class \code{idframe}
#' @param par a list of arguments passed to par() before plotting.
#' @param col line color, to be passed to plot.(Default=\code{"steelblue"})
#' @param ... additional arguments to be passed to the \code{tfplot} function
#' 
#' @seealso \code{\link[tfplot]{tfplot}}
#' @examples
#' data(cstr)
#' plot(cstr,col="blue")
#' 
#' @export
plot.idframe <- function(x,par=list(mar=c(3,4,2,2)),
                         col="steelblue",...){
    require(tfplot)
    if(nInputSeries(x)==0){
      data <- outputData(x)
    } else if(nOutputSeries(x)==0){
      data <- inputData(x)
    } else{
      data <- cbind(outputData(x),inputData(x))
      colnames(data) <- c(outputNames(x),inputNames(x))
    }
    tfplot(data,Xaxis=NULL,par=par,col=col,...)
}

#' @export
summary.idframe <- function(x){
  out_sum <- summary(outputData(x))
  in_sum <- summary(inputData(x))
  
  out <- list(out_sum=out_sum,in_sum=in_sum,Ts=deltat(x),
              unit=x$unit,nsample = dim(outputData(x))[1])
  
  class(out) <- "summary.idframe"
  return(out)
}

#' @export
print.summary.idframe <- function(x,...){
  cat("\t\t Number of samples:");cat(x$nsample)
  cat("\nSampling time: ")
  cat(x$Ts);cat(" ");cat(x$unit)
  
  cat("\n\n")
  cat("Outputs \n")
  print(x$out_sum)
  cat("\n")
  
  cat("Inputs \n")
  print(x$in_sum)
}

#' @export
time.idframe <- function(data){
  time(data$output)
}

#' @export
frequency.idframe <- function(data){
  frequency(data$output)
}

#' @export
deltat.idframe <- function(data){
  deltat(data$output)
}

#' S3 class for storing frequency response data
#' 
#' @param response complex vector/matrix containing the response
#' @param freq the frequencies at which the response is observed/estimated
#' @param Ts sampling time of data
#' 
#' @return an idfrd object
#' 
#' @note
#' The class can currently store only SISO Responses. Future versions will 
#' have support for multivariate data
#' 
#' @seealso
#' \code{\link{plot.idfrd}} for generating bode plots; \code{\link{spa}} and
#' \code{\link{etfe}} for estimating the frequency response given input/output data
#' 
#' @export
idfrd <- function(response,freq,Ts){
  out <- list(response=response,freq=freq,Ts=Ts)
  class(out) <- "idfrd"
  return(out)
}

#' Plotting idfrd objects
#' 
#' Generates the bode plot of the given frequency response data. It uses the
#' ggplot2 plotting engine
#' 
#' @param x An object of class \code{idframe}
#' 
#' @seealso \code{\link[ggplot2]{ggplot}}
#' 
#' @examples
#' data(frf)
#' frf <- spa(data) # Estimates the frequency response from data
#' plot(frf)
#' 
#' @export
plot.idfrd <- function(x){
  require(ggplot2);require(reshape2);require(signal)

  mag <- 20*log10(Mod(x$resp))
  phase <- -360/2/pi*unwrap(Arg(x$resp))
  sys_df <- data.frame(Frequency = x$freq,Gain = mag,Phase = phase)
  melted_sys_df <- melt(sys_df, id.var = c("Frequency"))
  
  bode <-  ggplot(sys_df, aes(x = Frequency)) + 
    geom_line(colour="steelblue") + scale_x_log10() + theme_bw() + 
    geom_vline(xintercept=max(x$freq),size=1.2)
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
