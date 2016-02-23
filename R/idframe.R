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
  dims <- sapply(l2,ncol)
  colnames(l2[[1]]) <- sapply(1:dims[1],
                              function(x) paste("y",x,sep = ""))
  colnames(l2[[2]]) <- sapply(1:dims[2],
                              function(x) paste("u",x,sep = "")) 
  if(!is.null(end)){
    start <- end - Ts*(n-1)
  } 
  
  l3 <- lapply(l2,ts,start=start,deltat=Ts)
  
  # Object Constructor
  dat <- list(output=l3[[1]],input=l3[[2]],unit=unit)
  class(dat) <- "idframe"
  return(dat)
}

#' Plotting idframe objects
#' 
#' Plotting method for objects inherting from class \code{idframe}
#' 
#' @param x an \code{idframe} object
#' @param col line color, to be passed to plot.(Default=\code{"steelblue"})
#' @param lwd line width, in millimeters(Default=\code{1})
#' @param main the plot title. (Default = \code{NULL})
#' 
#' @examples
#' data(cstr)
#' plot(cstr,col="blue")
#' 
#' @import ggplot2 reshape2
#' 
#' @export
plot.idframe <- function(x,col="steelblue",lwd=1,main=NULL){
    if(nInputSeries(x)==0){
      data <- outputData(x)
    } else if(nOutputSeries(x)==0){
      data <- inputData(x)
    } else{
      data <- cbind(outputData(x),inputData(x))
      colnames(data) <- c(outputNames(x),inputNames(x))
    }
    ggplot(melt(data.frame(time=as.numeric(time(data)), data), id.vars="time"), 
          aes(time, value)) + geom_line(size=lwd,color=col) +
      facet_grid(variable ~ .,scale="free") + theme_bw(14,"sans") + ylab("") +
      theme(axis.title.x=element_text(size=11)) + ggtitle(main)
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

#' S3 class constructor for storing frequency response data
#' 
#' @param respData frequency response data. For SISO systems, supply a 
#' vector of frequency response values. For MIMO systems with Ny 
#' outputs and Nu inputs, supply an array of size c(Ny,Nu,Nw).
#' @param freq frequency points of the response
#' @param Ts sampling time of data
#' @param spec power spectra and cross spectra of the system 
#' output disturbances (noise). Supply an array of size (Ny,Ny,Nw)
#' @param covData response data covariance matrices. Supply an array
#' of size (Ny,Nu,Nw,2,2). covData[ky,ku,kw,,] is the covariance matrix
#' of respData[ky,ku,kw]
#' @param noiseCov power spectra variance. Supply an array of 
#' size (Ny,Ny,Nw)
#' 
#' @return an idfrd object
#' 
#' @seealso
#' \code{\link{plot.idfrd}} for generating bode plots, 
#' \code{\link{spa}} and \code{\link{etfe}} for estimating the 
#' frequency response given input/output data
#' 
#' @export
idfrd <- function(respData,freq,Ts,spec=NULL,covData=NULL,
                  noiseCov=NULL){
  # For SISO systems
  if(is.vector(respData)||dim(respData)[1]==nrow(freq)){
    dim(respData) <- c(1,1,nrow(freq))
    dim(spec) <- c(1,1,nrow(freq))
  }
  
  if(dim(respData)[1]==dim)
  out <- list(response=respData,freq=freq,Ts=Ts,spec=spec,covData=
                covData,noiseCov = noiseCov)
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
#' @import ggplot2 reshape2 signal
#' 
#' @export
plot.idfrd <- function(x,col="steelblue",lwd=1){
  nfreq <- dim(x$freq)[1]
  mag <- 20*log10(Mod(x$resp))
  nout <- dim(mag)[1]; nin <- dim(mag)[2]
  dim(mag) <- c(nin*nout,nfreq)
  
  temp <- aperm(Arg(x$resp),c(3,2,1));dim(temp) <- c(nfreq,4)
  l <- t(split(temp, rep(1:ncol(temp), each = nrow(temp))))
  phase <- 180/pi*t(sapply(l,signal::unwrap))

  g <- vector("list",nin*nout)
  
  for(i in 1:length(g)){
    df <- data.frame(Frequency=x$freq[,],Magnitude=mag[i,],
                     Phase = phase[i,])
    melt_df <- reshape2::melt(df,id.var="Frequency")
    yindex <- (i-1)%/%nin + 1;uindex <- i-nin*(yindex-1)
    subtitle <- paste("From: u",uindex," to y",yindex,sep="")
    g[[i]] <- ggplot(melt_df, aes(Frequency, value)) + 
      geom_line(size=lwd,color=col) + scale_x_log10() + 
      facet_grid(variable ~ .,scale="free_y") +
      theme_bw(14,"sans") + ylab("") + ggtitle(subtitle) +
      xlab(ifelse(yindex==nout,"Frequency","")) + 
      theme(axis.title.x=element_text(color = "black",face = "plain"),
            title=element_text(size=9,color = "gray",face="bold")) + 
      geom_vline(xintercept=max(x$freq),size=1)
  }
  
  multiplot(plotlist=g,layout=matrix(1:length(g),nrow=nout,byrow=T))
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
