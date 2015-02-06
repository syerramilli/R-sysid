#' Impute missing values
#' 
#' Uses the bootstrap EM Algorithm on incomplete data and creates imputed datasets
#' 
#' @param data an object of class \code{idframe}
#' @param m the number of imputed datasets to create.
#' 
#' @return 
#' A list containing the following elements
#' 
#' \tabular{ll}{
#'    \code{imputations} \tab a list of length \code{m} with an imputed dataset in each 
#'    entry. Each element is of class \code{idframe} \cr
#'    \code{m} \tab an integer indicating the number of imputations run
#'  }
#' 
#' @details
#' This function uses the \code{\link[Amelia]{amelia}} function, provided by the 
#' \pkg{amelia} package, to perform multiple imputation.
#' 
#' @references
#' Honaker, J., King, G., Blackwell, M. (2011). Amelia II: A Program for Missing Data. 
#' Journal of Statistical Software, 45(7), 1–47. URL http://www.jstatsoft.org/v45/i07/.
#' 
#' @seealso \code{\link[Amelia]{amelia}}
#' @export 
dataImpute <- function(data,m=5){
  # check if the class is correct
  if(class(data)!='idframe')
    stop("Not an idframe data")
  
  ninputs <- dim(data$input)[2]
  dataMatrix <- cbind(data$input,data$output)
  
  a.out <- amelia(dataMatrix,m=5,p2s = 0)
}