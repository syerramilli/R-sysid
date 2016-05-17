#' Data simulated from an ARMAX model 
#' 
#' This dataset contains 2555 samples simulated from the following ARMAX model:
#' \deqn{
#'  y[k] = \frac{0.6q^{-2} - 0.2q^{-3}}{1 - 0.5q^{-1}} u[k] + 
#'  \frac{1-0.3q^{-1}}{1 - 0.5q^{-1}} e[k]
#' }
#' 
#' The model is simulated with a 2555 samples long full-band PRBS input.
#' The noise variance is set to 0.1
#' 
#' @format an \code{idframe} object with 2555 samples, one input and one
#' output
#' 
"armaxsim"             

#' Data simulated from an ARX model 
#' 
#' This dataset contains 2555 samples simulated from the following ARX model:
#' \deqn{
#'  y[k] = \frac{0.6q^{-2} - 0.2q^{-3}}{1 - 0.5q^{-1}} u[k] + 
#'  \frac{1}{1 - 0.5q^{-1}} e[k]
#' }
#' 
#' The model is simulated with a 2555 samples long full-band PRBS input.
#' The noise variance is set to 0.1
#' 
#' @format an \code{idframe} object with 2555 samples, one input and one
#' output
#'
"arxsim"               

#' Data simulated from an BJ model 
#' 
#' This dataset contains 2046 samples simulated from the following BJ model:
#' \deqn{
#'  y[k] = \frac{0.6q^{-2} - 0.2q^{-3}}{1 - 0.5q^{-1}} u[k] + 
#'  \frac{1+0.2q^{-1}}{1 - 0.3q^{-1}} e[k]
#' }
#' 
#' The model is simulated with a 2046 samples long full-band PRBS input.
#' The noise variance is set to 0.1
#' 
#' @format an \code{idframe} object with 2046 samples, one input and one
#' output
#'
"bjsim"                

#' Continuous stirred tank reactor data (idframe)
#' 
#' The Process is a model of a Continuous Stirring Tank Reactor, 
#' where the reaction is exothermic and the concentration is 
#' controlled by regulating the coolant flow.
#' \cr
#' 
#' Inputs: q, Coolant Flow l/min
#' Outputs:
#' \describe{
#' \item{Ca}{Concentration mol/l}
#' \item{T}{Temperature Kelvin}}
#' 
#' @format an \code{idframe} object with 7500 samples, one input and two
#' outputs
"cstr"                 

#' Continuous stirred tank reactor data (data.frame)
#' 
#' The Process is a model of a Continuous Stirring Tank Reactor, 
#' where the reaction is exothermic and the concentration is 
#' controlled by regulating the coolant flow.
#' \cr
#' 
#' Inputs: q, Coolant Flow l/min
#' Outputs:
#' \describe{
#' \item{Ca}{Concentration mol/l}
#' \item{T}{Temperature Kelvin}}
#' 
#' @format an \code{data.frame} object with 7500 rows and three columns:
#' q, Ca and T
#' 
#' @source \url{ftp://ftp.esat.kuleuven.ac.be/pub/SISTA/espinosa/datasets/cstr.dat}
"cstrData"             

#' Continuous stirred tank reactor data with missing values
#' 
#' This dataset is derived from the \code{cstr} dataset with few samples
#' containing missing values, in one or all variables. It is used to 
#' demonstrate the capabilities of the \code{misdata} routine.
#' 
#' @format an \code{idframe} object with 7500 samples, one input and two
#' outputs
#' 
#' @seealso \code{\link{cstr}}, \code{\link{misdata}}
"cstr_mis"             

#' Frequency response data
#' 
#' This dataset contains frequency response data of an unknown SISO system.
#' 
#' @format an \code{idfrd} object with response at 128 frequency points 
"frd"                  

#' Data simulated from an OE model 
#' 
#' This dataset contains 2555 samples simulated from the following OE model:
#' \deqn{
#'  y[k] = \frac{0.6q^{-2} - 0.2q^{-3}}{1 - 0.5q^{-1}} u[k] + e[k]
#' }
#' 
#' The model is simulated with a 2555 samples long full-band PRBS input.
#' The noise variance is set to 0.1
#' 
#' @format an \code{idframe} object with 2555 samples, one input and one
#' output
#'
"oesim"