# Generic form
#' Multiple assignment operator
#' 
#' Assign multiple variables from a list or function return object
#' 
#' @param l,r the variables to be assigned, and the list or 
#' function return object
#' 
#' @export
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
#' @export
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)
  
#   if (length(r) > length(l))
#     warning("RHS has more args than LHS. Only first", length(l), "used.")
  
  if (length(l) > length(r))  {
    warning("LHS has more args than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }
  
  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)
  
  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  
  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
#' @export
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}