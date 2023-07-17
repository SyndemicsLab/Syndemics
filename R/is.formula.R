#' Formula Objects
#' Generic test of an object being interpretable as a formula
#' 
#' @param x object to be tested
#' 
#' @export

is.formula <- function(x){
  inherits(x, "formula")
}
