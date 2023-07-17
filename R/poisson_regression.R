#' Poisson Regression
#' Regresses \code{data} using input \code{formula} by calling \code{glm} and
#' and forcing poisson family with a log link function.
#'
#' @param data Dataframe: input data to the poisson regression model
#' @param formula Formula object: formula input into the regression model should only
#' contain variable names held within \code{data}
#'
#' @export

poisson_regression <- function(data, formula){
  model <- glm(formula = formula, data = data,
               family = poisson(link = "log"))

  return(model)
}
