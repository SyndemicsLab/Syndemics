#' Generates All Possible Combination of Interaction Terms
#' Function that returns a list of every possible combination of interaction terms
#'
#' @param y Column: Column on the estimation side of the equation.
#' @param variables List of Columns: Columns to return every combination of
#'
#' @export

formula_list <- function(y, variables){
  n <- length(variables)
  formulae <- list()

  for(i in 1:n){
    combinations <- combn(variables, i, simplify = FALSE)

    for(combination in combinations){
      interaction_terms <- paste(combination, collapse = "*")

      formula_without_interaction <- as.formula(paste(y, "~", paste(combination, collapse = " + ")))
      formula_with_interaction <- as.formula(paste(y, "~", interaction_terms, "+", paste(combination, collapse = " + ")))
      formula_only_interaction <- as.formula(paste(y, "~", interaction_terms))

      formulae <- c(formulae, formula_without_interaction, formula_with_interaction, formula_only_interaction)
    }
  }
  return(formulae)
}
