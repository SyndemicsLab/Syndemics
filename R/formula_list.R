#' Generates All Possible Combination of Interaction Terms
#' Function that returns a list of every possible combination of interaction terms
#'
#' @param y Column: Column on the estimation side of the equation.
#' @param variables List of Columns: Columns to return every combination of
#'
#' @export

formula_list <- function(y, variables) {
  n <- length(variables)

  base_formula <- paste(y, "~", paste(variables, collapse = " + "))
  formulae <- list(as.formula(base_formula))

  for(i in 2:n) {
    combinations <- combn(variables, i, simplify = FALSE)
    for(combination in combinations) {
      interaction_terms <- paste(combination, collapse = "*")

      remaining_vars <- setdiff(variables, combination)
      remaining_string <- if(length(remaining_vars) > 0) paste(paste(remaining_vars, collapse = " + "), "+") else ""
      modified_base_formula <- paste(y, "~", remaining_string, interaction_terms)

      interaction_formula <- paste(base_formula, "+", interaction_terms)

      formulae <- c(formulae, as.formula(interaction_formula), as.formula(modified_base_formula))
    }
  }

  return(formulae)
}

formula_list(y = "N_ID", variables = c("A", "B", "C", "D", "E", "F"))
