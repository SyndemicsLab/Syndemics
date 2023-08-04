#' Generates All Possible Combination of Interaction Terms
#' Function that returns a list of every possible combination of interaction terms
#'
#' @param y Column: Column on the estimation side of the equation.
#' @param x List: Columns to return every combination of
#'
#' @export

formula_list <- function(y, x) {
  n <- length(x)
  all_formulas <- list()

  # Additive terms
  for (i in 1:n) {
    combinations <- combn(x, i)
    for (j in 1:ncol(combinations)) {
      combination <- combinations[,j]
      all_formulas <- c(all_formulas, paste(y, "~", paste0(combination, collapse = "+")))
    }
  }

  # Interaction terms
  for (i in 2:n) {
    combinations_interaction <- combn(x, i)
    for (j in 1:ncol(combinations_interaction)) {
      combination_interaction <- combinations_interaction[,j]
      interaction_formula <- paste(combination_interaction, collapse = "*")

      # Additive terms with each interaction term
      for (k in 1:(n-1)) {
        combinations_additive <- combn(x, k)
        for (l in 1:ncol(combinations_additive)) {
          combination_additive <- combinations_additive[,l]
          all_formulas <- c(all_formulas, paste0(y, "~", paste(combination_additive, collapse = "+"), "+", interaction_formula))
        }
      }

      # Only interaction term
      all_formulas <- c(all_formulas, paste0(y, "~", interaction_formula))
    }
  }

  return(lapply(unique(all_formulas), as.formula))
}
