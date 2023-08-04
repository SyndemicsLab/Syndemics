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

  # Individual terms
  for (i in 1:n) {
    all_formulas <- c(all_formulas, paste0(y, "~", x[i]))
  }

  # Additive and interaction terms
  for (i in 2:n) {
    combinations <- combn(x, i)
    for (j in 1:ncol(combinations)) {
      combination <- combinations[,j]

      # Additive terms
      all_formulas <- c(all_formulas, paste0(y, "~", paste(combination, collapse="+")))

      # Interaction term
      interaction_formula <- paste(combination, collapse="*")
      all_formulas <- c(all_formulas, paste0(y, "~", interaction_formula))

      # Additive terms with interaction term
      for (k in 1:(i-1)) {
        combinations_additive <- combn(combination, k)
        for (l in 1:ncol(combinations_additive)) {
          combination_additive <- combinations_additive[,l]
          all_formulas <- c(all_formulas, paste0(y, "~", paste(combination_additive, collapse="+"), "+", interaction_formula))
        }
      }
    }
  }

  return(lapply(unique(all_formulas), as.formula))
}
