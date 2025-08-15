#' Formula Objects
#' Generic test of an object being interpretable as a formula
#'
#' @param x object to be tested
#'
#' @export

is.formula <- function(x){
  inherits(x, "formula")
}

#' Generates All Possible Combination of Interaction Terms
#' Function that returns a list of every possible combination of interaction terms
#'
#' @param y Column: Column on the estimation side of the equation.
#' @param x List: Columns to return every combination of
#' @importFrom utils combn
#' @importFrom stats as.formula
#'
#' @export

formula_list <- function(y, x) {
  n <- length(x)
  all_formulas <- list()

  for (i in 1:n) {
    all_formulas <- c(all_formulas, paste0(y, "~", x[i]))
  }

  for (i in 2:n) {
    combinations <- combn(x, i)
    for (j in 1:ncol(combinations)) {
      combination <- combinations[,j]

      all_formulas <- c(all_formulas, paste0(y, "~", paste(combination, collapse="+")))
      interaction_formula <- paste(combination, collapse="*")
      all_formulas <- c(all_formulas, paste0(y, "~", interaction_formula))

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

#' Helper function for stepwise regression
#' @param data dataframe
#' @param y string: LHS of formula object
#' @param x string: RHS of formula object
#' @param method string: either 'poisson' or 'negbin'
#' @param direction string: stepwise direction
#' @param p.threshold numeric: threshold for stepwise selection
#' @param k integer: limit for k-way interaction terms
#' @param verbose logical: whether to print intermediate models
#'
#' @keywords internal
#' @importFrom MASS glm.nb
#' @importFrom utils capture.output
#' @importFrom stats AIC coef confint formula glm poisson step

step_regression <- function(data, y, x, method = "poisson", direction = "both",
                            p.threshold = 0.05, k = 2, verbose = TRUE) {
  formula_init <- as.formula(paste(y, "~", paste(x, collapse = " + ")))
  formula_max <- as.formula(paste(y, "~ (", paste(x, collapse = " + "), ")^", k))

  if (!verbose) {
    capture.output({
      if (method == "poisson") {
        init_mod <- glm(formula_init, family = poisson, data = data)
      } else {
        init_mod <- MASS::glm.nb(formula_init, data = data)
      }

      final_mod <- suppressWarnings(step(init_mod,
                                         scope = list(upper = formula_max, lower = formula_init),
                                         direction = direction,
                                         k = log(nrow(data))))
    })
  } else {
    if (method == "poisson") {
      init_mod <- glm(formula_init, family = poisson, data = data)
    } else {
      init_mod <- MASS::glm.nb(formula_init, data = data)
    }

    final_mod <- step(init_mod,
                      scope = list(upper = formula_max, lower = formula_init),
                      direction = direction,
                      k = log(nrow(data)))
  }

  intercept <- coef(final_mod)[1]
  estimate <- exp(intercept)
  ci <- exp(confint(final_mod)[1, ])

  results <- list(
    model = method,
    formula = formula(final_mod),
    summary = summary(final_mod),
    estimate = unname(round(estimate, 2)),
    lower_ci = unname(round(ci[1], 2)),
    upper_ci = unname(round(ci[2], 2)),
    AIC = AIC(final_mod)
  )

  return(results)
}
