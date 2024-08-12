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

#' Internal function to create a formula based on correlation among databases listed in \code{CRC}
#'
#' @param corr_matrix A correlation matrix
#' @param threshold Threshold for interacting two databases
#' @param freq.column frequency column, e.g. N_ID
#'
#' @keywords internal
formula_corr <- function(corr_matrix, threshold, freq.column){
  corr_upper <- corr_matrix
  corr_upper[upper.tri(corr_matrix, diag = TRUE)] <- NA

  interaction_indices <- which(abs(corr_upper) > threshold, arr.ind = TRUE)

  interactions_df <- data.frame(var1 = rownames(corr_upper)[interaction_indices[, 1]],
                                var2 = colnames(corr_upper)[interaction_indices[, 2]])

  if (nrow(interactions_df) == 0) {
    interactions_df <- data.frame(var1 = character(),
                                  var2 = character(),
                                  stringsAsFactors = FALSE)
  }

  interaction_terms <- apply(interactions_df, 1, function(row) {
    paste0(row[1], "*", row[2])
  })

  non_interaction_vars <- setdiff(rownames(corr_matrix), c(interactions_df$var1, interactions_df$var2))
  formula_string <- paste(paste(non_interaction_vars, collapse = " + "), "+", paste(interaction_terms, collapse = " + "))
  formula_string <- paste(freq.column, "~", formula_string)

  formula_object <- as.formula(formula_string)

  return(formula_object)
}

#' @keywords internal
#' @importFrom MASS glm.nb

step_regression <- function(data, y, x, method = "poisson", direction = "both", p.threshold = 0.05, k = 2, verbose = TRUE) {
  formula_init <- as.formula(paste(y, "~", paste(x, collapse = " + ")))
  formula_max <- as.formula(paste(y, "~ (", paste(x, collapse = " + "), ")^", k))

  if (!verbose) {
    capture.output({
      if (method == "poisson") {
        init_mod <- glm(formula_init, family = poisson, data = data)
      } else if (method == "negbin") {
        init_mod <- glm.nb(formula_init, data = data)
      }

      final_mod <- suppressWarnings(step(init_mod,
                                         scope = list(upper = formula_max, lower = formula_init),
                                         direction = direction,
                                         k = log(nrow(data))))
    })
  } else {
    if (method == "poisson") {
      init_mod <- glm(formula_init, family = poisson, data = data)
    } else if (method == "negbin") {
      init_mod <- glm.nb(formula_init, data = data)
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
    estimate = unname(estimate),
    lower_ci = unname(ci[1]),
    upper_ci = unname(ci[2]),
    AIC = AIC(final_mod)
  )

  return(results)
}

