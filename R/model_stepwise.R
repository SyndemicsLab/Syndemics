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
