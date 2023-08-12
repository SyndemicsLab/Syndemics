step_regression <- function(data, y, x,
                            threshold = c(0.05, 0.1),
                            direction = c("forward", "backward"),
                            max.interactions = 2,
                            method = c("poisson", "negbin")) {

  stepwise_regression <- function(model, direction, threshold) {
    step(model, direction = direction, test = "Chisq", k = log(nrow(data)), trace = 0,
         scope = list(lower = model, upper = formula_step(y = y, x = x, interactions = combn(x, max.interactions, paste, collapse = ":"))))
  }

  results <- data.frame(exp_intercept = numeric(), formula = character(),
                        AIC = numeric(), regression_type = character(),
                        p_threshold = numeric(), direction = character())

  for (m in seq_along(method)) {
    for (thresh in seq_along(threshold)) {
      for (dir in seq_along(direction)) {
        if (method[m] == "poisson") {
          model <- glm(formula_step(y = y, x = x), data = data, family = poisson)
        } else if(method[m] == "negbin") {
          model <- MASS::glm.nb(formula_step(y = y, x = x), data = data)
        }

        stepwise_model <- stepwise_regression(model, direction[dir], threshold[thresh])

        new_row <- data.frame(
          formula = format(stepwise_model$call$formula),
          estimate = unname(exp(coef(stepwise_model)[1])),
          AIC = AIC(stepwise_model),
          regression_type = method[m],
          p_threshold = threshold[thresh],
          direction = direction[dir]
        )

        results <- rbind(results, new_row)
      }
    }
  }

  return(results)
}
