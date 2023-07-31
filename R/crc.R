#' Spatial Capture Re-Capture
#'
#' A method for estimation of 'unknowns' through knowledge about knowns as described by
#' Barocas, Joshua A et al. “Estimated Prevalence of Opioid Use Disorder in Massachusetts, 2011-2015: A Capture-Recapture Analysis.” doi:10.2105/AJPH.2018.304673
#' \code{crc} now builds on other functions inside this package: \code{corr_formula} builds a correlation matrix between \code{binary.variables} using \code{freq.column}
#' then constructs a recommended formula for poisson regression models. If the correlation is greater than \code{corr.threshold} it assumes an interaction term instead of additive.
#' There is an option to use the Good-Turing method for unknown estimation, which does not require parameterization from the correlation matrix or formula
#'
#'
#' @import data.table
#' @importFrom ggplot2 theme
#' @importFrom ggcorrplot ggcorrplot
#'
#' @param data Dataframe: A dataframe containing a frequency column and binary columns indicating involvement in the given database
#' @param freq.column Column: A column containing the frequency of observed combinations
#' @param binary.variables List of Columns: List containing columns of binary variables indicating involvement in the given database
#' @param method String: Selection for the spatial capture-recapture method - either poisson, good-turing, or negbin
#' @param formula.selection String: Selection for formula decision when \code{method} is poisson or negbin - either aic or corr
#' @param corr.threshold Numeric: Threshold for forcing interaction term between binary columns. Only applicable when \code{formula.selection} is \code{"corr"}
#' @param formula Formula: Allows definition of custom formula object for poisson regression. In the case of a specified formula, both \code{formula.selection} methods will produce the same results
#'
#' @importFrom MASS glm.nb
#'
#' @export

crc <- function(data, freq.column, binary.variables, method = "poisson", formula.selection = "aic", corr.threshold = 0.2, formula = NULL){
  dt <- data.table::as.data.table(data)

  if(is.null(formula)){
    if(formula.selection == "corr"){
      form <- corr_formula(corr, corr.threshold, freq.column)
    } else if(formula.selection == "aic"){
      form <- formula_list(freq.column, binary.variables)
    }
  } else if(!is.formula(formula)){
    stop("Expected Formula Object when Specifying Formula")
  } else form <- formula

# Correlation Testing ======================
  data_expansion <- data.table()
  data_expansion <- dt[rep(1:.N, get(freq.column))][, (freq.column) := NULL]

  corr <- cor(data_expansion)

  corr_plot <- ggcorrplot::ggcorrplot(corr, type = "upper", lab = TRUE) +
    ggplot2::theme(legend.position = "none")

# Poisson Modeling =========================
  if(method == "poisson"){
    if(formula.selection == "aic"){
      results <- list()
      for(i in seq_along(form)){
        result <- tryCatch({
          model <- poisson_regression(dt, form[[i]])

          exp_intercept <- unname(round(exp(coef(model)["(Intercept)"]), 2))
          aic <- round(AIC(model), 2)

          ci <- suppressMessages(confint(model, "(Intercept)", level = 0.95))
          lower_ci <- unname(round(exp(ci[1]), 2))
          upper_ci <- unname(round(exp(ci[2]), 2))

          list(formula = as.character(deparse(form[[i]])), estimate = exp_intercept, AIC = aic,
               lower_ci = lower_ci, upper_ci = upper_ci)

        }, error = function(e) {
          list(formula = as.character(deparse(form[[i]])), estimate = NA, AIC = NA,
               lower_ci = NA, upper_ci = NA, error = toString(e$message))
        })

        results[[i]] <- result
      }
      model <- do.call(rbind, lapply(results, function(x) as.data.frame(t(unlist(x)))))
      model <- model[order(model$AIC), ]

    } else if(formula.selection == "corr"){
      tmp <- poisson_regression(dt, form)
      ci_intercept <- suppressMessages(exp(confint(tmp)[1, ]))

      formula_string = as.character(deparse(form))

      model <- list(
        corr_matrix = corr,
        corr_plot = corr_plot,
        model = method,
        formula = formula_string,
        summary = summary(tmp),
        estimate = round(exp(coef(tmp)[1]), 2),
        lower_ci = unname(round(exp(ci_intercept[1]), 2)),
        upper_ci = unname(round(exp(ci_intercept[2]), 2))
      )
    }
  }
# Neg-Bin Modeling =========================
  if(method == "negbin"){
    if(formula.selection == "aic"){
      results <- list()
      for(i in seq_along(form)){
        result <- tryCatch({
          model <- MASS::glm.nb(data = dt, formula = form[[i]])

          exp_intercept <- unname(round(exp(coef(model)["(Intercept)"]), 2))
          aic <- round(AIC(model), 2)

          ci <- suppressMessages(confint(model, "(Intercept)", level = 0.95))
          lower_ci <- unname(round(exp(ci[1]), 2))
          upper_ci <- unname(round(exp(ci[2]), 2))

          list(formula = as.character(deparse(form[[i]])), estimate = exp_intercept, AIC = aic,
               lower_ci = lower_ci, upper_ci = upper_ci)

        }, error = function(e) {
          list(formula = as.character(deparse(form[[i]])), estimate = NA, AIC = NA,
               lower_ci = NA, upper_ci = NA, error = toString(e$message))
        })

        results[[i]] <- result
      }
      model <- do.call(rbind, lapply(results, function(x) as.data.frame(t(unlist(x)))))
      model <- model[order(model$AIC), ]

    } else if(formula.selection == "corr"){
      tmp <- MASS::glm.nb(formula = form, data = dt)
      ci_intercept <- suppressMessages(exp(confint(tmp)[1, ]))

      formula_string = as.character(deparse(form))

      model <- list(
        corr_matrix = corr,
        corr_plot = corr_plot,
        model = method,
        formula = formula_string,
        summary = summary(tmp),
        estimate = round(exp(coef(tmp)[1]), 2),
        lower_ci = unname(round(exp(ci_intercept[1]), 2)),
        upper_ci = unname(round(exp(ci_intercept[2]), 2))
      )
    }
  }
# Good-Turing Modeling =======================
  if(method == "good-turing"){
    gt <- good_turing(data = dt, freq.column, binary.variables)

    model <- list(
      corr_matrix = corr,
      corr_plot = corr_plot,
      model = method,
      formula = NULL,
      summary = NULL,
      estimate = round(gt$estimated_unseen, 2),
      lower_ci = round(gt$confidence_interval$conf.low, 2),
      upper_ci = round(gt$confidence_interval$conf.high, 2)
    )
  }

  return(model)
}

