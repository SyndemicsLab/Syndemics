#' Spatial Capture Re-Capture
#'
#' A method for estimation of 'unknowns' through knowledge about knowns as described by
#' Barocas, Joshua A et al. “Estimated Prevalence of Opioid Use Disorder in Massachusetts, 2011-2015: A Capture-Recapture Analysis.” doi:10.2105/AJPH.2018.304673
#' \code{crc} now builds on other functions inside this package: \code{corr_formula} builds a correlation matrix between \code{binary.variables} using \code{freq.column}
#' then constructs a recommended formula for poisson regression models. If the correlation is greater than \code{corr.threshold} it assumes an interaction term instead of additive.
#' There are options to use stepwise selection which step-wise selects models based on \code{opts.stepwise}'s \code{threshold} and \code{DBCount}, which
#' rowwise sums the \code{binary.variables}, sums \code{freq.column} by grouping the rowwise sum, and uses poisson regression to estimate \code{freq.column ~ DBCount}
#'
#'
#'
#' @param data Dataframe: A dataframe containing a frequency column and binary columns indicating involvement in the given database
#' @param freq.column Column: A column containing the frequency of observed combinations
#' @param binary.variables List of Columns: List containing columns of binary variables indicating involvement in the given database
#' @param method String: Selection for the spatial capture-recapture method - either poisson, negbin, or DBCount
#' @param formula.selection String: Selection for formula decision when \code{method} is poisson or negbin - either aic, corr, or stepwise
#' @param corr.threshold Numeric: Threshold for forcing interaction term between binary columns. Only applicable when \code{formula.selection} is \code{"corr"}
#' @param formula Formula: Allows definition of custom formula object for poisson regression. In the case of a specified formula, both \code{formula.selection} methods will produce the same results
#' @param opts.stepwise List: List of \code{direction}: 'forward', 'backward', or 'both', \code{threshold}: p-value threshold for stepwise selection, and \code{verbose} if you would like every stepped-through model to be printed
#'
#' @import data.table
#' @importFrom ggplot2 theme
#' @importFrom ggcorrplot ggcorrplot
#' @importFrom MASS glm.nb
#' @export

crc <- function(data, freq.column, binary.variables, method = "poisson", formula.selection = "stepwise", corr.threshold = 0.2, formula = NULL,
                opts.stepwise = list(direction = "both",
                                     threshold = 0.05,
                                     verbose = TRUE)){
  dt <- data.table::as.data.table(data)

  data_expansion <- data.table()
  data_expansion <- dt[rep(1:.N, get(freq.column))][, (freq.column) := NULL]

  corr <- cor(data_expansion)

  corr_plot <- ggcorrplot::ggcorrplot(corr, type = "upper", lab = TRUE) +
    ggplot2::theme(legend.position = "none")

  if(is.null(formula)){
    if(formula.selection == "corr"){
      form <- formula_corr(corr, corr.threshold, freq.column)
    } else if(formula.selection == "aic"){
      form <- formula_list(freq.column, binary.variables)
    }
  } else if(!is.formula(formula)){
    stop("Expected Formula Object when Specifying Formula")
  } else form <- formula

  if(method == "poisson"){
    if(formula.selection == "aic"){
      results <- list()
      for(i in seq_along(form)){
        result <- tryCatch({
          model <- glm(form[[i]], data = dt, family = "poisson")

          intercept <- exp(coef(model)["(Intercept)"])
          aic <- AIC(model)

          ci <- suppressMessages(confint(model, "(Intercept)", level = 0.95))
          lower_ci <- exp(ci[1])
          upper_ci <- exp(ci[2])

          data.frame(formula = paste(deparse(form[[i]]), collapse = " "),
                     estimate = round(intercept, 2), AIC = round(aic, 2),
                     lower_ci = unname(round(lower_ci, 2)), upper_ci = unname(round(upper_ci, 2)), error = NA,
                     row.names = NULL)

        }, error = function(e) {
          data.frame(formula = paste(deparse(form[[i]]), collapse = " "),
                     estimate = NA, AIC = NA,
                     lower_ci = NA, upper_ci = NA, error = toString(e$message),
                     row.names = NULL)
        })

        results[[i]] <- result
      }
      model <- do.call(rbind, results)
      model <- model[order(model$AIC), ]

    } else if(formula.selection == "corr"){
      tmp <- glm(formula = form, data = dt, family = "poisson")
      ci_intercept <- suppressMessages(exp(confint(tmp)[1, ]))

      formula_string = paste(deparse(form), collapse = " ")

      model <- list(
        corr_matrix = corr,
        corr_plot = corr_plot,
        model = method,
        formula = formula_string,
        summary = summary(tmp),
        estimate = unname(round(exp(coef(tmp)["(Intercept)"]), 2)),
        lower_ci = unname(round(exp(ci_intercept[1]), 2)),
        upper_ci = unname(round(exp(ci_intercept[2]), 2)),
        AIC = AIC(tmp)
      )
    } else if(formula.selection == "stepwise"){
      model <- step_regression(data, freq.column, binary.variables,
                               p.threshold = opts.stepwise$threshold,
                               direction = opts.stepwise$direction,
                               method = method,
                               verbose = opts.stepwise$verbose)
    }
  }

  if(method == "negbin"){
    if(formula.selection == "aic"){
      results <- list()
      for(i in seq_along(form)){
        result <- tryCatch({
          model <- MASS::glm.nb(formula = form[[i]], data = dt)

          intercept <- exp(coef(model)["(Intercept)"])
          aic <- AIC(model)

          ci <- suppressMessages(confint(model, "(Intercept)", level = 0.95))
          lower_ci <- exp(ci[1])
          upper_ci <- exp(ci[2])

          data.frame(formula = paste(deparse(form[[i]]), collapse = " "),
                     estimate = round(intercept, 2), AIC = round(aic, 2),
                     lower_ci = unname(round(lower_ci, 2)), upper_ci = unname(round(upper_ci, 2)), error = NA,
                     row.names = NULL)

        }, error = function(e) {
          data.frame(formula = paste(deparse(form[[i]]), collapse = " "),
                     estimate = NA, AIC = NA,
                     lower_ci = NA, upper_ci = NA, error = toString(e$message),
                     row.names = NULL)
        })

        results[[i]] <- result
      }
      model <- do.call(rbind, results)
      model <- model[order(model$AIC), ]

    } else if(formula.selection == "corr"){
      tmp <- MASS::glm.nb(formula = form, data = dt)
      ci_intercept <- suppressMessages(exp(confint(tmp)[1, ]))

      formula_string = paste(deparse(form), collapse = " ")

      model <- list(
        corr_matrix = corr,
        corr_plot = corr_plot,
        model = method,
        formula = formula_string,
        summary = summary(tmp),
        estimate = unname(round(exp(coef(tmp)["(Intercept)"]), 2)),
        lower_ci = unname(round(exp(ci_intercept[1]), 2)),
        upper_ci = unname(round(exp(ci_intercept[2]), 2)),
        AIC = AIC(tmp)
      )
    } else if(formula.selection == "stepwise"){
      model <- step_regression(data, freq.column, binary.variables,
                               p.threshold = opts.stepwise$threshold,
                               direction = opts.stepwise$direction,
                               method = method,
                               verbose = opts.stepwise$verbose)
    }
  }

  if(method == "DBCount"){
    dbc <- setDT(dt)[, .(dbcnt = rowSums(.SD)), by = c(binary.variables, freq.column), .SDcols = binary.variables
                     ][, .(N = sum(get(freq.column))), by = dbcnt]

    dbc_model <- glm(N ~ dbcnt, data = dbc, family = "poisson")
    ci_intercept <- suppressMessages(confint(dbc_model, "(Intercept)", level = 0.95))

    model <- list(
      corr_matrix = corr,
      corr_plot = corr_plot,
      model = method,
      formula = NULL,
      summary = summary(dbc_model),
      estimate = unname(round(exp(coef(dbc_model)["(Intercept)"]), 2)),
      lower_ci = unname(round(exp(ci_intercept[1]), 2)),
      upper_ci = unname(round(exp(ci_intercept[2]), 2)),
      AIC = AIC(dbc_model)
    )
  }

  return(model)
}
