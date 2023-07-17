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
#' @param method String: Selection for the spatial capture-recapture method - either poisson or good-turing
#' @param corr.threshold Numeric: Threshold for forcing interaction term between binary columns. Default is 0.2
#' @param formula Formula: Allows definition of custom formula object for poisson regression
#'
#' @export

crc <- function(data, freq.column, binary.variables, method = "poisson", corr.threshold = 0.2, formula = NULL){
  dt <- data.table::as.data.table(data)

  if(is.null(formula)){
    form <- corr_formula(corr, corr.threshold, freq.column)
  } else if(!is.formula(formula)){
    stop("Expected Formula Object")
  } else form <- formula

  # Correlation Testing ===============================
  data_expansion <- data.table()
  data_expansion <- dt[rep(1:.N, get(freq.column))][, (freq.column) := NULL]

  corr <- cor(data_expansion)

  corr_plot <- ggcorrplot::ggcorrplot(corr, type = "upper", lab = TRUE) +
    ggplot2::theme(legend.position = "none")

  #Poisson Modeling ===================================
  if(method == "poisson"){
    tmp <- poisson_regression(dt, form)
    ci_intercept <- confint(tmp)[1, ]

    formula_string = as.character(form)

    model <- list(
      model = method,
      formula = formula_string,
      summary = summary(tmp),
      estimate = exp(coef(tmp)[1]),
      lower_ci = exp(ci_intercept[1]),
      upper_ci = exp(ci_intercept[2])
    )
  }

  #Good-Turing Modeling =================================
  if(method == "good-turing"){
    gt <- good_turing(data = dt, freq.column, binary.variables)

    model <- list(
      model = method,
      formula = NULL,
      summary = NULL,
      estimate = gt$estimated_unseen,
      lower_ci = gt$confidence_interval$conf.low,
      upper_ci = gt$confidence_interval$conf.high
    )
  }

  return(model)
}
