#' Good-Turing Estimation of Unseen Species
#' Provides methodology for the Good-Turing estimation of unseen species for spatial
#' capture-recapture methodology.
#'
#' @import data.table
#' @importFrom DescTools BinomCI
#'
#' @param data Dataframe: Data for use in the Good-Turing estimation
#' @param freq.column Column: A column containing the frequency of observed combinations
#' @param binary.variables List of Columns: List containing columns of binary variables indicating involvement in the given database
#'
good_turing <- function(data, freq.column, binary.variables) {
  dt <- as.data.table(data)

  if (!isTRUE(all(duplicated(dt[, ..binary.variables]) == FALSE))) {
    grouped_data <- dt[, .(summed_frequency = sum(get(freq.column))), by = binary.variables]

    species <- do.call(paste, c(grouped_data[, ..binary.variables, with = FALSE], sep = "_"))
    frequency <- grouped_data[[freq.column]]

    warning("Species vector non-unique: grouped by binary columns and summed respective frequencies")
  } else {
    species <- do.call(paste, c(dt[, ..binary.variables, with = FALSE], sep = "_"))
    frequency <- dt[[freq.column]]
  }

  frequency_table <- table(species)
  num_species <- length(frequency_table)
  observed_frequencies <- table(frequency)
  max_frequency <- max(frequency)

  num_observations <- sum(frequency)

  est_unseen <- round((1 - sum(observed_frequencies) / num_observations) * num_observations)
  ci <- BinomCI(est_unseen, num_observations, conf.level = 0.95, method = "wilson")

  result <- list(
    estimated_unseen = est_unseen,
    confidence_interval = ci
  )
  return(result)
}
