
formula_corr <- function(corr_matrix, threshold, freq.column){
  corr_upper <- corr_matrix; corr_upper[upper.tri(corr_matrix, diag = TRUE)] <- NA
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
