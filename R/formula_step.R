formula_step <- function(y, x, interactions = NULL) {
  if (!is.null(interactions) && length(interactions) > 0) {
    interaction_terms <- paste(interactions, collapse = "+")
    return(as.formula(paste(y, "~", paste(x, collapse = "+"), "+", interaction_terms)))
  } else {
    return(as.formula(paste(y, "~", paste(x, collapse = "+"), "- 1")))
  }
}
