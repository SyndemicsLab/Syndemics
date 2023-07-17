#' User-friendly split: Separates a dataframe into a list of dataframes along a specified column with an extra option to write out the data
#'
#' @param data dataframe
#' @param column.name variable: column to separate upon
#' @param save boolean: by default FALSE indicating not to write to csv
#'
#' @importFrom dplyr select
#'
#' @export

separate <- function(data, column.name, save = FALSE){
  out <- split(data, {{column.name}})

  names <- unique({{column.name}})
  if(save == TRUE){
    for(i in 1:length(out)){
      write.csv(out[[i]], paste0(names[i], ".csv"))
    }
  }
  return(out)
}
