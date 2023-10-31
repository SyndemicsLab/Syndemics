#' A Function to Change RESPOND inputs from base-case by some percent change
#' @param data dataframe: dataframe from RESPOND base input
#' @param filter string: treatment block to target the change
#' @param cycle list: cycles to edit
#' @param pct_change num: percent to change transition state, +3% -3% would be 1.03, .97 respectively
#'
#' @import data.table
#' @export
DSA <- function(data, filter, cycle, pct_change){
  data <- as.data.table(data)
  for(c in cycle){
    DT <- data[initial_block == filter, paste0("to_", filter, "_cycle", c) := get(paste0("to_", filter, "_cycle", c))*pct_change
    ][, paste0("to_corresponding_post_trt_cycle", c) := 1 - get(paste0("to_", filter, "_cycle", c))]
  }
  return(DT)
}
