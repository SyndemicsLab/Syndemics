#' Aggregate selected tables from a set of RESPOND simulation runs
#'
#' @param N Numeric: The number of output folders to be loaded. Assumes that
#' output folders are named as \code{output1}, \code{output2}, ...,
#' \code{outputN}.
#' @param outputFileNames Character: A single string or vector of strings
#' representing the file paths (relative to output#) of files to include in the
#' returned object.
#' @param pathPrefix Character: (Optional) A relative or absolute path that can
#' be used when loading data not located in the working directory.
#' @return List of Lists: The contents of the specified output folders/files.
#' The top-level keys are the name of each output folder, i.e. \code{output1},
#' \code{output2}, etc. These subsequent lists are broken down by the contents of
#' outputFileNames, e.g.
#' ```
#' outputTables
#' $output1
#' - $"all_types_overdose1.csv"
#' - $"general_stats1.csv"
#' - $"cost_life/total_costs1.csv"
#' ```
#' @md
#' @importFrom utils read.csv
#'
#' @export
loadOutputFiles <- function(N, outputFileNames, pathPrefix = "") {
  # append the trailing slash if the prefix does not already
  if (pathPrefix != "" && !endsWith(pathPrefix, "/")) {
    pathPrefix <- paste0(pathPrefix, "/")
  }
  outputNumbers <- 1:N
  outputTables <- list()
  # read files and store them in outputTables
  for (index in seq_along(outputNumbers)) {
    outputFolder <- paste0("output", index)
    currentOutputTables <- list()
    for (outputFile in outputFileNames) {
      currentOutputTables[[outputFile]] <- read.csv(paste0(pathPrefix, outputFolder, "/", outputFile))
    }
    outputTables[[outputFolder]] <- currentOutputTables
  }
  return(outputTables)
}
