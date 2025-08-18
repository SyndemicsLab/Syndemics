#' Combine OUD Count Files into One Data Table
#'
#' Reads all CSV files in a given directory, merges them into a single data.table,
#' and adds a \code{source_file} column identifying the origin of each row.
#'
#' @param path Character: Path to the directory containing CSV files.
#'
#' @return A \code{data.table} containing all rows from all files, with an added
#'   \code{source_file} column.
#'
#' @importFrom data.table fread rbindlist
#' @export
combine_files <- function(path) {
  source_file <- NULL
  files <- list.files(path, full.names = TRUE)
  rbindlist(
    lapply(files, function(f) {
      d <- fread(f)
      d[, source_file := basename(f)]
      d
    }),
    fill = TRUE
  )
}

#' Plot OUD Counts Over Time
#'
#' Creates a line plot showing total OUD counts over time for each data source.
#'
#' @param data A \code{data.table} containing at least \code{year}, \code{N_ID}, and \code{source_file} columns.
#'
#' @return A \code{ggplot} object showing yearly total counts per source.
#'
#' @importFrom data.table .N .SD
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_y_continuous theme_minimal theme
#' @importFrom scales label_comma
#' @export
time_trends <- function(data) {
  N_ID <- source_file <- total <- NULL
  yearly_totals <- data[, list(total = sum(N_ID, na.rm = TRUE)), by = c(year, source_file)]

  p <- ggplot(yearly_totals, aes(x = year, y = total, color = source_file)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts Over Time",
         x = "Year",
         y = "Total Count",
         color = "Data Source") +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

#' Compare Annual OUD Counts Across Files
#'
#' Produces a table comparing yearly OUD counts across all provided datasets.
#'
#' @param data A \code{data.table} containing \code{year}, \code{N_ID}, and \code{source_file} columns.
#'
#' @return A \code{data.table} where each row is a year and each column (after the first)
#'   corresponds to counts from a specific source file.
#'
#' @importFrom data.table dcast
#' @export
compare_by_year <- function(data) {
  N_ID <- total <- source_file <- NULL
  yearly_totals <- data[, list(total = sum(N_ID, na.rm = TRUE)), by = c(year, source_file)]
  comparison_table <- dcast(yearly_totals, year ~ source_file, value.var = "total")
  print(comparison_table)
  return(comparison_table)
}
#' Filter OUD Data by Source File Pattern
#'
#' Extracts rows from the dataset where the source_file column matches a specified pattern.
#'
#' @param data A \code{data.table} containing a \code{source_file} column.
#' @param pattern A character string containing a regular expression pattern to match 
#'   against the \code{source_file} column.
#' @param ignore_case Logical; if TRUE, pattern matching is case-insensitive. Default is TRUE.
#'
#' @return A filtered \code{data.table} containing only rows where source_file matches the pattern.
#' @export
get_filtered_data <- function(data, pattern, ignore_case = TRUE) {
  source_file <- NULL
  return(data[grepl(pattern, source_file, ignore.case = ignore_case)])
}

#' Plot OUD Counts by Demographic Category
#'
#' Creates a line plot of OUD counts over time, broken down by a specified demographic variable.
#'
#' @param data A \code{data.table} containing \code{year}, \code{N_ID}, and the grouping column.
#' @param group_col Character string specifying the column name to group by (e.g., "final_re", "final_sex", "age_grp_twenty").
#' @param labels Named vector mapping group codes to readable labels.
#' @param title Character string for the plot title.
#' @param legend_title Character string for the legend title.
#'
#' @return A \code{ggplot} object visualizing counts by the specified demographic.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_y_continuous theme_minimal theme
#' @importFrom scales label_comma
#' @export
plot_oud_data <- function(data, group_col, labels, title, legend_title) {
  N_ID <- total <- group_name <- NULL
  totals <- data[, list(total = sum(N_ID, na.rm = TRUE)), by = c("year", group_col)]
  totals$group_name <- labels[as.character(totals[[group_col]])]
  
  p <- ggplot(totals, aes(x = year, y = total, color = group_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = title,
         x = "Year",
         y = "Count",
         color = legend_title) +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  print(p)
  return(p)
}

#' Histogram of OUD Counts
#'
#' Creates a histogram showing the distribution of OUD counts.
#'
#' @param data A \code{data.table} containing an \code{N_ID} column.
#'
#' @return A \code{ggplot} object showing the distribution of counts.
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs scale_y_continuous theme_minimal
#' @importFrom scales label_comma
#' @export
histogram <- function(data) {
  N_ID <- NULL
  p <- ggplot(data, aes(x = N_ID)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    labs(title = "Distribution of OUD Counts",
         x = "Count",
         y = "Frequency") +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal()

  print(p)
  return(p)
}
