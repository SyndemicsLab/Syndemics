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
  yearly_totals <- data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, source_file)]

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
  yearly_totals <- data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, source_file)]
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
plot_race_data <- function(race_data) {
  race_labels <- c("1" = "White", "2" = "Black", "3" = "Asian / Pacific Islander",
                   "4" = "Hispanic", "5" = "Native American / Other")
  race_totals <- race_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, final_re)]
  race_totals$race_name <- race_labels[as.character(race_totals$final_re)]

  p <- ggplot(race_totals, aes(x = year, y = total, color = race_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Race Over Time",
         x = "Year",
         y = "Count",
         color = "Race") +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

#' Plot OUD Counts by Sex
#'
#' Creates a line plot of OUD counts over time, broken down by sex.
#'
#' @param sex_data A \code{data.table} containing \code{year}, \code{N_ID}, and \code{final_sex} columns.
#'
#' @return A \code{ggplot} object visualizing counts by sex.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_y_continuous theme_minimal theme
#' @importFrom scales label_comma
#' @export
plot_sex_data <- function(sex_data) {
  sex_labels <- c("1" = "Male", "2" = "Female")
  sex_totals <- sex_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, final_sex)]
  sex_totals$sex_name <- sex_labels[as.character(sex_totals$final_sex)]

  p <- ggplot(sex_totals, aes(x = year, y = total, color = sex_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Sex Over Time",
         x = "Year",
         y = "Count",
         color = "Sex") +
    scale_y_continuous(labels = scales::label_comma()) +
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

#' Plot OUD Counts by Age Group
#'
#' Creates a line plot of OUD counts over time, broken down by age group.
#'
#' @param age_data A \code{data.table} containing \code{year}, \code{N_ID}, and \code{age_grp_twenty} columns.
#'
#' @return A \code{ggplot} object visualizing counts by age group.
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs scale_y_continuous theme_minimal theme
#' @importFrom scales label_comma
#' @export
plot_age_data <- function(age_data) {
  age_labels <- c("1" = "0-19 years", "2" = "20-39 years", "3" = "40-59 years",
                  "4" = "60-79 years", "5" = "80+ years")
  age_totals <- age_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, age_grp_twenty)]
  age_totals$age_name <- age_labels[as.character(age_totals$age_grp_twenty)]

  p <- ggplot(age_totals, aes(x = year, y = total, color = age_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Age Group Over Time",
         x = "Year",
         y = "Count",
         color = "Age Group") +
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
