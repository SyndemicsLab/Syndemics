#' OUD Count Comparison and Visualization
#'
#' A collection of functions for analyzing and visualizing OUD count data across multiple files.
#' These functions support loading and combining datasets, filtering
#' by demographic indicators (race, sex, age), and creating plots.
#'
#' Functions included:
#' \describe{
#'   \item{\code{combine_files(path)}}{Reads and merges all files in a given directory, adding a \code{source_file} column.}
#'   \item{\code{time_trends(data)}}{Creates a line plot showing total OUD counts over time for each data source.}
#'   \item{\code{compare_by_year(data)}}{Generates a table comparing annual counts across files.}
#'   \item{\code{get_race_data(data)}}{Filters data for files with "Race" in the filename.}
#'   \item{\code{get_sex_data(data)}}{Filters data for files with "Sex" in the filename.}
#'   \item{\code{get_age_data(data)}}{Filters data for files with "Twenty" in the filename (age groups).}
#'   \item{\code{get_monthly_data(data)}}{Filters data for files with "Monthly" in the filename.}
#'   \item{\code{plot_race_data(race_data)}}{Plots total OUD counts by race over time.}
#'   \item{\code{plot_sex_data(sex_data)}}{Plots total OUD counts by sex over time.}
#'   \item{\code{plot_age_data(age_data)}}{Plots total OUD counts by age group over time.}
#'   \item{\code{histogram(data)}}{Creates a histogram showing the distribution of OUD counts.}
#' }
#'
#'
#' @section Dependencies: \code{data.table} and \code{ggplot2}
#'
#'
#' @export


# combine files into one big dataframe!
combine_files <- function(path) {
  files <- list.files(path, full.names = TRUE)
  rbindlist(
    lapply(files, function(f) {
      d <- fread(f)
      d[, source_file := basename(f)]  # adds column with file name
      d
    }),
    fill = TRUE
  )
}

# line chart showing trends over time
time_trends <- function(data) {
  yearly_totals <- data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, source_file)]

  p <- ggplot(yearly_totals, aes(x = year, y = total, color = source_file)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts Over Time",
         x = "Year",
         y = "Total Count",
         color = "Data Source") +
    scale_y_continuous(labels = scales::label_comma())
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

# compare different files side by side
compare_by_year <- function(data) {
  yearly_totals <- data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, source_file)]

  # make each file a column
  comparison_table <- dcast(yearly_totals, year ~ source_file, value.var = "total")

  print(comparison_table)
  return(comparison_table)
}

# filter data by file type
get_race_data <- function(data) {
  return(data[grepl("Race", source_file)])
}

get_sex_data <- function(data) {
  return(data[grepl("Sex", source_file)])
}

get_age_data <- function(data) {
  return(data[grepl("Twenty", source_file)])
}

get_monthly_data <- function(data) {
  return(data[grepl("Monthly", source_file)])
}

# plot race breakdown
plot_race_data <- function(race_data) {
  # readable race labels
  race_labels <- c("1" = "White", "2" = "Black", "3" = "Asian / Pacific Islander",
                   "4" = "Hispanic", "5" = "Native American / Other")

  # calculate totals by race and year
  race_totals <- race_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, final_re)]
  race_totals$race_name <- race_labels[as.character(race_totals$final_re)]

  # create plot
  p <- ggplot(race_totals, aes(x = year, y = total, color = race_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Race Over Time",
         x = "Year",
         y = "Count",
         color = "Race") +
    scale_y_continuous(labels = scales::label_comma())
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

# plot sex breakdown
plot_sex_data <- function(sex_data) {
  # readable sex labels
  sex_labels <- c("1" = "Male", "2" = "Female")

  # calculate totals by sex and year
  sex_totals <- sex_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, final_sex)]
  sex_totals$sex_name <- sex_labels[as.character(sex_totals$final_sex)]

  # create plot
  p <- ggplot(sex_totals, aes(x = year, y = total, color = sex_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Sex Over Time",
         x = "Year",
         y = "Count",
         color = "Sex") +
    scale_y_continuous(labels = scales::label_comma())
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

# plot age breakdown
plot_age_data <- function(age_data) {
  # readable age labels
  age_labels <- c("1" = "0-19 years", "2" = "20-39 years", "3" = "40-59 years",
                  "4" = "60-79 years", "5" = "80+ years")

  # calculate totals by age and year
  age_totals <- age_data[, .(total = sum(N_ID, na.rm = TRUE)), by = .(year, age_grp_twenty)]
  age_totals$age_name <- age_labels[as.character(age_totals$age_grp_twenty)]

  # create plot
  p <- ggplot(age_totals, aes(x = year, y = total, color = age_name)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(title = "OUD Counts by Age Group Over Time",
         x = "Year",
         y = "Count",
         color = "Age Group") +
    scale_y_continuous(labels = scales::label_comma())
    theme_minimal() +
    theme(legend.position = "bottom")

  print(p)
  return(p)
}

# histogram
histogram <- function(data) {
  p <- ggplot(data, aes(x = N_ID)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    labs(title = "Distribution of OUD Counts",
         x = "Count",
         y = "Frequency") +
    scale_y_continuous(labels = scales::label_comma())
    theme_minimal()

  print(p)
  return(p)
}
