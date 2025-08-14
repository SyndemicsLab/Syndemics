#' Taking in the CDC NVSS Yearly life tables, extract and build the background mortality table expected by RESPOND
#'
#' Input: CDC NVSS Life Tables, output file name, stratifications
#' Output: Resulting background mortality compatible with RESPOND
#'
#' @param files A set of files to extract the background mortality out of
#' @param outputfile The name of the file to output the background mortality
#' @param races A list of races
#' @param sexes A list of sexes
#' @param age_groups A list of age groups
#' @param bin_size The size ages are grouped by
#'
#' @import data.table
#' @importFrom utils write.csv
#' @export

build_background_mortality_file <- function(files,
                                            outputfile,
                                            races = c("black", "hispanic", "white"),
                                            sexes = c("female", "male"),
                                            age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100"),
                                            bin_size = 20) {

  background_mortality <- lapply(files, extract_background_mortality,
                                 bin_size = bin_size, age_groups = age_groups)
  result_table <- create_and_fill_table(background_mortality, races, sexes, age_groups)
  if (!missing(outputfile)) {
    write.csv(result_table, outputfile, row.names = FALSE)
  }

  return(result_table)
}

#' Function used to extract background mortality values based on age from a single yearly CDC NVSS life table
#'
#' @param file_path The path to the CDC NVSS Life Table
#' @param bin_size The size ages are grouped by
#' @param age_groups A list of age groups
#'
#' @import data.table
#' @importFrom readxl read_excel
#' @keywords internal

extract_background_mortality <- function(file_path, bin_size = 20, age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100")) {
  data <- readxl::read_excel(file_path, skip = 1)
  dt <- as.data.table(data)[(2:101)]

  # Rename columns to standard names
  setnames(dt, "Probability of dying between ages x and x + 1", "year_prob", skip_absent = TRUE)
  setnames(dt, "Number dying between ages x and x + 1", "year_deaths", skip_absent = TRUE)
  
  #Data table bindings
  year_prob <- year_deaths <- V1 <- NULL

  dt[, year_prob := as.numeric(year_prob)
     ][, year_deaths := as.numeric(year_deaths)]

  bin_groups <- (seq(nrow(dt)) - 1) %/% bin_size
  deaths_by_group <- dt[, sum(year_deaths), by = bin_groups][, V1]

  # 100k originates from the CDC NVSS data - reported in rates per 100,000 persons
  weekly_rates <- (deaths_by_group / 100000) / 52
  weekly_probs <- 1 - exp(-weekly_rates)

  result <- data.table(
    agegrp = age_groups,
    weekly_probability = weekly_probs
  )

  return(result)
}

#' Create and fill the table with mortality values for all demographic combinations
#'
#' @param background_mortality List of extracted background mortality data.tables
#' @param races A list of races
#' @param sexes A list of sexes
#' @param age_groups A list of age groups
#'
#' @import data.table
#' @keywords internal

create_and_fill_table <- function(background_mortality,
                                  races = c("black", "hispanic", "white"),
                                  sexes = c("female", "male"),
                                  age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100")) {
  #Data table bindings
  agegrp <- NULL
  
  combinations <- expand.grid(races = races, sexes = sexes, stringsAsFactors = FALSE)
  combinations <- as.data.table(combinations)
  result_table <- combinations[rep(seq_len(nrow(combinations)), each = length(age_groups))]
  result_table[, agegrp := rep(age_groups, times = nrow(combinations))]
  n_race_sex_combos <- length(races) * length(sexes)

  mortality_data <- data.table()
  for (i in seq_along(background_mortality)) {
    group_index <- ((i - 1) %% n_race_sex_combos) + 1
    bg_mort <- background_mortality[[i]]
    demo_info <- combinations[group_index]
    mortality_group <- cbind(demo_info[rep(1, nrow(bg_mort))], bg_mort)
    mortality_data <- rbind(mortality_data, mortality_group)
  }

  setorder(mortality_data, races, sexes, agegrp)

  return(mortality_data)
}
