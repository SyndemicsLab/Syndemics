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
#' @export

build_background_mortality_file <- function(files,
                                            outputfile,
                                            races = c("black", "hispanic", "white"),
                                            sexes = c("female", "male"),
                                            age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100"),
                                            bin_size = 20) {
    background_mortality <- lapply(files, extract_background_mortality)
    base_table <- build_base_table()
    return(fill_base_table(base_table, background_mortality))
}

#' Function used to extract background mortality values based on age from a single yearly CDC NVSS life table
#'
#' Input: CDC Life Tables
#' Output: data.table containing the weekly probabilities
#'
#' @param file_path The path to the CDC NVSS Life Table
#' @param bin_size The size ages are grouped by
#' @param age_groups A list of age groups
#'
#' @import data.table
#' @import readxl
#' @keywords internal

extract_background_mortality <- function(file_path, bin_size = 20, age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100")) {
    data <- read_excel(file_path, skip = 1)
    dt <- data.table::as.data.table(data)[(2:101)]
    setnames(dt, "Probability of dying between ages x and x + 1", "year_prob", skip_absent = TRUE)
    setnames(dt, "Number dying between ages x and x + 1", "year_deaths", skip_absent = TRUE)
    year_prob <- NULL
    dt <- dt[, year_prob := as.numeric(year_prob)]
    year_deaths <- NULL
    dt <- dt[, year_deaths := as.numeric(year_deaths)]
    deaths <- dt[, sum(year_deaths), by = (seq(nrow(dt)) - 1) %/% bin_size][, 2]
    weekly_rates <- (deaths / 100000) / 52
    weekly_probs <- 1 - exp(-weekly_rates)
    colname <- c("agegrp")
    weekly_probs[, (colname) := age_groups]
    return(weekly_probs)
}

#' Build the union data.table of various yearly extracted life probabilities
#'
#' Input: List of data.tables containing the weekly probabilities and their stratifications
#' Output: Joined List of data.tables
#'
#' @param lt A list of data tables containing the weekly probabilities
#'
#' @import data.table
#' @keywords internal

build_union <- function(lt) {
    union <- rbindlist(lt)
    setnames(union, "V1", "weekly_probability", skip_absent = TRUE)
    return(union)
}

#' Build the base table to fill, stratified by age, sex, and race
#'
#' Input: Demographics to stratify
#' Output: Empty Shell table
#'
#' @param races A list of races
#' @param sexes A list of sexes
#' @param age_groups A list of age groups
#'
#' @import data.table
#' @importFrom tidyr crossing
#' @keywords internal

build_base_table <- function(races = c("black", "hispanic", "white"),
                             sexes = c("female", "male"),
                             age_groups = c("1_20", "21_40", "41_60", "61_80", "81_100")) {
    crosses <- tidyr::crossing(races, sexes)
    full_table <- as.data.table(crosses[rep(seq_len(nrow(crosses)), length(age_groups)), ])
    full_table <- full_table[order(races, sexes)]
    return(full_table)
}

#' Fill the shell table with values extracted
#'
#' Input: Empty shell table and extracted background mortality
#' Output: a full data.table containing the extracted mortalities
#'
#' @param base_table The empty shell table
#' @param background_mortality The extracted background mortality
#' @param races A list of races
#' @param sexes A list of sexes
#'
#' @import data.table
#' @import rlist
#' @keywords internal

fill_base_table <- function(base_table,
                            background_mortality,
                            races = c("black", "hispanic", "white"),
                            sexes = c("female", "male")) {
    unions <- list.cbind(lapply(
        split(
            background_mortality,
            ceiling(
                seq_along(background_mortality) / (length(races) * length(sexes))
            )
        ),
        build_union
    ))
    binded <- cbind(base_table, unions)
    return(binded[, !c("2.agegrp", "3.agegrp", "4.agegrp", "5.agegrp", "6.agegrp", "7.agegrp")])
}

# Should be the root folder that holds the CDC life tables
# dirs <- c("~/")
# files <- list.files(dirs, pattern = "\\.xlsx$", full.names = TRUE, recursive = TRUE)

# End name of the output file
# outputfile <- "background_mortality.csv"

# Run and Build the Resulting File
# complete_table <- build_background_mortality_file(files, outputfile)
# write.csv(complete_table, outputfile)
