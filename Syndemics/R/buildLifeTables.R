#' Build RESPOND background_mortality Inputs From CDC Yearly US Life Tables
#'
#' Input: CDC Life Tables
#' Output: background_mortality.csv for the corresponding year
#'
#' @import data.table
#' @import readxl
#' @import rlist
#' @import tidyverse
#' @export

extract_background_mortality <- function(file_path, bin_size = 20, age_groups = c("1_20","21_40","41_60","61_80","81_100")) {
    data <- read_excel(file_path, skip=1)
    dt <- data.table::as.data.table(data)[(2:101)]
    setnames(dt, "Probability of dying between ages x and x + 1", "year_prob")
    setnames(dt, "Number dying between ages x and x + 1", "year_deaths")
    dt <- dt[, year_prob:=as.numeric(year_prob)]
    dt <- dt[, year_deaths:=as.numeric(year_deaths)]
    deaths <- dt[, sum(year_deaths), by=(seq(nrow(dt))-1) %/% bin_size][,2]
    weekly_rates <- (deaths/100000)/52
    weekly_probs <- 1 - exp(-weekly_rates)
    colname <- c("agegrp")
    weekly_probs[, (colname) := age_groups]
    return(weekly_probs)
}

build_union <- function(lt) {
  union <- rbindlist(lt)
  setnames(union, 'V1', 'weekly_probability', skip_absent=TRUE)
  return(union)
}

build_base_table <- function(races = c("black", "hispanic", "white"),
                             sexes = c("female", "male"),
                             age_groups = c("1_20","21_40","41_60","61_80","81_100")){
  crosses <- tidyr::crossing(races, sexes)
  full_table <- as.data.table(crosses[rep(seq_len(nrow(crosses)), length(age_groups)), ])
  full_table <- full_table[order(races, sexes)]
  return(full_table)
}

fill_base_table <- function(base_table,
                            background_mortality,
                            races = c("black", "hispanic", "white"),
                            sexes = c("female", "male")){
  unions <- list.cbind(lapply(
    split(background_mortality,
          ceiling(
            seq_along(background_mortality)/(length(races)*length(sexes)))
          ),
    build_union))
  binded <- cbind(base_table, unions)
  return(binded[, !c("2.agegrp", "3.agegrp", "4.agegrp", "5.agegrp", "6.agegrp", "7.agegrp")])
}

build_background_mortality_file <-function(files,
                                           outputfile,
                                           races = c("black", "hispanic", "white"),
                                           sexes = c("female", "male"),
                                           age_groups = c("1_20","21_40","41_60","61_80","81_100"),
                                           bin_size = 20){
  background_mortality <- lapply(files, extract_background_mortality)
  base_table <- build_base_table()
  return(fill_base_table(base_table, background_mortality))
}

# Should be the root folder that holds the CDC life tables
dirs <- c("~/")
files <- list.files(dirs, pattern="\\.xlsx$", full.names = TRUE, recursive=TRUE)

# End name of the output file
outputfile <- "background_mortality.csv"

# Run and Build the Resulting File
complete_table <- build_background_mortality_file(files)
write.csv(complete_table, outputfile)
