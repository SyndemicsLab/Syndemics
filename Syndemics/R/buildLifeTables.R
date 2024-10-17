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

capture_background_mortality <- function(file_path, bin_size = 20) {
    data <- read_excel(file_path, skip=1)
    dt <- data.table::as.data.table(data)[(2:101)]
    setnames(dt, "Probability of dying between ages x and x + 1", "year_prob")
    setnames(dt, "Number dying between ages x and x + 1", "year_deaths")
    dt <- dt[, year_prob:=as.numeric(year_prob)]
    dt <- dt[, year_deaths:=as.numeric(year_deaths)]
    deaths <- dt[, sum(year_deaths), by=(seq(nrow(dt))-1) %/% bin_size][,2]
    weekly_rates <- (deaths/100000)/52
    weekly_probs <- 1 - exp(-weekly_rates)
    age_groups <- c("1_20","21_40","41_60","61_80","81_100")
    colname <- c("agegrp")
    weekly_probs[, (colname) := age_groups]
    return(weekly_probs)
}

build_union <- function(lt) {
  union <- rbind(lt[[1]], lt[[2]], lt[[3]], lt[[4]], lt[[5]], lt[[6]])
  names(union)[names(union) == 'V1'] <- 'weekly_probability'
  return(union)
}

races <- c("black", "hispanic", "white")
sexes <- c("female", "male")
crosses <- tidyr::crossing(races, sexes)
full_table <- as.data.table(crosses[rep(seq_len(nrow(crosses)), 5), ])
full_table <- full_table[order(races, sexes)]

dirs <- c("/home/matt/Repos/TestData/RESPOND/life_tables")
files <- list.files(dirs, pattern="\\.xlsx$", full.names = TRUE, recursive=TRUE)
res <- lapply(files, capture_background_mortality)

probs <- list.cbind(lapply(split(res, ceiling(seq_along(res)/6)), build_union))
res <- cbind(full_table, probs)
res <- res[, !c("2.agegrp", "3.agegrp", "4.agegrp", "5.agegrp", "6.agegrp", "7.agegrp")]
write.csv(res, "background_mortality.csv")
