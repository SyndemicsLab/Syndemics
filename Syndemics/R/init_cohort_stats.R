#' Get descriptive initial cohort statistics
#' @description This function gets descriptive statistics of the initial cohort
#' for HEP-CE projects. It does this by connecting to an inputs.db file
#' using SQLite, looking at the init_cohort table, and pulling
#' descriptive statistics (average age, proportion male, drug behavior
#' distribution, fibrosis state distribution, etc).
#' @param db_path Path where the inputs.db file is saved
#' @param table_name Name of the initial cohort table in the
#' input.db file (e.g., init_cohort, population)
#' @return A table with descriptive statistics of the initial cohort
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl summarize group_by mutate select
#' bind_rows collect
#' @importFrom purrr map_dfr
#' @export
get_init_cohort_statistics <- function(db_path, table_name) {
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)

    cohort <- tbl(con, table_name)

    age_stats <- cohort |>
        summarize(value = mean(age_months, na.rm = TRUE) / 12) |>
        mutate(
            variable = "age",
            level = "mean_years",
            statistic = "mean"
        ) |>
        as.data.frame()

    var_names <- c(
        "gender",
        "drug_behavior",
        "fibrosis_state",
        "identified_as_hcv_positive",
        "link_state"
    )

    init_cohort_table <-
        map_dfr(var_names, prop_breakdown, cohort = cohort) |>
        select(variable, level, statistic, value) |>
        collect() |>
        bind_rows(age_stats)

    return(init_cohort_table)
}

#' Break down the initial cohort into proportions
#' @description This function calculates the proportion breakdown of a given
#' variable in the initial cohort.
#' @param var_name The name of the variable for which to calculate the
#' proportion breakdown.
#' @param cohort The initial cohort data frame (as a dplyr table) from which to
#' calculate the proportion breakdown.
#' @return A table with the proportion breakdown of the specified value.
#' @importFrom dplyr group_by summarize mutate transmute
#' @keywords internal
prop_breakdown <- function(var_name, cohort) {
    prop_breakdown <- cohort |>
        group_by(.data[[var_name]]) |>
        summarize(n = n(na.rm = TRUE), .groups = "drop") |>
        mutate(
            value = as.double(n) / sum(n),
            variable = var_name,
            level = as.character(.data[[var_name]]),
            statistic = "proportion",
            .keep = "none"
        ) |>
        as.data.frame()
    return(prop_breakdown)
}
