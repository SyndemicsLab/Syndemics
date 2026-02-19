#' @description This function connects to an inputs.db file using SQLite,  looks
#' at the init_cohort table, and pulls descriptive statistics (average age,
#' proportion male, drug behavior distribution, fibrosis state
#' distribution, etc)
#' @param db_path Path where the inputs.db file is saved
#' @return A table with descriptive statistics of the initial cohort
#'
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl summarise group_by mutate transmute select
#' bind_rows collect
#' @export
get_init_cohort_statistics <- function(db_path) {
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con), add = TRUE)

    cohort <- tbl(con, "init_cohort")

    age_stats <- cohort |>
        summarise(value = mean(age_months, na.rm = TRUE) / 12) |>
        mutate(
            variable = "age",
            level = "mean_years",
            statistic = "mean"
        )

    prop_breakdown <- function(var_name) {
        prop_breakdown <- cohort |>
            group_by(.data[[var_name]]) |>
            summarise(n = n(), .groups = "drop") |>
            mutate(value = n / sum(n)) |>
            transmute(
                variable = var_name,
                level = as.character(.data[[var_name]]),
                statistic = "proportion",
                value
            )
        return(prop_breakdown)
    }

    sex_stats <- prop_breakdown("gender")
    drug_behavior_stats <- prop_breakdown("drug_behavior")
    fibrosis_stats <- prop_breakdown("fibrosis_state")
    hcv_id_stats <- prop_breakdown("identified_as_hcv_positive")
    link_state_stats <- prop_breakdown("link_state")

    init_cohort_table <- bind_rows(
        age_stats,
        sex_stats,
        drug_behavior_stats,
        fibrosis_stats,
        hcv_id_stats,
        link_state_stats
    ) |>
        select(variable, level, statistic, value) |>
        collect()

    return(init_cohort_table)
}
