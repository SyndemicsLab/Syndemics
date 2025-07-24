library(data.table)

# combines all files in a folder into one data table
combine_files <- function(path) {
  files <- list.files(path, full.names = TRUE)
  rbindlist(
    lapply(files, function(f) {
      d <- fread(f)
      d[, source := basename(f)]  # adds column with file name
      d
    }),
    fill = TRUE
  )
}

# detects outliers in subgroups
detect_outliers <- function(df, value_var = "N_ID",
                            group_candidates = c("year","month","age_grp_twenty","final_sex","final_re")) {

  # uses only the grouping columns that are present in the data
  present_groups <- intersect(group_candidates, names(df))

  if (length(present_groups) == 0) {
    stop("no valid grouping columns")
  }

  # groups and summarizes
  summary_dt <- df[
    , .(
      mean_val = mean(get(value_var), na.rm = TRUE),
      sd_val   = sd(get(value_var), na.rm = TRUE),
      latest   = get(value_var)
    ),
    by = c(present_groups, "source")
  ]

  # flags outliers (any value more than 2 SD away from the group mean)
  summary_dt[, outlier := abs(latest - mean_val) > 2 * sd_val]

  return(summary_dt)
}

# examples
# loads all OUD files
big_df <- combine_files("~/Syndemics/Syndemics/Syndemics/OUDCountData")

# checks what columns you have
print(names(big_df))

# detect outliers
outliers <- detect_outliers(big_df)
# views the first few rows
head(outliers)

# filters only the rows flagged as outliers
flagged_outliers <- outliers[outlier == TRUE]
head(flagged_outliers)
