library(dplyr)
library(ggplot2)

compare_moud_counts <- function(old_path, new_path) {
  old_df <- read.csv(old_path, stringsAsFactors = FALSE)
  old_df <- mutate(old_df, version = "Old")

  new_df <- read.csv(new_path, stringsAsFactors = FALSE)
  new_df <- mutate(new_df, version = "New")

  combined <- bind_rows(old_df, new_df)
  combined <- mutate(combined, date = as.Date(paste(year, month, "01", sep = "-")))

  ggplot(combined, aes(x = date, y = N_ID, color = version, linetype = version)) +
    geom_line(size = 1) +
    facet_wrap(~ treatment, scales = "free_y") +
    labs(
      title = "Monthly MOUD Counts by Treatment Type: Old vs New Dataset",
      x = "Date",
      y = "MOUD Count",
      color = "Version",
      linetype = "Version"
    ) +
    scale_color_manual(values = c("Old" = "grey", "New" = "purple")) +
    theme_minimal() +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_moud_differences <- function(old_path, new_path) {
  old_df <- read.csv(old_path, stringsAsFactors = FALSE)
  old_df <- dplyr::select(old_df, treatment, month, year, N_ID)
  old_df <- rename(old_df, old_count = N_ID)

  new_df <- read.csv(new_path, stringsAsFactors = FALSE)
  new_df <- dplyr::select(new_df, treatment, month, year, N_ID)
  new_df <- rename(new_df, new_count = N_ID)

  diff_df <- left_join(old_df, new_df, by = c("treatment", "month", "year"))
  diff_df <- mutate(
    diff_df,
    difference = new_count - old_count,
    date = as.Date(paste(year, month, "01", sep = "-"))
  )

  ggplot(diff_df, aes(x = date, y = difference, fill = difference > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ treatment, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "firebrick")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Change in MOUD Counts by Treatment Type: New – Old",
      x = "Date",
      y = "Difference in MOUD Count (New – Old)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}
