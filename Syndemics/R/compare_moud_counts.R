library(dplyr)
library(ggplot2)
library(readr)

compare_moud_counts <- function(old_path, new_path) {
  old_df <- read_csv(old_path, show_col_types = FALSE) |>
    mutate(version = "Old")

  new_df <- read_csv(new_path, show_col_types = FALSE) |>
    mutate(version = "New")

  combined <- bind_rows(old_df, new_df) |>
    mutate(date = as.Date(paste(year, month, "01", sep = "-")))

  ggplot(combined, aes(x = date, y = N_ID, color = version, linetype = version)) +
    geom_line(size = 1) +
    facet_wrap(~ treatment, scales = "free_y") +
    labs(
      title = "Monthly MOUD Counts by Treatment Type (Old vs New)",
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

compare_moud_counts(
  "~/Syndemics/Syndemics/Syndemics/MOUD Data/MOUDCount_03JUN2025.csv",
  "~/Syndemics/Syndemics/Syndemics/MOUD Data/MOUDCount_23JUN2025.csv"
)

plot_moud_differences <- function(old_path, new_path) {
  old_df <- read_csv(old_path, show_col_types = FALSE) |>
    select(treatment, month, year, N_ID) |>
    rename(old_count = N_ID)

  new_df <- read_csv(new_path, show_col_types = FALSE) |>
    select(treatment, month, year, N_ID) |>
    rename(new_count = N_ID)

  diff_df <- left_join(old_df, new_df, by = c("treatment", "month", "year")) |>
    mutate(
      difference = new_count - old_count,
      date = as.Date(paste(year, month, "01", sep = "-"))
    )

  ggplot(diff_df, aes(x = date, y = difference, fill = difference > 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ treatment, scales = "free_y") +
    scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "firebrick")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "Change in MOUD Counts Over Time (New - Old)",
      x = "Date",
      y = "Difference in Count"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

plot_moud_differences(
  "~/Syndemics/Syndemics/Syndemics/MOUD Data/MOUDCount_03JUN2025.csv",
  "~/Syndemics/Syndemics/Syndemics/MOUD Data/MOUDCount_23JUN2025.csv"
)
