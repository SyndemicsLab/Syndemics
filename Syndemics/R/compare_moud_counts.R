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

