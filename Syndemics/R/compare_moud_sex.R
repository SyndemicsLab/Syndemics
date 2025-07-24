compare_moud_sex <- function(old_path, new_path) {
  N_ID <- treatment <- new_count <- old_count <- difference <- final_sex <- NULL

  # read and label the datasets
  old_df <- read.csv(old_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(version = "Old")

  new_df <- read.csv(new_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(version = "New")

  # combine datasets and add date
  combined <- dplyr::bind_rows(old_df, new_df) %>%
    dplyr::mutate(date = as.Date(paste(year, month, "01", sep = "-")))

  # plot 1: time series of MOUD counts by treatment and sex
  count_plot <- ggplot2::ggplot(combined, ggplot2::aes(x = date, y = N_ID, color = version, linetype = version)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~ treatment + final_sex, scales = "free_y") +
    ggplot2::labs(
      title = "Monthly MOUD Counts by Treatment Type and Sex: Old vs New Dataset",
      x = "Date",
      y = "MOUD Count",
      color = "Version",
      linetype = "Version"
    ) +
    ggplot2::scale_color_manual(values = c("Old" = "grey", "New" = "purple")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  # prepare difference dataset
  old_df2 <- read.csv(old_path, stringsAsFactors = FALSE) %>%
    dplyr::select(treatment, month, year, final_sex, N_ID) %>%
    dplyr::rename(old_count = N_ID)

  new_df2 <- read.csv(new_path, stringsAsFactors = FALSE) %>%
    dplyr::select(treatment, month, year, final_sex, N_ID) %>%
    dplyr::rename(new_count = N_ID)

  diff_df <- dplyr::left_join(old_df2, new_df2, by = c("treatment", "month", "year", "final_sex")) %>%
    dplyr::mutate(
      difference = new_count - old_count,
      date = as.Date(paste(year, month, "01", sep = "-"))
    )

  # plot 2: difference in MOUD counts by treatment and sex
  difference_plot <- ggplot2::ggplot(diff_df, ggplot2::aes(x = date, y = difference, fill = difference > 0)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::facet_wrap(~ treatment + final_sex, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c("TRUE" = "darkgreen", "FALSE" = "firebrick")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Change in MOUD Counts by Treatment Type and Sex: New vs Old",
      x = "Date",
      y = "Difference in MOUD Count (New vs Old)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  return(list(count_plot = count_plot, difference_plot = difference_plot, difference_data = diff_df))
}
