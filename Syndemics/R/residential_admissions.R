#' Process TEDS-A data for Massachusetts by year
#'
#' Loads and processes the TEDS-A dataset for a specified year, filtering for Massachusetts residents
#' and specific service, substance use, and demographic criteria. Returns the average number of admissions per month.
#'
#' @param year Integer. The year of the TEDS-A dataset (e.g., 2022).
#' @param data_path Character. Path to the directory containing the .rdata files.
#'
#' @return Numeric. Average number of filtered admissions per month.
#' @export
#'
#' @examples
#' \dontrun{
#'   monthly_avg <- process_teds_year(2022, "C:/Users/laczoty/Desktop")
#'   print(monthly_avg)
#' }
process_teds_year <- function(year, data_path) {
  # Load necessary library
  library(tidyverse)
  
  # Construct file path and object name
  file_name <- paste0("tedsa_puf_", year, "_R.rdata")
  full_path <- file.path(data_path, file_name)
  object_name <- paste0("TEDSA_PUF_", year)
  
  # Load data
  load(full_path)
  
  # Convert to tibble
  teds <- as_tibble(get(object_name))
  
  # Filter for Massachusetts
  teds <- teds %>%
    filter(STFIPS == 25)
  
  # Define strata filters (currently commented for customization)
  teds_strat <- teds %>%
    filter(
      # GENDER == 2,
      # RACE == 5 & ETHNIC == 4,
      # RACE %in% 4:5 & ETHNIC %in% c(1, 2, 3, 5),
      # AGE %in% 8:11
      # AGE == 12
      TRUE  # placeholder to ensure code runs if filters are commented out
    )
  
  # Final filtering for service type, opioids, and IDU status
  teds_final <- teds_strat %>%
    filter(
      SERVICES %in% c(4, 5),
      (SUB1 %in% 5:7 | SUB2 %in% 5:7),
      IDU == 1
    )
  
  # Return monthly admissions number
  return(nrow(teds_final) / 12)
}


