
# Syndemics

  <!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/Syndemics)](https://CRAN.R-project.org/package=Syndemics)
  ![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Syndemics)
  [![R-CMD-check](https://github.com/SyndemicsLab/Syndemics/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/SyndemicsLab/Syndemics/actions/workflows/R-CMD-check.yaml)
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->

The goal of Syndemics is to provide R support and ease-of-access when working with projects using models developed at the [Syndemics Lab](https://www.syndemicslab.org/). The current implementation supports modeling unknown patient populations through capture-recapture and provides some still-under-development interactions with the Researching Effective Strategies to Prevent Opioid Death [(RESPOND) model](https://www.syndemicslab.org/respond).


## Installation

You can install the development version of Syndemics from [GitHub](https://github.com/SyndemicsLab/Syndemics) with:

``` r
# install.packages("devtools")
devtools::install_github("SyndemicsLab/Syndemics", subdir = "Syndemics")
```

## Capture-Recapture for the Massachusetts Opioid Population
The idea behind capture-recapture is to treat the Massachusetts Public Health Data Warehouse, a longitudinally linked database, as a series of different 'captures' across several different databases.

``` r
library(Syndemics)
library(dplyr)
library(Syndemics)
library(doParallel)

results <- list()
unique_years <- unique(data$year)
n_cores <- detectCores() - 1

registerDoParallel(n_cores)
for(i in unique_years) {
  estimates <- foreach(j = 1:1000, .combine = 'rbind', .packages = c("dplyr", "Syndemics")) %dopar% {
    set.seed(j)
    
    output <- data %>% 
      mutate(N_ID = ifelse(N_ID == -1, round(runif(nrow(.), 1, 10)), N_ID)) %>%
      filter(year == i) %>% 
      select(-c(final_re, final_sex, age_grp_ten)) %>%
      unique()
    
    Syndemics::crc(output, "N_ID", c("Casemix", "APCD", "BSAS", "PMP", "Matris", "Death"),
                   formula.selection = "stepwise", method = "poisson", 
                   opts.stepwise = list(direction = "forward",
                                        verbose = FALSE))$estimate
  }
  results[[as.character(i)]] <- list(crcMean = mean(estimates), crcMinimum = min(estimates), crcMaximum = max(estimates))
}
stopImplicitCluster()

output <- do.call(rbind, results) %>% 
  as.data.frame() %>%
  tibble::rownames_to_column(var = "year") %>%
  mutate(year = as.integer(year)) %>%
  left_join(., data %>% 
              select(-c(final_re, final_sex, age_grp_ten)) %>%
              unique() %>%
              group_by(year) %>%
              summarise(., known = sum(N_ID))
            ) %>% 
  rowwise() %>%
  mutate(estimate = crcMean + known,
         estimateMin = crcMinimum + known,
         estimateMax = crcMaximum + known)
```

