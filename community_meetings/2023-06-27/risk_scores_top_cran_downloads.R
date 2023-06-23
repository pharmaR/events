
library(dplyr)
library(cranlogs)
library(purrr)

# identify last available day of data
date_avail <- cran_downloads("dplyr", "last-day") |> pull(date)

# test small batch
# few_pkgs <- c("dplyr", "tidyr")
pkgs_by_day <- cran_downloads(packages = few_pkgs,
      from = paste(date_avail - 364), to = paste(date_avail))

# for fun... see if it matches what I calculate
# top100 <- cran_top_downloads(when = 'last-month', count = 100)

# Get daily downloads for all pkgs from Rstudio CRAN Mirror for the last year
avail_pkgs <- available.packages("https://cran.rstudio.com/src/contrib")[,1]

all_pkgs_by_day <- map_dfr(avail_pkgs, ~ cran_downloads(packages = .x,
                           from = date_avail - 89, to = date_avail))

all_date_summary <- all_pkgs_by_day %>%
  group_by(package) %>%
  summarize(min_date = min(date),
            max_date = max(date),
            sum = sum(count, na.rm = TRUE))

length(unique(all_pkgs_by_day$package))

all_date_summary %>% nrow

all_date_summary %>%
  filter(sum != 0) %>%
  nrow

