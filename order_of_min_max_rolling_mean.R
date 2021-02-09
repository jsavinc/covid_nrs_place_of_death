## Script showing difference in how the order of min/max and rolling mean (moving average) operations produces different results
## To show the variation in historical weekly figures, we show min & max values over a period of several years for each week
## To produce a rolling mean of these values there are several options for the order in which these are computed:
## A) rolling mean over weekly min/max values (the min/max having been computed over the period of years for each week)
## B) Min/max over rolling average over weeks within each year - rolling mean is computed for each year separately, and then a min/max is computed over the years

library(tidyverse)
library(zoo)  # for rolling mean function

set.seed(42)
test_data <- tibble(year = rep(2015:2019, each = 4), week = rep(1:4, times = 5), n = as.integer(runif(n=20, min = 0, max = 100)))

approach_A <-
  test_data %>%
  group_by(week) %>%
  summarise(
    weekly_mean = mean(n),
    weekly_min = min(n), 
    weekly_max = max(n),
    .groups = "drop"
  ) %>%
  mutate(
    rolling_mean_A = zoo::rollmean(x=weekly_mean, k=2, fill=NA, align="right"),
    rolling_mean_min_A = zoo::rollmean(x=weekly_min, k=2, fill=NA, align="right"),
    rolling_mean_max_A = zoo::rollmean(x=weekly_max, k=2, fill=NA, align="right")
  )

approach_B <-
  test_data %>%
  group_by(year) %>%
  mutate(
    yearly_rolling_mean = zoo::rollmean(x=n, k=2, fill=NA, align="right")
  ) %>%
  ungroup %>%
  group_by(week) %>%
  summarise(
    rolling_mean_B = mean(yearly_rolling_mean),
    rolling_mean_min_B = min(yearly_rolling_mean), 
    rolling_mean_max_B = max(yearly_rolling_mean),
    .groups = "drop"
  )

approach_A
approach_B
## Note how the rolling mean is identical between the two, but approach A produces a wider range
## This is because the order in approach B is mean -> range
## Whereas in approach A it is range -> mean
