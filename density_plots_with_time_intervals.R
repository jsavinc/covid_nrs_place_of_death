# a toy example for plotting density graphs from time data with a start and end
# date; ggplot doesn't understand time intervals natively, so I use seq.Date to
# enumerate days in the interval to use in the plot. A vectorized version of
# seq.Date is needed to do this efficiently and quickly!


library(tidyverse)
library(lubridate)
library(tictoc)

tic()  # start timing

example_n <- 5e5

example_tbl <- tibble(
  id = 1:example_n,
  date_start = sample(x = seq.Date(
    from = ymd("2020-01-01"),
    to = ymd("2020-12-31"),
    by = 1
    ), 
    size = example_n, 
    replace = TRUE
  ),
  date_end = date_start + ddays(sample(x=0:365, size = example_n, replace = TRUE))
)

vectorised_seq_date <- Vectorize(FUN = seq.Date)

example_tbl <-
  example_tbl %>%
  mutate(
    enumerated_days = vectorised_seq_date(from=date_start, to=date_end, by = 1)
    )

## check that the number of days matches the difference between the dates
example_tbl %>%
  mutate(
    n_days = as.integer(date_end - date_start, units = "days") + 1,
    n_days_enum = map_int(enumerated_days, length)
  ) %>%
  {stopifnot(all(.$n_days==.$n_days_enum))}

example_tbl %>%
  mutate(days = vectorised_seq_date(from=date_start, to=date_end, by = 1)) %>%
  unnest(days) %>%  # needed for ggplot, otherwise it complains about wrong data type
  ggplot(aes(x = days)) +
  geom_density()

# end timing
toc()
