## Function to scrape the Scottish Government website for a list of public
## holiday dates
## Date: 2022-05-05

library(rvest)  # for scraping HTML
library(tidyverse)  # for pipes & tidy data

## dates are listed in HTML tables on the Scottish Government website:
url <- "https://www.mygov.scot/scotland-bank-holidays"


## using rvest we can produce a list of tables at the url above
## luckily, the only tables are those containing public holidays, so no
## filtering is needed
list_of_holidays_by_year <-
  read_html(url) %>%  # use rvest to read url
  html_table(header = TRUE)  # use rvest to read html tables

## Each of the list items is now a tibble, we only need the 1st (date) and 3rd
## column (holiday name).
## The year is the name of the first column
## Note that the second and third column names are mostly blank, which violates
## data frame/tibble namimng rules, and will cause further operations to fail
## so we need to change the names to something sensible first
## this could also be done with names.repair but I'm just using names() here

table_of_holidays <-  
  ## map_dfr will merge the outputs of the list into a single table
  map_dfr(
    .x = list_of_holidays_by_year,
    .f = function(this_tbl) {
      year <- names(this_tbl)[1]  # year is name of 1st col
      ## some columns have blank names and this makes tibble operations fail
      ## so we name the c9olumns manually first
      names(this_tbl) <- c("date", "weekday", "holiday")
      
      this_tbl %>%
        select(c(1,3)) %>%
        mutate(
          year = year,  # add a year column
          date = paste0(date, " ", year),  # add the year to the day & month
          ## %d is the numeric day (5 or 05)
          ## %B is the month name spelled out ("May")
          ## %Y is the numeric, 4-digit year that we add manually, e.g. 2022
          date = parse_date(date, format = "%d %B %Y")
        )
    }
  ) %>%
  arrange(date) %>%  # sort by date
  relocate(year)  # move year columnm to first place

## save holidays
write_csv(x = table_of_holidays, file = "./public_holidays_scotland.csv")
