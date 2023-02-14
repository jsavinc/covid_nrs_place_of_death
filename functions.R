# Functions for use in the analysis


compute_start_date_from_week_number <- function(week_number, year_number) {
  ## this assumes a valid week number! There are 52 or 53 weeks in a year
  isoweek_string <- glue::glue("{year_number}-W{str_pad(week_number, width=2, pad='0')}-1")
  # computed_dates <- ISOweek::ISOweek2date(isoweek_string)
  computed_dates <- rep(NA_Date_,times = length(week_number))
  computed_dates[which(!is.na(week_number))] <- ISOweek::ISOweek2date(isoweek_string[which(!is.na(week_number))])  # return NA instead of failing with ISOweek!
  ## invalid weeks will result in a computed date in a year after the requested year...
  ## a valid week can result in a date that starts in the year before!
  if (any(year_number<year(ymd(computed_dates)), na.rm = TRUE)) stop("invalid week number provided!")
  return(computed_dates)
}

# Place of death was named differently between datasets (location/place) and the different places were also named slightly differently, so we find all of them and recode them to the same:
order_of_place_of_death_levels <- c("Hospital", "Care home", "Home & other non-institution", "Other", "All")

recode_place_of_death <- function(data_tbl) {
  data_tbl %>%
    mutate(place_of_death = case_when(
      str_detect(place_of_death, pattern = regex(pattern = "all", ignore_case = TRUE)) ~ "All",
      str_detect(place_of_death, pattern = regex(pattern = "care.*home", ignore_case = TRUE)) ~ "Care home",
      str_detect(place_of_death, pattern = regex(pattern = "(non.*institut)|home", ignore_case = TRUE)) ~ "Home & other non-institution",
      str_detect(place_of_death, pattern = regex(pattern = "hospital", ignore_case = TRUE)) ~ "Hospital",
      str_detect(place_of_death, pattern = regex(pattern = "other", ignore_case = TRUE)) ~ "Other",
      TRUE ~ NA_character_
    ) %>%
      factor(x = ., levels = order_of_place_of_death_levels)
    )
}


## Consistent Cause of death coding
# The `opendatascot`-imported data distinguishes "all causes" deaths from "covid-related" deaths - the non-covid-related deaths are the difference between those. In contrast, the other weekly datasets code cause of death as either covid-related or non-covid-related, in which case we need to compute all cause deaths as the sum of the two.
# 
# We'll change the wording of these to a consistent format so it's easier to compute consistent columns. The below function will also compute non-covid deaths for data where all deaths and covid deaths are provided, and all deaths where covid- and non-covid-related deaths are provided.
# 
# This involves pivotting the number of deaths variable into wide format based on the cause of death. Because of this, we need to deal with implicit missing values:
#   
#   The first COVID-19-related deaths were reported from week 12 onwards. In some of the datasets, a missing/omitted value represents 0 cases in that category, when a 0 should have been recorded. E.g. deaths in "Other institutions" are very low, and if a record is missing for that category, we should assume 0 deaths.
# 
# On 3 Februrary 2021 as I was updating the script, the weekly figures started distinguishing cause of death: COVID-19 can be a contributory factor, or an underlying cause of death. For the purposes of this study, COVID-19 deaths are in the minority, and we will disregard this distinction for now.
# 
## helper function to make consistently coded cause of death
recode_cause_of_death <- function(data_tbl) {
  data_tbl %>%
    mutate(
      cause_of_death = case_when(
        cause_of_death == "all-causes" ~ "deaths_all_causes",
        cause_of_death == "all-causes-average-of-corresponding-week-over-previous-5-years" ~ "deaths_nrs_past_average_all_causes",
        cause_of_death == "covid-19-related" ~ "deaths_covid_related",
        cause_of_death == "COVID-19 mentioned" ~ "deaths_covid_related",
        cause_of_death == "COVID-19 contributory factor" ~ "deaths_covid_related",
        cause_of_death == "COVID-19 underlying cause" ~ "deaths_covid_related",
        cause_of_death == "Non-COVID-19" ~ "deaths_non_covid",
        TRUE ~ NA_character_
      )
    )
}

## calculate number of deaths after recoding cause of death -
## recoding results in two entries for covid-related deaths, so we need to sum over them
## this is achieved by grouping over all variables other than number of deaths
recalculate_number_of_deaths_after_recoding_cause_of_death <- function(data_tbl) {
  data_tbl %>%
    group_by_all %>% 
    ungroup(number_of_deaths) %>%  # this is done by grouping over everything except the number of deaths
    summarise(number_of_deaths = sum(number_of_deaths), .groups = "drop")  # recompute the sum of deaths
}

## covid death reporting started in week 12, 2020 - before then it doesn't make sense to distunguish between covid and non-covid deaths, and only the all cause death figure should be kept
## by using the 'overrun' weeks we ensure that 2021 or later isn't affected
## note: we're keeping all cause deaths!
set_covid_and_non_covid_deaths_to_zero_before_reporting_started <- function(data_tbl) {
  data_tbl %>%
    mutate(
      deaths_non_covid = if_else(
        condition = week_number_run_over < 12,
        true = NA_real_,
        false = deaths_non_covid
      ),
      deaths_covid_related = if_else(
        condition = week_number_run_over < 12,
        true = NA_real_,
        false = deaths_covid_related
      )
    )
}


## Consistent use of missing value
replace_na_with_0_from_week_12 <- function(data_tbl) {
  data_tbl %>%
    mutate(
      value = if_else(
        condition = week_number_roll_over >=12 & is.na(number_of_deaths),
        true = 0,
        false = number_of_deaths
      )
    )
}

## helper function that links the 9-character geography code to its name
make_human_readable_ref_area <- function(data_tbl, ref_area = refArea) {
  enquoted_ref_area <- enquo(ref_area)
  data_tbl %>%
    mutate(
      !!enquoted_ref_area := phsmethods::match_area(!!enquoted_ref_area)
    )
}


## helper function to determine number of isoweeks in a year
calculate_how_many_weeks_in_year <- function(year) {
  ## this is silly - the last day fo the year is sometimes in the next iso-year
  candidates <- paste0(year,"-12-",23:31)
  as.integer(max(lubridate::isoweek(candidates)))
}
## this is extremely daft but I couldn't work out how to neatly calculate the nubmer of iso weeks in a given year, so they're now hardcoded.
table_of_iso_weeks_in_year <- 
  2010:2040 %>%
  tibble(
    year = .,
    num_weeks = map_dbl(.x = ., calculate_how_many_weeks_in_year) 
  )

## helper function to compute 'week run-over'
## e.g. if we're counting from 2020, weeks in 2021 are 53+ and in 2022 105+
compute_week_run_over <-  Vectorize(
  function(week_number, year, start_year = 2020) {
  if (year == start_year) return(week_number)
  years_to_use <- seq.int(start_year, year - 1, by = 1)
  weeks_per_year <- table_of_iso_weeks_in_year$num_weeks[which(table_of_iso_weeks_in_year$year %in% years_to_use)]
  return(sum(weeks_per_year) + week_number)
  }
)

## helper function to parse year and week numbers from ad-hoc data
## I repeat this bit a lot!
parse_year_and_week_number_from_ad_hoc_data <- function(data_tbl) {
  data_tbl %>%
  mutate(
    year = 2000L + parse_integer(str_sub(week_number, 1, 2)),
    week_number = parse_integer(str_sub(week_number, 4, 5)),
    date_w_c = compute_start_date_from_week_number(week_number = week_number, year = year),
    week_number_run_over = compute_week_run_over(week_number, year, start_year = 2020)  # invent a "run-over" week number - weeks 54+ are in 2021, weeks 106+ are in 2022
  )
}


## helper table & function to prettify causes of death reported
causes_of_death_nrs <- tribble(
  ~raw,       ~pretty,
  "all_causes", "All causes",
  "cancer",   "Cancer", 
  "dementia_alzhemiers", "Dementia / Alzheimers",
  "dementia_alzheimers", "Dementia / Alzheimers",
  "circulatory", "Circulatory (heart disease and stroke)",
  "respiratory", "Respiratory" ,
  "covid_19",  "COVID-19",
  "other_causes", "Other"
)

return_pretty_cause_of_death <- function(raw_cause_of_death) {
  causes_of_death_nrs$pretty[match(x = raw_cause_of_death, table = causes_of_death_nrs$raw)]
}


## helper function to return the historic period for average number of death calculations
# TODO: at some point this will need updating!
historic_period_depending_on_year <- function(year) {
  case_when(
    year %in% 2020:2021 ~ "2015-2019",
    year == 2022 ~ "2016-2019, 2021",
    year == 2023 ~ "2017-2019, 2021-2022",
    year == 2024 ~ "2018-2019, 2021-2023",
    TRUE ~ NA_character_
  )
}

## helper function for generating sequences of years excluding 2020
## this is for use with computign historic periods
generate_5_years_historic_period_excl_2020 <- function(starting_year) {
  seq_years <- seq.int(from = starting_year, by = 1, length.out = 5)
  if (2020 %in% seq_years) {
    seq_years <- c(seq_years, tail(seq_years, 1) + 1L)
  }
  seq_years <- seq_years[seq_years!=2020]
  return(seq_years)
}

## helper function that creates week 53 data by taking week 52 in years with no year 53 and relabels it as 53
## CAUTION: only use this for computing averages! otherwise it's making up data that wasnae there
create_week_53_data_for_years_with_52_weeks <- function(data_tbl) {
  years_with_52_weeks <- table_of_iso_weeks_in_year %>% filter(num_weeks==52) %>% .$year
  
  real_data <-
    data_tbl %>%
    filter(!(year %in% years_with_52_weeks & week_number==53))  # remove any entries for week 53 in years with 52 weeks - in some datasets they existed but had 0 listed
  fake_data <-
    data_tbl %>%
    filter(year %in% years_with_52_weeks & week_number==52) %>%
    mutate(week_number=53)
  return(
    bind_rows(
      real_data,
      fake_data
    )
  )
}


## compute moving average over weekly data, using average over k weeks
## using historical data to compute the moving average in the first few weeks of current data
## Note: historical data needs to be the non-summarised (raw) data!
## to compute moving average in first few week(s) of year, we add in the previous years' data and code the weeks so that they retain the right order
## i.e. in 2020 the week_number_run_over starts at 1, and the last week of 2019 is coded as 0, the 2nd last as -1, etc.
## once the moving average has been computed, we remove the past data;
## this approach works for 2021 data seamlessly, using the last weeks of 2020 for the moving average in 2021!
compute_moving_average_for_current_data <- function(current_data, historical_data, num_weeks_to_average) {
  bind_rows(
    historical_data %>%  # add historical data and format it so that it matches current data
      filter(year == 2019) %>%  # keep only last year before 2020
      rename(deaths_all_causes = number_of_deaths) %>%  # in historical deaths there is no split between covid/non-covid so we rename to all cause deaths
      mutate(week_number_run_over = -(52-week_number)),
    current_data
  ) %>%
    arrange(week_number_run_over) %>%  # just in case the order got messed up, re-order by week_number_run_over for the computation
    group_by_all() %>% ungroup(matches("^deaths_|week|date|year|historic_period")) %>%  # clever way of getting grouping by ref_area, sex, age, whatever is available
    mutate(
      across(
        .cols = matches("^deaths_"), 
        .fns = ~zoo::rollmean(.x, k=num_weeks_to_average, fill=NA, align="right"),  # right align = for 4-week average, at week 2, we use weeks -1,0,1,2
        .names = "ma{num_weeks_to_average}w_{.col}"
      )
    ) %>%
    ungroup %>%
    filter(year > 2019)  # keep only current data
}

## silly function that takes the population estimate and computes the Scotland total from it, adding it as one of the entries
add_scotland_total_to_population_estimate <- function(data_tbl) {
  if ("hb" %in% names(data_tbl)) {  # figure out whether we're modifying LA or HB entry
    ref_area <- "hb"
  } else if ("la" %in% names(data_tbl)) {
    ref_area <- "la"
  }
  scotland_total <- sum(data_tbl$all_ages)
  new_row <- data_tbl[nrow(data_tbl),]
  new_row[1, ref_area] <- "Scotland"
  new_row$all_ages <- scotland_total
  bind_rows(data_tbl, new_row)
}

## compute y var from x, given a linear regression
compute_y_from_x_using_regression <- function(x, regression_model) {
  coef(regression_model)[1] + (x * coef(regression_model)[2])
}
## compute x var from y, given a linear regression
compute_x_from_y_using_regression <- function(y, regression_model) {
  (y - coef(regression_model)[1]) / coef(regression_model)[2]
}

## parse the date given in ref_period depending on format
## this assumes there is a ref_period variable in the data!
parse_ref_period = function(data_tbl) {
  if (!"ref_period" %in% names(data_tbl)) 
    stop("A 'ref_period' column not found in the data!")
  ## this assumes the entirety of ref_period has the same format
  ## I think it's unlikely for the data to come as a mix of the two formats
  if (str_starts(data_tbl[["ref_period"]][1], pattern = "w\\/c")) {
    ## continue with assumption that ref_period is given as w/c
    data_tbl %>%
      mutate(
        date_w_c = 
          ymd(str_extract(ref_period, pattern = "\\d{4}\\-\\d{1,2}\\-\\d{1,2}")),
        year = as.integer(str_sub(ISOweek::ISOweek(date_w_c), 1, 4)),
        week_number = lubridate::isoweek(date_w_c),
        week_number_run_over = compute_week_run_over(week_number = week_number, year = year, start_year = 2020)  # invent a "run-over" week number - weeks 54+ are in 2021, weeks 106+ are in 2022
      )
  }
  else {
    ## otherwise, ref_period is given in isoweek format: e.g. 2020-W01
    data_tbl %>%
      mutate(
        year = parse_integer(str_extract(ref_period, pattern="^(202[0-3])")),
        week_number = parse_integer(str_remove(ref_period, pattern = "^(202[0-3])\\-")),
        date_w_c = compute_start_date_from_week_number(week_number = week_number, year = year),
        number_of_deaths = as.numeric(number_of_deaths),  # convert to numeric!
        week_number_run_over = compute_week_run_over(week_number, year, start_year = 2020)  # invent a "run-over" week number - weeks 54+ are in 2021, weeks 106+ are in 2022
      )
  }
    
}

## helper function to infer how many years of Covid data to average from for
## historic figures, given current year covid data started in 2020: 2020 has no
## historic figures, in 2021 we take the weekly average from 2020, in 2022 we
## take from 2020-2021, and so on
infer_year_range_for_calculating_covid_deaths <- function(year) {
  year_range <- seq.int(from = year-5L, to = year-1L)
  year_range <- year_range[year_range>=2020]
  return(year_range)
}
