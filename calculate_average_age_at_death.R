## Calculating average age at death in Scotland



# packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)


# download data -----------------------------------------------------------

## age at death is available from the 'Time series' data on NRS website
## as of 2023-07-18, data are available 1974-2021

deaths_url <- 
  "https://www.nrscotland.gov.uk/files//statistics/time-series/death-21/deaths-time-series-21-dt.3.xlsx"

temp_file <- tempfile()
download.file(url = deaths_url, destfile = temp_file, mode = "wb")  # mode = "wb" needed on Windows machine


# wrangle data ------------------------------------------------------------

## The data are structured as vertically stacked tables in the same spreadsheet;
## e.g. from rows 4-60 (not actual rows!) is data for both sexes, ages 1-60,
## from rows 63-120 is both sexes, age 61-120, then for females separately
## 121-180, and 181

deaths_by_age <- read_excel(path = temp_file, sheet = 1)

cell_ranges <- tribble(
  # cell ranges here include the table headers, as well as the 'NS' and 'All'
  # age rows, so I can test if I get the same numbers as reported when summing
  # across age
  ~sex, ~cell_range,
  "both", "A4:AW65",
  "both", "A67:AW122",
  "male", "A126:AW187",
  "male", "A189:AW242",
  "female", "A246:AW307",
  "female", "A309:AW364",
)

## load chunks of data from the above cell ranges, do some basic cleaning on
## them, and stack them vertically into a single tibble
deaths_by_age_raw <- pmap_dfr(
  .l = cell_ranges,
  .f = function(sex, cell_range) {
    table_chunk <-
      read_excel(path = temp_file, sheet = 1, range = cell_range, col_types = "text") %>%
      janitor::clean_names() %>%
      mutate(
        sex = sex,
        age = if_else(age=="-", "0", age)  # replace dash with 0 in age column
        )
  }
)

## helper function for formatting the data
pivoting_function <- function(x) {
  pivot_longer(
    data = x,
    cols = x1974:x2021,  # janitor adds x in front of numbers so they are valid column names
    names_to = "year",  # pivot year columns into rows
    values_to = "n",  # n stands for number of deaths
    names_transform = ~ as.integer(str_sub(.x, 2, 5)),  # transform years to integers
    values_transform = as.integer  # also transform n to integer
  )
}

deaths_by_age <-
  deaths_by_age_raw %>%
  filter(!age %in% c("All","NS")) %>%  # remove totals and age not specified
  pivoting_function %>%
  mutate(
    age = as.integer(age)
    ) %>%
  replace_na(replace = list(n=0L))

deaths_by_age_totals <-
  deaths_by_age_raw %>% 
  filter(age %in% c("All")) %>%
  pivoting_function

deaths_by_age_not_specified <-
  deaths_by_age_raw %>% 
  filter(age %in% c("NS")) %>%
  pivoting_function %>%
  replace_na(replace = list(n=0L)) %>%
  rename(n_not_specified = n)

## merge the data summed across age with the existing totals, and number of age not specified deaths
check_data <-
  deaths_by_age %>%
  group_by(year, sex) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  full_join(deaths_by_age_totals, by = c("sex","year")) %>%
  full_join(deaths_by_age_not_specified, by = c("sex","year"))

## show only rows where my sum is not the same as the total + age not specified deaths
## this should be an empty tibble, ebcause all rows summed correctl!
check_data %>%
  filter(n!=total+n_not_specified)


# compute average age at death --------------------------------------------

average_age_at_death <-
  deaths_by_age %>%
  group_by(year, sex) %>%
  summarise(
    age_at_death = weighted.mean(age, w = n),
    n = sum(n),
    .groups = "drop"
  )

write_csv(average_age_at_death, file = "./outputs/average_age_at_death_2021.csv")


# make plot ---------------------------------------------------------------

(fig_average_age_at_death <-
  ggplot(
    data = average_age_at_death,
    aes(x = year, y = age_at_death, colour = sex)
    # aes(x = year, y = age_at_death, linetype = sex)
    ) +
  geom_line(linewidth = rel(1.2)) +
  theme_minimal() +
  scale_colour_grey() +
  # scale_colour_viridis_d() +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8), minor_breaks = NULL) +
  labs(
    x = NULL, y = NULL,
    title = "Average age at death in Scotland, 1974-2021",
  ) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
    )
)

ggsave(
  plot = fig_average_age_at_death,
  path = "./outputs/",
  filename = "average_age_at_death_2021.pdf",
  dpi = 300,
  units = "cm",
  width = 15,
  height = 12,
  device = cairo_pdf,
  bg = "white"
)

ggsave(
  plot = fig_average_age_at_death,
  path = "./outputs/",
  filename = "average_age_at_death_2021.png",
  dpi = 300,
  units = "cm",
  width = 15,
  height = 12,
  bg = "white"
)


# proportion of age groups ------------------------------------------------

proportion_of_85_plus <-
  deaths_by_age %>%
  filter(age >= 85) %>%
  group_by(year, sex) %>%
  summarise(n_85plus = sum(n), .groups="drop") %>%
  left_join(average_age_at_death, by = c("year","sex")) %>%
  mutate(prop_85plus = n_85plus / n)
