# Compiling statistics for a briefing report, Sept/Oct 2023
# Based on 2001/2011 census data


# packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(readxl)
library(ggrepel)


# load data ---------------------------------------------------------------


raw_living_alone_by_simd <-
  pmap_dfr(
    .l = tribble(
      ~census_year, ~path,
      2001L, "X:/R1743/Care in the Last Days of Life/Cleared outputs/2023-01-19 SIMD reanalysis excl ch residents/alone_by_simd_2001.csv",
      2011L, "X:/R1743/Care in the Last Days of Life/Cleared outputs/2023-01-19 SIMD reanalysis excl ch residents/alone_by_simd_2011.csv",
    ),
    .f = function(census_year, path) {
      read_csv(path) %>%
        mutate(census_year = census_year)
    }
  ) %>%
  mutate(
    simd_quintile = 
      coalesce(simd2004_quintile, simd2012_quintile) %>%  # take whichever one is not NA
      factor(., levels = 1:5, labels = c("1=most deprived", as.character(2:5)))
      )
  
raw_coresident_with_carer_by_simd <-
  pmap_dfr(
    .l = tribble(
      ~census_year, ~path,
      2001L, "X:/R1743/Care in the Last Days of Life/Cleared outputs/2023-01-19 SIMD reanalysis excl ch residents/informal_carer_by_simd_2001.csv",
      2011L, "X:/R1743/Care in the Last Days of Life/Cleared outputs/2023-01-19 SIMD reanalysis excl ch residents/informal_carer_by_simd_2011.csv",
    ),
    .f = function(census_year, path) {
      read_csv(path) %>%
        mutate(census_year = census_year)
    }
  ) %>%
  mutate(
    simd_quintile = 
      coalesce(simd2004_quintile, simd2012_quintile) %>%  # take whichever one is not NA
      factor(., levels = 1:5, labels = c("1=most deprived", as.character(2:5)))
      )

raw_partial_descriptives_alone_2011 <- 
  read_excel("X:/R1743/Care in the Last Days of Life/Cleared outputs/Output Cleared 20190118/2019-01_DeprivationPaper-for_release.xlsx", sheet = 2, range = "C9:G44")

# wrangle data ------------------------------------------------------------

living_alone_by_simd <-
  raw_living_alone_by_simd %>%
  mutate(across(matches("percent"), ~parse_number(.x)/100)) %>%
  (function(x) {
    bind_rows(
      x,
      x %>%
        filter(residence_type %in% c("Communal","Households")) %>%
        mutate(residence_type = "All linked to census") %>%
        group_by(residence_type, census_year, simd2004_quintile, simd2012_quintile, simd_quintile) %>%
        summarise(
          across(c(total_deaths, Alone_n, `Not alone_n`), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          Alone_percent_of_total_deaths = Alone_n / total_deaths,
          Not_alone_percent_of_total_deaths = `Not alone_n` / total_deaths
          ) %>%
        group_by(residence_type, census_year) %>%
        mutate(
          Alone_percent = Alone_n / sum(Alone_n),
          `Not alone_percent` = `Not alone_n` / sum(`Not alone_n`)
          )
    )
  })

coresident_with_carer_by_simd <-
  raw_coresident_with_carer_by_simd %>%
  mutate(across(matches("percent"), ~parse_number(.x)/100)) %>%
  (function(x) {
    bind_rows(
      x,
      x %>%
        filter(residence_type %in% c("Communal","Households")) %>%
        mutate(residence_type = "All linked to census") %>%
        group_by(residence_type, census_year, simd2004_quintile, simd2012_quintile, simd_quintile) %>%
        summarise(
          across(c(total_deaths, `Carer available_n`, `No carer_n`), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
        mutate(
          Carer_available_percent_of_total_deaths = `Carer available_n` / total_deaths,
          No_carer_percent_of_total_deaths = `No carer_n` / total_deaths
        ) %>%
        group_by(residence_type, census_year) %>%
        mutate(
          `Carer available_percent` = `Carer available_n` / sum(`Carer available_n`),
          `No carer_percent` = `No carer_n` / sum(`No carer_n`)
        )
    )
  })

living_alone <- 
  living_alone_by_simd %>%
  group_by(census_year, residence_type) %>%
  summarise(
    across(c(total_deaths, Alone_n, `Not alone_n`, Missing_n), sum),
    .groups = "drop"
  ) %>%
  mutate(
    prop_alone = Alone_n / total_deaths,
    prop_missing = Missing_n / total_deaths
  )

coresident_with_carer <-
  coresident_with_carer_by_simd %>%
  group_by(census_year, residence_type) %>%
  summarise(
    across(c(total_deaths, `Carer available_n`, `No carer_n`, Missing_n), sum),
    .groups = "drop"
  ) %>%
  mutate(
    prop_alone = `Carer available_n` / total_deaths,
    prop_missing = Missing_n / total_deaths
  )


# wrangle data from previous reports --------------------------------------

# Note: AS reported slightly different figures over several publications; to
# keep numbers consistent, I'll calculate percentages from AS's data, and
# re-compute figures using my own totals

# Generally these are reported as totals for the entire population who died
# within a year of census and had a linked census record. This means both
# communal and household residence types are included.

denominators <-
  living_alone %>% 
  filter(residence_type %in% c("Communal", "Households")) %>% 
  select(census_year, residence_type, total_deaths) %>%
  (function (x) {
    bind_rows(
      x,
      x %>% mutate(residence_type = "All") %>% count(residence_type, census_year, wt = total_deaths, name = "total_deaths")
    )
  })

denominators_living_alone <-
  living_alone %>% 
  filter(residence_type %in% c("Households")) %>%
  select(census_year, living_alone = Alone_n)
  
denominators_living_with_carer <-
  coresident_with_carer %>% 
  filter(residence_type %in% c("Households")) %>%
  select(census_year, coresident_n = `Carer available_n`)
  

partial_descriptives_alone_2011 <-
  raw_partial_descriptives_alone_2011 %>%
  set_names(nm = c("variable","level","not_alone_prop","alone_prop","total")) %>%
  drop_na(level) %>% 
  fill(variable, .direction = "down") %>%
  filter(!str_starts(variable, "SIMD")) %>%  # remove the SIMD entries, they are SIMD x tenure
  mutate(across(where(is.character), ~str_replace_all(.x, pattern = "\\\r\\\n", replacement = " "))) %>%
  select(-not_alone_prop) %>%  # this is a category of all 
  mutate(
    census_year = 2011L,
    residence_type = "All",
    alone_prop = as.numeric(alone_prop),
    total = as.integer(total),
    alone_n = as.integer(round(alone_prop * total))
  ) %>%
  group_by(
    variable
  ) %>%
  mutate(alone_prop = alone_n / sum(alone_n)) %>%
  ungroup %>%
  left_join(denominators_living_alone, by = join_by(census_year)) %>%
  mutate(alone_n_recomputed = as.integer(round(alone_prop * living_alone))) %>%
  select(residence_type, census_year, alone_n_recomputed, alone_prop)
  
  


# figures - living alone --------------------------------------------------


## Line graph for changes in number of living alone by SIMD, with year on x-axis
## This doesn't get the point across so well, needs proportion also

(
  fig_living_alone_by_simd_n_lines <-
    living_alone_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = census_year, y = Alone_n, colour = simd_quintile, shape = simd_quintile
    )) +
    geom_line(linewidth = rel(1.2)) +
    geom_point(size = rel(3)) +
    theme_minimal() +
    scale_x_continuous(breaks = c(2001, 2011), labels = c(2001, 2011)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::comma,
      limits = c(0, NA)
    ) +
    scale_colour_viridis_d() +
    theme(legend.title = element_blank()) +
    labs(title = "Number of people living alone",
         x = NULL,
         y = NULL)
)

(
  fig_living_alone_by_simd_prop_lines <-
    living_alone_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = census_year, y = Alone_percent, colour = simd_quintile, shape = simd_quintile
    )) +
    geom_line(linewidth = rel(1.2)) +
    geom_point(size = rel(3)) +
    theme_minimal() +
    scale_x_continuous(breaks = c(2001, 2011), labels = c(2001, 2011)) +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, NA)
    ) +
    scale_colour_viridis_d() +
    theme(legend.title = element_blank()) +
    labs(title = "Percentage of people living alone",
         x = NULL,
         y = NULL)
)

(
  fig_living_alone_by_simd_lines <-
    wrap_plots(
      fig_living_alone_by_simd_n_lines,
      fig_living_alone_by_simd_prop_lines, 
      guides = "collect"
    ) &
    theme(legend.position = "top")
)


(
  fig_living_alone_by_simd_n_bars <-
    living_alone_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = as.integer(simd_quintile), y = Alone_n, fill = factor(census_year)
    )) +
    geom_col(position=position_dodge(width=0.7), width = 0.9, colour = "grey10") +
    theme_minimal() +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::comma,
      limits = c(0, NA)
    ) +
    theme(legend.title = element_blank()) +
    labs(title = "Number of people living alone",
         x = "SIMD quintile (1=most deprived)",
         y = NULL)
)

(
  fig_living_alone_by_simd_prop_bars <-
    living_alone_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = as.integer(simd_quintile), y = Alone_percent, fill = factor(census_year)
    )) +
    geom_text(
      aes(
        y = Alone_percent + 0.02,
        label = scales::label_percent(accuracy = 0.1)(Alone_percent)
      ),
      position = position_dodge(width = 0.9),
      angle = 45
    ) +
    geom_col(position=position_dodge(width=0.9), width = 0.9, colour = "grey10") +
    theme_minimal() +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, NA)
    ) +
    theme(legend.title = element_blank()) +
    labs(title = "Percentage of people living alone",
         x = "SIMD quintile (1=most deprived)",
         y = NULL)
)


(
  fig_living_alone_by_simd_bars <-
    wrap_plots(
      fig_living_alone_by_simd_n_bars,
      fig_living_alone_by_simd_prop_bars, 
      guides = "collect"
    ) &
    theme(legend.position = "top")
)


living_alone_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  select(census_year, residence_type, simd_quintile, total_deaths, Alone_n, Alone_percent)

living_alone_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  group_by(census_year) %>%
  summarise(Alone_n = sum(Alone_n))


living_alone_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  select(census_year, residence_type, simd_quintile, total_deaths, Alone_n, Alone_percent) %>%
  pivot_wider(names_from = census_year, values_from = c(total_deaths, Alone_n, Alone_percent))

living_alone_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  group_by(census_year, residence_type) %>%
  summarise(
    ratio_deaths_most_to_least_deprived = total_deaths[simd_quintile=="1=most deprived"]/total_deaths[simd_quintile=="5"],
    ratio_living_alone_most_to_least_deprived = Alone_n[simd_quintile=="1=most deprived"]/Alone_n[simd_quintile=="5"],
    .groups = "drop"
  )

# Approx. 400 fewer people were living alone in their last 12 months of life as
# of 2011 census compared to 2001

# The ranking by deprivation quintile didn't change over time - the majority of
# people who lived alone lived in the msot deprived quintile, folllowed by 2nd
# most, 3rd, etc.

# In absolute terms, deaths decreased in quintiles 1-4 betwen 2001 and 2011, and
# increased in quintile 5 (least deprived)

# In terms of percentage, quintile 1 (mos t deprived) remained stable between
# 2001 and 2011, with quintile 3 decreasing and quintiles 2,4,5 increasing;
# these are very small changes, <1% in most cases, with quintile 5 increasing by
# 1 percentage point between 2001 and 2011

# The ratio of people living alone in the most deprived divided by the least deprived quintile also decreased between 2001 and 2011 from 2.6 to 2.4.




# figures - living with unpaid carer --------------------------------------

(
  fig_coresident_with_carer_by_simd_n_bars <-
    coresident_with_carer_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = as.integer(simd_quintile), y = `Carer available_n`, fill = factor(census_year)
    )) +
    geom_col(position=position_dodge(width=0.9), width = 0.9, colour = "grey10") +
    theme_minimal() +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6),
      labels = scales::comma,
      limits = c(0, NA)
    ) +
    theme(legend.title = element_blank()) +
    labs(title = "Number",
         x = "SIMD quintile (1=most deprived)",
         y = NULL)
)

(
  fig_coresident_with_carer_by_simd_prop_bars <-
    coresident_with_carer_by_simd %>%
    filter(residence_type == "All linked to census") %>%
    ggplot(aes(
      x = as.integer(simd_quintile), y = `Carer available_percent`, fill = factor(census_year)
    )) +
    geom_col(position=position_dodge(width=0.9), width = 0.9, colour = "grey10") +
    geom_text(
      aes(
        y = `Carer available_percent` + 0.016,
        label = scales::label_percent(accuracy = 0.1)(`Carer available_percent`)
      ),
      position = position_dodge(width = 0.9),
      angle = 45
    ) +
    theme_minimal() +
    scale_y_continuous(
      breaks = scales::pretty_breaks(n = 6), 
      labels = scales::label_percent(accuracy = 1),
      limits = c(0, NA)
    ) +
    theme(legend.title = element_blank()) +
    labs(title = "Percentage",
         x = "SIMD quintile (1=most deprived)",
         y = NULL)
)


(
  fig_coresident_with_carer_by_simd_bars <-
    wrap_plots(
      fig_coresident_with_carer_by_simd_n_bars,
      fig_coresident_with_carer_by_simd_prop_bars, 
      guides = "collect"
    ) +
    plot_annotation(title = "People coresident with unpaid carer(s)") &
    theme(legend.position = "top")
)


coresident_with_carer_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  select(census_year, residence_type, simd_quintile, total_deaths, `Carer available_n`, `Carer available_percent`)

coresident_with_carer_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  select(census_year, residence_type, simd_quintile, total_deaths, `Carer available_n`, `Carer available_percent`) %>%
  pivot_wider(names_from = census_year, values_from = c(total_deaths, `Carer available_n`, `Carer available_percent`))

coresident_with_carer_by_simd %>%
  filter(residence_type == "All linked to census") %>%
  group_by(census_year, residence_type) %>%
  summarise(
    ratio_deaths_most_to_least_deprived = total_deaths[simd_quintile=="1=most deprived"]/total_deaths[simd_quintile=="5"],
    ratio_living_alone_most_to_least_deprived = `Carer available_n`[simd_quintile=="1=most deprived"]/`Carer available_n`[simd_quintile=="5"],
    .groups = "drop"
  )

## The number of people coresident with another person who reported providing
## unpaid care on the census increased across SIMDs between 2001 and 2011. The
## increase was small in SIMD quintile 1, and increased more in quintiles 2-5,
## especially in 4 and 5. The ranking swapped between quintiles 1 & 2, with
## quintile 2 representing the majority of people living with an unpaid carer



# living alone & with informal carer on same graph ------------------------

(
  fig_living_alone_or_coresident_with_carer_by_simd_bars <-
    wrap_plots(
      fig_living_alone_by_simd_prop_bars + 
        labs(title = "Living alone"),
      fig_coresident_with_carer_by_simd_prop_bars + 
        labs(title = "Living with unpaid carer"),
      guides = "collect"
    ) &
    theme(
      legend.position = "top", 
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      ) &
    plot_annotation(
      title = "Decedents by SIMD quintile",
      subtitle = "Decedents who died within 12 months of census date, by household composition & census year.\nPercentages represent the split of people with residential status across SIMD quintiles.\nIn 2011, SIMD 2 became the majority quintile for people living with unpaid carer."
      )
)

fig_living_alone_or_coresident_with_carer_by_simd_bars %>%
  ggsave(filename = "./outputs/policy_brief_oct2023/fig_living_alone_or_with_carer_by_simd_bar_chart_percent.png", dpi = 300, width = 10, height = 6, bg = "white")
