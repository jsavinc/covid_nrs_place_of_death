## script to check overlap of SGUR classes within postcode area


# packages ----------------------------------------------------------------

library(tidyverse)

# load data ---------------------------------------------------------------

## using the SGUR 2016 file

if (!file.exists("./downloaded_data/00544930.csv")) {
  download.file(
    url = "https://www.gov.scot/binaries/content/documents/govscot/publications/advice-and-guidance/2018/03/scottish-government-urban-rural-classification-2016/documents/scottish-government-urban-rural-classification-2016-postcode-lookup/scottish-government-urban-rural-classification-2016-postcode-lookup/govscot%3Adocument/00544930.csv",
    destfile = "./downloaded_data/00544930.csv",
    mode = "wb"
  )
}

# Note: a small number of postcodes end with 3 letters (the 3rd is always an A)
# e.g. standard format is "AB1 1PK", some have "AB1 1PKA"

postcodes <-
  read_csv(file = "./downloaded_data/00544930.csv") %>%
  janitor::clean_names() %>%
  mutate(
    postcode_sector = str_remove(string = postcode, pattern = "[A-Z]{2,3}$"),
    postcode_district = str_remove(string = postcode, pattern = "\\s{0,1}\\d[A-Z]{2,3}$"),
    postcode_area = str_extract(string = postcode, pattern = "^[A-Z]{1,2}")
    )

## population by postcode

temp_file <- tempfile()
temp_dir <- tempdir()
download.file(url = "https://www.nrscotland.gov.uk/files/geography/2016-2/2016-2-pcindex-text-cut.zip",mode = "wb", destfile = temp_file)
unzip(zipfile = temp_file, overwrite = TRUE, exdir = temp_dir)
postcodes2 <-
  read_csv(file = file.path(temp_dir, "SmallUser.txt")) %>%
  janitor::clean_names() %>%
  mutate(postcode_area = str_extract(string = postcode, pattern = "^[A-Z]{1,2}"))
## check
# postcodes2 %>% select(matches("postcode")) %>% head

postcodes <-
  postcodes %>%
  left_join(
    postcodes2 %>% select(postcode, census_population_count2011), by = "postcode"
  )

# postcode area by SGUR ---------------------------------------------------

postcodes %>%
  group_by(postcode_area) %>%
  # summarise(n_distinct_sgur = n_distinct(urban_rural8fold2013_2014code)) %>%
  # summarise(n_distinct_sgur = n_distinct(ur8fold)) %>%
  summarise(n_distinct_sgur = n_distinct(ur3fold)) %>%
  arrange(desc(n_distinct_sgur))

## conclusion: postcode areas include up to 7 different classes of urban-rural
## area, so it's best to use the U-R indicator alongside postcode to match

# postcode district by SGUR -----------------------------------------------

postcodes %>%
  group_by(postcode_district) %>%
  # summarise(n_distinct_sgur = n_distinct(ur8fold)) %>%
  summarise(n_distinct_sgur = n_distinct(ur3fold)) %>%
  # summarise(n_distinct_sgur = n_distinct(urban_rural8fold2013_2014code)) %>%
  arrange(desc(n_distinct_sgur))

## postcode districts also contain up to 5 different urban-rural classes


# postcode sector by SGUR -------------------------------------------------

postcodes %>%
  group_by(postcode_sector) %>%
  summarise(n_distinct_sgur = n_distinct(ur8fold)) %>%
  # summarise(n_distinct_sgur = n_distinct(urban_rural8fold2013_2014code)) %>%
  arrange(desc(n_distinct_sgur))

## postcode sectors contain up to 4 U-R classes




# proportion of UR by postcode areas --------------------------------------

## Note: this uses population weighting per postcode, using the 2011 census to
## estimate the proportion of the population living by postcode area that is
## split into one or more Urban-rural classifications

proportion_table <- 
  map_dfr(
    .x = c("ur2fold", "ur3fold", "ur6fold", "ur8fold"),
    .f = function(ur_variable) {
      map_dfr(
        .x = c("postcode_area", "postcode_sector", "postcode_district"),
        .f = function(variable) {
          postcodes %>%
            group_by(!!rlang::sym(variable)) %>%
            summarise(
              n_freq = n_distinct(!!rlang::sym(ur_variable)),
              pop = sum(census_population_count2011, na.rm = TRUE)
              ) %>%
            count(n_freq, wt = pop) %>%
            mutate(
              ur = {{ur_variable}},
              area = {{variable}},
              percent = scales::label_percent(accuracy = 0.01)(n/sum(n))
            )
        }
      )
    }
  ) %>%
    relocate(ur, area, n_freq, n, percent)

proportion_table %>%
  write_csv(file = "./outputs/prop_pop_living_in_postcodes_with_multiple_UR_classifications.csv")

(fig_proportions_ur_postcode <-
  proportion_table %>% 
  mutate(
    prop = parse_number(percent)/100,
    ur = 
      ur %>% str_remove("^ur") %>% str_replace("(\\d)", "\\1-")
    ) %>%
  ggplot(data = ., aes(x = n_freq, y = prop)) +
    geom_col() +
    facet_grid(ur ~ area) +
    scale_x_continuous(breaks = 1:8) +
    scale_y_continuous(labels = scales::percent) +
    labs(
      x = "Urban-rural split", y = NULL,
      title = str_wrap("Proportion of population by postcode divisions and how many Urban-rural classes they map onto"),
      subtitle = str_wrap("All postcode areas map onto 2 U-R classes, so 100% of the population live in such areas.\n The majority of people live in postcode districts and sectors that map onto 2 classes, regardless of the classification."),
      caption = str_wrap("2016 Urban-rural classification was used alongside the 2016 NRS postcode directory. Population estimates are from the 2011 census.")
    )
)

ggsave(
  plot = fig_proportions_ur_postcode,
  filename = "./outputs/fig_prop_pop_living_in_postcodes_with_multiple_UR_classifications.png",
  dpi = 300,
  bg = "white",
  width = 8,
  height = 6
  )
