
# Overview ----------------------------------------------------------------

## This function loads a table containing the filenames & urls of 
## data sources, and then checks if newer data is available, downloading it in
## the process. Once that's done, it will render one or more .Rmd reports using
## the latest data.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(curl)
library(knitr)
library(rmarkdown)

# Load table keeping track of latest data ---------------------------------

file_data_sources <- "./table_of_data_sources.csv"
download_dir <- "./downloaded_data"
if (!dir.exists(download_dir)) dir.create(download_dir)  # create download dir if it doesn't exist

if (file.exists(file_data_sources)) {
  table_of_data_sources <- read_csv(file = "./table_of_data_sources.csv")
} else {
  table_of_data_sources <- tibble(
    short_name = c("sex_age","la","hb"),
    url = c(
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-age-sex.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-health-board-location.xlsx"
    )
  ) %>% mutate(
    filename = basename(url),
    last_modified = ymd_hms("2020-01-01_01:01:01"),  # start with a date in the past
    path_latest = NA_character_  
  )
}

# Function to download latest data if available ---------------------------

download_file_if_newer_available <- function(url, filename, last_modified, directory) {
  todays_file <- curl_fetch_disk(url, path = tempfile())
  if (last_modified < todays_file$modified) {
    date_string <- strftime(todays_file$modified, format = "%F")  # prefix filename with last modified date
    ## copy file to folder
    filename_prefixed <- paste0(date_string,"_",filename)
    file_path <- file.path(directory, filename_prefixed)
    file.copy(from = todays_file$content, to = file_path, overwrite = TRUE)
    ## set last_modified date & save
    table_of_data_sources$last_modified[table_of_data_sources$filename == filename] <<- todays_file$modified
    table_of_data_sources$path_latest[table_of_data_sources$filename == filename] <<- file_path
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Run the data updating function ------------------------------------------

purrr::walk(
  .x = 1:nrow(table_of_data_sources),
  .f = function(x) {
    download_file_if_newer_available(
      url = table_of_data_sources$url[x],
      filename = table_of_data_sources$filename[x],
      last_modified = table_of_data_sources$last_modified[x],
      directory = download_dir
    )
  }
)

# Save the table with updated latest data info ----------------------------

write_csv(table_of_data_sources, file = file_data_sources)

# Pass latest data to analysis function -----------------------------------

dir_reports <- "./reports"
if (!dir.exists(dir_reports)) dir.create(dir_reports)

render(
  input = "./covid_nrs_place_of_death.Rmd",
  output_format = paste0("covid_nrs_place_of_death_",today()),
  output_dir = dir_reports,
  params = list(data_sources = table_of_data_sources)  # pass the table of data sources
)