
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
file_historical_data_sources <- "./table_of_historical_data_sources.csv"

download_dir <- "./downloaded_data"
if (!dir.exists(download_dir)) dir.create(download_dir)  # create download dir if it doesn't exist

historical_dir <- "./historical_data"
if (!dir.exists(historical_dir)) dir.create(historical_dir)  # create download dir if it doesn't exist

if (file.exists(file_data_sources)) {
  table_of_data_sources <- read_csv(file = file_data_sources)
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

if (file.exists(file_historical_data_sources)) {
  table_of_historical_data_sources <- read_csv(file = file_historical_data_sources)
} else {
  table_of_historical_data_sources <- tibble(
    short_name = c("overall","sex_age","la","hb"),
    url = c(
      "https://www.nrscotland.gov.uk/files/statistics/covid19/weekly-deaths-by-location-2015-2019.csv",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-age-group-sex-15-19.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-council-areas.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-health-board-location-15-19.xlsx"
    )
  ) %>% mutate(
    filename = basename(url),
    last_modified = ymd_hms("2020-01-01_01:01:01"),  # start with a date in the past
    path_latest = NA_character_  
  )
}

# Function to download latest data if available ---------------------------

download_file_if_newer_available <- function(url, filename, last_modified, directory, record_tbl) {
  todays_file <- curl_fetch_disk(url, path = tempfile())
  message(paste0("Checking if we have latest file for: ",filename))
  message(paste0("Latest file was last modified at: ",todays_file$modified))
  if (last_modified < todays_file$modified) {
    message("Newer file available, downloading...")
    date_string <- strftime(todays_file$modified, format = "%F")  # prefix filename with last modified date
    ## copy file to folder
    filename_prefixed <- paste0(date_string,"_",filename)
    file_path <- file.path(directory, filename_prefixed)
    file.copy(from = todays_file$content, to = file_path, overwrite = TRUE)
    ## set last_modified date & save
    record_tbl$last_modified[which(record_tbl$filename == filename)] <- todays_file$modified
    record_tbl$path_latest[which(record_tbl$filename == filename)] <- file_path
  } else {
    message("We already have the most recent file! No download needed.")
  }
  message("...")
  return(record_tbl)
}

# Run the data updating function ------------------------------------------

for (x in 1:nrow(table_of_data_sources)) {
  table_of_data_sources <- 
    download_file_if_newer_available(
      url = table_of_data_sources$url[x],
      filename = table_of_data_sources$filename[x],
      last_modified = table_of_data_sources$last_modified[x],
      directory = download_dir, 
      record_tbl = table_of_data_sources
    )
}

for (x in 1:nrow(table_of_historical_data_sources)) {
  table_of_historical_data_sources <- 
    download_file_if_newer_available(
      url = table_of_historical_data_sources$url[x],
      filename = table_of_historical_data_sources$filename[x],
      last_modified = table_of_historical_data_sources$last_modified[x],
      directory = historical_dir, 
      record_tbl = table_of_historical_data_sources
    )
}

# Save the table with updated latest data info ----------------------------

write_csv(table_of_data_sources, file = file_data_sources)
write_csv(table_of_historical_data_sources, file = file_historical_data_sources)

# Pass latest data to analysis function -----------------------------------

dir_reports <- "./reports"
if (!dir.exists(dir_reports)) dir.create(dir_reports)

render(
  input = "./covid_nrs_place_of_death.Rmd",
  output_format = paste0("covid_nrs_place_of_death_",today()),
  output_dir = dir_reports,
  params = list( # pass the table of data sources
    data_sources = table_of_data_sources,
    historical_data_sources = table_of_historical_data_sources
    ) 
)