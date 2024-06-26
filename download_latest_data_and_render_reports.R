
# Overview ----------------------------------------------------------------

## This function loads a table containing the filenames & urls of 
## data sources, and then checks if newer data is available, downloading it in
## the process. Once that's done, it will render one or more .Rmd reports using
## the latest data.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)  # dealing with dates
library(curl)  # for downloading & last modified date extraction
library(knitr)  # for rendering .Rmd
library(rmarkdown)  # for rendering .Rmd
library(extrafont)  # for setting fonts
library(rvest)  # for extracting url from links
library(xml2)  # for the url_absolute() function

# Find latest NRS weekly deaths spreadsheet URL ---------------------------

# Note: as of 2024-02-02, the filename appears to change weekly again, so I'm
# back to using a script to find the latest filename

url_nrs_weekly_deaths <- "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-deaths-registered-in-scotland"

all_nrs_links <-
  read_html(url_nrs_weekly_deaths) %>%
  html_nodes("a") %>%
  html_attr("href") %>%  # find all links on the page
  tibble(links = .)

nrs_filename_pattern <- "weekly-deaths-.*\\.xlsx"

weekly_deaths_url_relative <-
  all_nrs_links %>%
  filter(str_detect(links, pattern = nrs_filename_pattern)) %>%
  filter(!str_detect(links, pattern = "monthly")) %>%  # remove monthly data
  filter(!str_detect(links, pattern = "excess")) %>%  # remove excess deaths data
  pull(links)

stopifnot(length(weekly_deaths_url_relative)==1)  # stop if more than one URL found

## get absolute URL
weekly_deaths_url_absolute <- url_absolute(weekly_deaths_url_relative, base = url_nrs_weekly_deaths)

# 2023 notes below:

## fixed url now and filename no longer updated weekly, so I need to rename the file myself after it's downloaded

# url_nrs_weekly_deaths_2023 <- "https://www.nrscotland.gov.uk/files//statistics/vital-events/weekly-deaths/weekly-deaths-23.xlsx"

## Note: previously the filename for the most recent spreadsheet reporting
## weekly deaths changed weekly and reflected the date of publication (week
## number & year); additionally, every 4 weeks or so the publication included
## more detailed statistics, which was also reflected in the filename.
## Therefore, I was inferring the latest filename by scraping the NRS website
## and finding the most likely link. As of late March 2023, the website has been
## renamed to no longer be Covid-specific (though the format of the report has
## not changed and still contains Covid-related deaths). 

## Below is the previously used code to infer the clatest URL for the report

# url_nrs_weekly_deaths <- "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-and-monthly-data-on-births-and-deaths/deaths-involving-coronavirus-covid-19-in-scotland"

# all_nrs_links <-
#   read_html(url_nrs_weekly_deaths) %>% 
#   html_nodes("a") %>%
#   html_attr("href")  # find all links on the page
# 
# nrs_filename_pattern <- "covid-deaths-(2\\d)-data.*xlsx"  # this will last for the decade... 202X
# 
# # TODO: incorporate both weekly and monthly data?
# weekly_deaths_url_relative <- # keep only links that are probably weekly deaths
#   tail(  # take last entry (n=1)
#     sort(  # sort so that last entry has the highest week number
#       unique(  # on some occasions, duplicate urls are extracted
#         all_nrs_links[which(
#           str_detect(all_nrs_links, pattern = nrs_filename_pattern) &
#           !str_detect(all_nrs_links, pattern = "monthly")  # remove "monthly" file
#           )])
#       ),
#     n = 1
#   )
# 
# weekly_deaths_url_absolute <- url_absolute(weekly_deaths_url_relative, base = url_nrs_weekly_deaths)


## the format changed between 2021 and 2022 for weekly deaths data, and the 2022 data no longer include 2020 figures, so we need to load the 2020 data separately from the latest available data
weekly_deaths_2020_and_2021_last_file <- "https://www.nrscotland.gov.uk/files/statistics/covid19/covid-deaths-21-data-week-52.xlsx"

# Load table keeping track of latest data ---------------------------------

file_data_sources <- "./table_of_data_sources.csv"
file_historical_data_sources <- "./table_of_historical_data_sources.csv"
file_case_trends_data_sources <- "./table_of_case_trends_data_sources.csv"

download_dir <- "./downloaded_data"
if (!dir.exists(download_dir)) dir.create(download_dir)  # create download dir if it doesn't exist

historical_dir <- "./historical_data"
if (!dir.exists(historical_dir)) dir.create(historical_dir)  # create download dir if it doesn't exist

if (file.exists(file_data_sources)) {
  table_of_data_sources <- read_csv(file = file_data_sources)
} else {
  table_of_data_sources <- tibble(
    short_name = c("main_report","sex_age","la","hb"),
    url = c(
      # weekly_deaths_url_absolute,  # found automatically above!
      weekly_deaths_url_absolute,  # inferred latest URL
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
    short_name = c("overall","sex_age","la","hb","2020","2021","2022"),
    url = c(
      "https://www.nrscotland.gov.uk/files/statistics/covid19/weekly-deaths-by-location-2015-2019.csv",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-age-group-sex-15-19.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-council-areas.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-health-board-location-15-19.xlsx",
      "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-21-data-week-52.xlsx",  # last 2021 data which included 2020 also
      "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-22-data-week-25.xlsx",  # last 2022 data which included 2021 also
      "https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-22-data-final.xlsx"  # finalised 2022 data
    )
  ) %>% mutate(
    # filename = basename(url),
    last_modified = ymd_hms("2020-01-01_01:01:01"),  # start with a date in the past
    path_latest = NA_character_  
  )
}

## Case trend data from the following pages
## Daily COVID-19 Cases in Scotland, subsection Daily Case Trends By...:
## LA: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/427f9a25-db22-4014-a3bc-893b68243055
## HB: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland/resource/2dd8534b-0a6f-4744-9253-9565d62f96c2

if (file.exists(file_case_trends_data_sources)) {
  table_of_case_trends_data_sources <- read_csv(file = file_case_trends_data_sources)
} else {
  table_of_case_trends_data_sources <- tibble(
    short_name = c("la","hb"),
    url = c(
      "https://www.opendata.nhs.scot/datastore/dump/427f9a25-db22-4014-a3bc-893b68243055?bom=True",
      "https://www.opendata.nhs.scot/datastore/dump/2dd8534b-0a6f-4744-9253-9565d62f96c2?bom=True"
    )
  ) %>% mutate(
    filename = c("trend_ca.csv","trend_hb.csv"),
    # last_modified = ymd_hms("2020-01-01_01:01:01"),  # start with a date in the past
    path_latest = NA_character_  
  )
}


# Function to download latest data if available ---------------------------

# download_file_if_newer_available <- function(url, filename, last_modified, directory, record_tbl) {
download_file_if_newer_available <- function(url, short_name, filename=NULL, last_modified, directory, record_tbl) {  # note:removed filename argument, I can get it from the url!
  if (short_name == "main_report") {
    url <- weekly_deaths_url_absolute  # replace with latest url of main report 
  }
  todays_file <- curl_fetch_disk(url, path = tempfile())
  message(paste0("Checking if we have latest file for: ",short_name))
  message(paste0("The latest local file is: ", basename(record_tbl$path_latest[which(record_tbl$short_name == short_name)])))
  message(paste0("Latest file was last modified at: ",todays_file$modified))
  if (last_modified < todays_file$modified) {
    message("Newer file available, downloading...")
    date_string <- strftime(todays_file$modified, format = "%F")  # prefix filename with last modified date
    ## copy file to folder
    if (is.null(filename)) {  # if a filename is not provided infer name from url
      filename <- basename(url)
      }
    file_path <-  file.path(directory, paste0(date_string,"_",filename))
    file.copy(from = todays_file$content, to = file_path, overwrite = TRUE)
    ## set last_modified date & save
    if (short_name == "main_report") {
      record_tbl$url[which(record_tbl$short_name == short_name)] <- url  # replace with latest url of main report 
    }
    record_tbl$last_modified[which(record_tbl$short_name == short_name)] <- todays_file$modified
    record_tbl$filename[which(record_tbl$short_name == short_name)] <- filename
    record_tbl$path_latest[which(record_tbl$short_name == short_name)] <- file_path
  } else {
    message("We already have the most recent file! No download needed.")
  }
  message("...")
  return(record_tbl)
}

download_file_regardless <- function(url, filename, directory, record_tbl) {
  todays_date_string <- today()
  todays_filename <- paste0(todays_date_string, "_", filename)
  file_path <- file.path(directory,todays_filename)
  message(paste0("Downloading file: ",todays_filename))
  curl_download(url = url, destfile = file_path, quiet = FALSE)
  message(paste0("Downloaded file to: ",file_path))
  record_tbl$path_latest[which(record_tbl$filename == filename)] <- file_path
  return(record_tbl)
}

# Run the data updating function ------------------------------------------

## current data sources
for (x in 1:nrow(table_of_data_sources)) {
  table_of_data_sources <- 
    download_file_if_newer_available(
      url = table_of_data_sources$url[x],
      # filename = table_of_data_sources$filename[x],
      short_name = table_of_data_sources$short_name[x],
      last_modified = table_of_data_sources$last_modified[x],
      directory = download_dir, 
      record_tbl = table_of_data_sources
    )
}

## historical data sources
for (x in 1:nrow(table_of_historical_data_sources)) {
  table_of_historical_data_sources <- 
    download_file_if_newer_available(
      url = table_of_historical_data_sources$url[x],
      short_name = table_of_historical_data_sources$short_name[x],
      last_modified = table_of_historical_data_sources$last_modified[x],
      directory = historical_dir, 
      record_tbl = table_of_historical_data_sources
    )
}

## for the daily case trends data, just download anyway

for (x in 1:nrow(table_of_case_trends_data_sources)) {
  table_of_case_trends_data_sources <- 
    download_file_regardless(
      url = table_of_case_trends_data_sources$url[x],
      filename = table_of_case_trends_data_sources$filename[x],
      # last_modified = table_of_case_trends_data_sources$last_modified[x],
      directory = download_dir, 
      record_tbl = table_of_case_trends_data_sources
    )
}

# Save the table with updated latest data info ----------------------------

write_csv(table_of_data_sources, file = file_data_sources)
write_csv(table_of_historical_data_sources, file = file_historical_data_sources)
write_csv(table_of_case_trends_data_sources, file = file_case_trends_data_sources)

# Download map data -------------------------------------------------------

dir_map_data <- "./map_data"
if (!dir.exists(dir_map_data)) dir.create(dir_map_data)

download_shapefile <- function(url, directory, subdirectory) {
  if (dir.exists(file.path(directory, subdirectory))) {
    message(glue::glue("Map data for:{subdirectory} has already been downloaded, skipping."))
    return(FALSE)
  }
  message(glue::glue("Downloading map data for:{subdirectory}...."))
  zip_file <- tempfile()
  downloaded_zip <- curl_download(url = url, destfile = zip_file, quiet = FALSE) 
  unzip(zipfile = zip_file, exdir = file.path(directory, subdirectory), overwrite = TRUE)
  return(TRUE)
}

health_boards_shapefile_url <- "https://maps.gov.scot/ATOM/shapefiles/SG_NHS_HealthBoards_2019.zip"  # massive resolution
health_boards_uk_wide_url <- "https://github.com/tomwhite/covid-19-uk-data/files/4563933/UK_covid_reporting_regions.zip"
# https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-buc
# la_shapefile_url <- "https://opendata.arcgis.com/datasets/910f48f3c4b3400aa9eb0af9f8989bbe_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"  # this is the May 2020 link that no longer works
la_shapefile_url <- "https://opendata.arcgis.com/api/v3/datasets/69dc11c7386943b4ad8893c45648b1e1_0/downloads/data?format=shp&spatialRefId=27700"

# TODO: these have been downloaded so don't need to be downloaded again... change to check if they exist already first?
download_shapefile(url = health_boards_shapefile_url, directory = dir_map_data, subdirectory = "hb")
download_shapefile(url = la_shapefile_url, directory = dir_map_data, subdirectory = "la")
download_shapefile(url = health_boards_uk_wide_url, directory = dir_map_data, subdirectory = "hb_uk_wide")

# Download population estimates data --------------------------------------

dir_population_estimates <- "./population_estimates"
if (!dir.exists(dir_population_estimates)) dir.create(dir_population_estimates)

population_estimates_2019_la_url <- "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/09ebfefb-33f4-4f6a-8312-2d14e2b02ace/download/ca2019_pop_est_29062021.csv"
population_estimates_2019_hb_url <- "https://www.opendata.nhs.scot/dataset/7f010430-6ce1-4813-b25c-f7f335bdc4dc/resource/27a72cc8-d6d8-430c-8b4f-3109a9ceadb1/download/hb2019_pop_est_29062021.csv"

population_estimates_2019_la_file <- "ca2019_pop_est_29062021.csv"
population_estimates_2019_hb_file <- "hb2019_pop_est_29062021.csv"

if (!file.exists(file.path(dir_population_estimates, population_estimates_2019_la_file))) {
  curl_download(url = population_estimates_2019_la_url, destfile = file.path(dir_population_estimates, population_estimates_2019_la_file), quiet = FALSE)
}
if (!file.exists(file.path(dir_population_estimates, population_estimates_2019_hb_file))) {
  curl_download(url = population_estimates_2019_hb_url, destfile = file.path(dir_population_estimates, population_estimates_2019_hb_file), quiet = FALSE)
}

# Import Calibri font -----------------------------------------------------

if (!any(fonttable()$FullName=="Calibri")) font_import(pattern = "calibri", prompt = FALSE)  # only run first time if Calibri not yet imported

# Render report with latest data ------------------------------------------

dir_reports <- "./reports"
if (!dir.exists(dir_reports)) dir.create(dir_reports)

message("Rendering data .rmd file...")
rmarkdown::render(
  input = "./covid_nrs_place_of_death_data_2024.Rmd",
  output_file = paste0("covid_nrs_place_of_death_data_",today()),
  output_dir = dir_reports,
  envir = new.env(),
  quiet = TRUE
)
message("Done!")

message("Rendering visualisations .rmd file...")
rmarkdown::render(
  input = "./covid_nrs_place_of_death_visualisations_2024.Rmd",
  output_file = paste0("covid_nrs_place_of_death_visualisations",today()),
  output_dir = dir_reports,
  envir = new.env(),
  quiet = TRUE
)
message("Done!")

# rmarkdown::render(
#   input = "./visualisations_for_short_paper_feb2023.qmd",
#   output_file = paste0("visualisations_for_short_paper_feb2023",today()),
#   output_dir = dir_reports,
#   envir = new.env(),
#   quiet = TRUE
# )

# rmarkdown::render(
#   input = "./visualisations_for_rse_curious_event.qmd",
#   output_file = paste0("visualisations_for_rse_curious_event",today()),
#   output_dir = dir_reports,
#   envir = new.env(),
#   quiet = TRUE
# )

# 
# render(
#   input = "./paper_reporting_death_at_home_increase.Rmd",
#   output_file = paste0("paper_reporting_death_at_home_increase_",today()),
#   output_dir = dir_reports,
#   envir = new.env()https
# )
# 
# render(
#   input = "./visualisations_for_blog.Rmd",
#   output_file = paste0("visualisations_for_blog_",today()),
#   output_dir = dir_reports,
#   envir = new.env()
# )
# 
# render(
#   input = "./highlands_data_and_visualisations.Rmd",
#   output_file = paste0("highlands_data_and_visualisations_",today()),
#   output_dir = dir_reports,
#   envir = new.env()
# )

message("...\nFinished successfully!")