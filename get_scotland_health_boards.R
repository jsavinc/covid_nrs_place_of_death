## Script to read health board codes and descriptive names and save them as .csv

library(tidyverse)
raw_hbs <- read_csv(file ="https://www.opendata.nhs.scot/datastore/dump/652ff726-e676-4a20-abda-435b98dd7bdc?bom=True")

clean_hbs <-
  raw_hbs %>%
  select(hb_code = HB, health_board_name = HBName)

write_csv(x = clean_hbs, file = "./scotland_health_board_names.csv")