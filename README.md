# covid_nrs_place_of_death

Analysis of place of death in NRS Covid-19 data. NRS publishes weekly data on deaths due to COVID-19, with aggregated data on age, sex, location (by health board or local authority), as well as grouped cause of death. The scripts below download the openly available data and produce various summary statistics, tables and figures.


# Contents

* [`download_latest_data_and_render_reports.R`](./download_latest_data_and_render_reports.R)  - this script will check the latest available data sources and compare to what's been downloaded before, updating if necessary. I wrote this so that I would keep copies of weekly data as they came out, for the sake of reproducibility of the data published in the blogs and to be able to check discrepancies if they arise. The script will then run the various `.Rmd` scripts contained in the repository, generating reports and the various tables and figures. Shapefile data for drawing maps of Scotland is also downloaded, as well as PHS-published daily covid case count data.


## RMarkdown files

* [`covid_nrs_place_of_death_data.Rmd`](./covid_nrs_place_of_death_data.Rmd) - this loads the data sources, cleans them and produces various intermediate tables; the resulting R workspace is saved as `workspace.RData` and is loaded by the subsequent scripts
* [`covid_nrs_place_of_death_visualisations.Rmd`](./covid_nrs_place_of_death_visualisations.Rmd) - loads `workspace.RData` and produces lots of figures showing the breakdown of deaths at home by various demographic characteristics; saves the resulting workspace as `workspace_with_visualisations.RData`
* [`paper_reporting_death_at_home_increase.Rmd`](./paper_reporting_death_at_home_increase.Rmd) - loads `workspace_with_visualisations.RData` and produces the tables, figures and statistics used for our draft paper with Iain Atherton & Catherine Mahoney
* [`visualisations_for_blog.Rmd`](./visualisations_for_blog.Rmd) - loads `workspace_with_visualisations.RData` and produces various visualisations and tables for the blog series on SCADR's website


## Supplementary files

* [`using_UPRN_to_determine_household_structure.Rmd`](./using_UPRN_to_determine_household_structure.Rmd) - this is a dummy script to mock up how we might determine household members living with decedents at the time of death, using UPRN data
* [`order_of_min_max_rolling_mean.R`](./order_of_min_max_rolling_mean.R) - test an intuition about computing rolling averages (moving averages) over historical data given in weekly figures, specifically about the order of computing the mean first followed by range (min/max) or vice versa


# Instructions to replicate

Assuming all the links to NRS and PHS published data remain intact (probably not very safe to assume, in hindsight...), you can `clone` this repository, and run [`download_latest_data_and_render_reports.R`](./download_latest_data_and_render_reports.R) from within an R session, provided you have the required libraries installed (the `.Rmd` files above list what libraries they need).

