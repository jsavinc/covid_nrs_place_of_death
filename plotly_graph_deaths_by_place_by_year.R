## Graph: Proportion of deaths in Scotland by place, 2015-2021
## plotly version


# Load packages -----------------------------------------------------------

library(tidyverse)  # for tidy workflow
library(lemon)  # for adding all axis labels to facet_wrap plots
library(plotly)  # for interactive plots!
library(htmlwidgets)  # for saving standalone html file



# Load data ---------------------------------------------------------------

load(file = "./workspace_with_visualisations.RData")


# Theme setup to match blog style -----------------------------------------

## set theme to match the blog style so far
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(),
                  axis.title.y = element_text(margin = margin(0, 20, 0, 0)),  # remove gap to the left of y axis title and below the x axis title
                  axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
                  text=element_text(family="Calibri")
            ))


# Create graph ------------------------------------------------------------

plotly_proportion_of_deaths_by_place_by_hb <-
  (proportions_of_deaths_by_place_by_geography %>%
     filter(geography == "hb") %>%
     mutate(place_of_death = factor(x = place_of_death, levels = order_of_place_of_death_levels[c(1,3,2,4)])) %>%  # move home to 2nd position to match order in plot
     mutate(ref_area = factor(x = ref_area, levels = order_hb_by_population)) %>%  # arrange hb/la by population size
     ggplot(., aes(x=year, y=proportion_of_total, colour=place_of_death)) +
     # geom_point(aes(size = place_of_death)) +
     geom_line(
       aes(
         size = place_of_death, 
         group = place_of_death, 
         text = paste0(  # using dummy text aesthetic, construct pairs of variable name: value, separated with line break <br|
           "HB: ", ref_area,
           "<br>Place of death: ", place_of_death,
           "<br>Year: ", year,
           "<br>Proportion: ", scales::percent(proportion_of_total, accuracy = 0.1)
         )
       )
     ) +
     # geom_line(aes(size = place_of_death)) +
     geom_text(data = ~filter(.x, year==2021), aes(x=2021.2, y=proportion_of_total, label="*"), inherit.aes = FALSE) +
     facet_rep_wrap(~ ref_area, repeat.tick.labels = TRUE) +
     scale_size_manual(values = c(1,1.8,1,1), name = "Place of death", labels = order_of_place_of_death_levels[c(1,3,2,4)]) +
     scale_x_continuous(breaks = 2015:2021) +
     scale_y_continuous(label = function(x) scales::percent(x, accuracy = 1)) +
     scale_colour_viridis_d(option="C", name="Place of death", labels = order_of_place_of_death_levels[c(1,3,2,4)]) +
     theme(
       legend.position = "top",
       legend.text = element_text(size = 12),
       plot.caption = element_text(size = 10, colour = "gray60"),
       panel.border = element_rect(fill = NA, colour = "gray80"),  # draw a transparent rectangle as the border
       strip.background = element_rect(fill = NA, colour = "gray80")
     ) +
     labs(
       x = NULL, y= "Proportion of yearly deaths (%)",
       subtitle = paste0(c(paste0("* ",only_2021_text),"Health boards sorted by 2020 population estimate."),collapse="\n"),
       caption = paste0(c(source_nrs_text),collapse="\n")
     ) +
     guides(colour = guide_legend(nrow=2, title = NULL), 
            size = guide_legend(nrow=2, title = NULL)) +
     NULL) %>%
  ggplotly(p = ., tooltip = c("text")) %>%
  layout(legend = list(y = 1.1, orientation = 'h'))  # y=1.1 puts legend above plot

# TODO: add average home death increase in Scotland


# Toggle lines to be hidden by default ------------------------------------

## First convert to a plotly object that can be interrogated
plotly_proportion_of_deaths_by_place_by_hb <- plotly_build(plotly_proportion_of_deaths_by_place_by_hb)
## I looked through the different traces here and found which ones were the lines I wanted
for (i in c(1:14, 29:56)) {
  plotly_proportion_of_deaths_by_place_by_hb$x$data[[i]]$visible <- "legendonly"
}


# Save as standalone html -------------------------------------------------

dir_plotly <- "./plotly"
if (!dir.exists(dir_plotly)) dir.create(dir_plotly)

plotly_proportion_of_deaths_by_place_by_hb %>%
  partial_bundle() %>%
  saveWidget(file = paste0(dir_blog,"/plotly_proportion_of_deaths_by_place_by_hb.html"))


# Upload to plotly.com ----------------------------------------------------

## Note: this won't work if the API key & username aren't set as environment variables, e.g.
# Sys.setenv("plotly_username"="XX")
# Sys.setenv("plotly_api_key"="YYY")

api_create(
  x = plotly_proportion_of_deaths_by_place_by_hb,
  filename = "Proportion of deaths by place in Scotland, 2015-2021",
  fileopt = "overwrite",
  sharing = "public"
)