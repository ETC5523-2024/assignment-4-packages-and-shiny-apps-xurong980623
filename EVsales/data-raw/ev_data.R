## code to prepare `EVsales` dataset goes here
library(tidyverse)
ev_data <- read_csv("data-raw/IEA Global EV Data 2024.csv")

# Define region groups
region_mapping <- function(region) {
  if (region %in% c("USA", "Canada", "Mexico")) {
    return("North America")
  } else if (region %in% c("Brazil", "Argentina", "Chile", "Colombia", "Peru")) {
    return("South America")
  } else if (region == "China") {
    return("China")
  } else if (region %in% c("UK", "United Kingdom")) {
    return("UK")
  } else if (region %in% c("Germany", "France", "Italy", "Spain", "Netherlands", "Belgium", "Sweden", "Norway", "Finland", "Denmark", "Switzerland", "Austria", "Ireland", "Portugal", "Greece", "Luxembourg", "Poland", "Czech Republic", "Hungary", "Slovakia", "Slovenia")) {
    return("Europe")
  } else if (region %in% c("Australia", "New Zealand")) {
    return("Australia")
  } else if (region %in% c("Bulgaria", "Costa Rica", "Croatia", "Cyprus", "Estonia", "Iceland", "India", "Indonesia", "Israel", "Japan", "Korea", "Latvia", "Lithuania", "Romania", "Seychelles", "South Africa", "Thailand", "Turkiye", "United Arab Emirates")) {
    return("Other")
  } else if (region == "World") {
    return("World")
  } else {
    return(NA)
  }
}

# Apply region groups to the dataset and filter out NAs
ev_data <- ev_data |>
  mutate(Grouped_Region = sapply(region, region_mapping)) |>
  filter(!is.na(Grouped_Region))

# Convert 'year' to numeric for comparison and back to factor for plotting
ev_data <- ev_data |>
  mutate(year = as.numeric(as.character(year)))|>
  select("year","category","parameter","Grouped_Region","powertrain","value")

usethis::use_data(ev_data, overwrite = TRUE)
