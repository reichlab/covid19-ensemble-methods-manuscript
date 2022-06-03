library(tidyverse)
library(grid)
library(dplyr)
library(ggplot2)
library(ggtext)
library(grid)
library(ggpubr)
library(ggridges)
library(scales)
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(glue)

spatial_resolution <- "state"

if (spatial_resolution == "euro_countries") {
  hub <- "ECDC"
  hub_repo_path <- "../covid19-forecast-hub-europe/"
} else {
  hub <- "US"
  hub_repo_path <- "../covid19-forecast-hub/"
}
submissions_root <- paste0(hub_repo_path, "data-processed/")

pwd <- setwd(hub_repo_path)
if (hub == "US") {
  system("git checkout 3532bcba304cef2b4872dd2add1f83909f717d91")
} else if (hub == "ECDC") {
  system("git checkout 6f8659c5a75ed42c2af93483807c1ee4177a8cd4")
}
setwd(pwd)

model_abbrs <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# drop ensembles from list of models to load from hub repo
# we'll load ensembles from covid19-ensemble-methods-manuscript
model_abbrs <-
  model_abbrs[
    !(model_abbrs %in%
      c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble", "COVIDhub_CDC-ensemble",
        "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "KITmetricslab-select_ensemble",
        "EuroCOVIDhub-ensemble"))
  ]

start_monday <- as.Date("2020-07-27") - 12 * 7
last_monday <- as.Date("2022-03-14")

# load forecasts for selected states
location_abbrs <- c("CA", "FL", "NY", "TX")
locations <- covidData::fips_codes %>%
  dplyr::filter(abbreviation %in% location_abbrs) %>%
  dplyr::pull(location)

required_quantiles <- 0.500
temporal_resolution <- "wk"
max_horizon <- 4L

targets <- c(
  paste0(1:max_horizon, " wk ahead inc case"),
  paste0(1:max_horizon, " wk ahead inc death")
)

# Dates specifying mondays when forecasts were submitted that are relevant
# to this analysis
monday_dates <- seq(from = start_monday, to = last_monday, by = 7*5)

# load component forecasts from hub repo
component_forecasts <- load_covid_forecasts_relative_horizon(
  hub = hub,
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path,
  monday_dates = monday_dates,
  as_of = NULL,
  model_abbrs = model_abbrs,
  timezero_window_size = 6,
  locations = locations,
  targets = targets,
  max_horizon = max_horizon,
  required_quantiles = required_quantiles
)

# keep only component forecasts with all four horizons
component_forecasts <- component_forecasts %>%
  dplyr::mutate(target_var = ifelse(grepl("case", target), "Cases", "Deaths")) %>%
  dplyr::group_by(model, location, forecast_week_end_date, target_var) %>%
  dplyr::mutate(n_forecasts = n()) %>%
  dplyr::filter(n_forecasts == 4)

cases_truth_data <- covidData::load_data(
  as_of = "2022-05-15",
  spatial_resolution = "state",
  temporal_resolution = "weekly",
  measure = "cases") %>%
  dplyr::filter(location %in% locations) %>%
  dplyr::left_join(covidData::fips_codes, by = "location")

deaths_truth_data <- covidData::load_data(
  as_of = "2022-05-15",
  spatial_resolution = "state",
  temporal_resolution = "weekly",
  measure = "deaths") %>%
  dplyr::filter(location %in% locations) %>%
  dplyr::left_join(covidData::fips_codes, by = "location")

p <- ggplot() +
  geom_line(
    data = cases_truth_data,
    mapping = aes(x = date, y = inc)
  ) +
  geom_line(
    data = component_forecasts %>% dplyr::filter(grepl("case", target)),
    mapping = aes(x = target_end_date, y = value,
                  group = paste0(model, forecast_week_end_date)),
    color = "cornflowerblue"
  ) +
  scale_y_continuous("Weekly Cases", labels = comma) +
  facet_wrap( ~ location_name, ncol = 1, scales = "free_y") +
  theme_bw()

pdf("manuscript/figures/component_forecast_medians_5_locations_cases.pdf", width = 8, height = 10)
print(p)
dev.off()


p <- ggplot() +
  geom_line(
    data = deaths_truth_data,
    mapping = aes(x = date, y = inc)
  ) +
  geom_line(
    data = component_forecasts %>%
      dplyr::filter(grepl("death", target)),
    mapping = aes(x = target_end_date,
                  y = value,
                  group = paste0(model, forecast_week_end_date)
      ),
    color = "cornflowerblue"
  ) +
  scale_y_continuous("Weekly Deaths", labels = comma) +
  facet_wrap( ~ location_name, ncol = 1, scales = "free") +
  theme_bw()

pdf("manuscript/figures/component_forecast_medians_5_locations_deaths.pdf", width = 8, height = 10)
print(p)
dev.off()
