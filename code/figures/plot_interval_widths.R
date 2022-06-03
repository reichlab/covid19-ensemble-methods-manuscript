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

# Get forecasts
locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2, location != "US") %>%
  dplyr::pull(location)

forecasts <- purrr::map_dfr(
  c("inc_case", "inc_death"),
  function(target_var) {
    if (target_var == "inc_death") {
      required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      temporal_resolution <- "wk"
      max_horizon <- 4L
      targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
    } else if (target_var == "inc_case") {
      if (spatial_resolution == "euro_countries") {
        required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      } else {
        required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
      }

      temporal_resolution <- "wk"
      max_horizon <- 4L
      targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
    }

    # Dates specifying mondays when forecasts were submitted that are relevant
    # to this analysis
    monday_dates <- seq(from = start_monday, to = last_monday, by = 7)

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

    # load ensemble forecasts from covid19-ensemble-methods-manuscript repo
    ensemble_forecasts <- load_covid_forecasts_relative_horizon(
      hub = hub,
      source = "local_hub_repo",
      hub_repo_path = file.path(
        "code/retrospective-forecasts",
        spatial_resolution,
        target_var,
        sep = "/"),
      data_processed_subpath = "",
      monday_dates = monday_dates,
      as_of = NULL,
      model_abbrs = c(
        "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
        "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all"
      ),
      timezero_window_size = 6,
      locations = locations,
      targets = targets,
      max_horizon = max_horizon,
      required_quantiles = required_quantiles
    ) %>%
      dplyr::mutate(
        model = dplyr::case_when(
          model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal Weighted Median",
          TRUE ~ "Rel. WIS Weighted Median"
        )
      )

    forecasts <- dplyr::bind_rows(component_forecasts, ensemble_forecasts) %>%
      dplyr::filter(forecast_week_end_date <= last_monday)

    # keep just forecasts with required number of predictive quantiles
    forecasts <- forecasts %>%
      dplyr::group_by(model, location, forecast_week_end_date, target_end_date) %>%
      dplyr::mutate(n = n()) %>%
      dplyr::filter(n == length(required_quantiles)) %>%
      dplyr::select(-n) %>%
      dplyr::ungroup()

    fwed_multi_model <- forecasts %>%
      dplyr::distinct(model, forecast_week_end_date) %>%
      dplyr::count(forecast_week_end_date) %>%
      dplyr::filter(n > 1) %>%
      dplyr::pull(forecast_week_end_date)

    forecasts <- forecasts %>%
      dplyr::filter(forecast_week_end_date %in% fwed_multi_model)

    return(forecasts)
  })

# keep only states and DC -- territories were forecasted less often
forecasts <- forecasts %>%
  dplyr::filter(location <= "56") %>%
  dplyr::mutate(quantile = format(quantile, digits = 3, nsmall = 3)) %>%
  dplyr::filter(quantile %in% c("0.025", "0.975"))

# calculate width of 95% intervals
interval_widths <- forecasts %>%
  tidyr::pivot_wider(names_from = "quantile", values_from = "value") %>%
  dplyr::mutate(interval_width = `0.975` - `0.025`)

# rank of interval width within each location, forecast_week_end_date, target combination
interval_width_ranks <- interval_widths %>%
  dplyr::group_by(location, forecast_week_end_date, target) %>%
  dplyr::mutate(
    std_rank = (rank(interval_width, ) - 1) / (n() - 1)
  )

# code from https://stackoverflow.com/questions/61733297/apply-bold-font-on-specific-axis-ticks
highlight <- function(x, pat, color="black", family="") {
  ifelse(
    grepl(pat, x),
    glue("<b style='font-family:{family}; color:{color}'>{x}</b>"),
    x
  )
}

p <- ggplot(
    interval_width_ranks %>%
      dplyr::filter(grepl("case", target)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        model = reorder(model, std_rank, FUN = median)
      ),
    mapping = aes(y = model, x = std_rank, fill = factor(stat(quantile), levels = as.character(1:4)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles") +
  scale_x_continuous(name="Standardized Rank of 95% Interval Widths", 
    #expand=expansion(add=c(2, 1)/max(inc_scores$n_models)), 
    limits=c(0,1)) +
  scale_y_discrete(labels = function(x) highlight(x, "Equal Weighted Median|Rel. WIS Weighted Median", "red")) +
  ggtitle("Standardized Ranks of 95% Interval Widths: Case Forecasts") +
  theme_bw() +
  theme(axis.text.y=element_markdown())
  

pdf(file = "manuscript/figures/95_PI_width_ranks_cases.pdf", width=8, height=10)
print(p)
dev.off()


p <- ggplot(
    interval_width_ranks %>%
      dplyr::filter(grepl("death", target)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        model = reorder(model, std_rank, FUN = median)
      ),
    mapping = aes(y = model, x = std_rank, fill = factor(stat(quantile), levels = as.character(1:4)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles") +
  scale_x_continuous(name="Standardized Rank of 95% Interval Widths", 
    #expand=expansion(add=c(2, 1)/max(inc_scores$n_models)), 
    limits=c(0,1)) +
  scale_y_discrete(labels = function(x) highlight(x, "Equal Weighted Median|Rel. WIS Weighted Median", "red")) +
  ggtitle("Standardized Ranks of 95% Interval Widths: Death Forecasts") +
  theme_bw() +
  theme(axis.text.y=element_markdown())
  

pdf(file = "manuscript/figures/95_PI_width_ranks_deaths.pdf", width=8, height=10)
print(p)
dev.off()
