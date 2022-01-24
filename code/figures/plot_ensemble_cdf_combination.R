library(dplyr)
library(ggplot2)
library(scales)
library(covidData)
library(covidHubUtils)
library(covidEnsembles)

# target_var <- "inc_case"
# spatial_resolution <- "euro_countries"
target_var <- "inc_death"
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
  system("git checkout 0d0dac949cd69c662acef964fbdcd2e99fa5d0bb")
} else if (hub == "ECDC") {
  system("git checkout 863d3ede001f1e17c4b97892f4bfb3ff721f779a")
}
setwd(pwd)

model_abbrs <- list.dirs(
  submissions_root,
  full.names = FALSE,
  recursive = FALSE
)

model_abbrs <-
  model_abbrs[
    !(model_abbrs %in%
      c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble",
        "COVIDhub-trained_ensemble", "KITmetricslab-select_ensemble",
        "EuroCOVIDhub-ensemble"))
  ]


if (target_var == "inc_death") {
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  temporal_resolution <- "wk"
  max_horizon <- 4L
  targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
  # full_history_start <- lubridate::ymd("2020-06-22") - 7 * 10
} else if (target_var == "inc_case") {
  if (spatial_resolution == "euro_countries") {
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  } else {
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  }

  temporal_resolution <- "wk"
  max_horizon <- 4L
  targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
  # full_history_start <- lubridate::ymd("2020-09-14") - 7 * 10
}

# Get observed values ("truth" in Zoltar's parlance)
observed_by_location_target_end_date <-
  get_observed_by_location_target_end_date(
    as_of = "2021-08-08",
    targets = targets,
    spatial_resolution = spatial_resolution
  )

component_forecasts <- load_covid_forecasts_relative_horizon(
  hub = hub,
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path,
  monday_dates = "2021-02-15",
  as_of = NULL,
  model_abbrs = model_abbrs,
  timezero_window_size = 6,
  locations = "39",
  targets = targets,
  max_horizon = max_horizon,
  required_quantiles = required_quantiles
)


# component_forecasts <- covidHubUtils::load_forecasts(
#   dates = "2021-02-15",
#   forecast_date_window_size = 6,
#   locations = covidData::fips_codes %>%
#     dplyr::filter(abbreviation == "OH") %>%
#     dplyr::pull(location),
#   targets = paste0(1:4, " wk ahead inc death"),
#   source = "local_hub_repo",
#   hub_repo_path = "../../covid19-forecast-hub"
# )

# trained_ensemble_forecast <- covidHubUtils::load_forecast_files_repo(
#   file_paths = "../covid-19-iif-blog-post-data/post2/retrospective-forecasts/state/prospective_selection/inc_death-2021-02-15-prospective_selection-include_full_history_FALSE.csv"
# ) %>%
#   dplyr::filter(location == "39")


ensemble_forecasts <- load_covid_forecasts_relative_horizon(
  hub = hub,
  source = "local_hub_repo",
  hub_repo_path = paste0("code/retrospective-forecasts/state/", target_var),
  data_processed_subpath = "",
  monday_dates = "2021-02-15",
  as_of = NULL,
  model_abbrs = c(
    "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
    "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
    "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all",
    "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all"
  ),
  timezero_window_size = 6,
  locations = "39",
  targets = targets,
  max_horizon = max_horizon,
  required_quantiles = required_quantiles
)


forecast_data <- dplyr::bind_rows(
  component_forecasts,
  ensemble_forecasts
)

plot_data_forecast <- forecast_data %>%
  dplyr::filter(
    !is.na(quantile),
    target_end_date == as.Date("2021-02-20")
  ) %>%
  dplyr::mutate(
    quantile = as.numeric(quantile),
    model_display = dplyr::case_when(
      model == "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal\nWeighted Mean",
      model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal\nWeighted Median",
      model == "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all" ~ "Rel. WIS\nWeighted Mean",
      model == "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all" ~ "Rel. WIS\nWeighted Median",
      TRUE ~ "Component"
    )
  )


p <-
  ggplot() +
  geom_line(
    data = plot_data_forecast,
    mapping = aes(
      x = value, y = quantile,
      color = model_display, linetype = model_display, size = model_display,
      group = model)) +
  geom_line(
    data = plot_data_forecast %>%
      dplyr::filter(model_display != "Component"),
    mapping = aes(
      x = value, y = quantile,
      color = model_display, linetype = model_display, size = model_display,
      group = model)) +
#  scale_x_log10() +
#  xlim(c(0, 20000)) +
  coord_cartesian(xlim = c(0, 20000)) +
  scale_color_manual(
    "Forecaster",
    values = c(
      "Component" = "gray",
      "Equal\nWeighted Mean" = "#ef8a62",
      "Equal\nWeighted Median" = "#67a9cf",
      "Rel. WIS\nWeighted Mean" = "#b2182b",
      "Rel. WIS\nWeighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Forecaster",
    values = c(
      "Component" = 1,
      "Equal\nWeighted Mean" = 4,
      "Equal\nWeighted Median" = 5,
      "Rel. WIS\nWeighted Mean" = 2,
      "Rel. WIS\nWeighted Median" = 1
    )
  ) +
  scale_size_manual(
    "Forecaster",
    values = c(
      "Component" = 0.5,
      "Equal\nWeighted Mean" = 1,
      "Equal\nWeighted Median" = 1,
      "Rel. WIS\nWeighted Mean" = 1,
      "Rel. WIS\nWeighted Median" = 1
    )
  ) +
  xlab("Predicted Quantile Value (Incident Deaths)") +
  ylab("Quantile Probability") +
  theme_bw()


pdf("manuscript/figures/ensemble_cdf_combination.pdf", width = 8, height = 3)
print(p)
dev.off()
