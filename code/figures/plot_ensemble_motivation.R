library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)
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
  system("git checkout 3532bcba304cef2b4872dd2add1f83909f717d91")
} else if (hub == "ECDC") {
  system("git checkout 863d3ede001f1e17c4b97892f4bfb3ff721f779a")
}
setwd(pwd)

model_abbrs <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

model_abbrs <-
  model_abbrs[
    !(model_abbrs %in%
      c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble",
        "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
        "KITmetricslab-select_ensemble", "EuroCOVIDhub-ensemble"))
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
    as_of = "2022-05-16",
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


# ensemble_forecasts <- load_covid_forecasts_relative_horizon(
#   hub = hub,
#   source = "local_hub_repo",
#   hub_repo_path = paste0("code/retrospective-forecasts/state/", target_var),
#   data_processed_subpath = "",
#   monday_dates = "2021-02-15",
#   as_of = NULL,
#   model_abbrs = c(
#     "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
#     "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
#     "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all",
#     "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all"
#   ),
#   timezero_window_size = 6,
#   locations = "39",
#   targets = targets,
#   max_horizon = max_horizon,
#   required_quantiles = required_quantiles
# )


forecast_data <- #dplyr::bind_rows(
  component_forecasts#,
#   ensemble_forecasts
# )

points <- forecast_data %>%
    dplyr::filter(quantile == 0.5) %>%
    dplyr::mutate(type = "point") %>%
    dplyr::rename(point = value) %>%
    dplyr::select(-quantile)

quantiles <- forecast_data %>%
    dplyr::filter(quantile %in% c("0.025", "0.25", "0.75", "0.975")) %>%
    dplyr::mutate(
      type = "quantile",
      quantile = as.numeric(quantile),
      endpoint_type = ifelse(quantile < 0.5, "lower", "upper"),
      alpha = ifelse(endpoint_type == "lower",
                     format(2 * quantile, digits = 3, nsmall = 3),
                     format(2 * (1 - quantile), digits = 3, nsmall = 3)),
      `Prediction Interval` = forcats::fct_rev(paste0((1 - as.numeric(alpha)) * 100, "%"))) %>%
    dplyr::select(-quantile, -alpha) %>%
    tidyr::pivot_wider(names_from = "endpoint_type", values_from = "value")

plot_data_forecast <- dplyr::bind_rows(points, quantiles)



components_to_plot <- c("epiforecasts-ensemble1", "UMass-MechBayes")
panel_a_plot_data_forecast <- plot_data_forecast %>%
  dplyr::filter(model %in% components_to_plot) %>% #,
#    "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
#    "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all")) %>%
  dplyr::mutate(
    model = dplyr::case_when(
      model == components_to_plot[1] ~ "Component A",
      model == components_to_plot[2] ~ "Component B"#,
#      model == "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal Weighted Mean",
#      model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal Weighted Median"
    )
  )

# ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[1:3]
# ribbon_colors <- RColorBrewer::brewer.pal(4, "Greys")[3:4]
# get_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(4, "Blues"))
# blues = get_palette(9)
ribbon_colors <- "#67a9cf"

truth <- dplyr::bind_rows(
  covidData::load_data(
    as_of = "2021-02-15",
    spatial_resolution = "state",
    location_code = "39",
    measure = substr(target_var, 5, nchar(target_var)),
    geography = "US") %>%
#    dplyr::filter(location == "39") %>%
    dplyr::mutate(as_of = "2021-02-15"),
  covidData::load_data(
    as_of = "2021-02-22",
    spatial_resolution = "state",
    location_code = "39",
    measure = substr(target_var, 5, nchar(target_var)),
    geography = "US") %>%
#    dplyr::filter(location == "39") %>%
    dplyr::mutate(as_of = "2021-02-22")
)
  # covidData::load_data(as_of = "2021-02-22") %>%
  #   dplyr::filter(location == "39") %>%
  #   dplyr::mutate(as_of = "2021-02-22"))

p_a <-
  ggplot2::ggplot(data = panel_a_plot_data_forecast, ggplot2::aes(x= target_end_date)) +
  ggplot2::geom_ribbon(
    data = panel_a_plot_data_forecast %>%
      dplyr::filter(
        type == "quantile",
        `Prediction Interval` == "95%"
      ),
    mapping = ggplot2::aes(
      ymin = lower,
      ymax = upper,
      group = interaction(`Prediction Interval`, model,
                          location, forecast_week_end_date),
      fill = `Prediction Interval`),
    show.legend = FALSE,
    alpha = 0.7) +
  ggplot2::scale_fill_manual(name = "Prediction Interval", values = ribbon_colors) +
  # plot point forecasts
  ggplot2::geom_line(data = panel_a_plot_data_forecast %>%
              dplyr::filter(!is.na(point)),
            mapping = ggplot2::aes(x = target_end_date, 
                                    y = point, 
                                    group = interaction(model, location, forecast_week_end_date)),
            color = "#2166ac") +
  ggplot2::geom_point(data = panel_a_plot_data_forecast %>%
                dplyr::filter(!is.na(point)),
              mapping = ggplot2::aes(x = target_end_date, 
                                    y = point),
            color = "#2166ac") +
  #truth
#  ggnewscale::new_scale_color() +
  ggplot2::geom_line(
    data = truth,
    mapping = ggplot2::aes(x = date, y = inc, color = as_of, size = as_of)) +
  scale_color_manual(
    "Data Report Date",
    values = c("2021-02-15" = "#5aae61", "2021-02-22" = "black")
  ) +
  scale_size_manual(
    "Data Report Date",
    values = c("2021-02-15" = 0.9, "2021-02-22" = 0.5)
  ) +
  # ggplot2::geom_label(
  #   data = data.frame(
  #     text = LETTERS[1:4],
  #     model = c("Component Forecaster 1", "Component Forecaster 2", "Median", "Trained")),
  #   mapping = aes(label = text),
  #   x = -Inf, y = Inf,
  #   hjust = -0.5, vjust = 1.5,
  #   inherit.aes = FALSE
  # ) +
  ggplot2::facet_wrap(~ model, scales = "free_y") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Weekly Deaths") +
  ggplot2::scale_y_continuous(labels = comma) +
#  ggplot2::ggtitle("(a) Forecasts of incident deaths in Ohio from February 15, 2021") +
  ggplot2::theme_bw() +
  ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title.position = "plot"
  )




# PANEL B

start_monday <- as.Date("2020-07-27") - 12*7
most_recent_monday <- as.Date("2022-05-16")

target_var <- "inc_case"
#target_var <- "inc_death"
spatial_resolution <- "state"

hub_repo_path <- paste0(
  "../",
  ifelse(
    spatial_resolution == "euro_countries",
    "covid19-forecast-hub-europe",
    "covid19-forecast-hub"),
  "/")
submissions_root <- paste0(hub_repo_path, "data-processed/")

# check out version of hub repo used for analysis
pwd <- setwd(hub_repo_path)
system("git checkout 3532bcba304cef2b4872dd2add1f83909f717d91")
setwd(pwd)

model_abbrs <- get_candidate_models(
  submissions_root = submissions_root,
  include_designations = c("primary", "secondary"),
  include_COVIDhub_ensemble = FALSE,
  include_COVIDhub_baseline = TRUE)

# model_abbrs <- list.dirs(
#   submissions_root,
#   full.names = FALSE,
#   recursive = FALSE
# )

model_abbrs <-
  model_abbrs[
    !(model_abbrs %in%
      c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble",
        "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble",
        "KITmetricslab-select_ensemble", "EuroCOVIDhub-ensemble"))
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
    as_of = "2022-05-16",
    targets = targets,
    spatial_resolution = spatial_resolution
  )

# if (!is.null(target_end_date_locations_drop)) {
#   observed_by_location_target_end_date <- dplyr::anti_join(
#     observed_by_location_target_end_date,
#     target_end_date_locations_drop,
#     by = c("location", "target_end_date")
#   )
# }

# Dates specifying mondays when forecasts were submitted that are relevant to
# this analysis: forecast_date and the previous window_size weeks
monday_dates <- seq(from = start_monday, to = most_recent_monday, by = 7)

forecasts <- load_covid_forecasts_relative_horizon(
  hub = "US",
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path,
  monday_dates = monday_dates,
  as_of = NULL,
  model_abbrs = model_abbrs,
  timezero_window_size = 6,
  locations = unique(observed_by_location_target_end_date$location),
  targets = targets,
  max_horizon = max_horizon,
  required_quantiles = required_quantiles
)

# keep just forecasts with required number of predictive quantiles
forecasts <- forecasts %>%
  dplyr::group_by(model, location, forecast_week_end_date, target_end_date) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n == 7) %>%
  dplyr::select(-n) %>%
  dplyr::ungroup()

# keep just forecast week end dates with multiple models
fwed_multi_model <- forecasts %>%
  dplyr::distinct(model, forecast_week_end_date) %>%
  dplyr::count(forecast_week_end_date) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(forecast_week_end_date)

forecasts <- forecasts %>%
  dplyr::filter(forecast_week_end_date %in% fwed_multi_model)

rel_wis <- purrr::map_dfr(
  unique(forecasts$forecast_week_end_date),
  function(fwed) {
    qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecasts %>% dplyr::filter(forecast_week_end_date == fwed),
      model_col = "model",
      id_cols = c("forecast_week_end_date", "location", "target_end_date"),
      quantile_name_col = "quantile",
      quantile_value_col = "value"
    )
    y <- attr(qfm, "row_index") %>%
      dplyr::mutate(target_end_date = as.character(target_end_date)) %>%
      dplyr::left_join(
        observed_by_location_target_end_date,
        by = c("location", "target_end_date")) %>%
      dplyr::pull(observed)
    
    fwed_rel_wis <- covidEnsembles:::calc_relative_wis(y, qfm) %>%
      dplyr::mutate(forecast_week_end_date = fwed) %>%
      `rownames<-`(NULL)
    
    return(fwed_rel_wis)
  }) %>%
  dplyr::filter(!is.na(rel_wis))

# p <- ggplot(
#     data = rel_wis,
#     mapping = aes(x = forecast_week_end_date, y = rel_wis, color = model)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()

# plotly::ggplotly(p)

data_all_models <- rel_wis %>%
  dplyr::mutate(
    model_display = dplyr::case_when(
      model == "LNQ-ens1" ~ "LNQ-ens1",
      model == "Karlen-pypm" ~ "Karlen-pypm",
      # model == "RobertWalraven-ESG" ~ "Component 3",
      # model == "CU-select" ~ "Component 4",
      model == "COVIDhub-baseline" ~ "Baseline",
      TRUE ~ "Other"
    )
  )

# augment with missing observations
all_dates <- unique(data_all_models$forecast_week_end_date)
data_all_models <- purrr::map_dfr(
  unique(data_all_models$model),
  function(model_val) {
    data_this_model <- data_all_models %>%
      dplyr::filter(model == model_val)
    missing_dates <- all_dates[!(all_dates %in% unique(data_this_model$forecast_week_end_date))]
    if (length(missing_dates) > 0) {
      new_data <- data.frame(
        model = model_val,
        rel_wis = NA,
        forecast_week_end_date = missing_dates,
        model_display = data_this_model$model_display[1]
      )
      data_this_model <- dplyr::bind_rows(
        data_this_model,
        new_data
      )
    }
    return(data_this_model)
  }
)

data_key_models <- data_all_models %>% dplyr::filter(model_display != "Other")

color_values <- c(
  "LNQ-ens1" = "#bd0026",
  "Karlen-pypm" = "#fd9d59",
#  "Component 2" = "#ff9041",
  # "Component 1" = "#2166ac",
  # "Component 2" = "#67a9cf",
  # "Component 3" = "#b2182b",
  # "Component 4" = "#ef8a62",
  "Baseline" = "black",
  "Other" = "gray")
shape_values <- c(
  "Component 1" = 0,
  "Component 2" = 17,
  # "Component 3" = 15,
  # "Component 4" = 16,
  "Baseline" = 16,
  "Other" = 16)

p_b <- ggplot(mapping = aes(
  x = forecast_week_end_date,
  y = rel_wis,
  color = model_display,
  # shape = model_display,
  # alpha = model_display,
  group = model)) +
  # geom_point(data = data_all_models) +
  geom_line(data = data_all_models) +
  # geom_point(data = data_key_models, size = 2) +
  geom_line(data = data_key_models, size = 1) +
  scale_color_manual(
    "Forecaster",
    values = color_values
  ) +
  # scale_shape_manual(
  #   "Forecaster",
  #   values = shape_values
  # ) +
#  coord_cartesian(ylim = c(0, 10)) +
  scale_y_log10() +
#  scale_y_continuous(limits = c(0, 10)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ylab("Relative WIS (log scale)") +
  xlab("Forecast Date") +
#  ggtitle("(b) Component forecaster relative WIS, incident cases in the US") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
    plot.title.position = "plot"
  )



legend_a <- ggpubr::get_legend(p_a, position = "right")
p_a <- p_a + theme(legend.position = "none")

legend_b <- ggpubr::get_legend(p_b, position = "right")
p_b <- p_b +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0, 0, 0, 2), rep("lines", 4))
  )


pdf("manuscript/figures/ensemble_motivation.pdf", width = 8, height = 6)
plot_layout <- grid.layout(
  nrow = 6, ncol = 2,
  widths = unit(c(1, 0.25), rep("null", 2)),
  heights = unit(c(1, 0.9, 0.2, 1.2, 0.8, 0.2), c("lines", "null", "null", "lines", "null", "null")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text("(a) Forecasts of incident deaths in Ohio from February 15, 2021",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))

print(p_a, vp = viewport(layout.pos.row = 2:3, layout.pos.col = 1))
print(as_ggplot(legend_a), vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

grid.text("(b) Component forecaster relative WIS for forecasts of incident cases in the US",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

print(p_b, vp = viewport(layout.pos.row = 5:6, layout.pos.col = 1))
print(as_ggplot(legend_b), vp = viewport(layout.pos.row = 5, layout.pos.col = 2))

dev.off()




# supplementary plots with facet per model, color by rank
# start with cases -- re-using data loaded above
data_ranked <- data_all_models %>%
  dplyr::group_by(forecast_week_end_date) %>%
  dplyr::mutate(
    std_rank = ifelse(
      is.na(rel_wis),
      NA_real_,
      (rank(rel_wis) - 1) / (sum(!is.na(rel_wis)) - 1)
    )
  )

p_per_model <- ggplot() +
  geom_line(
    data = data_ranked,
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      group = model),
    color = "gray") +
  geom_line(
    data = data_ranked %>%
      dplyr::mutate(facet_var = model),
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      group = model),
    size = 1,
    color = "black") +
  geom_point(
    data = data_ranked %>%
      dplyr::mutate(facet_var = model),
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      color = std_rank,
      group = model),
    size = 1) +
  scale_color_viridis_c(
    "Standardized Model Rank",
    option = "plasma") +
  facet_wrap(vars(facet_var), ncol = 4) +
#  coord_cartesian(ylim = c(0, 10)) +
  scale_y_log10() +
#  scale_y_continuous(limits = c(0, 10)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ylab("Relative WIS (log scale)") +
  xlab("Forecast Date") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1.0),
    plot.title.position = "plot",
    legend.position = "bottom"
  )
p_per_model


pdf("manuscript/figures/rel_wis_ranks_cases.pdf", width = 9, height = 13)
print(p_per_model)
dev.off()

data_ranked_cases <- data_ranked


# deaths -- need to reload data
target_var <- "inc_death"

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
    as_of = "2022-05-16",
    targets = targets,
    spatial_resolution = spatial_resolution
  )

forecasts <- load_covid_forecasts_relative_horizon(
  hub = "US",
  source = "local_hub_repo",
  hub_repo_path = hub_repo_path,
  monday_dates = monday_dates,
  as_of = NULL,
  model_abbrs = model_abbrs,
  timezero_window_size = 6,
  locations = unique(observed_by_location_target_end_date$location),
  targets = targets,
  max_horizon = max_horizon,
  required_quantiles = required_quantiles
)

# keep just forecasts with required number of predictive quantiles
forecasts <- forecasts %>%
  dplyr::group_by(model, location, forecast_week_end_date, target_end_date) %>%
  dplyr::mutate(n = n()) %>%
  dplyr::filter(n == 23) %>%
  dplyr::select(-n) %>%
  dplyr::ungroup()

# keep just forecast week end dates with multiple models
fwed_multi_model <- forecasts %>%
  dplyr::distinct(model, forecast_week_end_date) %>%
  dplyr::count(forecast_week_end_date) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(forecast_week_end_date)

forecasts <- forecasts %>%
  dplyr::filter(forecast_week_end_date %in% fwed_multi_model)

rel_wis <- purrr::map_dfr(
  unique(forecasts$forecast_week_end_date),
  function(fwed) {
    qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
      forecasts %>% dplyr::filter(forecast_week_end_date == fwed),
      model_col = "model",
      id_cols = c("forecast_week_end_date", "location", "target_end_date"),
      quantile_name_col = "quantile",
      quantile_value_col = "value"
    )
    y <- attr(qfm, "row_index") %>%
      dplyr::mutate(target_end_date = as.character(target_end_date)) %>%
      dplyr::left_join(
        observed_by_location_target_end_date,
        by = c("location", "target_end_date")) %>%
      dplyr::pull(observed)
    
    fwed_rel_wis <- covidEnsembles:::calc_relative_wis(y, qfm) %>%
      dplyr::mutate(forecast_week_end_date = fwed) %>%
      `rownames<-`(NULL)
    
    return(fwed_rel_wis)
  }) %>%
  dplyr::filter(!is.na(rel_wis))

# p <- ggplot(
#     data = rel_wis,
#     mapping = aes(x = forecast_week_end_date, y = rel_wis, color = model)) +
#   geom_point() +
#   geom_line() +
#   theme_bw()

# plotly::ggplotly(p)

data_all_models <- rel_wis %>%
  dplyr::mutate(
    model_display = dplyr::case_when(
      model == "LNQ-ens1" ~ "Component 1",
      model == "Karlen-pypm" ~ "Component 2",
      # model == "RobertWalraven-ESG" ~ "Component 3",
      # model == "CU-select" ~ "Component 4",
      model == "COVIDhub-baseline" ~ "Baseline",
      TRUE ~ "Other"
    )
  )

# augment with missing observations
all_dates <- unique(data_all_models$forecast_week_end_date)
data_all_models <- purrr::map_dfr(
  unique(data_all_models$model),
  function(model_val) {
    data_this_model <- data_all_models %>%
      dplyr::filter(model == model_val)
    missing_dates <- all_dates[!(all_dates %in% unique(data_this_model$forecast_week_end_date))]
    if (length(missing_dates) > 0) {
      new_data <- data.frame(
        model = model_val,
        rel_wis = NA,
        forecast_week_end_date = missing_dates,
        model_display = data_this_model$model_display[1]
      )
      data_this_model <- dplyr::bind_rows(
        data_this_model,
        new_data
      )
    }
    return(data_this_model)
  }
)

data_ranked <- data_all_models %>%
  dplyr::group_by(forecast_week_end_date) %>%
  dplyr::mutate(
    std_rank = ifelse(
      is.na(rel_wis),
      NA_real_,
      (rank(rel_wis) - 1) / (sum(!is.na(rel_wis)) - 1)
    )
  )

p_per_model <- ggplot() +
  geom_line(
    data = data_ranked,
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      group = model),
    color = "gray") +
  geom_line(
    data = data_ranked %>%
      dplyr::mutate(facet_var = model),
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      group = model),
    size = 1,
    color = "black") +
  geom_point(
    data = data_ranked %>%
      dplyr::mutate(facet_var = model),
    mapping = aes(
      x = forecast_week_end_date,
      y = rel_wis,
      color = std_rank,
      group = model),
    size = 1) +
  scale_color_viridis_c(
    "Standardized Model Rank",
    option = "plasma") +
  facet_wrap(vars(facet_var), ncol = 5) +
#  coord_cartesian(ylim = c(0, 10)) +
  scale_y_log10() +
#  scale_y_continuous(limits = c(0, 10)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  ylab("Relative WIS (log scale)") +
  xlab("Forecast Date") +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 7),
    axis.text.x = element_text(size = 7, angle = 90, vjust = 0.5, hjust = 1.0),
    plot.title.position = "plot",
    legend.position = "bottom"
  )
p_per_model


pdf("manuscript/figures/rel_wis_ranks_deaths.pdf", width = 9, height = 13)
print(p_per_model)
dev.off()

data_ranked_deaths <- data_ranked
