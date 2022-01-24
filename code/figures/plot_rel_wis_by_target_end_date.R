library(tidyverse)
library(grid)
library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)
library(scales)
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(plotly)

# target_var <- "inc_case"
# spatial_resolution <- "state"

data_as_of <- "2021-12-05"

euro_hub_locations <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR",
  "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
  "PL", "PT", "RO", "SI", "SK", "FI", "SE", "GB", "IS", "LI", "NO", "CH")

#for (spatial_resolution in c("state", "euro_countries")) {
for (spatial_resolution in "euro_countries") {
  if (spatial_resolution == "euro_countries") {
    hub <- "ECDC"
    hub_repo_path <- "../covid19-forecast-hub-europe/"
    baseline <- "EuroCOVIDhub-baseline"
  } else {
    hub <- "US"
    hub_repo_path <- "../covid19-forecast-hub/"
    baseline <- "COVIDhub-baseline"
  }
  submissions_root <- paste0(hub_repo_path, "data-processed/")

  pwd <- setwd(hub_repo_path)
  if (hub == "US") {
    system("git checkout c6c24d2feabd9f9550d51e6de336dad87ffc9477")
  } else if (hub == "ECDC") {
    system("git checkout 6f8659c5a75ed42c2af93483807c1ee4177a8cd4")
  }
  setwd(pwd)

  model_abbrs <- baseline

  start_monday <- as.Date("2020-07-27") - 12 * 7
  last_monday <- as.Date("2021-10-11")

  # Get observed values ("truth" in Zoltar's parlance) for both cases and deaths
  # across all data as of dates needed below
  observed_by_location_target_end_date <- purrr::pmap_dfr(
    tidyr::expand_grid(
      target_var = c("inc_case", "inc_death"),
      as_of_date_val = c(
        seq.Date(from = start_monday, to = last_monday, by = 7) - 1,
        as.Date(data_as_of))
    ),
    function(target_var, as_of_date_val) {
      get_observed_by_location_target_end_date(
        as_of = as_of_date_val,
        targets = paste0(1:4, " wk ahead ", gsub("_", " ", target_var)),
        spatial_resolution = spatial_resolution
      ) %>%
      dplyr::mutate(as_of = as_of_date_val)
    })

  # Get forecasts
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
        locations = unique(observed_by_location_target_end_date$location),
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
          "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
          "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all",
          "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all"
        ),
        timezero_window_size = 6,
        locations = unique(observed_by_location_target_end_date$location),
        targets = targets,
        max_horizon = max_horizon,
        required_quantiles = required_quantiles
      ) %>%
        dplyr::mutate(
          model = dplyr::case_when(
            model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal Weighted Median",
            model == "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all" ~ "Equal Weighted Mean",
            model == "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all" ~ "Rel. WIS Weighted Median",
            model == "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all" ~ "Rel. WIS Weighted Mean"
          )
        )

      forecasts <- dplyr::bind_rows(component_forecasts, ensemble_forecasts)

      fwed_multi_model <- forecasts %>%
        dplyr::distinct(model, forecast_week_end_date) %>%
        dplyr::count(forecast_week_end_date) %>%
        dplyr::filter(n > 1) %>%
        dplyr::pull(forecast_week_end_date)

      forecasts <- forecasts %>%
        dplyr::filter(forecast_week_end_date %in% fwed_multi_model)

      return(forecasts)
    })

  rel_wis_by_forecast_date <- purrr::pmap_dfr(
    tidyr::expand_grid(
      target_var = c("inc_case", "inc_death"),
      horizon = 1:4
    ),
    function(target_var, horizon) {
      purrr::map_dfr(
        forecasts %>%
          dplyr::filter(
            grepl(gsub("_", " ", target_var), target),
            horizon == UQ(horizon)) %>%
          dplyr::pull(forecast_week_end_date) %>%
          unique(),
        function(fwed) {
          qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
            forecasts %>%
              dplyr::filter(
                forecast_week_end_date == fwed,
                grepl(gsub("_", " ", target_var), target),
                horizon == UQ(horizon)
              ),
            model_col = "model",
            id_cols = c("forecast_week_end_date", "location", "target_end_date"),
            quantile_name_col = "quantile",
            quantile_value_col = "value"
          )
          y <- attr(qfm, "row_index") %>%
            dplyr::mutate(target_end_date = as.character(target_end_date)) %>%
            dplyr::left_join(
              observed_by_location_target_end_date %>%
                dplyr::filter(
                  as_of == data_as_of,
                  base_target == paste0("wk ahead ", gsub("_", " ", target_var))
                  ),
              by = c("location", "target_end_date")) %>%
            dplyr::pull(observed)
          
          fwed_rel_wis <- covidEnsembles:::calc_relative_wis(
              y = y,
              qfm = qfm,
              baseline = ifelse(hub == "US", "COVIDhub-baseline", "EuroCOVIDhub-baseline")) %>%
            dplyr::mutate(forecast_week_end_date = fwed) %>%
            `rownames<-`(NULL)
          
          return(fwed_rel_wis)
        }) %>%
        dplyr::filter(!is.na(rel_wis)) %>%
        dplyr::mutate(
          target_var = ifelse(target_var == "inc_case", "Cases", "Deaths"),
          horizon = horizon
        )
    })

  if (hub == "US") {
    relevant_locations <- "US"
  } else {
    relevant_locations <- euro_hub_locations
  }
  plot_data <- dplyr::bind_rows(
    covidData::load_data(
      spatial_resolution = "national",
      temporal_resolution = "weekly",
      measure = "cases",
      geography = ifelse(hub == "US", "US", "global")) %>%
      dplyr::filter(location %in% relevant_locations) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(inc = sum(inc)) %>%
      dplyr::transmute(
        forecast_date = date,
        target_end_date = date,
        value = inc,
        quantity = ifelse(hub == "US", "All States", "All Countries"),
        model_brief = "Reported Incidence",
        target_variable = "Cases"
      ),
    covidData::load_data(
      spatial_resolution = "national",
      temporal_resolution = "weekly",
      measure = "deaths",
      geography = ifelse(hub == "US", "US", "global")) %>%
      dplyr::filter(location %in% relevant_locations) %>%
      dplyr::group_by(date) %>%
      dplyr::summarize(inc = sum(inc)) %>%
      dplyr::transmute(
        forecast_date = date,
        target_end_date = date,
        value = inc,
        quantity = ifelse(hub == "US", "All States", "All Countries"),
        model_brief = "Reported Incidence",
        target_variable = "Deaths"
      ),
    rel_wis_by_forecast_date %>%
      dplyr::mutate(
        quantity = paste0("Forecast Horizon: ", horizon, ifelse(horizon == 1, " week", " weeks"))
      ) %>%
      dplyr::transmute(
        forecast_date = forecast_week_end_date,
        value = rel_wis,
        combine_method = ifelse(
          model %in% c("COVIDhub-baseline", "EuroCOVIDhub-baseline"),
          "Baseline",
          model),
        quantity,
        target_variable = target_var
      )
  )

  plot_data <- plot_data %>%
    dplyr::mutate(
      quantity = factor(quantity,
        levels = c(ifelse(hub == "US", "All States", "All Countries"),
          paste0("Forecast Horizon: ", 1:4, ifelse(1:4 == 1, " week", " weeks")))
      )
    )

  if (hub == "US") {
    scores_ylim <- c(0.2, 5.0)
  } else {
    scores_ylim <- c(0.1, 20.5)
  }

  p_cases_data <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Cases", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
        ),
      mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
    scale_color_manual("National Data", values = "black") +
    geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
    facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
      limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
      expand = expansion()) +
    scale_y_continuous(labels = comma) +
    ylab("") +
    xlab("") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      legend.position = "none")

  p_cases_scores <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Cases", forecast_date >= "2020-07-27",
          combine_method %in% c("Baseline", "Equal Weighted Mean", "Equal Weighted Median",
            "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
        ),
      mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
    geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
    facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
    scale_color_manual(
      "Combination Method",
      values = c(
        "Baseline" = "#000000",
        "Equal Weighted Mean" = "#fd9d59",
        "Equal Weighted Median" = "#67a9cf",
        "Rel. WIS Weighted Mean" = "#bd0026",
        "Rel. WIS Weighted Median" = "#2166ac")
    ) +
    scale_linetype_manual(
      "Combination Method",
      values = c(
        "Baseline" = 1,
        "Equal Weighted Mean" = 4,
        "Equal Weighted Median" = 5,
        "Rel. WIS Weighted Mean" = 2,
        "Rel. WIS Weighted Median" = 1)
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
      limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
      expand = expansion()) +
    scale_y_log10(limits = scores_ylim) +
    ylab("") +
    xlab("") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1.05),
      plot.title = element_blank(),
      plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
      legend.position = "none")

  p_deaths_data <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Deaths", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
        ),
      mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
    scale_color_manual("National Data", values = "black") +
    geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
    facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
      limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
      expand = expansion()) +
    scale_y_continuous(labels = comma) +
    ylab("") +
    xlab("") +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      legend.position = "none")

  p_deaths_scores <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Deaths", forecast_date >= "2020-07-27",
          combine_method %in% c("Baseline", "Equal Weighted Mean", "Equal Weighted Median",
            "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
        ),
      mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
    geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
    facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
    scale_color_manual(
      "Combination Method",
      values = c(
        "Baseline" = "#000000",
        "Equal Weighted Mean" = "#fd9d59",
        "Equal Weighted Median" = "#67a9cf",
        "Rel. WIS Weighted Mean" = "#bd0026",
        "Rel. WIS Weighted Median" = "#2166ac")
    ) +
    scale_linetype_manual(
      "Combination Method",
      values = c(
        "Baseline" = 1,
        "Equal Weighted Mean" = 4,
        "Equal Weighted Median" = 5,
        "Rel. WIS Weighted Mean" = 2,
        "Rel. WIS Weighted Median" = 1)
    ) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
      limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
      expand = expansion()) +
    scale_y_log10(limits = scores_ylim) +
    ylab("") +
    xlab("") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1.05),
      plot.title = element_blank(),
      plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
      legend.position = "none")


  p_data_temp <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Deaths", model_brief == "Reported Incidence", forecast_date >= "2020-07-27"
        ), # %>%
        # dplyr::mutate(
        #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
        # ),
      mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
    scale_color_manual("National Data", values = "black") +
    theme_bw()
  legend_data <- ggpubr::get_legend(p_data_temp)

  p_wis_temp <- ggplot() +
    geom_line(
      data = plot_data %>%
        dplyr::filter(
          target_variable == "Deaths", forecast_date >= "2020-07-27",
          combine_method %in% c("Baseline", "Equal Weighted Mean", "Equal Weighted Median",
            "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
        ),
      mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
    scale_color_manual(
      "Combination Method",
      values = c(
        "Baseline" = "#000000",
        "Equal\nWeighted Mean" = "#ef8a62",
        "Equal\nWeighted Median" = "#67a9cf",
        "Rel. WIS\nWeighted Mean" = "#b2182b",
        "Rel. WIS\nWeighted Median" = "#2166ac")
    ) +
    scale_linetype_manual(
      "Combination Method",
      values = c(
        "Baseline" = 1,
        "Equal\nWeighted Mean" = 4,
        "Equal\nWeighted Median" = 5,
        "Rel. WIS\nWeighted Mean" = 2,
        "Rel. WIS\nWeighted Median" = 1
      )
    ) +
    theme_bw()

  legend_wis_horiz <- ggpubr::get_legend(p_wis_temp, position = "bottom")



  #png("manuscript/figures/scores_by_week.png", width = 8, height = 8, units = "in", res = 600)
  pdf(
    paste0("manuscript/figures/rel_wis_by_week_",
           ifelse(hub == "US", "US", "EU"),
           ".pdf"),
    width = 8, height = 10)
  if (hub == "US") {
    panel_padding <- c(0.113, 0.073)
  } else {
    panel_padding <- c(0.095, 0.055)
  }
  plot_layout <- grid.layout(
    nrow = 10, ncol = 5,
    widths = unit(c(2, panel_padding[1], 0.8, panel_padding[2], 0.8), c("lines", rep("null", 4))),
    heights = unit(c(1.1, 1.2, 0.2, rep(1, 5), 1.5, 2), c("lines", "null", "lines", rep("null", 5), "lines", "lines")))

  grid.newpage()
  pushViewport(viewport(layout = plot_layout))

  grid.text("        Cases",
    just = "center",
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

  grid.text("        Deaths",
    just = "center",
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1, layout.pos.col = 5))

  print(as_ggplot(legend_wis_horiz), vp = viewport(layout.pos.row = 10, layout.pos.col = 2:5))
  print(p_cases_data, vp = viewport(layout.pos.row = 2, layout.pos.col = 2:3))
  print(p_deaths_data, vp = viewport(layout.pos.row = 2, layout.pos.col = 4:5))
  print(p_cases_scores, vp = viewport(layout.pos.row = 4:8, layout.pos.col = 3))
  print(p_deaths_scores, vp = viewport(layout.pos.row = 4:8, layout.pos.col = 5))
  grid.text("Forecast Creation Date",
    just = "center",
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1 + 8, layout.pos.col = 2:5))
  grid.text("Weekly\nCases or Deaths",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1 + 1, layout.pos.col = 1))

  print(
    ggplot() +
      geom_line(
        data = data.frame(x = c(1, 1), y = c(0.095, 0.97)),
        mapping = aes(x = x, y = y)) +
      xlim(0, 1) +
      scale_y_continuous(limits = c(0, 1), expand = expansion(0, 0)) +
      theme_void(),
    vp = viewport(layout.pos.row = 1 + 3:7, layout.pos.col = 1)
  )
  grid.text("       Relative WIS (log scale)",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1 + 3:8, layout.pos.col = 1))

  dev.off()
}
