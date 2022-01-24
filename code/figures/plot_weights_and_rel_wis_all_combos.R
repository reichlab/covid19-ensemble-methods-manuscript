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

# helper function to load estimated model weights from rel WIS weighted median
get_weights <- function(spatial_resolution, target_var) {
  model_folder <- paste0(
    "code/retrospective-fits/", spatial_resolution, "/", target_var, "/",
    "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all/"
  )
  model_fit_files <- Sys.glob(paste0(
    model_folder,
    "*combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all.rds"
  ))

  weight_estimates <- purrr::map_dfr(
    model_fit_files,
    function(filename) {
      forecast_date <- as.Date(substr(filename, nchar(model_folder) + 1, nchar(model_folder) + 10))
      model_fit <- readRDS(filename)
      return(
        model_fit$coefficients %>%
          dplyr::mutate(
            forecast_date = forecast_date,
            target_var = target_var,
            theta = model_fit$par
          )
      )
    }
  )

  return(weight_estimates)
}


for (spatial_resolution in c("state", "euro_countries")) {
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
    system("git checkout c6c24d2feabd9f9550d51e6de336dad87ffc9477")
  } else if (hub == "ECDC") {
    system("git checkout 6f8659c5a75ed42c2af93483807c1ee4177a8cd4")
  }
  setwd(pwd)

  model_abbrs <- list.dirs(
    submissions_root,
    full.names = FALSE,
    recursive = FALSE
  )
  # drop ensembles from list of models to load from hub repo
  # we'll load ensembles from covid19-ensemble-methods-manuscript
  model_abbrs <-
    model_abbrs[
      !(model_abbrs %in%
        c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble",
          "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "KITmetricslab-select_ensemble",
          "EuroCOVIDhub-ensemble"))
    ]

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
          "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all"
        ),
        timezero_window_size = 6,
        locations = unique(observed_by_location_target_end_date$location),
        targets = targets,
        max_horizon = max_horizon,
        required_quantiles = required_quantiles
      ) %>%
        dplyr::mutate(
          model = ifelse(
            model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all",
            "Equal Weighted Median",
            "Rel. WIS Weighted Median"
          )
        )

      forecasts <- dplyr::bind_rows(component_forecasts, ensemble_forecasts)

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

  # get weight estimates
  weight_estimates <- purrr::map_dfr(
    c("inc_case", "inc_death"),
    function(target_var) {
      get_weights(
        spatial_resolution = spatial_resolution,
        target_var = target_var) %>%
        dplyr::mutate(
          target_var = ifelse(target_var == "inc_case", "Cases", "Deaths")
        )
    })

  model_names_by_weight <- weight_estimates %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(max_weight = max(beta)) %>%
    dplyr::arrange(max_weight) %>%
    dplyr::pull(model)
  max_weight_components <- weight_estimates %>%
    dplyr::group_by(model, target_var) %>%
    dplyr::summarize(max_weight = max(beta), .groups = "drop") %>%
    dplyr::group_by(target_var) %>%
    dplyr::slice_max(max_weight, n = 2) %>%
    dplyr::arrange(target_var, max_weight) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      model_display = model
      # model_display = paste0(
      #   "Component ",
      #   as.integer(factor(model, levels = unique(model))))
    )

  rel_wis_by_forecast_date <- purrr::map_dfr(
    c("inc_case", "inc_death"),
    function(target_var) {
      purrr::map_dfr(
        forecasts %>%
          dplyr::filter(grepl(gsub("_", " ", target_var), target)) %>%
          dplyr::pull(forecast_week_end_date) %>%
          unique(),
        function(fwed) {
          qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
            forecasts %>%
              dplyr::filter(
                forecast_week_end_date == fwed,
                grepl(gsub("_", " ", target_var), target)
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
          target_var = ifelse(target_var == "inc_case", "Cases", "Deaths")
        )
    })

  rel_wis_trailing_12_week <- purrr::map_dfr(
    c("inc_case", "inc_death"),
    function(target_var) {
      purrr::map_dfr(
        forecasts %>%
          dplyr::filter(grepl(gsub("_", " ", target_var), target)) %>%
          dplyr::pull(forecast_week_end_date) %>%
          unique(),
        function(fwed) {
          current_models <- forecasts %>%
            dplyr::filter(
              forecast_week_end_date == fwed,
              grepl(gsub("_", " ", target_var), target)) %>%
            dplyr::pull(model) %>%
            unique()
          trailing_forecasts <- forecasts %>%
            dplyr::filter(
              model %in% current_models,
              fwed - forecast_week_end_date <= 12*7,
              target_end_date <= fwed,
              grepl(gsub("_", " ", target_var), target)
            )
          if (nrow(trailing_forecasts) == 0) {
            return(NULL)
          }
          qfm <- covidEnsembles::new_QuantileForecastMatrix_from_df(
            trailing_forecasts,
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
                  as_of == fwed + 1,
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
          target_var = ifelse(target_var == "inc_case", "Cases", "Deaths")
        )
    })

  augment_rel_wis <- function(
    rel_wis,
    component_models
  ) {
    rel_wis <- rel_wis %>%
      dplyr::left_join(
        component_models %>% dplyr::select(model, target_var, model_display),
        by = c("model", "target_var")) %>%
      dplyr::mutate(
        model_display = dplyr::case_when(
          !is.na(model_display) ~ model_display,
          model == "Equal Weighted Median" ~ "Equal Weighted Median",
          model == "Rel. WIS Weighted Median" ~ "Rel. WIS Weighted Median",
          model %in% c("COVIDhub-baseline", "EuroCOVIDhub-baseline") ~ "Baseline",
          TRUE ~ "Other"
        )
      )

    # augment with missing observations
    rel_wis <- purrr::map_dfr(
      c("Cases", "Deaths"),
      function(target_var) {
        tv_rel_wis <- rel_wis %>% dplyr::filter(target_var == UQ(target_var))
        all_dates <- unique(tv_rel_wis$forecast_week_end_date)
        tv_rel_wis <- purrr::map_dfr(
          unique(tv_rel_wis$model),
          function(model_val) {
            data_this_model <- tv_rel_wis %>%
              dplyr::filter(model == model_val)
            missing_dates <- all_dates[!(all_dates %in% unique(data_this_model$forecast_week_end_date))]
            if (length(missing_dates) > 0) {
              new_data <- data.frame(
                model = model_val,
                rel_wis = NA,
                forecast_week_end_date = missing_dates,
                model_display = data_this_model$model_display[1],
                target_var = target_var
              )
              data_this_model <- dplyr::bind_rows(
                data_this_model,
                new_data
              )
            }
            return(data_this_model)
          }
        )
        return(tv_rel_wis)
      }
    )

    return(rel_wis)
  }

  rel_wis_by_forecast_date <- augment_rel_wis(
    rel_wis = rel_wis_by_forecast_date,
    component_models = max_weight_components
  )
  rel_wis_by_forecast_date_key_models <- rel_wis_by_forecast_date %>%
    dplyr::filter(model_display != "Other")

  rel_wis_trailing_12_week <- augment_rel_wis(
    rel_wis = rel_wis_trailing_12_week,
    component_models = max_weight_components
  ) %>%
    dplyr::filter(!(model_display %in% c("Rel. WIS Weighted Median", "Equal Weighted Median")))
  rel_wis_trailing_12_week_key_models <- rel_wis_trailing_12_week %>%
    dplyr::filter(
      model_display != "Other"
      # model_display %in% c("Component 1", "Component 2", "Component 3", "Component 4", "Baseline")
    )

  # shape_values <- c(
  #   "Rel. WIS Weighted Median" = 15,
  #   "Equal Weighted Median" = 16,
  #   "Component 1" = 0,
  #   "Component 2" = 17,
  #   "Component 3" = 5,
  #   "Component 4" = 18,
  #   "Baseline" = 16,
  #   "Other" = 16)
  # component_nums_to_keep <- paste0("Component ", seq_along(unique(max_weight_components$model)))
  # shape_values <- shape_values[
  #   c("Rel. WIS Weighted Median", "Equal Weighted Median",
  #     component_nums_to_keep,
  #     "Baseline", "Other")
  # ]
  # names(shape_values[component_nums_to_keep]) <-
  #   unique(max_weight_components$model)

  component_nums_to_keep <- paste0("Component ", seq_along(unique(max_weight_components$model)))

  color_values <- c(
    "Rel. WIS Weighted Median" = "#2166ac",
    "Equal Weighted Median" = "#67a9cf",
    "Component 1" = "#bd0026",
    "Component 2" = "#ff9041",
    "Component 3" = "#d6510e",
    "Component 4" = "#fed976",
    # "Component 4" = "#d65b1d",
    # "Component 1" = "#d7301f",
    # "Component 2" = "#ef6548",
    # "Component 3" = "#fc8d59",
    # "Component 4" = "#fdbb84",
    # "Component 3" = "#1a9641",
    # "Component 4" = "#a6d96a",
    "Baseline" = "black",
    "Other" = "gray")
  color_values <- color_values[
    c("Rel. WIS Weighted Median", "Equal Weighted Median",
      component_nums_to_keep,
      "Baseline", "Other")
  ]
  names(color_values) <- c(
    "Rel. WIS Weighted Median", "Equal Weighted Median",
    unique(max_weight_components$model),
    "Baseline", "Other"
  )

  add_baseline <- ("COVIDhub-baseline" %in% model_names_by_weight) |
                  ("EuroCOVIDhub-baseline" %in% model_names_by_weight)
  num_key_components <- length(unique(max_weight_components$model_display))
  # key_component_colors <- c("#d7301f", "#ef6548", "#fc8d59", "#fdbb84")
  greys <- rev(grDevices::gray.colors(
    n = length(model_names_by_weight) - add_baseline))
  fill_values <- greys
  # fill_values <- c(
  #   greys,
  #   key_component_colors[seq_len(num_key_components)])
    # "#b2182b",
    # "#ef8a62")
  fill_names <-
    model_names_by_weight[!(model_names_by_weight %in% c("COVIDhub-baseline", "EuroCOVIDhub-baseline"))]
  if (add_baseline) {
    fill_values <- c(fill_values, "black")
    fill_names <- c(
      fill_names,
      ifelse(hub == "US", "COVIDhub-baseline", "EuroCOVIDhub-baseline")
    )
  }
  fill_values <- c(fill_values, fill_values)
  names(fill_values) <- c(
    paste0(fill_names, "_Cases"),
    paste0(fill_names, "_Deaths")
  )
  key_component_colors <- max_weight_components %>%
    dplyr::left_join(
      data.frame(
        model_display = names(color_values),
        color_hex = color_values
      ),
      by = c("model_display")
    ) %>%
    dplyr::mutate(model_target = paste(model, target_var, sep = "_"))
  key_colors <- key_component_colors$color_hex
  names(key_colors) <- key_component_colors$model_target
  fill_values[names(fill_values) %in% key_component_colors$model_target] <-
    key_colors[
      names(fill_values[names(fill_values) %in% key_component_colors$model_target])
    ]

  make_p_rel_wis <- function(data_all_models, data_key_models, facet_label, facet_by_target_var = FALSE) {
    color_values <- color_values[names(color_values) %in% unique(data_all_models$model_display)]
    shape_values <- shape_values[names(shape_values) %in% unique(data_all_models$model_display)]
    data_all_models <- data_all_models %>%
      dplyr::mutate(model_display = factor(model_display,
                                            levels = names(color_values)))
    data_key_models <- data_key_models %>%
      dplyr::mutate(model_display = factor(model_display,
                                            levels = names(color_values)))
    p_rel_wis <- ggplot(
      data = data_all_models %>%
        dplyr::filter(model_display == "Other") %>%
        dplyr::mutate(
          log_rel_wis = log(rel_wis),
          facet_var = facet_label
        )
      ) +
      geom_line(
        mapping = aes(
          x = forecast_week_end_date,
          y = rel_wis,
          # y = log_rel_wis,
          color = model_display,
          # shape = model_display,
          # alpha = model_display,
          group = model)
      ) +
      geom_line(
        data = data_key_models %>%
          dplyr::mutate(
            facet_var = facet_label
          ),
        mapping = aes(
          x = forecast_week_end_date,
          y = rel_wis,
          # y = log_rel_wis,
          color = model_display,
          # shape = model_display,
          # alpha = model_display,
          group = model),
        size = 1
      ) +
      # geom_point(
      #   data = data_key_models %>%
      #     dplyr::mutate(
      #       facet_var = facet_label
      #     ),
      #   mapping = aes(
      #     x = forecast_week_end_date,
      #     y = rel_wis,
      #     # y = log_rel_wis,
      #     color = model_display,
      #     shape = model_display,
      #     # alpha = model_display,
      #     group = model),
      #   size = 2
      # ) +
      scale_color_manual(
        "Forecaster",
        values = color_values
      ) +
      # scale_shape_manual(
      #   "Forecaster",
      #   values = shape_values
      # ) +
#        facet_wrap( ~ facet_var, ncol = 1, scales = "free_y") +
      ylab("") +
      xlab("Forecast Date") +
      scale_y_log10() +
      scale_x_date(
        limits = c(as.Date("2020-07-01"), as.Date("2021-10-15")),
        date_breaks = "1 month",
        date_labels = "%b %Y") +
      coord_cartesian(ylim = c(0.25, 10)) +
      guides(fill = "none") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
        plot.margin = unit(c(0, 0, 0, -0.4), rep("cm", 4))
      )
    
    if (facet_by_target_var) {
      p_rel_wis <- p_rel_wis +
        facet_wrap( ~ target_var, nrow = 1)
    } else {
      p_rel_wis <- p_rel_wis +
        facet_wrap( ~ facet_var, ncol = 1, scales = "free_y")
    }

    return(p_rel_wis)
  }

  p_rel_wis_by_forecast_date <- make_p_rel_wis(
    data_all_models = rel_wis_by_forecast_date,
    data_key_models = rel_wis_by_forecast_date_key_models,
    facet_label = "Relative WIS, Forecasts Issued on Forecast Date",
    facet_by_target_var = TRUE
  )

  p_rel_wis_trailing_12_weeks <- make_p_rel_wis(
    data_all_models = rel_wis_trailing_12_week %>%
      dplyr::filter(forecast_week_end_date >= min(weight_estimates$forecast_date)),
    data_key_models = rel_wis_trailing_12_week_key_models %>%
      dplyr::filter(forecast_week_end_date >= min(weight_estimates$forecast_date)),
    facet_label = "Relative WIS, Trailing 12 Weeks Available as of Forecast Date",
    facet_by_target_var = TRUE
  )

  p_weights <- ggplot(
    data = weight_estimates %>%
      dplyr::filter(forecast_date <= last_monday) %>%
      dplyr::mutate(
        facet_var = factor(
          "Component Weights",
          levels = c("Relative WIS", "Component Weights", "Theta")
        ),
        model_target = paste(model, target_var, sep = "_")
      )
  ) +
    geom_col(
      mapping = aes(x = forecast_date, y = beta, fill = model_target),
      color = "black") +
    scale_fill_manual(
      "Forecaster",
      values = fill_values
    ) +
    # facet_wrap( ~ facet_var, ncol = 1, scales = "free_y") +
    facet_wrap( ~ target_var, nrow = 1) +
    ylab("") +
    xlab("Forecast Date") +
    scale_x_date(
      limits = c(as.Date("2020-07-01"), as.Date("2021-10-15")),
      date_breaks = "1 month",
      date_labels = "%b %Y") +
    guides(fill = "none") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
      plot.margin = unit(c(0, 0, 0, -0.4), rep("cm", 4))
    )

  # p_theta <- ggplot(
  #   data = weight_estimates %>%
  #     dplyr::filter(target_var == "Cases") %>%
  #     dplyr::distinct(forecast_date, theta) %>%
  #     dplyr::mutate(
  #       facet_var = factor(
  #         "Theta",
  #         levels = c("Relative WIS", "Component Weights", "Theta")
  #       )
  #     )
  # ) +
  #   geom_line(mapping = aes(x = forecast_date, y = theta)) +
  #   facet_wrap( ~ facet_var, ncol = 1, scales = "free_y") +
  #   ylab("") +
  #   xlab("Forecast Date") +
  #   scale_x_date(
  #     limits = c(as.Date("2020-07-01"), as.Date("2021-11-01")),
  #     date_breaks = "1 month",
  #     date_labels = "%b %Y") +
  #   guides(fill = "none") +
  #   theme_bw() +
  #   theme(
  # #    legend.position = "none",
  #     axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
  #     plot.margin = unit(c(0, 0, 0, -0.4), rep("cm", 4))
  #   )


  legend_pos <- "bottom"
  legend <- ggpubr::get_legend(p_rel_wis_by_forecast_date, position = legend_pos)

  p_rel_wis_by_forecast_date <- p_rel_wis_by_forecast_date + theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
  )
  p_rel_wis_trailing_12_weeks <- p_rel_wis_trailing_12_weeks + theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank(),
    strip.text = element_blank()
  )
  p_weights <- p_weights + theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank()#,
    # axis.text.x = element_blank(),
    # axis.title.x = element_blank()
  )
  # p_theta <- p_theta + theme(
  #   legend.position = "none"
  # )

  pdf(
    paste0(
      'manuscript/figures/ensemble_rel_wis_and_component_weights_',
      ifelse(spatial_resolution == "state", "US", "EU"), '_',
      "Cases_and_Deaths",
      # target_var,
      '.pdf'
    ),
    width = 8,
    height = 8)
  if (legend_pos == "right") {
    plot_layout <- grid.layout(
      nrow = 5, ncol = 5,
      widths = unit(c(1, 1, 0.013, 1, 0.4), c("lines", "lines", "null", "null", "null")),
      heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
      # widths = unit(c(1, 0.02, 1, 0.4), c("lines", "null", "null", "null")),
      # heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
  } else {
    plot_layout <- grid.layout(
      nrow = 6, ncol = 5,
      widths = unit(c(1, 1, 0.005, 0.01, 1), c("lines", "lines", "null", "null", "null")),
      heights = unit(c(1, 0.02, 0.95, 0.02, 1.3, 3), c(rep("null", 5), "lines")))
      # widths = unit(c(1, 0.02, 1, 0.4), c("lines", "null", "null", "null")),
      # heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
  }
  grid.newpage()
  pushViewport(viewport(layout = plot_layout))

  grid.text(
    "Forecast Date   ",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.text(
    "Relative WIS (log scale)   ",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

  grid.text(
    "Trailing 12 Weeks  ",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  grid.text(
    "Relative WIS (log scale)",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

  grid.text(
    "               Component Forecaster Weight",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 5, layout.pos.col = 2))

  # grid.text(
  #   expression(paste("              Weighting Parameter ", theta)),
  #   just = "center",
  #   rot = 90,
  #   gp = gpar(fontsize = 11),
  #   vp = viewport(layout.pos.row = 5, layout.pos.col = 1))


  print(p_rel_wis_by_forecast_date,
    vp = viewport(layout.pos.row = 1, layout.pos.col = 3:5))
  print(p_rel_wis_trailing_12_weeks,
    vp = viewport(layout.pos.row = 3, layout.pos.col = 3:5))
  print(p_weights,
    vp = viewport(layout.pos.row = 5, layout.pos.col = 3:5))

  if (legend_pos == "right") {
    print(
      as_ggplot(legend),
      vp = viewport(layout.pos.row = 3, layout.pos.col = 5)
    )
  } else {
    print(
      as_ggplot(legend),
      vp = viewport(layout.pos.row = 6, layout.pos.col = 3:5)
    )
  }
  dev.off()


  # # plot of effective weights by location, accounting for forecast missingness
  # effective_weights_by_location_date <- purrr::pmap_dfr(
  #   tidyr::expand_grid(
  #     location = unique(forecasts$location_name),
  #     fwed = unique(forecasts$forecast_week_end_date)
  #   ),
  #   function(location, fwed) {
  #     fwed_weights <- weight_estimates %>%
  #       dplyr::filter(forecast_date == (fwed + 2))
  #     available_models <- forecasts %>%
  #       dplyr::filter(
  #         forecast_week_end_date == fwed,
  #         location_name == UQ(location)) %>%
  #       dplyr::pull(model) %>%
  #       unique()
  #     effective_weights <- fwed_weights %>%
  #       dplyr::filter(model %in% available_models) %>%
  #       dplyr::mutate(
  #         beta = beta / sum(beta),
  #         location_name = location
  #       )
  #     return(effective_weights)
  #   }
  # )

  # p_effective_weights <- ggplot(data = effective_weights_by_location_date) +
  #   geom_col(mapping = aes(x = location_name, y = beta, fill = model)) +
  #   facet_wrap( ~ forecast_date) +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # # plotly_effective_weights <- plotly::ggplotly(p_effective_weights)
  # # pwd <- getwd()
  # # setwd("manuscript/figures")
  # # htmlwidgets::saveWidget(
  # #   as_widget(plotly_effective_weights),
  # #   paste0(
  # #     "effective_weights_by_location_",
  # #     ifelse(spatial_resolution == "state", "US", "EU"), "_",
  # #     target_var, ".html"
  # #   )
  # # )
  # # setwd(pwd)


  # # plot of location counts by model over time
  # location_counts <- component_forecasts %>%
  #   dplyr::distinct(model, location, forecast_week_end_date) %>%
  #   dplyr::count(model, forecast_week_end_date, name = "num_locations")
  # p_location_counts <- ggplot(data = location_counts) +
  #   geom_histogram(mapping = aes(x = num_locations)) +
  #   facet_wrap(~ forecast_week_end_date)
  # pdf(
  #   paste0(
  #     'manuscript/figures/location_counts_',
  #     ifelse(spatial_resolution == "state", "US", "EU"), '_',
  #     target_var, '.pdf'
  #   ),
  #   width = 8,
  #   height = 8)
  # print(p_location_counts)
  # dev.off()

  # model_counts <- component_forecasts %>%
  #   dplyr::distinct(model, location, forecast_week_end_date) %>%
  #   dplyr::count(location, forecast_week_end_date, name = "num_models") %>%
  #   dplyr::ungroup()
  # p_model_counts <- ggplot(data = model_counts) +
  #   geom_line(mapping = aes(x = location, y = num_models, group = forecast_week_end_date)) +
  #   facet_wrap(~ forecast_week_end_date)
  # pdf(
  #   paste0(
  #     'manuscript/figures/model_counts_',
  #     ifelse(spatial_resolution == "state", "US", "EU"), '_',
  #     target_var, '.pdf'
  #   ),
  #   width = 8,
  #   height = 8)
  # print(p_model_counts)
  # dev.off()

  # model_location_date_avail <- component_forecasts %>%
  #   dplyr::distinct(model, location, forecast_week_end_date)
  # all_combos <- tidyr::expand_grid(
  #   model = unique(model_location_date_avail$model),
  #   location = unique(model_location_date_avail$location),
  #   forecast_week_end_date = unique(model_location_date_avail$forecast_week_end_date)
  # )
  # model_location_date_avail <- dplyr::bind_rows(
  #   all_combos %>%
  #     dplyr::semi_join(model_location_date_avail) %>%
  #     dplyr::mutate(avail = TRUE),
  #   all_combos %>%
  #     dplyr::anti_join(model_location_date_avail) %>%
  #     dplyr::mutate(avail = FALSE)
  # )
  # p_model_availability <- ggplot(data = model_location_date_avail) +
  #   ggplot2::geom_raster(mapping = aes(y = model, x = location, fill = avail)) +
  #   facet_wrap( ~ forecast_week_end_date, ncol = 4) +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  # pdf(
  #   paste0(
  #     'manuscript/figures/model_availability_',
  #     ifelse(spatial_resolution == "state", "US", "EU"), '_',
  #     target_var, '.pdf'
  #   ),
  #   width = 20,
  #   height = 24)
  # print(p_model_availability)
  # dev.off()

  # model_location_counts <- location_counts %>%
  #   dplyr::count(forecast_week_end_date, num_locations, name = "num_models")
  # p_model_counts <- ggplot(data = location_counts) +
  #   geom_line(mapping = aes(x = forecast_week_end_date, y = num_locations, color = model))
}

# information for text about model weights
weight_estimates %>%
  dplyr::filter(target_var == "Cases") %>%
  dplyr::group_by(model) %>%
  dplyr::summarize(min_weight = min(beta), max_weight = max(beta)) %>%
  dplyr::arrange(min_weight) %>%
  as.data.frame()

# # total number of models included at any point
# length(fill_model_names)
