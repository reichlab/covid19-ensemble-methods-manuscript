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

data_as_of <- "2022-05-16"

# helper function to load estimated model weights from rel WIS weighted median
get_weights <- function(spatial_resolution, target_var, post_hoc = FALSE) {
  if (post_hoc) {
    model_folder <- paste0(
      "code/retrospective-weights/", spatial_resolution, "/", target_var, "/",
      "post_hoc_weights/"
    )
    model_fit_files <- Sys.glob(paste0(
      model_folder,
      "*post_hoc_weights.csv"
    ))

    weight_estimates <- purrr::map_dfr(
      model_fit_files,
      function(filename) {
        forecast_date <- as.Date(substr(filename, nchar(model_folder) + 1, nchar(model_folder) + 10))
        model_weights <- readr::read_csv(filename)
        return(
          model_weights %>%
            dplyr::filter(abs(quantile_level - 0.5) < 0.0001) %>%
            dplyr::mutate(
              forecast_date = forecast_date,
              target_var = target_var
            ) %>%
            dplyr::select(-quantile_level)
        )
      }
    )
  } else {
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
              weight = beta,
              theta = model_fit$par
            ) %>%
            dplyr::select(-beta)
        )
      }
    )
  }

  return(weight_estimates)
}

spatial_resolution <- "state"
# for (spatial_resolution in c("state", "euro_countries")) {
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
        c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-ensemble", "COVIDhub_CDC-ensemble",
          "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "KITmetricslab-select_ensemble",
          "EuroCOVIDhub-ensemble"))
    ]

  start_monday <- as.Date("2020-07-27") - 12 * 7
  last_monday <- as.Date("2022-03-14")

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
          "post_hoc_weights",
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
          model = dplyr::case_when(
            model == "post_hoc_weights" ~ "Post Hoc Weighted Mean",
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
    }) %>%
    dplyr::filter(forecast_date <= last_monday)

  post_hoc_weight_estimates <- purrr::map_dfr(
    c("inc_case", "inc_death"),
    function(target_var) {
      get_weights(
        spatial_resolution = spatial_resolution,
        target_var = target_var,
        post_hoc = TRUE) %>%
        dplyr::mutate(
          target_var = ifelse(target_var == "inc_case", "Cases", "Deaths")
        )
    }) %>%
    dplyr::filter(forecast_date <= last_monday)

  model_names_by_weight <- post_hoc_weight_estimates %>%
    dplyr::group_by(model) %>%
    dplyr::summarize(max_weight = max(weight)) %>%
    dplyr::arrange(max_weight) %>%
    dplyr::pull(model)
  # max_weight_components <- weight_estimates %>%
  #   dplyr::group_by(model, target_var) %>%
  #   dplyr::summarize(max_weight = max(weight), .groups = "drop") %>%
  #   dplyr::group_by(target_var) %>%
  #   dplyr::slice_max(max_weight, n = 5) %>%
  #   dplyr::arrange(target_var, max_weight) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     model_display = model
  #     # model_display = paste0(
  #     #   "Component ",
  #     #   as.integer(factor(model, levels = unique(model))))
  #   )

  # manually specify component forecasters to highlight for each target variable
  highlighted_components <- data.frame(
    model = c("LNQ-ens1", "Karlen-pypm", "JHUAPL-Bucky",
              "UMass-MechBayes", "Karlen-pypm", "SteveMcConnell-CovidComplete"),
    target_var = rep(c("Cases", "Deaths"), each = 3)
  ) %>%
    dplyr::mutate(model_display = model)

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

  ensemble_methods <- c("Post Hoc Weighted Mean", "Equal Weighted Median",
                        "Rel. WIS Weighted Median")
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
          current_models <- current_models[!(current_models %in% ensemble_methods)]
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
          model == "Post Hoc Weighted Mean" ~ "Post Hoc Weighted Mean",
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
    component_models = highlighted_components
  )
  rel_wis_by_forecast_date_key_models <- rel_wis_by_forecast_date %>%
    dplyr::filter(model_display != "Other")

  rel_wis_trailing_12_week <- augment_rel_wis(
    rel_wis = rel_wis_trailing_12_week,
    component_models = highlighted_components
  ) %>%
    dplyr::filter(!(model_display %in% c("Rel. WIS Weighted Median", "Equal Weighted Median", "Post Hoc Weighted Mean")))
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

  for (target_var in c("Cases", "Deaths")) {
    target_var_start_date <- weight_estimates %>%
      dplyr::filter(target_var == !!target_var) %>%
      dplyr::pull(forecast_date) %>%
      min()
    target_highlighted_components <- highlighted_components %>%
      dplyr::filter(target_var == !!target_var)
    num_key_components <- nrow(target_highlighted_components)
    component_nums_to_keep <- paste0("Component ", seq_len(num_key_components))
    color_values <- c(
      "Post Hoc Weighted Mean" = "#5aae61",
      # "Post Hoc Weighted Mean" = "#c51b7d",
      "Rel. WIS Weighted Median" = "#2166ac",
      "Equal Weighted Median" = "#67a9cf",
      # "Component 1" = "#b30000",
      # "Component 2" = "#ef6548",
      # "Component 3" = "#fdbb84",
      "Component 1" = "#a90026",
      "Component 2" = "#fc4e2a",
      "Component 3" = "#feb24c",
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
        "Post Hoc Weighted Mean",
        component_nums_to_keep,
        "Baseline", "Other")
    ]
    names(color_values) <- c(
      "Rel. WIS Weighted Median", "Equal Weighted Median",
      "Post Hoc Weighted Mean",
      unique(target_highlighted_components$model),
      "Baseline", "Other"
    )

    add_baseline <- ("COVIDhub-baseline" %in% model_names_by_weight) |
                    ("EuroCOVIDhub-baseline" %in% model_names_by_weight)
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
    key_component_colors <- target_highlighted_components %>%
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
      linetype_values <- rep(1, length(color_values))
      names(linetype_values) <- names(color_values)
      linetype_values[names(linetype_values) == "Post Hoc Weighted Mean"] <- 4
      # shape_values <- shape_values[names(shape_values) %in% unique(data_all_models$model_display)]
      data_all_models <- data_all_models %>%
        dplyr::mutate(
          model_display = factor(model_display, levels = names(color_values)),
          estimation_timing = factor(
            ifelse(
              model_display == "Post Hoc Weighted Mean",
              "Post Hoc",
              "Real Time"
            ),
            levels = c("Real Time", "Post Hoc")
          )
        )
      data_key_models <- data_key_models %>%
        dplyr::mutate(
          model_display = factor(model_display, levels = names(color_values)),
          estimation_timing = factor(
            ifelse(
              model_display == "Post Hoc Weighted Mean",
              "Post Hoc",
              "Real Time"
            ),
            levels = c("Real Time", "Post Hoc")
          )
        )
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
            # linetype = model_display,
            # shape = model_display,
            # alpha = model_display,
            group = model),
          size = 0.25,
          show.legend = FALSE
        ) +
        geom_line(
          data = data_key_models %>%
            dplyr::mutate(facet_var = facet_label) %>%
            dplyr::filter(!(model_display %in% c("Post Hoc Weighted Mean",
                                                 "Rel. WIS Weighted Median",
                                                 "Equal Weighted Median"))),
          mapping = aes(
            x = forecast_week_end_date,
            y = rel_wis,
            # y = log_rel_wis,
            color = model_display,
            linetype = model_display,
            # linetype = estimation_timing,
            # linetype = factor((model_display %in% c("Rel. WIS Weighted Median"))),
            # linetype = factor((model_display %in% c("Rel. WIS Weighted Median",
            #                                          "Equal Weighted Median"))),
            # shape = model_display,
            # alpha = model_display,
            group = model),
          size = 0.85
        ) +
        geom_line(
          data = data_key_models %>%
            dplyr::mutate(
              facet_var = facet_label
            ) %>%
            dplyr::filter(model_display %in% c("Post Hoc Weighted Mean",
                                               "Rel. WIS Weighted Median",
                                               "Equal Weighted Median")),
          mapping = aes(
            x = forecast_week_end_date,
            y = rel_wis,
            # y = log_rel_wis,
            color = model_display,
            linetype = model_display,
            # linetype = estimation_timing,
            # linetype = factor((model_display %in% c("Rel. WIS Weighted Median"))),
            # linetype = factor((model_display %in% c("Rel. WIS Weighted Median",
            #                                          "Equal Weighted Median"))),
            # shape = model_display,
            # alpha = model_display,
            group = model),
          size = 0.85
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
        scale_linetype_manual(
          "Forecaster",
          values = linetype_values
          # "Estimation Timing",
          # values = c("Real Time" = 1, "Post Hoc" = 2),
          # values = c(1, 2)
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
          limits = c(as.Date("2020-07-15"), as.Date(last_monday)),
          date_breaks = "1 month",
          date_labels = "%b %Y") +
        coord_cartesian(ylim = c(0.11, 7.5)) +
        guides(
          fill = "none"#,
          # linetype = "none"
        ) +
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
      data_all_models = rel_wis_by_forecast_date %>%
        dplyr::filter(target_var == !!target_var,
                      forecast_week_end_date >= target_var_start_date - 2),
      data_key_models = rel_wis_by_forecast_date_key_models %>%
        dplyr::filter(target_var == !!target_var,
                      forecast_week_end_date >= target_var_start_date - 2),
      facet_label = "Relative WIS, Forecasts Issued on Forecast Date",
      facet_by_target_var = FALSE
    )

    p_rel_wis_trailing_12_weeks <- make_p_rel_wis(
      data_all_models = rel_wis_trailing_12_week %>%
        dplyr::filter(
          forecast_week_end_date >= target_var_start_date - 2,
          target_var == !!target_var),
      data_key_models = rel_wis_trailing_12_week_key_models %>%
        dplyr::filter(
          forecast_week_end_date >= target_var_start_date - 2,
          target_var == !!target_var),
      facet_label = "Relative WIS, Trailing 12 Weeks Available as of Forecast Date",
      facet_by_target_var = TRUE
    )

    p_weights <- ggplot(
      data = weight_estimates %>%
        dplyr::filter(forecast_date <= last_monday,
                      forecast_date >= target_var_start_date,
                      target_var == !!target_var) %>%
        dplyr::mutate(
          facet_var = factor(
            "Component Weights",
            levels = c("Relative WIS", "Component Weights", "Theta")
          ),
          model_target = paste(model, target_var, sep = "_")
        )
    ) +
      geom_col(
        mapping = aes(x = forecast_date, y = weight, fill = model_target),
        color = "black") +
      scale_fill_manual(
        "Forecaster",
        values = fill_values
      ) +
      # facet_wrap( ~ facet_var, ncol = 1, scales = "free_y") +
      # facet_wrap( ~ target_var, nrow = 1) +
      ylab("") +
      xlab("Forecast Date") +
      scale_x_date(
        limits = c(as.Date("2020-07-15"), as.Date(last_monday)),
        date_breaks = "1 month",
        date_labels = "%b %Y") +
      guides(fill = "none") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
        plot.margin = unit(c(0, 0, 0, -0.4), rep("cm", 4))
      )

    p_post_hoc_weights <- ggplot(
      data = post_hoc_weight_estimates %>%
        dplyr::filter(forecast_date <= last_monday,
                      forecast_date >= target_var_start_date,
                      target_var == !!target_var) %>%
        dplyr::mutate(
          facet_var = factor(
            "Component Weights",
            levels = c("Relative WIS", "Component Weights", "Theta")
          ),
          model_target = paste(model, target_var, sep = "_")
        )
    ) +
      geom_col(
        mapping = aes(x = forecast_date, y = weight, fill = model_target),
        color = "black") +
      scale_fill_manual(
        "Forecaster",
        values = fill_values
      ) +
      # facet_wrap( ~ facet_var, ncol = 1, scales = "free_y") +
      # facet_wrap( ~ target_var, nrow = 1) +
      ylab("") +
      xlab("Forecast Date") +
      scale_x_date(
        limits = c(as.Date("2020-07-15"), as.Date(last_monday)),
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
      axis.title.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank()
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
    p_post_hoc_weights <- p_post_hoc_weights + theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_blank(),
      axis.text.x = element_blank(),
      axis.title.x = element_blank()
    )
    # p_theta <- p_theta + theme(
    #   legend.position = "none"
    # )

    pdf(
      paste0(
        'manuscript/figures/ensemble_rel_wis_and_component_weights_',
        ifelse(spatial_resolution == "state", "US", "EU"), '_',
        # "Cases_and_Deaths",
        target_var,
        '.pdf'
      ),
      width = 8,
      height = 9)
      # height = 8)
    if (legend_pos == "right") {
      plot_layout <- grid.layout(
        # nrow = 5, ncol = 5,
        nrow = 7, ncol = 5,
        widths = unit(c(1, 1, 0.013, 1, 0.4), c("lines", "lines", "null", "null", "null")),
        heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
        # widths = unit(c(1, 0.02, 1, 0.4), c("lines", "null", "null", "null")),
        # heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
    } else {
      plot_layout <- grid.layout(
        nrow = 9, ncol = 5,
        # nrow = 6, ncol = 5,
        widths = unit(c(1, 1, 0.009, 0.01, 1), c("lines", "lines", "null", "null", "null")),
        heights = unit(c(1, 0.95, 0.02, 0.95, 0.02, 0.95, 0.02, 1.3, 3), c("lines", rep("null", 7), "lines")))
        # heights = unit(c(1, 0.02, 0.95, 0.02, 1.3, 3), c(rep("null", 5), "lines")))
        # widths = unit(c(1, 0.02, 1, 0.4), c("lines", "null", "null", "null")),
        # heights = unit(c(1, 0.02, 1, 0.02, 1.3), rep("null", 5)))
    }
    grid.newpage()
    pushViewport(viewport(layout = plot_layout))

    n_title_rows = 1
    grid.text(
      paste0("       ", target_var, ": Forecaster Relative WIS and Ensemble Weights"),
      x = unit(0.0, "npc"),
      just = "left",
      gp = gpar(fontsize = 12),
      vp = viewport(layout.pos.row = 1, layout.pos.col = 3:5))
    grid.text(
      "   Forecast Date   ",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 1, layout.pos.col = 1))
    grid.text(
      "   Relative WIS (log scale)   ",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 1, layout.pos.col = 2))

    grid.text(
      "Trailing 12 Weeks  ",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 3, layout.pos.col = 1))
    grid.text(
      "Relative WIS (log scale)",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 3, layout.pos.col = 2))

    grid.text(
      "  Post Hoc Weighted Mean",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 5, layout.pos.col = 1))
    grid.text(
      # "               Component Forecaster Weight",
      "  Component Weight",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 5, layout.pos.col = 2))
    grid.text(
      "               Rel. WIS Weighted Median",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 7, layout.pos.col = 1))
    grid.text(
      # "               Component Forecaster Weight",
      "               Component Weight",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = n_title_rows + 7, layout.pos.col = 2))

    # grid.text(
    #   expression(paste("              Weighting Parameter ", theta)),
    #   just = "center",
    #   rot = 90,
    #   gp = gpar(fontsize = 11),
    #   vp = viewport(layout.pos.row = 5, layout.pos.col = 1))


    print(p_rel_wis_by_forecast_date,
      vp = viewport(layout.pos.row = n_title_rows + 1, layout.pos.col = 4:5))
    print(p_rel_wis_trailing_12_weeks,
      vp = viewport(layout.pos.row = n_title_rows + 3, layout.pos.col = 4:5))
    print(p_post_hoc_weights,
      vp = viewport(layout.pos.row = n_title_rows + 5, layout.pos.col = 3:5))
    print(p_weights,
      vp = viewport(layout.pos.row = n_title_rows + 7, layout.pos.col = 3:5))
      # vp = viewport(layout.pos.row = 5, layout.pos.col = 3:5))

    if (legend_pos == "right") {
      print(
        as_ggplot(legend),
        vp = viewport(layout.pos.row = n_title_rows + 3, layout.pos.col = 5)
      )
    } else {
      print(
        as_ggplot(legend),
        vp = viewport(layout.pos.row = n_title_rows + 8, layout.pos.col = 1:5)
        # vp = viewport(layout.pos.row = 6, layout.pos.col = 3:5)
      )
    }
    dev.off()
  }
# }

# information for text about model weights
weight_estimates %>%
  dplyr::filter(target_var == "Cases") %>%
  dplyr::group_by(model) %>%
  dplyr::summarize(min_weight = min(weight), max_weight = max(weight)) %>%
  dplyr::arrange(min_weight) %>%
  as.data.frame()


n_to_cum_weight <- function(q, cum_weight) {
  min(which(cumsum(sort(q, decreasing = TRUE)) > cum_weight))
}

p_cum_weight_model_counts <- dplyr::bind_rows(
  post_hoc_weight_estimates %>%
    dplyr::group_by(target_var, forecast_date) %>%
    dplyr::summarize(
      n_to_weight = n_to_cum_weight(weight, cum_weight = 0.25),
      cum_weight = 0.25
    ),
  post_hoc_weight_estimates %>%
    dplyr::group_by(target_var, forecast_date) %>%
    dplyr::summarize(
      n_to_weight = n_to_cum_weight(weight, cum_weight = 0.5),
      cum_weight = 0.5
    ),
  post_hoc_weight_estimates %>%
    dplyr::group_by(target_var, forecast_date) %>%
    dplyr::summarize(
      n_to_weight = n_to_cum_weight(weight, cum_weight = 0.75),
      cum_weight = 0.75
    ),
) %>%
  ggplot() +
    geom_bar(mapping = aes(x = n_to_weight, y = ..prop..)) +
    facet_grid(
      target_var ~ cum_weight,
      labeller = labeller(.cols = function(x) { paste0("Cumulative Weight ", x) })) +
    xlab("Number of models required to reach cumulative weight") +
    ylab("Proportion of forecast dates") +
    theme_bw()

lag_1_ac <- function(x) {
  temp <- acf(x, na.action = na.pass, plot = FALSE)$acf
  return(temp[2, 1, 1])
}

post_hoc_weight_ac <- post_hoc_weight_estimates %>%
  tidyr::pivot_wider(names_from = "model", values_from = "weight") %>%
  dplyr::group_by(target_var) %>%
  dplyr::summarise(across(-forecast_date, lag_1_ac)) %>%
  tidyr::pivot_longer(-target_var, names_to = "model", values_to = "lag_1_ac")

rel_wis_weight_ac <- weight_estimates %>%
  tidyr::pivot_wider(names_from = "model", values_from = "weight") %>%
  dplyr::group_by(target_var) %>%
  dplyr::summarise(across(-forecast_date, lag_1_ac)) %>%
  tidyr::pivot_longer(-target_var, names_to = "model", values_to = "lag_1_ac")

combined_weight_ac <- dplyr::bind_rows(
  post_hoc_weight_ac %>% dplyr::mutate(weighting = "Post Hoc Weighted Mean"),
  rel_wis_weight_ac %>% dplyr::mutate(weighting = "Rel. WIS Weighted Median"),
)
combined_weight_ac_medians <- combined_weight_ac %>%
  dplyr::group_by(target_var, weighting) %>%
  dplyr::summarise(median_lag_1_ac = median(lag_1_ac, na.rm = TRUE))


p_ac <- ggplot() +
    geom_vline(xintercept = 0, color = "darkgrey") +
    geom_histogram(
      data = combined_weight_ac,
      mapping = aes(x = lag_1_ac, y = ..density..)) +
    geom_vline(
      data = combined_weight_ac_medians,
      mapping = aes(xintercept = median_lag_1_ac), color = "red", linetype = 2) +
    facet_grid(weighting ~ target_var) +
    # facet_grid(weighting ~ target_var) +
    xlim(-1, 1) +
    xlab("Lag 1 autocorrelation of component forecaster weights") +
    theme_bw()


pdf(
  'manuscript/figures/compare_weight_distributions_and_lag_1_ac.pdf',
  width = 8,
  height = 9)

plot_layout <- grid.layout(
  nrow = 4, ncol = 1,
  # widths = unit(c(1, 1, 0.009, 0.01, 1), c("lines", "lines", "null", "null", "null")),
  heights = unit(c(2, 1, 2, 1), c("lines", "null", "lines", "null")))
grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text(" (a) Component forecaster weight concentration for post hoc weighted mean ensemble",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))

grid.text(" (b) Lag 1 autocorrelation of component forecaster weights",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))

print(p_cum_weight_model_counts,
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_ac,
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

dev.off()

# total number of models included at any point


