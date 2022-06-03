# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)
library(knitr)
library(here)
library(scales)

setwd(here())


# subset_scores_to_common <- function(scores) {
#   id_counts_by_model <- scores %>%
#     dplyr::mutate(
#       combined_id = paste0(location, forecast_date, target_end_date)
#     ) %>%
#     dplyr::count(model_brief, combined_id) %>%
#     tidyr::pivot_wider(names_from = "model_brief", values_from = "n")
#   combined_ids_to_drop <- id_counts_by_model[["combined_id"]][
#     id_counts_by_model %>%
#       dplyr::select(-combined_id) %>%
#       as.matrix() %>%
#       apply(1, function(x) { any(is.na(x)) })
#   ]
#   scores <- scores %>%
#     dplyr::filter(
#       !(paste0(location, forecast_date, target_end_date) %in% combined_ids_to_drop)
#     )
#   return(scores)
# }

# # read in scores for US
# all_scores_us <- dplyr::bind_rows(
#   readRDS("code/scores/retrospective_scores-state-inc_death.rds"),
#   readRDS("code/scores/retrospective_scores-state-inc_case.rds")
# ) %>%
#   dplyr::filter(
#     true_value >= 0,
#     horizon_group == "All Horizons", quantile_groups == "Per Model",
#     drop_anomalies == FALSE,
#     # first subset to considered combine methods for main figure
#     combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median",
#                           "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median",
#                           "Arith. Rel. WIS Weighted Mean", "Arith. Rel. WIS Weighted Median"),
#     # trained methods: all variations other than equal weighted, top 10
#     # untrained methods: untrained, equal weighted
#     (window_size == "Trained on 12 weeks" &
#       !(top_models == "All Models" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median"))) | # & combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) |
#     (window_size == "Untrained" &
#       combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median")),
#     # Keep subset of models for prospective evaluation
#     (forecast_date < "2021-05-03" |
#       (top_models == "Top 10" &
#         combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median",
#           "Arith. Rel. WIS Weighted Mean", "Arith. Rel. WIS Weighted Median")) |
#       (top_models == "All Models" &
#         window_size == "Untrained" &
#         combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median")))
#   ) %>%
#   dplyr::mutate(
#     # window_size = factor(
#     #   window_size,
#     #   levels = c("Untrained", "Trained on 4 weeks", "Trained on 8 weeks", "Trained on 12 weeks", "Trained on full history")
#     # ),
#     combine_method = factor(
#       combine_method,
#       levels = c("Equal Weighted Mean", "Equal Weighted Median",
#                  "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median",
#                  "Arith. Rel. WIS Weighted Mean", "Arith. Rel. WIS Weighted Median")),
#     model_brief = paste0(combine_method, "-",
#       quantile_groups, "-",
#       window_size, "-",
#       top_models, "-",
#       horizon_group),
#     phase = ifelse(forecast_date >= "2021-05-03", "Prospective Evaluation: US", "Model Development: US")
#   )

# # Subset to relevant comparators
# subset_scores_us <- dplyr::bind_rows(
#   all_scores_us %>%
#     dplyr::filter(
#       window_size == "Trained on 12 weeks",
#       top_models == "Top 10",
#       drop_anomalies == "FALSE",
#       horizon_group == "All Horizons",
#       phase == "Prospective Evaluation: US"),
#   all_scores_us %>%
#     dplyr::filter(
#       model %in% c("combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state",
#                    "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state"),
#       phase == "Prospective Evaluation: US"
#     )
# )

# # ensure that we have the same scores for all model variations
# orig_score_count <- nrow(subset_scores_us)
# all_scores_us <- dplyr::bind_rows(
#   subset_scores_to_common(
#     subset_scores_us %>% dplyr::filter(phase == "Model Development: US")),
#   subset_scores_to_common(
#     subset_scores_us %>% dplyr::filter(phase == "Prospective Evaluation: US"))
# )
# common_score_count <- nrow(subset_scores_us)
# orig_score_count == common_score_count



# score_diffs <- all_scores_us %>%
#   dplyr::filter(combine_method %in% c("Arith. Rel. WIS Weighted Mean", "Arith. Rel. WIS Weighted Median", "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) %>%
#   dplyr::select(location, target_variable, forecast_date, target_end_date, wis, combine_method) %>%
#   tidyr::pivot_wider(names_from = "combine_method", values_from = "wis") %>%
#   dplyr::mutate(
#     diff_mean = `Arith. Rel. WIS Weighted Mean` - `Rel. WIS Weighted Mean`,
#     diff_median = `Arith. Rel. WIS Weighted Median` - `Rel. WIS Weighted Median`
#   ) %>%
#   dplyr::select(
#     location, target_variable, forecast_date, target_end_date, diff_mean, diff_median
#   ) %>%
#   tidyr::pivot_longer(cols = c("diff_mean", "diff_median"), names_to = "model_pair", values_to = "wis_diff")

# score_diffs %>%
#   group_by(model_pair, target_variable) %>%
#   summarize(
#     mean_wis_diff = mean(wis_diff),
#     median_wis_diff = median(wis_diff)
#   )

# dummy_data <- score_diffs %>%
#   dplyr::filter(model_pair == "diff_median") %>%
#   dplyr::group_by(target_variable) %>%
#   dplyr::summarize(wis_diff = max(abs(wis_diff)))
# dummy_data <- dplyr::bind_rows(
#   dummy_data,
#   dummy_data %>% dplyr::mutate(wis_diff = -wis_diff)
# )

# p <- ggplot(
#     data = score_diffs %>%
#       dplyr::filter(model_pair == "diff_median")
#       # dplyr::mutate(
#       #   model_pair = ifelse(
#       #     model_pair == "diff_median",
#       #     "Weighted Median Ensembles",
#       #     "Weighted Mean Ensembles")
#       # )
#   ) +
#   geom_boxplot(mapping = aes(y = wis_diff)) +
#   geom_blank(data = dummy_data, mapping = aes(y=wis_diff)) +
#   facet_wrap( ~ target_variable, scales = "free_y") +
#   # xlab("Model Pair") +
#   ylab("WIS, Arithmetic Mean Rel. WIS Weighted Median\nminus WIS, Geometric Mean Rel. WIS Weighted Median") +
#   theme_bw()


# pdf('manuscript/figures/wis_boxplots_arith_vs_geom_rel_wis_US.pdf', width=8, height=8)
# print(p)
# dev.off()


# # Investigate the forecast dates with the most extreme differences
# score_diffs %>%
#   dplyr::filter(model_pair == "diff_median") %>%
#   dplyr::slice_max(wis_diff, n = 6)

# score_diffs %>%
#   dplyr::filter(model_pair == "diff_median") %>%
#   dplyr::slice_min(wis_diff, n = 6)

# mondays <- c(
#   score_diffs %>%
#     dplyr::filter(model_pair == "diff_median") %>%
#     dplyr::slice_min(wis_diff, n = 1) %>%
#     dplyr::pull(forecast_date),
#   score_diffs %>%
#     dplyr::filter(model_pair == "diff_median") %>%
#     dplyr::slice_max(wis_diff, n = 1) %>%
#     dplyr::pull(forecast_date)
# )


first_forecast_date <- lubridate::ymd("2020-07-27")
last_forecast_date <- lubridate::ymd("2022-03-14")
num_forecast_weeks <- as.integer(last_forecast_date -
                         first_forecast_date) / 7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

hub <- "US"
hub_repo_path <- "../covid19-forecast-hub/"
submissions_root <- paste0(hub_repo_path, "data-processed/")

pwd <- setwd(hub_repo_path)
system("git checkout 3532bcba304cef2b4872dd2add1f83909f717d91")
setwd(pwd)

model_abbrs <-get_candidate_models(
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

# Get observed values ("truth" in Zoltar's parlance) for cases
# across all data as of dates needed below
observed_by_location_target_end_date <- purrr::pmap_dfr(
  tidyr::expand_grid(
    target_var = c("inc_case"),
    as_of_date_val = lubridate::ymd(forecast_dates) - 1),
  function(target_var, as_of_date_val) {
    get_observed_by_location_target_end_date(
      as_of = as_of_date_val,
      targets = paste0(1:4, " wk ahead ", gsub("_", " ", target_var)),
      spatial_resolution = "state"
    ) %>%
    dplyr::mutate(as_of = as_of_date_val)
  })

# Get forecasts
forecasts <- purrr::map_dfr(
  c("inc_case"),
  function(target_var) {
    if (target_var == "inc_death") {
      required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      temporal_resolution <- "wk"
      max_horizon <- 4L
      targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
    } else if (target_var == "inc_case") {
      required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)

      temporal_resolution <- "wk"
      max_horizon <- 4L
      targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", target_var))
    }

    # load component forecasts from hub repo
    forecasts <- load_covid_forecasts_relative_horizon(
      hub = hub,
      source = "local_hub_repo",
      hub_repo_path = hub_repo_path,
      monday_dates = forecast_dates,
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




rel_wis_trailing_12_week <- purrr::pmap_dfr(
  tidyr::expand_grid(
    target_var = "inc_case",
    agg_method = c("geom_mean", "mean")
  ),
  function(target_var, agg_method) {
    purrr::map_dfr(
      lubridate::ymd(forecast_dates) - 2,
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
            baseline = ifelse(hub == "US", "COVIDhub-baseline", "EuroCOVIDhub-baseline"),
            agg_method = agg_method) %>%
          dplyr::mutate(forecast_week_end_date = fwed, agg_method = agg_method) %>%
          `rownames<-`(NULL)
        
        return(fwed_rel_wis)
      }) %>%
      dplyr::filter(!is.na(rel_wis)) %>%
      dplyr::mutate(
        target_var = ifelse(target_var == "inc_case", "Cases", "Deaths")
      )
  })

p <- rel_wis_trailing_12_week %>%
  tidyr::pivot_wider(names_from = "agg_method", values_from = "rel_wis") %>%
  as.data.frame() %>%
  ggplot() +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(mapping = aes(x = mean, y = geom_mean)) +
    xlab("Relative WIS, Aggregating by Arithmetic Mean") +
    ylab("Relative WIS, Aggregating by Geometric Mean") +
    theme_bw()

pdf("manuscript/figures/compare_wis_agg_methods.pdf", width = 8, height = 8)
print(p)
dev.off()
