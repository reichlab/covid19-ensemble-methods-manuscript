# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

knitr::opts_chunk$set(echo = FALSE, cache.lazy = FALSE)
options(width = 200)
options(error = recover)

# Dates of forecast submission for forecasts included in this analysis
first_forecast_date <- lubridate::ymd("2020-07-27")
last_forecast_date <- lubridate::ymd("2021-10-11")
num_forecast_weeks <- as.integer(last_forecast_date -
                         first_forecast_date) / 7 + 1

forecast_dates <- first_forecast_date +
  seq(from = 0, length = num_forecast_weeks) * 7

for (spatial_scale in c("euro_countries", "state")) {
#for (spatial_scale in c("state")) {
  response_vars <- c("inc_death", "inc_case")

  for (response_var in response_vars) {
    all_scores <- calc_retrospective_ensemble_scores(
      submissions_root = "~/research/epi/covid/covid19-ensemble-methods-manuscript/code/retrospective-forecasts/",
      forecast_dates = forecast_dates,
      spatial_scales = spatial_scale,
    #  spatial_scales = "state",
    #  spatial_scales = c("national", "state", "state_national", "county"),
      response_vars = response_var,
    #  response_vars = "inc_case",
    #  response_vars = NULL,
    #  response_vars = c("inc_case", "inc_death", "cum_death", "inc_hosp"),
      truth_as_of = as.Date("2021-12-05")
    )

    unique_models <- unique(all_scores$model)
    model_cases <- purrr::map_dfr(
      unique_models,
      function(model) {
        parse_model_case(model) %>% mutate(model = model)
      }
    ) %>%
      dplyr::mutate(
        top_models = ifelse(
          is.na(top_models) | as.character(top_models) %in% c("0", "all"),
          "All Models",
          paste0('Top ', top_models)),
        horizon_group = ifelse(
          horizon_group == "all",
          "All Horizons",
          "By Horizon"
        ),
        window_size = dplyr::case_when(
          window_size == "0" ~ "Untrained",
          window_size == "full_history" ~ "Trained on full history",
          TRUE ~ paste0("Trained on ", window_size, " weeks")
        ),
        combine_method = dplyr::case_when(
          combine_method == "ew" ~ "Equal Weighted Mean",
          combine_method == "median" ~ "Equal Weighted Median",
          combine_method == "rel_wis_weighted_mean" ~ "Rel. WIS Weighted Mean",
          combine_method == "rel_wis_weighted_median" ~ "Rel. WIS Weighted Median",
          combine_method == "convex" ~ "Weighted Mean",
          combine_method == "mean_weights_weighted_median" ~ "Mean Weights Weighted Median"
        ),
        combine_method = factor(
          combine_method,
          levels = c("Equal Weighted Mean", "Equal Weighted Median",
            "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median",
            "Weighted Mean", "Mean Weights Weighted Median")),
        quantile_groups = dplyr::case_when(
          quantile_groups == "per_model" ~ "Per Model",
          quantile_groups == "per_quantile" ~ "Per Quantile",
          quantile_groups == "3_groups" ~ "3 Groups"
        )
      )

    all_scores <- all_scores %>%
      dplyr::filter(forecast_date >= "2020-07-27") %>%
      dplyr::left_join(model_cases, by = "model") %>%
      dplyr::mutate(
        target_variable = dplyr::case_when(
          target_variable == "inc case" ~ "Cases",
          target_variable == "inc death" ~ "Deaths",
          TRUE ~ NA_character_
        )
      )

    saveRDS(
      all_scores,
      paste0("code/scores/retrospective_scores-",
        spatial_scale, "-",
        response_var,
        ".rds")
    )
  }
}

all_scores <- dplyr::bind_rows(
  readRDS(
    paste0("code/scores/retrospective_scores-",
        "state-",
        "inc_death", ".rds")
  ),
  readRDS(
    paste0("code/scores/retrospective_scores-",
        "state-",
        "inc_case", ".rds")
  )
)

all_scores %>%
#  dplyr::filter(target_variable == "inc case") %>%
  dplyr::filter(forecast_date >= "2020-07-27") %>%
#  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    top_models = ifelse(top_models == "top_0", "all", top_models),
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>% #pull(model_brief) %>% unique()
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::count(model_brief, target_variable) %>%
  tidyr::pivot_wider(names_from = "target_variable", values_from = "n") %>%
  as.data.frame()

s1 <- all_scores %>%
  dplyr::mutate(
    top_models = ifelse(top_models == "top_0", "all", top_models),
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>%
  dplyr::filter(
    target_variable == "Deaths",
    forecast_date >= "2020-07-27",
    model_brief == "Equal Weighted Median-Per Model-Trained on 8 weeks-All Models"
  )

s2 <- all_scores %>%
  dplyr::mutate(
    top_models = ifelse(top_models == "top_0", "all", top_models),
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>%
  dplyr::filter(
    target_variable == "Deaths",
    forecast_date >= "2020-07-27",
    model_brief == "Equal Weighted Median-Per Model-Trained on 8 weeks-Top 10"
  )

all_scores %>%
  dplyr::filter(target_variable == "inc death") %>%
#  dplyr::left_join(model_cases, by = "model") %>%
  dplyr::mutate(
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>%
#  dplyr::filter(model_brief %in% models_to_keep) %>%
  dplyr::count(model_brief, forecast_date) %>%
  tidyr::pivot_wider(names_from = "forecast_date", values_from = "n") %>%
  as.data.frame()
# for inc death, all problems are on or before 2020-07-20; can subset to 2020-07-27 or greater

