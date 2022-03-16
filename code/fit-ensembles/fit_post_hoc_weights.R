library(covidData)
library(covidEnsembles)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(zeallot)
library(gridExtra)
library(yaml)
library(reticulate)
Sys.setenv(CUDA_VISIBLE_DEVICES = '-1')
Sys.setenv(LANG = "en_US.UTF-8")


#options(warn=2, error=recover)

# extract arguments specifying details of analysis
#args <- c("local", "inc_death", "2021-04-05", "FALSE", "rel_wis_weighted_median", "renormalize", "per_quantile", "sort", "4", "0", "TRUE", "FALSE", "FALSE", "state", "FALSE", "all")
#args <- c("cluster_single_node", "inc_death", "2020-07-27", "FALSE", "convex", "renormalize", "per_model", "sort", "12", "0", "TRUE", "FALSE", "FALSE", "state", "FALSE", "all")
#args <- c("local", "inc_death", "2021-04-05", "FALSE", "rel_wis_weighted_median", "renormalize", "per_model", "sort", "4", "10", "TRUE", "FALSE", "FALSE", "state", "TRUE", "all")
#args <- c("local", "inc_case", "2021-06-07", "FALSE", "rel_wis_weighted_median", "renormalize", "per_model", "sort", "12", "5", "TRUE", "FALSE", "FALSE", "euro_countries", "TRUE", "all")
#args <- c("local", "inc_death", "2021-03-01", "FALSE", "ew", "renormalize", "per_model", "sort", "12", "0", "TRUE", "FALSE", "FALSE", "state", "FALSE", "all")
#args <- c("local", "inc_death", "2021-03-15")

args <- commandArgs(trailingOnly = TRUE)
run_setting <- args[1]
response_var <- args[2]
forecast_date <- lubridate::ymd(args[3])

# other settings that we do not need to vary for this analysis
intercept <- FALSE
combine_method <- "convex"
missingness <- "renormalize"
quantile_group_str <- "per_model"
noncross <- "sort"
window_size_arg <- "0"
top_models_arg <- "all"
check_missingness_by_target <- TRUE
do_standard_checks <- FALSE
do_baseline_check <- FALSE
drop_anomalies <- FALSE
horizon_group <- "all"
spatial_resolution_arg <- "state"

if (run_setting == "local") {
  # used by covidHubUtils::load_latest_forecasts for loading locally
  hub_repo_path <- paste0(
    "~/research/epi/covid/",
    ifelse(
      spatial_resolution_arg == "euro_countries",
      "covid19-forecast-hub-europe",
      "covid19-forecast-hub"),
    "/")
  submissions_root <- paste0(hub_repo_path, "data-processed/")
} else {
  hub_repo_path <- paste0(
    "/project/uma_nicholas_reich/",
    ifelse(
      spatial_resolution_arg == "euro_countries",
      "covid19-forecast-hub-europe",
      "covid19-forecast-hub"),
    "/")
  submissions_root <- paste0(hub_repo_path, "data-processed/")
  reticulate::use_python("/usr/bin/python3.8")
}

# List of candidate models for inclusion in ensemble
if (spatial_resolution_arg == "euro_countries") {
  candidate_model_abbreviations_to_include <-
    list.dirs(submissions_root, full.names=FALSE, recursive=FALSE)
} else {
  candidate_model_abbreviations_to_include <- get_candidate_models(
    submissions_root = submissions_root,
    include_designations = c("primary", "secondary"),
    include_COVIDhub_ensemble = FALSE,
    include_COVIDhub_baseline = TRUE)
}

# Drop models that are themselves ensembles of other hub models
candidate_model_abbreviations_to_include <-
  candidate_model_abbreviations_to_include[
    !(candidate_model_abbreviations_to_include %in%
      c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub-trained_ensemble", "COVIDhub-4_week_ensemble", "COVIDhub_CDC-ensemble", "KITmetricslab-select_ensemble", "EuroCOVIDhub-ensemble"))
  ]

if (missingness == "mean_impute") {
  case_missingness <- "impute"
  missingness <- "impute"
  impute_method <- "mean"
} else if (missingness == "renormalize" || missingness == "none") {
  case_missingness <- missingness
  missingness <- "impute"
  impute_method <- "none"
} else {
  case_missingness <- missingness
  impute_method <- NULL
}

if (response_var == "inc_death") {
  required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("state", "national")
  } else if (spatial_resolution_arg == "state_national") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "wk"
  if (horizon_group == "all") {
    max_horizon <- 4L
    targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", response_var))
  } else {
    max_horizon <- as.integer(horizon_group)
    targets <- paste0(max_horizon, " wk ahead ", gsub("_", " ", response_var))
  }
  forecast_week_end_date <- forecast_date - 2
  full_history_start <- lubridate::ymd("2020-06-22") - 7 * 10
} else if (response_var == "inc_case") {
  if (spatial_resolution_arg == "euro_countries") {
    required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
  } else {
    required_quantiles <- c(0.025, 0.100, 0.250, 0.500, 0.750, 0.900, 0.975)
  }

  if (spatial_resolution_arg == "all") {
    spatial_resolution <- c("county", "state", "national")
  } else if (spatial_resolution_arg == "state_national") {
    spatial_resolution <- c("state", "national")
  } else {
    spatial_resolution <- spatial_resolution_arg
  }
  temporal_resolution <- "wk"
  if (horizon_group == "all") {
    max_horizon <- 4L
    targets <- paste0(1:max_horizon, " wk ahead ", gsub("_", " ", response_var))
  } else {
    max_horizon <- as.integer(horizon_group)
    targets <- paste0(max_horizon, " wk ahead ", gsub("_", " ", response_var))
  }
  forecast_week_end_date <- forecast_date - 2
  full_history_start <- lubridate::ymd("2020-09-14") - 7 * 10
}

if (window_size_arg == "full_history") {
  window_size <- as.integer((forecast_date - full_history_start) / 7)
} else {
  window_size <- as.integer(window_size_arg)
}

if (quantile_group_str == "per_model") {
  quantile_groups <- rep(1, length(required_quantiles))
} else if(quantile_group_str == "3_groups") {
  if (length(required_quantiles) == 23) {
    quantile_groups <- c(rep(1, 4), rep(2, 23 - 8), rep(3, 4))
  } else if (length(required_quantiles) == 7) {
    quantile_groups <- c(1, rep(2, 5), 3)
  }
} else if(quantile_group_str == "per_quantile") {
  quantile_groups <- seq_along(required_quantiles)
} else {
  stop("invalid quantile_groups")
}

if (spatial_resolution_arg == "all") {
  spatial_resolution_path <- ""
} else if (spatial_resolution_arg == "state_national") {
  spatial_resolution_path <- "state_national"
} else {
  spatial_resolution_path <- spatial_resolution
}

if (top_models_arg == "all_models") {
  top_models <- 0L
} else {
  top_models <- as.integer(top_models_arg)
}

case_str <- "post_hoc_weights"

# create folder where model fits should be saved
fits_dir <- file.path(
  "code/retrospective-fits",
  spatial_resolution_path,
  response_var,
  case_str)
if (!dir.exists(fits_dir)) {
  dir.create(fits_dir, recursive = TRUE)
}
fit_filename <- paste0(
  fits_dir, "/",
  forecast_date, "-",
  case_str, ".rds")
partial_save_filename <- paste0(
  fits_dir, "/",
  forecast_date, "-",
  case_str, ".pkl")
loss_trace_filename <- paste0(
  fits_dir, "/",
  forecast_date, "-",
  case_str, "_loss_trace.rds")

# create folder where model weights should be saved
weights_dir <- file.path(
  "code/retrospective-weights",
  spatial_resolution_path,
  response_var,
  case_str)
if (!dir.exists(weights_dir)) {
  dir.create(weights_dir, recursive = TRUE)
}
weight_filename <- paste0(
  weights_dir, "/",
  forecast_date, "-",
  case_str, ".csv")

# create folder where model forecasts should be saved
forecasts_dir <- file.path(
  "code/retrospective-forecasts",
  spatial_resolution_path,
  response_var,
  case_str)
if (!dir.exists(forecasts_dir)) {
  dir.create(forecasts_dir, recursive = TRUE)
}
forecast_filename <- paste0(
  forecasts_dir, "/",
  forecast_date, "-",
  case_str, ".csv")


tic <- Sys.time()
#if (!file.exists(forecast_filename)) {
if (TRUE) {
  do_q10_check <- do_nondecreasing_quantile_check <- do_standard_checks

  # Dates specifying mondays when forecasts were submitted that are relevant to
  # this analysis: forecast_date and the previous window_size weeks
  monday_dates <- forecast_date

  all_locations <- covidHubUtils::hub_locations %>%
    dplyr::filter(geo_type == "state", fips != "US") %>%
    dplyr::pull(fips)

  # load forecasts for those locations
  forecasts <- covidEnsembles::load_covid_forecasts_relative_horizon(
    hub = "US",
    source = "local_hub_repo",
    hub_repo_path = hub_repo_path,
    monday_dates = monday_dates,
    as_of = NULL,
    model_abbrs = candidate_model_abbreviations_to_include,
    timezero_window_size = 6,
    locations = all_locations,
    targets = targets,
    max_horizon = max_horizon,
    required_quantiles = required_quantiles
  )

  # Get observed values ("truth" in Zoltar's parlance)
  # ... for locations having forecasts.
  observed_by_location_target_end_date <-
    get_observed_by_location_target_end_date(
      as_of = "2022-03-13",
      targets = targets,
      spatial_resolution = spatial_resolution,
      locations = unique(forecasts$location)
    )

  # obtain model eligibility by location
  # since we have not yet filtered by horizon/target, eligibility is based on
  # all four targets 1 - 4 wk ahead cum deaths
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts,
    model_col = 'model',
    id_cols = c('location', 'forecast_week_end_date', 'target'),
    quantile_name_col = 'quantile',
    quantile_value_col = 'value'
  )

  # consider refactoring to handle similar to covidHubUtils
  # (this could be unit tested)
  forecast_base_targets <- substr(
    forecasts$target,
    regexpr(' ', forecasts$target) + 1,
    nchar(forecasts$target)
  )
  model_eligibility <- covidEnsembles::calc_model_eligibility_for_ensemble(
    qfm = forecast_matrix,
    observed_by_location_target_end_date =
      observed_by_location_target_end_date %>%
        dplyr::filter(base_target %in% forecast_base_targets),
    missingness_by_target = check_missingness_by_target,
    do_q10_check = do_q10_check,
    do_nondecreasing_quantile_check = do_nondecreasing_quantile_check,
    do_baseline_check = do_baseline_check,
    do_sd_check = "exclude_none",
    sd_check_table_path = sd_check_table_path,
    sd_check_plot_path = sd_check_plot_path,
    baseline_tol = baseline_tol,
    window_size = window_size,
    decrease_tol = 0.0
  )

  # keep only model-location-targets that are eligible
  # here this is done by filtering the original forecasts data frame and
  # recreating the QuantileForecastMatrix
  forecasts <- forecasts %>%
    dplyr::left_join(
      model_eligibility %>%
        dplyr::transmute(
          model = model,
          location = location,
          forecast_week_end_date = forecast_week_end_date,
          target = target,
          eligibility = (overall_eligibility == "eligible")),
      by = c("model", "location", "forecast_week_end_date", "target")) %>%
    dplyr::filter(eligibility) %>%
    dplyr::select(-eligibility)
  
  forecast_matrix <- covidEnsembles::new_QuantileForecastMatrix_from_df(
    forecast_df = forecasts,
    model_col = "model",
    id_cols = c("location", "forecast_week_end_date", "target"),
    quantile_name_col = "quantile",
    quantile_value_col = "value"
  )

  # drop rows with no eligible models
  rows_to_keep <- apply(forecast_matrix, 1, function(qfm_row) any(!is.na(qfm_row))) %>%
    which()

  if (length(rows_to_keep) != nrow(forecast_matrix)) {
    forecast_matrix <- forecast_matrix[rows_to_keep, ]
  }

  # qfm_train and qfm_test are the same; copies of forecast_matrix
  qfm_train <- qfm_test <- forecast_matrix

  # observed responses to date
  y_train <- attr(qfm_train, 'row_index') %>%
    dplyr::mutate(
      target_end_date = as.character(
        lubridate::ymd(forecast_week_end_date) +
          as.numeric(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)) *
            ifelse(grepl("day", target), 1, 7)
      ),
      base_target = substr(target, regexpr(" ", target, fixed = TRUE) + 1, nchar(target))
    ) %>%
    dplyr::left_join(
      observed_by_location_target_end_date,
      by = c('location', 'target_end_date', 'base_target')
    ) %>%
    dplyr::pull(observed)

  # If any training data are missing, throw an error; the point is we're fitting post hoc weights...
  if (any(is.na(y_train))) {
    stop("Missing values in y_train")
  }

  # fit model
  qra_fit <- estimate_qra(
    qfm_train = qfm_train,
    y_train = y_train,
    qfm_test = qfm_test,
    intercept = intercept,
    combine_method = combine_method,
    quantile_groups = quantile_groups,
    noncross = noncross,
    backend = "qenspy",
    max_weight = 1.0,
    partial_save_frequency = Inf,
    partial_save_filename = "")

  # save estimated weights
  estimated_weights <- extract_weights_qenspy_qra_fit(qra_fit)
  write_csv(estimated_weights, weight_filename)

  # save csv formatted forecasts
  ensemble_predictions <- predict(qra_fit, qfm_test, sort_quantiles = TRUE) %>%
    as.data.frame()

  if (nrow(ensemble_predictions) > 0) {
    # save the results in required format
    formatted_ensemble_predictions <- ensemble_predictions %>%
      dplyr::transmute(
        forecast_date = forecast_date,
        target = target,
        target_end_date = covidHubUtils::calc_target_end_date(
          forecast_date,
          as.integer(substr(target, 1, regexpr(" ", target, fixed = TRUE) - 1)),
          rep(temporal_resolution, nrow(ensemble_predictions))),
        location = location,
        type = 'quantile',
        quantile = quantile,
        value = ifelse(
          quantile < 0.5,
          floor(value),
          ifelse(
            quantile == 0.5,
            round(value),
            ceiling(value)
          )
        )
      )

    formatted_ensemble_predictions <- bind_rows(
      formatted_ensemble_predictions,
      formatted_ensemble_predictions %>%
        filter(format(quantile, digits = 3, nsmall = 3) == "0.500") %>%
        mutate(
          type = "point",
          quantile = NA_real_
        )
    )

    write_csv(formatted_ensemble_predictions, forecast_filename)
  }
}
toc <- Sys.time()
toc - tic
