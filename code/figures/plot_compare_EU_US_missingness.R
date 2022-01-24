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

# function to load estimated component weights for a particular
# spatial resolution (i.e., hub setting) and target variable
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


data_forecasts_weights <- purrr::pmap(
  tidyr::expand_grid(
    target_var = c("inc_case", "inc_death"),
    # target_var = c("inc_death"),
    spatial_resolution = c("state", "euro_countries")
  ),
  function(target_var, spatial_resolution) {
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
          c("JHUAPL-SLPHospEns", "FDANIHASU-Sweight", "COVIDhub_CDC-ensemble",
            "COVIDhub-ensemble", "COVIDhub-trained_ensemble",
            "COVIDhub-4_week_ensemble", "KITmetricslab-select_ensemble",
            "EuroCOVIDhub-ensemble"))
      ]

    start_monday <- as.Date("2020-07-27") - 12 * 7
    last_monday <- as.Date("2021-10-11")


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
        as_of = "2021-11-07",
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

    weight_estimates <- get_weights(
      spatial_resolution = spatial_resolution,
      target_var = target_var)

    return(list(target_var = target_var,
                spatial_resolution = spatial_resolution,
                data = observed_by_location_target_end_date,
                forecasts = component_forecasts,
                weights = weight_estimates))
  }
)


case_target_vars <- sapply(
  data_forecasts_weights,
  function(x) x$target_var)
case_spatial_resolutions <- sapply(
  data_forecasts_weights,
  function(x) x$spatial_resolution)

effective_weights <- purrr::pmap_dfr(
  tidyr::expand_grid(
    target_var = c("inc_case", "inc_death"),
    spatial_resolution = c("state", "euro_countries")
  ),
  function(target_var, spatial_resolution) {
# for (target_var in c("inc_case", "inc_death")) {
#   for (spatial_resolution in c("state", "euro_countries")) {
    if (spatial_resolution == "state") {
      ordered_locations <- c("All models available", "Alabama", "Alaska",
        "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
        "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois",
        "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine",
        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
        "New Jersey", "New Mexico", "New York", "North Carolina",
        "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
        "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas",
        "Utah", "Vermont", "Virginia", "Washington", "West Virginia",
        "Wisconsin", "Wyoming", "District of Columbia", "American Samoa",
        "Guam", "Northern Mariana Isl.", "Puerto Rico", "Virgin Islands")
    } else {
      ordered_locations <- c("All models available", "Germany", "Poland",
        "Italy", "Czechia", "Spain", "Austria", "Belgium", "Bulgaria", "Cyprus",
        "Denmark", "Estonia", "Finland", "France", "United Kingdom",
        "Greece", "Croatia", "Hungary", "Iceland", "Ireland", "Liechtenstein",
        "Lithuania", "Luxembourg", "Latvia", "Malta", "Netherlands", "Norway",
        "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", "Switzerland")
    }
    case_ind <- which(
      case_target_vars == target_var &
      case_spatial_resolutions == spatial_resolution
    )
    weight_estimates <- data_forecasts_weights[[case_ind]]$weights
    forecasts <- data_forecasts_weights[[case_ind]]$forecasts

    # plot of effective weights by location, accounting for forecast missingness
    effective_weights_by_location_date <- purrr::pmap_dfr(
      tidyr::expand_grid(
        location = unique(forecasts$location_name),
        fwed = unique(forecasts$forecast_week_end_date)
      ),
      function(location, fwed) {
        fwed_weights <- weight_estimates %>%
          dplyr::filter(forecast_date == (fwed + 2))
        available_models <- forecasts %>%
          dplyr::filter(
            forecast_week_end_date == fwed,
            location_name == UQ(location)) %>%
          dplyr::pull(model) %>%
          unique()
        effective_weights <- fwed_weights %>%
          dplyr::filter(model %in% available_models) %>%
          dplyr::mutate(
            beta = beta / sum(beta),
            location_name = location
          )
        return(effective_weights)
      }
    ) %>%
      dplyr::bind_rows(
        weight_estimates %>% dplyr::mutate(
          location_name = "All models available"
        )
      ) %>%
      dplyr::mutate(
        location_name = factor(
          ifelse(location_name == "Northern Mariana Islands", "Northern Mariana Isl.", location_name),
          levels = ordered_locations),
        target_var = target_var,
        spatial_resolution = spatial_resolution
      )
    return(effective_weights_by_location_date)
  })

models_plotted <- effective_weights %>%
  dplyr::mutate(
    target_var = ifelse(target_var == "inc_case", "Cases", "Deaths"),
    spatial_resolution = ifelse(spatial_resolution == "state", "US", "EU")
  ) %>%
  dplyr::filter(
    forecast_date == "2021-10-11",
    target_var == "Deaths"
  ) %>%
  dplyr::pull(model) %>%
  unique()

set.seed(1)
models_plotted <- sample(models_plotted)

# greys <- grDevices::gray.colors(n = length(models_plotted), start = 0.1, end = 0.9, gamma = 1.0)
# fill_values <- greys
fill_values <- viridis::inferno(n = length(models_plotted), begin = 0.1, end = 0.9)
names(fill_values) <- models_plotted

p_effective_weights_us <- ggplot(
    data = effective_weights %>%
      dplyr::mutate(
        target_var = ifelse(target_var == "inc_case", "Cases", "Deaths"),
        spatial_resolution = ifelse(spatial_resolution == "state", "US", "EU")
      ) %>%
      dplyr::filter(
        forecast_date == "2021-10-11",
        target_var == "Deaths",
        spatial_resolution == "US")) +
  geom_col(mapping = aes(x = location_name, y = beta, fill = model),
    size = 0.5,
    color = "black") +
  # facet_grid(. ~ spatial_resolution, scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = expansion()) +
  scale_fill_manual(
    "Forecaster",
    values = fill_values
  ) +
  # facet_wrap( ~ forecast_date, ncol = 4) +
  xlab("Location") +
  ylab("Component Weight") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 8),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

p_effective_weights_eu <- ggplot(
    data = effective_weights %>%
      dplyr::mutate(
        target_var = ifelse(target_var == "inc_case", "Cases", "Deaths"),
        spatial_resolution = ifelse(spatial_resolution == "state", "US", "EU")
      ) %>%
      dplyr::filter(
        forecast_date == "2021-10-11",
        target_var == "Deaths",
        spatial_resolution == "EU")) +
  geom_col(mapping = aes(x = location_name, y = beta, fill = model),
    size = 0.5,
    color = "black") +
  # facet_grid(. ~ spatial_resolution, scales = "free_x", space = "free_x") +
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = expansion()) +
  scale_fill_manual(
    "Forecaster",
    values = fill_values
  ) +
  # facet_wrap( ~ forecast_date, ncol = 4) +
  xlab("Location") +
  ylab("Component Weight") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 8),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

case_ind <- which(
  case_target_vars == "inc_death" &
  case_spatial_resolutions == "state"
)
p_location_counts_us <- data_forecasts_weights[[case_ind]]$forecasts %>%
  dplyr::distinct(model, location, forecast_week_end_date) %>%
  dplyr::filter(forecast_week_end_date == "2021-10-09") %>%
  dplyr::count(model, name = "num_locations") %>%
  dplyr::mutate(
    top_10 = model %in% (effective_weights %>%
      dplyr::filter(target_var == "inc_death",
                    spatial_resolution == "state",
                    forecast_date == "2021-10-11",
                    location_name == "All models available") %>%
      dplyr::pull(model)),
    top_10 = factor(
      ifelse(top_10, "In top 10", "Not in top 10"),
      levels = c("Not in top 10", "In top 10")
    )
  ) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = num_locations, fill = top_10)) +
    scale_fill_discrete("Component Rank") +
    scale_y_continuous(
      breaks = c(0, 5, 10),
      expand = expansion()) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50)) +
    xlim(c(0, NA)) +
    ylim(c(0, 13)) +
    xlab("Number of Locations\nForecasted") +
    ylab("Number of Models") +
    theme_bw() +
    theme(panel.grid = element_line(color = "gray"))

case_ind <- which(
  case_target_vars == "inc_death" &
  case_spatial_resolutions == "euro_countries"
)
p_location_counts_eu <- data_forecasts_weights[[case_ind]]$forecasts %>%
  dplyr::distinct(model, location, forecast_week_end_date) %>%
  dplyr::filter(forecast_week_end_date == "2021-10-09") %>%
  dplyr::count(model, name = "num_locations") %>%
  dplyr::mutate(
    top_10 = model %in% (effective_weights %>%
      dplyr::filter(target_var == "inc_death",
                    spatial_resolution == "euro_countries",
                    forecast_date == "2021-10-11",
                    location_name == "All models available") %>%
      dplyr::pull(model)),
    top_10 = factor(
      ifelse(top_10, "In top 10", "Not in top 10"),
      levels = c("Not in top 10", "In top 10")
    )
  ) %>%
  ggplot() +
    geom_histogram(mapping = aes(x = num_locations, fill = top_10)) +
    scale_fill_discrete("Component Rank") +
    scale_y_continuous(breaks = c(0, 5, 10), expand = expansion()) +
    scale_x_continuous(breaks = c(0, 10, 20, 30)) +
    xlim(c(0, NA)) +
    ylim(c(0, 13)) +
    xlab("Number of Locations\nForecasted") +
    ylab("Number of Models") +
    theme_bw() +
    theme(panel.grid = element_line(color = "gray"))

legend_top_10 <- ggpubr::get_legend(p_location_counts_us, position = "right")
p_location_counts_us <- p_location_counts_us +
  theme(legend.position = "none")
p_location_counts_eu <- p_location_counts_eu +
  theme(legend.position = "none")

pdf(
  paste0(
    'manuscript/figures/effective_weights_one_week.pdf'
  ),
  width = 8,
  height = 8)
plot_layout <- grid.layout(
  nrow = 7, ncol = 2,
  widths = unit(c(1, 3), rep("null", 2)),
  heights = unit(c(1, 5, 0.95, 1, 5, 0.81, 2),
                 c("lines", "null", "null", "lines", "null", "null", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))
grid.text(
  "(a) US: Number of locations forecasted per model and effective model weights per location",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
# grid.text(
#   "(b)",
#   just = "left",
#   gp = gpar(fontsize = 11),
# vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.text(
  "(b) EU: Number of locations forecasted per model and effective model weights per location",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
# grid.text(
#   "(d)",
#   rot = 90,
#   gp = gpar(fontsize = 11),
#   vp = viewport(layout.pos.row = 3, layout.pos.col = 2))

print(p_effective_weights_us, vp = viewport(layout.pos.row = 2:3, layout.pos.col = 2))
print(p_location_counts_us, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

print(p_effective_weights_eu, vp = viewport(layout.pos.row = 5:6, layout.pos.col = 2))
print(p_location_counts_eu, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))

print(as_ggplot(legend_top_10),
  vp = viewport(
    y = unit(0, "npc"),
    layout.pos.row = 6:7, layout.pos.col = 1,
    just = c("top")))
dev.off()


# plots of effective weights by forecast date
for (target_var in c("inc_case", "inc_death")) {
  for (spatial_resolution in c("state", "euro_countries")) {
    models_plotted <- effective_weights %>%
      dplyr::filter(
        target_var == UQ(target_var),
        spatial_resolution == UQ(spatial_resolution)
      ) %>%
      dplyr::pull(model) %>%
      unique()

    set.seed(1)
    models_plotted <- sample(models_plotted)

    # greys <- grDevices::gray.colors(n = length(models_plotted), start = 0.1, end = 0.9, gamma = 1.0)
    # fill_values <- greys
    fill_values <- viridis::inferno(n = length(models_plotted), begin = 0.1, end = 0.9)
    names(fill_values) <- models_plotted

    p_effective_weights <- ggplot(
        data = effective_weights %>%
          dplyr::filter(
            as.integer(substr(forecast_date, 9, 10)) <= 7,
            target_var == UQ(target_var),
            spatial_resolution == UQ(spatial_resolution))) +
      geom_col(mapping = aes(x = location_name, y = beta, fill = model),
        size = 0.5,
        color = "black") +
      facet_wrap( ~ forecast_date, ncol = 2) +
      # facet_grid(. ~ spatial_resolution, scales = "free_x", space = "free_x") +
      scale_y_continuous(breaks = c(0, 0.5, 1), expand = expansion()) +
      scale_fill_manual(
        "Forecaster",
        values = fill_values
      ) +
      xlab("Location") +
      ylab("Component Weight") +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4, size = 7),
        axis.title.x = element_blank(),
        legend.position = "none"
      )

    pdf(
      paste0(
        'manuscript/figures/effective_weights_per_location_',
        target_var, '_', spatial_resolution, '.pdf'
      ),
      width = 8,
      height = 10)
    print(p_effective_weights)
    dev.off()
  }
}


# plots of number of locations forecasted by forecast date,
# fore each target variable and continent/hub/spatial_resolution
# target_var <- "inc_death"
# spatial_resolution <- "state"

for (target_var in c("inc_case", "inc_death")) {
  for (spatial_resolution in c("state", "euro_countries")) {
    case_ind <- which(
      case_target_vars == target_var &
      case_spatial_resolutions == spatial_resolution
    )

    location_counts <- data_forecasts_weights[[case_ind]]$forecasts %>%
      dplyr::distinct(model, location, forecast_week_end_date) %>%
    #  dplyr::filter(forecast_week_end_date == "2021-10-09") %>%
      dplyr::count(forecast_week_end_date, model, name = "num_locations") %>%
      dplyr::left_join(
        effective_weights %>%
          dplyr::filter(target_var == UQ(target_var),
                        spatial_resolution == UQ(spatial_resolution),
                        # forecast_date == "2021-10-11",
                        location_name == "All models available") %>%
          dplyr::transmute(model, beta, forecast_week_end_date = forecast_date - 2),
        by = c("forecast_week_end_date", "model")
      ) %>%
      dplyr::mutate(
        top_10 = factor(
          ifelse(!is.na(beta), "In top 10", "Not in top 10"),
          levels = c("Not in top 10", "In top 10")
        ),
        forecast_date = forecast_week_end_date + 2
      ) %>%
      dplyr::group_by(forecast_date) %>%
      dplyr::mutate(num_weighted = sum(top_10 == "In top 10")) %>%
      dplyr::filter(num_weighted > 0)

    p_location_counts <- ggplot(data = location_counts) +
      geom_histogram(mapping = aes(x = num_locations, fill = top_10)) +
      geom_hline(yintercept = 0, color = "grey") +
      scale_fill_discrete("Component Rank") +
      scale_y_continuous(
        breaks = c(0, 5, 10),
        expand = expansion()) +
      scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50)) +
      xlim(c(0, NA)) +
      ylim(c(0, NA)) +
      xlab("Number of Locations Forecasted") +
      ylab("Number of Models") +
      facet_wrap( ~ forecast_date, ncol = 7) +
      theme_bw() +
      theme(
        panel.grid = element_line(color = "gray"),
        legend.position = "bottom")

    pdf(
      paste0(
        'manuscript/figures/num_locations_per_model_',
        target_var, '_', spatial_resolution, '.pdf'
      ),
      width = 8,
      height = 10)
    print(p_location_counts)
    dev.off()
  }
}






# number of models per location -- not used
component_forecasts <- forecasts
model_counts <- component_forecasts %>%
  dplyr::distinct(model, location, forecast_week_end_date) %>%
  dplyr::count(location, forecast_week_end_date, name = "num_models") %>%
  dplyr::ungroup()
p_model_counts <- ggplot(data = model_counts) +
  geom_line(mapping = aes(x = location, y = num_models, group = forecast_week_end_date)) +
  facet_wrap(~ forecast_week_end_date)
pdf(
  paste0(
    'manuscript/figures/model_counts_',
    ifelse(spatial_resolution == "state", "US", "EU"), '_',
    target_var, '.pdf'
  ),
  width = 8,
  height = 8)
print(p_model_counts)
dev.off()
