library(tidyverse)
library(zeallot)
library(covidData)
library(covidEnsembles)
library(ggpubr)
library(scales)

# Location of main covid19-forecast-hub repo where component model submissions
# can be found
submissions_root <- paste0(
  "code/retrospective-forecasts/")

# Where we want to save the plots
plots_root <- paste0(
  "code/figures/")

# locations to include
all_locations <- covidData::fips_codes %>%
  dplyr::filter(nchar(location) == 2, location != "US") %>%
  dplyr::pull(location)
spatial_scale <- "state"

# load data and forecasts
data_and_forecasts <- purrr::map(
  c("cases", "deaths"),
  function (measure) {
    # load data and misc. set up
    if (measure == 'deaths') {
      data <- covidData::load_jhu_data(
        as_of = as.Date("2022-05-16"),
        spatial_resolution = 'state',
        temporal_resolution = 'weekly',
        measure = measure,
        geography = "US") %>%
        dplyr::filter(location %in% all_locations) %>%
        dplyr::left_join(covidData::fips_codes, by = 'location')

      # things needed to load forecasts
      horizon <- 4L
      types <- 'inc'
      required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      first_forecast_date <- lubridate::ymd("2020-07-27")
      last_forecast_date <- lubridate::ymd("2022-03-14")
      num_forecast_weeks <-
        as.numeric(last_forecast_date - first_forecast_date) / 7 + 1
    } else if (measure == 'cases') {
      data <- covidData::load_jhu_data(
        as_of = as.Date("2022-05-16"),
        spatial_resolution = 'state',
        temporal_resolution = 'weekly',
        measure = measure,
        geography = "US") %>%
        dplyr::filter(location %in% all_locations) %>%
        dplyr::left_join(covidData::fips_codes, by = 'location')

      # things needed to load forecasts
      horizon <- 4L
      types <- 'inc'
      required_quantiles <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      first_forecast_date <- lubridate::ymd("2020-09-14")
      last_forecast_date <- lubridate::ymd("2022-03-14")
      num_forecast_weeks <-
        as.numeric(last_forecast_date - first_forecast_date) / 7 + 1
    }

    # load forecasts
    all_forecasts <- load_retrospective_ensemble_forecasts(
      submissions_root = submissions_root,
      forecast_dates = lubridate::ymd(first_forecast_date) +
        seq(from = 0, length = num_forecast_weeks) * 7,
      all_locations = all_locations,
      spatial_scales = spatial_scale,
      response_vars = paste0("inc_", substr(measure, 1, nchar(measure) - 1))
    )

    all_forecasts <- all_forecasts %>%
      dplyr::mutate(
        model_brief = dplyr::case_when(
          model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Equal Weighted Median of All Models",
          model == "combine_method_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Equal Weighted Median of Top 10",
          model == "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Rel. WIS Weighted Median of Top 10"
        )
      ) %>%
      dplyr::filter(!is.na(model_brief))
    
    return(list(data = data, forecasts = all_forecasts))
  })
names(data_and_forecasts) <- c("cases", "deaths")

# utility function to identify the top n largest local maxima for a given location
top_n_maxima <- function(location_name, n, local_maxima) {
  local_maxima %>%
    dplyr::filter(location_name == UQ(location_name)) %>%
    dplyr::slice_max(inc, n = n) %>%
    dplyr::pull(date)
}

# utility function to plot identified local maxima
plot_identified_maxima <- function(data, local_maxima, measure) {
  if (measure == "cases") {
    ylabel <- "Weekly Cases"
    analysis_limits <- as.Date(c("2020-09-14", "2022-03-14"))
  } else {
    ylabel <- "Weekly Deaths"
    analysis_limits <- as.Date(c("2020-07-27", "2022-03-14"))
  }
  p <- ggplot() +
    geom_vline(
      data = local_maxima,
      mapping = aes(xintercept = date),
      color = "orange"
    ) +
    geom_vline(
      xintercept = analysis_limits,
      linetype = 2
    ) +
    geom_line(data = data,
              mapping = aes(x = date, y = inc)) +
    # scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_x_date(date_labels = "%b %Y") +
    ylab(ylabel) +
    facet_wrap( ~ abbreviation, scales = "free_y", ncol = 6) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0))
  return(p)
}

# identify local maxima for cases:
# week with largest incidence in a rolling 11 week window
first_forecast_date <- lubridate::ymd("2020-09-14")
last_forecast_date <- lubridate::ymd("2022-03-14")

all_forecast_dates_cases <- data_and_forecasts[["cases"]]$data %>%
  dplyr::filter(
    date >= (first_forecast_date - 2),
    date <= (last_forecast_date - 2)
  )

window <- 11
local_maxima_cases <- data_and_forecasts[["cases"]]$data %>%
  dplyr::group_by(location, location_name) %>%
#  dplyr::filter(location_name == "Oregon") %>%
  dplyr::mutate(
    rolling_max_inc = slider::slide_index_max(
      inc,
      date,
      before = lubridate::weeks((window - 1) / 2),
      after = lubridate::weeks((window - 1) / 2)),
  ) %>%
  dplyr::filter(
    !is.na(rolling_max_inc),
    inc == rolling_max_inc,
    date >= (first_forecast_date - 2),
    date <= (last_forecast_date - 2)
  )

# drop some of these that are not "real" maxima based on visual inspection
# of the plot to follow
local_maxima_cases <- local_maxima_cases %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    !(location_name == "Alabama" &
      !(date %in% top_n_maxima("Alabama", n = 3, local_maxima_cases))),
    !(location_name == "Alaska" &
      !(date %in% top_n_maxima("Alaska", n = 3, local_maxima_cases))),
    !(location_name == "Arizona" &
      !(date %in% top_n_maxima("Arizona", n = 4, local_maxima_cases))),
    !(location_name == "Arkansas" &
      !(date %in% top_n_maxima("Arkansas", n = 3, local_maxima_cases))),
    !(location_name == "California" &
      !(date %in% top_n_maxima("California", n = 3, local_maxima_cases))),
    !(location_name == "Colorado" &
      !(date %in% top_n_maxima("Colorado", n = 4, local_maxima_cases))),
    !(location_name == "Connecticut" &
      !(date %in% top_n_maxima("Connecticut", n = 4, local_maxima_cases))),
    !(location_name == "Delaware" &
      !(date %in% top_n_maxima("Delaware", n = 3, local_maxima_cases))),
    !(location_name == "District of Columbia" &
      !(date %in% top_n_maxima("District of Columbia", n = 3, local_maxima_cases))),
    !(location_name == "Florida" &
      !(date %in% top_n_maxima("Florida", n = 3, local_maxima_cases))),
    !(location_name == "Georgia" &
      !(date %in% top_n_maxima("Georgia", n = 3, local_maxima_cases))),
    !(location_name == "Hawaii" &
      !(date %in% top_n_maxima("Hawaii", n = 2, local_maxima_cases))),
    !(location_name == "Idaho" &
      !(date %in% top_n_maxima("Idaho", n = 3, local_maxima_cases))),
    !(location_name == "Illinois" &
      !(date %in% top_n_maxima("Illinois", n = 4, local_maxima_cases))),
    !(location_name == "Indiana" &
      !(date %in% top_n_maxima("Indiana", n = 3, local_maxima_cases))),
    !(location_name == "Iowa" &
      !(date %in% top_n_maxima("Iowa", n = 2, local_maxima_cases))),
    !(location_name == "Kansas" &
      !(date %in% top_n_maxima("Kansas", n = 3, local_maxima_cases))),
    !(location_name == "Kentucky" &
      !(date %in% top_n_maxima("Kentucky", n = 3, local_maxima_cases))),
    !(location_name == "Louisiana" &
      !(date %in% top_n_maxima("Louisiana", n = 3, local_maxima_cases))),
    # !(location_name == "Maine" &
    #   !(date %in% top_n_maxima("Maine", n = 4, local_maxima_cases))),
    !(location_name == "Maine" & date %in% as.Date(c("2021-10-02", "2022-02-19"))),
    !(location_name == "Maryland" &
      !(date %in% top_n_maxima("Maryland", n = 4, local_maxima_cases))),
    !(location_name == "Massachusetts" &
      !(date %in% top_n_maxima("Massachusetts", n = 3, local_maxima_cases))),
    !(location_name == "Michigan" &
      !(date %in% top_n_maxima("Michigan", n = 3, local_maxima_cases))),
    !(location_name == "Minnesota" &
      !(date %in% top_n_maxima("Minnesota", n = 3, local_maxima_cases))),
    !(location_name == "Mississippi" &
      !(date %in% top_n_maxima("Mississippi", n = 3, local_maxima_cases))),
    !(location_name == "Missouri" &
      !(date %in% top_n_maxima("Missouri", n = 3, local_maxima_cases))),
    !(location_name == "Montana" &
      !(date %in% top_n_maxima("Montana", n = 3, local_maxima_cases))),
    !(location_name == "Nebraska" &
      !(date %in% top_n_maxima("Nebraska", n = 3, local_maxima_cases))),
    !(location_name == "Nebraska" & date %in% as.Date(c("2022-03-05"))),
    !(location_name == "Nevada" &
      !(date %in% top_n_maxima("Nevada", n = 3, local_maxima_cases))),
    !(location_name == "New Hampshire" &
      !(date %in% top_n_maxima("New Hampshire", n = 3, local_maxima_cases))),
    !(location_name == "New Jersey" &
      !(date %in% top_n_maxima("New Jersey", n = 4, local_maxima_cases))),
    !(location_name == "New Jersey" & date %in% as.Date(c("2021-04-03"))),
    !(location_name == "New Mexico" &
      !(date %in% top_n_maxima("New Mexico", n = 2, local_maxima_cases))),
    !(location_name == "New York" &
      !(date %in% top_n_maxima("New York", n = 2, local_maxima_cases))),
    !(location_name == "North Carolina" &
      !(date %in% top_n_maxima("North Carolina", n = 3, local_maxima_cases))),
    !(location_name == "North Dakota" &
      !(date %in% top_n_maxima("North Dakota", n = 3, local_maxima_cases))),
    !(location_name == "Ohio" &
      !(date %in% top_n_maxima("Ohio", n = 3, local_maxima_cases))),
    !(location_name == "Oklahoma" &
      !(date %in% top_n_maxima("Oklahoma", n = 4, local_maxima_cases))),
    !(location_name == "Oklahoma" & date == "2020-11-28"),
    !(location_name == "Oregon" &
      !(date %in% top_n_maxima("Oregon", n = 4, local_maxima_cases))),
    !(location_name == "Pennsylvania" &
      !(date %in% top_n_maxima("Pennsylvania", n = 4, local_maxima_cases))),
    !(location_name == "Pennsylvania" & date == "2021-10-09"),
    !(location_name == "Rhode Island" &
      !(date %in% top_n_maxima("Rhode Island", n = 2, local_maxima_cases))),
    !(location_name == "South Carolina" &
      !(date %in% top_n_maxima("South Carolina", n = 3, local_maxima_cases))),
    !(location_name == "South Dakota" &
      !(date %in% top_n_maxima("South Dakota", n = 3, local_maxima_cases))),
    !(location_name == "Tennessee" &
      !(date %in% top_n_maxima("Tennessee", n = 3, local_maxima_cases))),
    !(location_name == "Texas" &
      !(date %in% top_n_maxima("Texas", n = 3, local_maxima_cases))),
    !(location_name == "Utah" &
      !(date %in% top_n_maxima("Utah", n = 4, local_maxima_cases))),
    !(location_name == "Utah" & date == "2021-01-09"),
    !(location_name == "Vermont" &
      !(date %in% top_n_maxima("Vermont", n = 2, local_maxima_cases))),
    !(location_name == "Virginia" &
      !(date %in% top_n_maxima("Virginia", n = 3, local_maxima_cases))),
    !(location_name == "Washington" &
      !(date %in% top_n_maxima("Washington", n = 3, local_maxima_cases))),
    !(location_name == "West Virginia" &
      !(date %in% top_n_maxima("West Virginia", n = 3, local_maxima_cases))),
    !(location_name == "Wisconsin" &
      !(date %in% top_n_maxima("Wisconsin", n = 2, local_maxima_cases))),
    !(location_name == "Wyoming" &
      !(date %in% top_n_maxima("Wyoming", n = 3, local_maxima_cases))),
    !(location_name == "American Samoa"),
    !(location_name == "Guam" &
      !(date %in% top_n_maxima("Guam", n = 3, local_maxima_cases))),
    !(location_name == "Northern Mariana Islands" &
      !(date %in% top_n_maxima("Northern Mariana Islands", n = 1, local_maxima_cases))),
    !(location_name == "Puerto Rico" &
      !(date %in% top_n_maxima("Puerto Rico", n = 3, local_maxima_cases))),
    !(location_name == "Virgin Islands" &
      !(date %in% top_n_maxima("Virgin Islands", n = 2, local_maxima_cases)))
  )

p <- plot_identified_maxima(data_and_forecasts[["cases"]]$data, local_maxima_cases, "cases")
print(p)

pdf(file = "manuscript/figures/peak_cases_us_identified.pdf", width = 8, height = 10)
print(p)
dev.off()

readr::write_csv(local_maxima_cases, file = "code/figures/local_maxima_cases.csv")


# identify local maxima for deaths:
# week with largest incidence in a rolling 11 week window
first_forecast_date <- lubridate::ymd("2020-07-27")
last_forecast_date <- lubridate::ymd("2022-03-14")

all_forecast_dates_deaths <- data_and_forecasts[["deaths"]]$data %>%
  dplyr::filter(
    date >= (first_forecast_date - 2),
    date <= (last_forecast_date - 2)
  )

local_maxima_deaths <- data_and_forecasts[["deaths"]]$data %>%
  dplyr::group_by(location, location_name) %>%
  dplyr::mutate(
    rolling_max_inc = slider::slide_index_max(
      inc,
      date,
      before = lubridate::weeks((window - 1) / 2),
      after = lubridate::weeks((window - 1) / 2)),
  ) %>%
  dplyr::filter(
    !is.na(rolling_max_inc),
    inc == rolling_max_inc,
    date >= (first_forecast_date - 2),
    date <= (last_forecast_date - 2)
  )

# drop some of these that are not "real" maxima based on visual inspection
# of the plot to follow
local_maxima_deaths <- local_maxima_deaths %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    !(location_name == "Alabama" &
      !(date %in% top_n_maxima("Alabama", n = 3, local_maxima_deaths))),
    !(location_name == "Alaska" &
      !(date %in% top_n_maxima("Alaska", n = 3, local_maxima_deaths))),
    !(location_name == "Alaska" & date == "2022-01-22"),
    !(location_name == "Arizona" &
      !(date %in% top_n_maxima("Arizona", n = 2, local_maxima_deaths))),
    !(location_name == "Arkansas" &
      !(date %in% top_n_maxima("Arkansas", n = 4, local_maxima_deaths))),
    !(location_name == "Arkansas" & date %in% as.Date(c("2020-09-19", "2021-10-16"))),
    !(location_name == "California" &
      !(date %in% top_n_maxima("California", n = 4, local_maxima_deaths))),
    !(location_name == "Colorado" &
      !(date %in% top_n_maxima("Colorado", n = 4, local_maxima_deaths))),
    !(location_name == "Colorado" & date == "2022-02-12"),
    !(location_name == "Connecticut" &
      !(date %in% top_n_maxima("Connecticut", n = 2, local_maxima_deaths))),
    !(location_name == "Delaware" &
      !(date %in% top_n_maxima("Delaware", n = 3, local_maxima_deaths))),
    !(location_name == "Delaware" & date == "2021-07-31"),
    !(location_name == "District of Columbia" &
      !(date %in% top_n_maxima("District of Columbia", n = 2, local_maxima_deaths))),
    !(location_name == "Florida" &
      !(date %in% top_n_maxima("Florida", n = 4, local_maxima_deaths))),
    !(location_name == "Georgia" &
      !(date %in% top_n_maxima("Georgia", n = 5, local_maxima_deaths))),
    !(location_name == "Georgia" & date == "2020-11-07"),
    !(location_name == "Hawaii" &
      !(date %in% top_n_maxima("Hawaii", n = 4, local_maxima_deaths))),
    !(location_name == "Hawaii" & date == "2021-01-30"),
    !(location_name == "Idaho" &
      !(date %in% top_n_maxima("Idaho", n = 3, local_maxima_deaths))),
    !(location_name == "Illinois" &
      !(date %in% top_n_maxima("Illinois", n = 4, local_maxima_deaths))),
    !(location_name == "Indiana" &
      !(date %in% top_n_maxima("Indiana", n = 3, local_maxima_deaths))),
    !(location_name == "Iowa" &
      !(date %in% top_n_maxima("Iowa", n = 3, local_maxima_deaths))),
    !(location_name == "Iowa" & date == "2021-02-06"),
    !(location_name == "Kansas" &
      !(date %in% top_n_maxima("Kansas", n = 2, local_maxima_deaths))),
    !(location_name == "Kentucky" &
      !(date %in% top_n_maxima("Kentucky", n = 4, local_maxima_deaths))),
    !(location_name == "Kentucky" & date %in% as.Date(c("2021-03-20", "2021-06-05", "2021-10-02"))),
    !(location_name == "Louisiana" &
      !(date %in% top_n_maxima("Louisiana", n = 4, local_maxima_deaths))),
    !(location_name == "Maine" &
      !(date %in% top_n_maxima("Maine", n = 3, local_maxima_deaths))),
    !(location_name == "Maryland" &
      !(date %in% top_n_maxima("Maryland", n = 4, local_maxima_deaths))),
    !(location_name == "Maryland" & date %in% as.Date(c("2021-05-29", "2022-01-01"))),
    !(location_name == "Massachusetts" &
      !(date %in% top_n_maxima("Massachusetts", n = 2, local_maxima_deaths))),
    !(location_name == "Michigan" &
      !(date %in% top_n_maxima("Michigan", n = 3, local_maxima_deaths))),
    !(location_name == "Michigan" & date == "2021-05-08"),
    !(location_name == "Minnesota" &
      !(date %in% top_n_maxima("Minnesota", n = 2, local_maxima_deaths))),
    !(location_name == "Mississippi" &
      !(date %in% top_n_maxima("Mississippi", n = 4, local_maxima_deaths))),
    !(location_name == "Missouri" &
      !(date %in% top_n_maxima("Missouri", n = 4, local_maxima_deaths))),
    !(location_name == "Missouri" & date == "2021-12-04"),
    !(location_name == "Montana" &
      !(date %in% top_n_maxima("Montana", n = 5, local_maxima_deaths))),
    !(location_name == "Montana" & date %in% as.Date(c("2021-01-30", "2021-12-04"))),
    !(location_name == "Nebraska" &
      !(date %in% top_n_maxima("Nebraska", n = 3, local_maxima_deaths))),
    !(location_name == "Nebraska" & date == "2021-10-02"),
    !(location_name == "Nevada" &
      !(date %in% top_n_maxima("Nevada", n = 6, local_maxima_deaths))),
    !(location_name == "Nevada" & date %in% as.Date(c("2021-08-14", "2021-12-11"))),
    !(location_name == "New Hampshire" &
      !(date %in% top_n_maxima("New Hampshire", n = 2, local_maxima_deaths))),
    !(location_name == "New Jersey" &
      !(date %in% top_n_maxima("New Jersey", n = 2, local_maxima_deaths))),
    !(location_name == "New Mexico" &
      !(date %in% top_n_maxima("New Mexico", n = 2, local_maxima_deaths))),
    !(location_name == "New Mexico" & date == "2021-05-29"),
    !(location_name == "New York" &
      !(date %in% top_n_maxima("New York", n = 2, local_maxima_deaths))),
    !(location_name == "North Carolina" &
      !(date %in% top_n_maxima("North Carolina", n = 3, local_maxima_deaths))),
    !(location_name == "North Dakota" &
      !(date %in% top_n_maxima("North Dakota", n = 2, local_maxima_deaths))),
    !(location_name == "Ohio" &
      !(date %in% top_n_maxima("Ohio", n = 3, local_maxima_deaths))),
    !(location_name == "Oklahoma" &
      !(date %in% top_n_maxima("Oklahoma", n = 4, local_maxima_deaths))),
    !(location_name == "Oklahoma" &
      date %in% as.Date(c("2021-04-10", "2021-05-29", "2021-10-23"))),
    !(location_name == "Oregon" &
      !(date %in% top_n_maxima("Oregon", n = 2, local_maxima_deaths))),
    !(location_name == "Pennsylvania" &
      !(date %in% top_n_maxima("Pennsylvania", n = 2, local_maxima_deaths))),
    !(location_name == "Rhode Island" &
      !(date %in% top_n_maxima("Rhode Island", n = 2, local_maxima_deaths))),
    !(location_name == "South Carolina" &
      !(date %in% top_n_maxima("South Carolina", n = 4, local_maxima_deaths))),
    !(location_name == "South Dakota" &
      !(date %in% top_n_maxima("South Dakota", n = 2, local_maxima_deaths))),
    !(location_name == "Tennessee" &
      !(date %in% top_n_maxima("Tennessee", n = 4, local_maxima_deaths))),
    !(location_name == "Tennessee" & date == "2021-12-25"),
    !(location_name == "Texas" &
      !(date %in% top_n_maxima("Texas", n = 4, local_maxima_deaths))),
    !(location_name == "Utah" &
      !(date %in% top_n_maxima("Utah", n = 3, local_maxima_deaths))),
    !(location_name == "Utah" & date %in% as.Date(c("2022-01-08"))),
    !(location_name == "Vermont" &
      !(date %in% top_n_maxima("Vermont", n = 4, local_maxima_deaths))),
    !(location_name == "Vermont" & date %in% as.Date(c("2021-11-06", "2021-12-18"))),
    !(location_name == "Virginia" &
      !(date %in% top_n_maxima("Virginia", n = 2, local_maxima_deaths))),
    !(location_name == "Virginia" & date %in% as.Date(c("2021-02-27"))),
    !(location_name == "Washington" &
      !(date %in% top_n_maxima("Washington", n = 3, local_maxima_deaths))),
    !(location_name == "West Virginia" &
      !(date %in% top_n_maxima("West Virginia", n = 4, local_maxima_deaths))),
    !(location_name == "West Virginia" & date %in% as.Date(c("2021-03-13"))),
    !(location_name == "Wisconsin" &
      !(date %in% top_n_maxima("Wisconsin", n = 2, local_maxima_deaths))),
    !(location_name == "Wyoming" &
      !(date %in% top_n_maxima("Wyoming", n = 3, local_maxima_deaths))),
    !(location_name == "Wyoming" & date %in% as.Date(c("2021-12-04"))),
    !(location_name == "American Samoa"),
    !(location_name == "Guam" &
      !(date %in% top_n_maxima("Guam", n = 3, local_maxima_deaths))),
    !(location_name == "Northern Mariana Islands" &
      !(date %in% top_n_maxima("Northern Mariana Islands", n = 1, local_maxima_deaths))),
    !(location_name == "Puerto Rico" &
      !(date %in% top_n_maxima("Puerto Rico", n = 3, local_maxima_deaths))),
    !(location_name == "Puerto Rico" & date == "2021-09-11"),
    !(location_name == "Virgin Islands" &
      !(date %in% top_n_maxima("Virgin Islands", n = 1, local_maxima_deaths)))
  )

# manually add in peaks that were not identified because of neighboring outliers
local_maxima_deaths <- dplyr::bind_rows(
  local_maxima_deaths,
  data_and_forecasts[["deaths"]]$data %>%
    dplyr::filter(date == "2021-08-28", location_name == "Arkansas") %>%
    dplyr::mutate(rolling_max_inc = inc),
  data_and_forecasts[["deaths"]]$data %>%
    dplyr::filter(date == "2022-01-22", location_name == "Maryland") %>%
    dplyr::mutate(rolling_max_inc = inc),
  data_and_forecasts[["deaths"]]$data %>%
    dplyr::filter(date %in% as.Date(c("2021-01-23", "2021-09-25")), location_name == "Oklahoma") %>%
    dplyr::mutate(rolling_max_inc = inc),
  data_and_forecasts[["deaths"]]$data %>%
    dplyr::filter(date %in% as.Date(c("2021-01-23", "2021-10-16")), location_name == "Virginia") %>%
    dplyr::mutate(rolling_max_inc = inc)
)



p <- plot_identified_maxima(data_and_forecasts[["deaths"]]$data, local_maxima_deaths, "deaths")
print(p)

pdf(file = "manuscript/figures/peak_deaths_us_identified.pdf", width = 8, height = 10)
print(p)
dev.off()

readr::write_csv(local_maxima_deaths, file = "code/figures/local_maxima_deaths.csv")



# plot for 4 locations with largest local peaks
make_peak_forecasts_plot <- function(local_maxima, all_forecasts, data, measure) {
  if (measure == "cases") {
    ylabel <- "Weekly Cases"
    # analysis_limits <- as.Date(c("2020-09-14", "2021-10-11"))
  } else {
    ylabel <- "Weekly Deaths"
    # analysis_limits <- as.Date(c("2020-07-27", "2021-10-11"))
  }

  max_peak_locations <- local_maxima %>%
    dplyr::group_by(location) %>%
    dplyr::slice_max(inc, n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::slice_max(inc, n = 4) %>%
    dplyr::pull(location)

  local_maxima_to_plot <- local_maxima %>%
    dplyr::filter(location %in% max_peak_locations)

  interval_predictions <- all_forecasts %>%
    dplyr::inner_join(
      local_maxima_to_plot %>%
        dplyr::transmute(location, forecast_date = date + 2 - 7),
      by = c("location", "forecast_date")) %>%
    dplyr::filter(
      grepl(substr(measure, 1, nchar(measure) - 1), target_variable),
      model_brief %in% c("Equal Weighted Median of All Models",
                        "Rel. WIS Weighted Median of Top 10")
    ) %>%
    dplyr::mutate(
      endpoint_type = ifelse(quantile < 0.5, 'lower', 'upper'),
      alpha = ifelse(
        endpoint_type == 'lower',
        format(2*quantile, digits=3, nsmall=3),
        format(2*(1-quantile), digits=3, nsmall=3))
    ) %>%
  #          dplyr::filter(alpha %in% c("0.050", "0.2")) %>%
    dplyr::filter(alpha == "0.050") %>%
    dplyr::select(-quantile) %>%
    tidyr::pivot_wider(names_from='endpoint_type', values_from='value')

  point_predictions <- all_forecasts %>%
    dplyr::inner_join(
      local_maxima_to_plot %>%
        dplyr::transmute(location, forecast_date = date + 2 - 7),
      by = c("location", "forecast_date")) %>%
    dplyr::filter(
      grepl(substr(measure, 1, nchar(measure) - 1), target_variable),
      model_brief %in% c("Equal Weighted Median of All Models",
                        "Rel. WIS Weighted Median of Top 10"),
      format(quantile, digits = 3, nsmall = 3) == "0.500"
    )

  p <- ggplot() +
    geom_ribbon(
      data = interval_predictions,
      mapping = aes(
        x = target_end_date,
        ymin = lower, ymax = upper,
  #                  fill=model_brief,
        fill = model_brief,
        group = paste0(model_brief, forecast_date, alpha, sep = "_")),
      alpha = 0.3) +
    geom_line(
      data = point_predictions,
      mapping = aes(
        x = target_end_date,
        y = value,
        color = model_brief,
        group = paste0(model_brief, forecast_date, sep = "_")
      )
    ) +
    geom_line(data = data %>%
                dplyr::mutate(
                  date = lubridate::ymd(date)
                ) %>%
                dplyr::filter(
                  location %in% max_peak_locations,
                  date >= "2020-08-01"#, date <= "2021-09-01"
                ),
              mapping = aes(x = date, y = inc)) +
    geom_vline(
      data = local_maxima %>% dplyr::filter(location %in% max_peak_locations),
      mapping = aes(xintercept = date)
    ) +
    scale_fill_discrete("Combination Method") +
    scale_color_discrete("Combination Method") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
    ggplot2::scale_y_continuous(labels = comma) +
    # geom_point(data = data %>%
    #             dplyr::mutate(
    #               date = lubridate::ymd(date)
    #             ),
    #             mapping = aes(x = date, y = inc)) +
    facet_wrap( ~ location_name, scales = "free_y") +
    # scale_fill_viridis_d() +
  #              scale_color_manual("Issue Date", values = c("black", "red")) +
  #              scale_linetype_discrete("Issue Date") +
  #              scale_shape_discrete("Issue Date") +
  #              scale_fill_viridis_d("Interval alpha", begin = 0.2, end = 0.8) +
  #  ggtitle(as.character(batch_locations)) +
    xlab("Date") +
    ylab(ylabel) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0),
      legend.position = "bottom"
    )

  return(p)
}

p_cases <- make_peak_forecasts_plot(
  local_maxima = local_maxima_cases,
  all_forecasts = data_and_forecasts[["cases"]]$forecasts,
  data = data_and_forecasts[["cases"]]$data,
  measure = "cases")

p_deaths <- make_peak_forecasts_plot(
  local_maxima = local_maxima_deaths,
  all_forecasts = data_and_forecasts[["deaths"]]$forecasts,
  data = data_and_forecasts[["deaths"]]$data,
  measure = "deaths")

legend <- ggpubr::get_legend(p_cases, position = "bottom")

p_cases <- p_cases + theme(legend.position = "none")
p_deaths <- p_deaths + theme(legend.position = "none")

pdf(file = "manuscript/figures/peak_forecasts_cases_deaths_us.pdf", width = 8, height = 9)
plot_layout <- grid.layout(
  nrow = 5,
  ncol = 2,
  widths = unit(c(1, 0.3), rep("null", 2)),
  heights = unit(c(1, 1, 1, 1, 2), c("lines", "null", "lines", "null", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text("(a) Forecasts of cases in states with the largest peaks",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 12),
  vp = viewport(
    layout.pos.row = 1,
    layout.pos.col = 1:2))


print(p_cases,
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1:2))

grid.text("(b) Forecasts of deaths in states with the largest peaks",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 12),
  vp = viewport(
    layout.pos.row = 3,
    layout.pos.col = 1:2))


print(p_deaths,
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))

print(as_ggplot(legend),
  vp = viewport(
    y = unit(0, "npc"),
    layout.pos.row =5,
    layout.pos.col = 1:2,
    just = c("top")))
dev.off()


# plot of errors for forecasts issued near peak, all location/peaks
get_errors_one_measure <- function(local_maxima, data, all_forecasts, measure) {
  point_predictions <- all_forecasts %>%
    dplyr::inner_join(
      local_maxima %>%
        dplyr::transmute(location, forecast_date = date + 2 - 7),
      by = c("location", "forecast_date")) %>%
    dplyr::filter(
      grepl(substr(measure, 1, nchar(measure) - 1), target_variable),
      format(quantile, digits = 3, nsmall = 3) == "0.500"
    )

  local_peak_errors <- point_predictions %>%
    dplyr::inner_join(
      local_maxima %>%
        dplyr::transmute(location, forecast_date = date + 2 - 7),
      by = c("location", "forecast_date")
    ) %>%
    dplyr::left_join(
      data,
      by = c("location", "target_end_date" = "date")
    ) %>%
    dplyr::mutate(
      error = value - inc
    )

  return(local_peak_errors)
}

local_peak_errors <- dplyr::bind_rows(
  get_errors_one_measure(
    local_maxima = local_maxima_cases,
    data = data_and_forecasts[["cases"]]$data,
    all_forecasts = data_and_forecasts[["cases"]]$forecasts,
    measure = "cases") %>%
    dplyr::mutate(measure = "Cases", setting = "Forecasts Near Peaks"),
  get_errors_one_measure(
    local_maxima = all_forecast_dates_cases,
    data = data_and_forecasts[["cases"]]$data,
    all_forecasts = data_and_forecasts[["cases"]]$forecasts,
    measure = "cases") %>%
    dplyr::mutate(measure = "Cases", setting = "All Forecasts"),
  get_errors_one_measure(
    local_maxima = local_maxima_deaths,
    data = data_and_forecasts[["deaths"]]$data,
    all_forecasts = data_and_forecasts[["deaths"]]$forecasts,
    measure = "deaths") %>%
    dplyr::mutate(measure = "Deaths", setting = "Forecasts Near Peaks"),
  get_errors_one_measure(
    local_maxima = all_forecast_dates_deaths,
    data = data_and_forecasts[["deaths"]]$data,
    all_forecasts = data_and_forecasts[["deaths"]]$forecasts,
    measure = "deaths") %>%
    dplyr::mutate(measure = "Deaths", setting = "All Forecasts")
)

pdf(file = "manuscript/figures/peak_forecast_errors_cases_deaths_us_all_points.pdf", width = 8, height = 6)
p <- ggplot() +
  geom_boxplot(
    data = local_peak_errors,
    mapping = aes(x = factor(horizon), y = error, color = model_brief)) +
  geom_point(
    data = local_peak_errors %>%
      dplyr::group_by(horizon, model_brief, setting, measure) %>%
      dplyr::summarize(mean_error = mean(error)),
    mapping = aes(x = factor(horizon), y = mean_error, group = model_brief),
    position = position_dodge(width = 0.75),
    size = 5, shape = "+") +
  geom_hline(yintercept = 0) +
  # scale_fill_discrete("Model") +
  scale_color_viridis_d("Ensemble Model", begin = 0.1, end = 0.8) +
  xlab("Forecast Horizon") +
  ylab("Error of the Predictive Median") +
#  facet_wrap( ~ measure, ncol = 1, scales = "free_y") +
  facet_grid(measure ~ setting, scales = "free_y") +
  theme_bw()
print(p)
dev.off()


plot_limits <- local_peak_errors %>%
  dplyr::group_by(measure, setting, model_brief, horizon) %>%
  dplyr::summarize(lower = quantile(error, 0.25), upper = quantile(error, 0.75)) %>%
  dplyr::group_by(measure, setting) %>%
  dplyr::summarize(lower = min(lower), upper = max(upper))

pdf(file = "manuscript/figures/peak_forecast_errors_cases_deaths_us_no_outliers.pdf", width = 8, height = 6)
p <- ggplot() +
  geom_boxplot(
    data = local_peak_errors %>%
      dplyr::left_join(plot_limits, by = c("measure", "setting")) %>%
      dplyr::mutate(
        error_censored = dplyr::case_when(
          error > upper ~ upper,
          error < lower ~ lower,
          TRUE ~ error)
      ),
    mapping = aes(x = factor(horizon), y = error_censored, color = model_brief),
    outlier.shape = NA,
    coef = 0) +
  geom_point(
    data = local_peak_errors %>%
      dplyr::group_by(horizon, model_brief, setting, measure) %>%
      dplyr::summarize(mean_error = mean(error)),
    mapping = aes(x = factor(horizon), y = mean_error, group = model_brief),
    position = position_dodge(width = 0.75),
    size = 5, shape = "+") +
  geom_hline(yintercept = 0) +
  # scale_fill_discrete("Model") +
  scale_color_viridis_d("Ensemble Model", begin = 0.1, end = 0.8) +
  xlab("Forecast Horizon") +
  ylab("Error of the Predictive Median") +
#  facet_wrap( ~ measure, ncol = 1, scales = "free_y") +
  facet_grid(measure ~ setting, scales = "free_y") +
  theme_bw()
print(p)
dev.off()




plot_limits <- local_peak_errors %>%
  dplyr::group_by(measure, setting, model_brief, horizon) %>%
  dplyr::summarize(lower = quantile(abs(error), 0.25), upper = quantile(abs(error), 0.75)) %>%
  dplyr::group_by(measure, setting) %>%
  dplyr::summarize(lower = min(lower), upper = max(upper))

pdf(file = "manuscript/figures/peak_forecast_abs_errors_cases_deaths_us_no_outliers.pdf", width = 8, height = 6)
p <- ggplot() +
  geom_boxplot(
    data = local_peak_errors %>%
      dplyr::left_join(plot_limits, by = c("measure", "setting")) %>%
      dplyr::mutate(
        error_censored = dplyr::case_when(
          abs(error) > upper ~ upper,
          abs(error) < lower ~ lower,
          TRUE ~ abs(error))
      ),
    mapping = aes(x = factor(horizon), y = error_censored, color = model_brief),
    outlier.shape = NA,
    coef = 0) +
  geom_point(
    data = local_peak_errors %>%
      dplyr::group_by(horizon, model_brief, setting, measure) %>%
      dplyr::summarize(mean_abs_error = mean(abs(error))),
    mapping = aes(x = factor(horizon), y = mean_abs_error, group = model_brief),
    position = position_dodge(width = 0.75),
    size = 5, shape = "+") +
  geom_hline(yintercept = 0) +
  # scale_fill_discrete("Model") +
  scale_color_viridis_d("Ensemble Model", begin = 0.1, end = 0.8) +
  xlab("Forecast Horizon") +
  ylab("Absolute Error of the Predictive Median") +
#  facet_wrap( ~ measure, ncol = 1, scales = "free_y") +
  facet_grid(measure ~ setting, scales = "free_y") +
  theme_bw()
print(p)
dev.off()


# pdf(file = "manuscript/figures/peak_forecast_errors_cases_deaths_us.pdf", width = 8, height = 6)
# p <- ggplot() +
#   geom_boxplot(
#     data = local_peak_errors %>% filter(setting == "Forecasts Near Peaks"),
#     mapping = aes(x = factor(horizon), y = error, color = model_brief)) +
#   geom_hline(yintercept = 0) +
#   # scale_fill_discrete("Model") +
#   scale_color_viridis_d("Ensemble Model", begin = 0.1, end = 0.8) +
#   xlab("Forecast Horizon") +
#   ylab("Error of the Predictive Median") +
#   facet_wrap( ~ measure, ncol = 1, scales = "free_y") +
# #  facet_grid(measure ~ setting, scales = "free_y") +
#   theme_bw()
# print(p)
# dev.off()

# pdf(file = "manuscript/figures/all_forecast_errors_cases_deaths_us.pdf", width = 8, height = 6)
# p <- ggplot(data = local_peak_errors %>% filter(setting == "All Forecasts")) +
#   geom_boxplot(mapping = aes(x = factor(horizon), y = error, color = model_brief)) +
#   geom_hline(yintercept = 0) +
#   # scale_fill_discrete("Model") +
#   scale_color_viridis_d("Ensemble Model", begin = 0.1, end = 0.8) +
#   xlab("Forecast Horizon") +
#   ylab("Error of the Predictive Median") +
#   facet_wrap( ~ measure, ncol = 1, scales = "free_y") +
# #  facet_grid(measure ~ setting, scales = "free_y") +
#   theme_bw()
# print(p)
# dev.off()



