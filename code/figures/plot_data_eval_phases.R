# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggnewscale)
library(knitr)
library(here)
library(scales)
library(zeallot)

setwd(here())

#us_locations <- c("39", "48")
us_locations <- c("29", "48")
euro_locations <- c("FR")

last_as_of <- as.Date("2022-05-16")
as_ofs <- seq.Date(
  from = as.Date("2020-03-29"),
  to = last_as_of,
  by = "week")

get_first_and_final_revisions <- function(setting, as_ofs) {
  if (setting == "US") {
    spatial_resolution <- "state"
    locations_to_keep <- us_locations
    geography <- "US"
    location_name_map <- covidData::fips_codes
  } else {
    spatial_resolution <- "national"
    locations_to_keep <- euro_locations
    geography <- "global"
    location_name_map <- covidData::global_locations
  }

  available_issues <- covidData::available_issue_dates(
    measure = "cases",
    geography = geography)

  result <- purrr::map_dfr(
    as_ofs,
    function(as_of) {
      as_of <- max(as_of, min(available_issues))
      one_as_of_result <- dplyr::bind_rows(
        covidData::load_data(
          as_of = as_of,
          spatial_resolution = spatial_resolution,
          temporal_resolution = "weekly",
          measure = "cases",
          geography = geography) %>%
          dplyr::mutate(
            target_variable = "Cases",
            as_of = as_of
          ) %>%
          dplyr::filter(location %in% locations_to_keep),
        covidData::load_data(
          as_of = as_of,
          spatial_resolution = spatial_resolution,
          temporal_resolution = "weekly",
          measure = "deaths",
          geography = geography) %>%
          dplyr::mutate(
            target_variable = "Deaths",
            as_of = as_of
          ) %>%
          dplyr::filter(location %in% locations_to_keep)
      ) %>%
        dplyr::left_join(location_name_map, by = "location")

      return(one_as_of_result)
    })

  result <- result %>%
    dplyr::group_by(location, location_name, date, target_variable) %>%
    dplyr::filter(
      as_of == min(as_of) | as_of == max(as_of)
    ) %>%
    dplyr::mutate(
      as_of_binary = ifelse(
        as_of == max(as_of),
        paste0("Report as of ", last_as_of),
        "First Report"
      )
    )

  return(result)
}

us_data <- get_first_and_final_revisions(
  setting = "US",
  as_ofs = as_ofs)
# temp_us_data <- get_first_and_final_revisions(
#   setting = "US",
#   as_ofs = tail(as_ofs, 1))

combined_data <- us_data %>%
  dplyr::mutate(location_name = paste0(location_name, ", U.S."))
combined_data <- dplyr::bind_rows(
  combined_data,
  combined_data %>%
    ungroup() %>%
    slice_max(date) %>%
    mutate(as_of_binary = "First Report")
)

#us_data %>% filter(date == "2022-02-12")

# load component forecasts
# Location of main covid19-forecast-hub repo where component model submissions
# can be found
submissions_root <- paste0(
  "code/retrospective-forecasts/")
first_forecast_date <- lubridate::ymd("2020-07-27")
last_forecast_date <- lubridate::ymd("2022-03-14")
num_forecast_weeks <-
  as.numeric(last_forecast_date - first_forecast_date) / 7 + 1

us_forecasts <- purrr::map_dfr(
  c('deaths', 'cases'),
  function(measure) {
    # load forecasts
    all_forecasts <- load_retrospective_ensemble_forecasts(
      submissions_root = submissions_root,
      forecast_dates = lubridate::ymd(first_forecast_date) +
        seq(from = 0, length = num_forecast_weeks) * 7,
      us_locations,
      spatial_scales = "state",
      response_vars = paste0("inc_", substr(measure, 1, nchar(measure) - 1))
    )

    all_forecasts <- all_forecasts %>%
      dplyr::mutate(
        model_brief = dplyr::case_when(
          model == "combine_method_ew-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Equal Weighted Mean",
          model == "combine_method_median-quantile_groups_per_model-window_size_0-top_models_0-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Equal Weighted Median",
          model == "combine_method_rel_wis_weighted_mean-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Rel. WIS Weighted Mean",
          model == "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all-estimation_scale_state" ~ "Rel. WIS Weighted Median"
        )
      )
  })

combined_forecasts <- us_forecasts %>%
  dplyr::mutate(
    location_name = paste0(location_name, ", U.S.")
  )

forecast_dates_to_plot <- seq.Date(
  from = as.Date("2022-03-14"),
  # by = -14*4,
  by = -42,
  length = 100)

forecast_dates_to_plot <- forecast_dates_to_plot[
  forecast_dates_to_plot %in% combined_forecasts$forecast_date]
forecast_medians <- combined_forecasts %>%
  dplyr::filter(
    model_brief %in% c("Equal Weighted Median", "Rel. WIS Weighted Median"),
    quantile == 0.5,
    forecast_date %in% forecast_dates_to_plot
  ) %>%
  dplyr::mutate(
    target_variable = ifelse(target_variable == "inc case",
                             "Cases", "Deaths")) %>%
  dplyr::bind_rows(
    dplyr::left_join(
      tidyr::expand_grid(
        model_brief = c("Equal Weighted Median", "Rel. WIS Weighted Median"),
        target_variable = c("Cases", "Deaths")),
      combined_data %>%
        dplyr::filter(
          as_of_binary == "First Report",
          date %in% (forecast_dates_to_plot - 2)
        ),
      by = "target_variable"
    ) %>%
    dplyr::transmute(
      model_brief = model_brief,
      target_variable = target_variable,
      forecast_date = date + 2,
      target_end_date = date,
      location = location, location_name = location_name,
      value = inc
    )
  )

forecast_intervals <- combined_forecasts %>%
  dplyr::filter(
    model_brief %in% c("Equal Weighted Median", "Rel. WIS Weighted Median"),
    quantile %in% c(0.025, 0.975),
    forecast_date %in% forecast_dates_to_plot
  ) %>%
  dplyr::mutate(
    target_variable = ifelse(target_variable == "inc case",
                             "Cases", "Deaths")) %>%
  dplyr::bind_rows(
    dplyr::left_join(
      tidyr::expand_grid(
        model_brief = c("Equal Weighted Median", "Rel. WIS Weighted Median"),
        target_variable = c("Cases", "Deaths"),
        quantile = c(0.025, 0.975)
      ),
      combined_data %>%
        dplyr::filter(
          as_of_binary == "First Report",
          date %in% (forecast_dates_to_plot - 2)
        ),
      by = "target_variable"
    ) %>%
      dplyr::transmute(
        model_brief = model_brief,
        target_variable = target_variable,
        forecast_date = date + 2,
        target_end_date = date,
        location = location, location_name = location_name,
        quantile = quantile,
        value = inc
      )
  ) %>%
  dplyr::select(model_brief, forecast_date, location, location_name, target_variable, target_end_date, quantile, value) %>%
  tidyr::pivot_wider(names_from = "quantile", values_from = "value", names_prefix = "q_")

p_both <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_ribbon(
    data = forecast_intervals %>%
      dplyr::filter(
        location != "FR",
        model_brief == "Equal Weighted Median") %>%
      dplyr::mutate(
        facet_var = factor(
          paste0(target_variable, " in ", location_name),
          # levels = c("Cases in Ohio, U.S.", "Deaths in Ohio, U.S.",
          levels = c("Cases in Missouri, U.S.", "Deaths in Missouri, U.S.",
                     "Cases in Texas, U.S.", "Deaths in Texas, U.S.")
        )
      ),
#        target_variable == "Deaths"),
    mapping = aes(
      x = target_end_date,
      ymin = q_0.025,
      ymax = q_0.975,
      fill = model_brief,
      group = paste0(forecast_date, model_brief)),
   alpha = 0.7
  ) +
  geom_line(
    data = combined_data %>%
      dplyr::filter(
        location != "FR",
        # target_variable == "Deaths",
        date >= as.Date("2020-04-01")) %>%
      dplyr::mutate(
        facet_var = factor(
          paste0(target_variable, " in ", location_name),
          # levels = c("Cases in Ohio, U.S.", "Deaths in Ohio, U.S.",
          levels = c("Cases in Missouri, U.S.", "Deaths in Missouri, U.S.",
                     "Cases in Texas, U.S.", "Deaths in Texas, U.S.")
        )
      ),
    mapping = aes(x = date, y = inc, color = as_of_binary, size = as_of_binary)) +
  scale_color_manual(
    "Data Report Date",
    values = c("First Report" = "#5aae61", "Report as of 2022-05-16" = "black")
  ) +
  scale_size_manual(
    "Data Report Date",
    values = c("First Report" = 1, "Report as of 2022-05-16" = 0.3)
  ) +
  ggnewscale::new_scale_color() +
  geom_line(
    data = forecast_medians %>%
      dplyr::filter(
        location != "FR",
        model_brief == "Equal Weighted Median") %>%
      dplyr::mutate(
        facet_var = factor(
          paste0(target_variable, " in ", location_name),
          # levels = c("Cases in Ohio, U.S.", "Deaths in Ohio, U.S.",
          levels = c("Cases in Missouri, U.S.", "Deaths in Missouri, U.S.",
                     "Cases in Texas, U.S.", "Deaths in Texas, U.S.")
        )
      ),
        # target_variable == "Deaths"),
    mapping = aes(
      x = target_end_date, y = value,
      color = model_brief,
      group = paste0(forecast_date, model_brief))
  ) +
  geom_point(
    data = forecast_medians %>%
      dplyr::filter(
        !is.na(horizon),
        location != "FR",
        model_brief == "Equal Weighted Median") %>%
      dplyr::mutate(
        facet_var = factor(
          paste0(target_variable, " in ", location_name),
          # levels = c("Cases in Ohio, U.S.", "Deaths in Ohio, U.S.",
          levels = c("Cases in Missouri, U.S.", "Deaths in Missouri, U.S.",
                     "Cases in Texas, U.S.", "Deaths in Texas, U.S.")
        )
      ),
        # target_variable == "Deaths"),
    mapping = aes(
      x = target_end_date, y = value,
      color = model_brief,
      group = paste0(forecast_date, model_brief))
  ) +
  scale_color_manual(
    "Ensemble Forecast",
    values = "#2166ac") +
  scale_fill_manual(
    "Ensemble Forecast",
    values = "#67a9cf") +
  geom_vline(xintercept = as.Date("2021-05-03"), linetype = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ facet_var, ncol = 1, scales = "free_y") +
  # ggtitle("Deaths") +
  xlab("") +
  ylab("Weekly Reported Cases or Deaths") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

pdf("manuscript/figures/data_eval_phases_with_forecasts.pdf", width = 8.25, height = 8)
print(p_both)
dev.off()
