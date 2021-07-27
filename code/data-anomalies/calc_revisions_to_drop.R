library(tidyverse)
library(covidData)
library(here)
setwd(here())

first_as_of_date <- as.Date("2020-04-26")
last_as_of_date <- as.Date("2021-07-25")
all_as_ofs <- seq.Date(
  from = first_as_of_date,
  to = last_as_of_date,
  by = 7
)


# add in dates offset_weeks after data that were revised,
# if the originally reported (unrevised) data would have been used
# as an input to forecasting
get_adjacent_revisions <- function(
  revisions_to_drop,
  revisions,
  offset_weeks) {
  offset_days <- offset_weeks * 7
  adjacent_revisions <- revisions %>%
    # keep all revisions that have a location/date matching one to plot
    dplyr::semi_join(revisions_to_drop, by = c("location", "date")) %>%
    # don't want to score if unrevised data would have been used as input
    # to forecasting models
    # keep as a revision that should be dropped if the first issue_date on
    # which revision occurred was more than offset_days after the date for
    # which data were revised
    dplyr::group_by(location, date) %>%
    dplyr::slice_min(issue_date, n = 1) %>%
    dplyr::filter(issue_date > date + offset_days + 1) %>%
    dplyr::distinct(location, date) %>%
    dplyr::mutate(
      date = date + offset_days,
      anomaly_type = "adjacent to revision"
    )
  return(adjacent_revisions)
}


# plot death outliers
for (measure in c("cases", "deaths")) {
#for (measure in "deaths") {
  outliers <- readr::read_csv(
    paste0("code/data-anomalies/outliers-inc-", measure, ".csv"),
    col_types = cols(
      location = col_character(),
      location_abbreviation = col_character(),
      date = col_date(format = ""),
      start_issue_date = col_date(format = ""),
      end_issue_date = col_date(format = "")
    )
  )

  revisions <- readr::read_csv(
    paste0("code/data-anomalies/revisions-inc-", measure, ".csv"),
    col_types = cols(
      location = col_character(),
      location_name = col_character(),
      date = col_date(format = ""),
      orig_obs = col_number(),
      issue_date = col_date(format = ""),
      real_diff = col_number(),
      relative_diff_orig_base = col_double(),
      relative_diff_revised_base = col_double()
    )
  ) %>%
    dplyr::filter(
      # revision large enough that it shouldn't be scored
      abs(real_diff) > 20,
      abs(relative_diff_orig_base) > 0.4 | abs(relative_diff_revised_base) > 0.4
    )

  # data_all_as_ofs <- purrr::map_dfr(
  #   all_as_ofs,
  #   function(as_of) {
  #     covidData::load_data(
  #       as_of = as_of,
  #       spatial_resolution = "state",
  #       temporal_resolution = "weekly",
  #       measure = measure
  #     ) %>%
  #       dplyr::mutate(as_of = as_of)
  #   }
  # )

  location_issues <- tidyr::expand_grid(
    location = covidData::fips_codes %>%
      dplyr::filter(nchar(location) == 2, !(location %in% c("US", "74"))) %>%
      dplyr::pull(location),
    as_of = all_as_ofs
  ) %>%
    dplyr::left_join(
      covidData::fips_codes %>%
        dplyr::select(location, location_name),
      by = "location")

  revisions_to_drop <- purrr::map_dfr(
    seq_len(nrow(location_issues)),
    function(i) {
    # data <- data_all_as_ofs %>%
    #   dplyr::filter(
    #     as_of == location_issues$as_of[i],
    #     location == location_issues$location[i])

    # data_first_observed <- data_all_as_ofs %>%
    #   dplyr::filter(
    #     date %in% data$date,
    #     date != max(data$date),
    #     location == location_issues$location[i]
    #   ) %>%
    #   dplyr::group_by(location, date) %>%
    #   dplyr::slice_min(as_of, n = 1)

    # combined_data <- dplyr::bind_rows(
    #   data %>%
    #     dplyr::mutate(report_type = "current"),
    #   data_first_observed %>%
    #     dplyr::mutate(report_type = "first observed")
    # ) %>%
    #   dplyr::mutate(
    #     report_type = factor(report_type,
    #       levels = c("first observed", "current"))
    #   )

    outliers_to_plot <- outliers %>%
      dplyr::filter(
        location == location_issues$location[i],
        start_issue_date <= location_issues$as_of[i],
        end_issue_date >= location_issues$as_of[i]
      ) %>%
      dplyr::mutate(
        anomaly_type = dplyr::case_when(
          date == "2020-12-26" ~ "christmas",
          date == "2021-01-02" ~ "new year",
          TRUE ~ "other outlier"
        )
      )# %>%
      # dplyr::left_join(data, by = c("location", "date"))

    revisions_to_drop <- revisions %>%
      dplyr::filter(
        # relevant for current location/as_of date combination
        location == location_issues$location[i],
        issue_date == location_issues$as_of[i]#,
        # for a date we're plotting
#        date %in% combined_data$date
      ) %>%
      dplyr::mutate(
        anomaly_type = "revision"
      )

    # drop revisions to values that were first reported as outliers
    initial_outliers <- revisions_to_drop %>%
      dplyr::inner_join(
        outliers,
        by = c("location", "date")
      ) %>%
      dplyr::filter(start_issue_date == date + 1)

    revisions_to_drop <- revisions_to_drop %>%
      dplyr::anti_join(initial_outliers, by = c("location", "date"))

    adjacent_revisions <- purrr::map_dfr(
      1:3,
      get_adjacent_revisions,
      revisions_to_drop = revisions_to_drop,
      revisions = revisions
    ) %>%
      dplyr::distinct() %>%
      dplyr::anti_join(revisions_to_drop, by = c("location", "date"))

    revisions_to_drop <- dplyr::bind_rows(
      revisions_to_drop,
      adjacent_revisions
    ) %>%
      dplyr::arrange(date) %>%
      dplyr::transmute(
        location = location,
        date = date,
        issue_date = location_issues$as_of[i],
        anomaly_type = anomaly_type
      )
      # dplyr::left_join(data_first_observed, by = c("location", "date"))

    return(revisions_to_drop)
  })

  readr::write_csv(
    revisions_to_drop,
    paste0("code/data-anomalies/revisions-to-drop-inc-", measure, ".csv")
  )
}
