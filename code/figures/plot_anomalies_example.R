library(tidyverse)
library(covidData)
library(here)
setwd(here())

# plot death outliers
hub <- "US"

if (hub == "US") {
  first_as_of_date <- as.Date("2020-04-26")
} else {
  first_as_of_date <- as.Date("2021-05-02")
}
# last_as_of_date <- as.Date("2021-12-05")
last_as_of_date <- as.Date("2022-05-15")
all_as_ofs <- seq.Date(
  from = first_as_of_date,
  to = last_as_of_date,
  by = 7
)

measure <- "deaths"
outliers <- readr::read_csv(
  paste0("code/data-anomalies/outliers-inc-",
    measure,
    "-", hub,
    ".csv"),
  col_types = cols(
    location = col_character(),
    location_abbreviation = col_character(),
    date = col_date(format = ""),
    start_issue_date = col_date(format = ""),
    end_issue_date = col_date(format = "")
  )
)

revisions <- readr::read_csv(
  paste0("code/data-anomalies/revisions-to-drop-inc-",
    measure,
    "-", hub,
    ".csv"),
  col_types = cols(
    location = col_character(),
    date = col_date(format = ""),
    issue_date = col_date(format = "")
  )
)

data_all_as_ofs <- purrr::map_dfr(
  all_as_ofs,
  function(as_of) {
    data_one_as_of <- covidData::load_data(
      as_of = as_of,
      spatial_resolution = ifelse(hub == "US", "state", "national"),
      temporal_resolution = "weekly",
      measure = measure,
      geography = ifelse(hub == "US", "US", "global")
    ) %>%
      dplyr::mutate(as_of = as_of)

    if (hub == "EU") {
      euro_hub_locations <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR",
        "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
        "PL", "PT", "RO", "SI", "SK", "FI", "SE", "GB", "IS", "LI", "NO", "CH")
      data_one_as_of <- data_one_as_of %>%
        dplyr::filter(location %in% euro_hub_locations)
    }

    return(data_one_as_of)
  }
)

location_issues <- tidyr::expand_grid(
  location = "48",
  as_of = last_as_of_date
#      as_of = all_as_ofs
)

if (hub == "US") {
  location_issues <- location_issues %>%
    dplyr::left_join(
      covidData::fips_codes %>%
        dplyr::select(location, location_name),
      by = "location")
} else if (hub == "EU") {
  location_issues <- location_issues %>%
    dplyr::left_join(covidData::global_locations, by = "location")
}

pdf(paste0("manuscript/figures/anomalies_example-inc-",
  measure,
  "-", hub,
  ".pdf"),
  width = 8, height = 8)
for (i in seq_len(nrow(location_issues))) {
  message(paste(i, "of", nrow(location_issues)))

  data <- data_all_as_ofs %>%
    dplyr::filter(
      as_of == location_issues$as_of[i],
      location == location_issues$location[i])

  data_first_observed <- data_all_as_ofs %>%
    dplyr::filter(
      date %in% data$date,
      date != max(data$date),
      location == location_issues$location[i]
    ) %>%
    dplyr::group_by(location, date) %>%
    dplyr::slice_min(as_of, n = 1)

  combined_data <- dplyr::bind_rows(
    data %>%
      dplyr::mutate(report_type = "current"),
    data_first_observed %>%
      dplyr::mutate(report_type = "first observed")
  ) %>%
    dplyr::mutate(
      report_type = factor(report_type,
        levels = c("first observed", "current"))
    )

  outliers_to_plot <- outliers %>%
    dplyr::filter(
      location == location_issues$location[i],
      start_issue_date <= location_issues$as_of[i],
      end_issue_date >= location_issues$as_of[i]
    ) %>%
    dplyr::mutate(
      anomaly_type = dplyr::case_when(
        date == "2020-12-26" | date == "2021-12-25" ~ "Christmas",
        date == "2021-01-02" ~ "New Year",
        date %in% as.Date(c("2020-11-28", "2021-11-27")) ~ "Thanksgiving",
        TRUE ~ "Other outlier"
      )
    ) %>%
    dplyr::left_join(data, by = c("location", "date"))
  
  revisions_to_plot <- revisions %>%
    dplyr::filter(
      location == location_issues$location[i],
      issue_date == location_issues$as_of[i]
    ) %>%
    dplyr::left_join(data_first_observed, by = c("location", "date")) %>%
    dplyr::mutate(
      anomaly_type = ifelse(
        anomaly_type == "revision",
        "Revision",
        "Adjacent to revision"
      )
    )

  anomalies_to_plot <- dplyr::bind_rows(
    outliers_to_plot,
    revisions_to_plot
  )

  p <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_line(
      data = combined_data %>%
        dplyr::mutate(report_type = ifelse(report_type == "current",
          "Reported as of 2022-05-15",
          "First Reported")),
      mapping = aes(x = date, y = inc, color = report_type)) +
    geom_point(
      data = combined_data %>%
        dplyr::mutate(report_type = ifelse(report_type == "current",
          "Reported as of 2022-05-15",
          "First Reported")),
      mapping = aes(x = date, y = inc, color = report_type)) +
    scale_color_manual(
      "Data Version",
      values = c("Reported as of 2022-05-15" = "black", "First Reported" = "#777777")
    )

  if (nrow(anomalies_to_plot) > 0) {
    p <- p +
      ggnewscale::new_scale_color() +
      geom_point(
        data = anomalies_to_plot,
        mapping = aes(
          x = date,
          y = inc,
          color = anomaly_type,
          shape = anomaly_type),
        size = 3) +
      scale_color_manual(
        "Anomaly Type",
        values = c(
          "Christmas" = "red",
          "New Year" = "cornflowerblue",
          "Thanksgiving" = "orange",
          "Other outlier" = "purple",
          "Revision" = "cyan",
          "Adjacent to revision" = "magenta")) +
      scale_shape_manual(
        "Anomaly Type",
        values = c(
          "Christmas" = 15,
          "New Year" = 16,
          "Thanksgiving" = 17,
          "Other outlier" = 18,
          "Revision" = 8,
          "Adjacent to revision" = 5))
  }

  p <- p +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
      limits = c(as.Date("2020-03-01"), as.Date("2022-05-30")),
      expand = expansion()) +
    ggtitle(paste0("Weekly deaths in ", location_issues$location_name[i])) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      panel.grid.major.x = element_line(colour = "darkgrey")
    )
  print(p)
}
dev.off()
