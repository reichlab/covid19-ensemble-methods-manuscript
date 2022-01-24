library(tidyverse)
library(covidData)
library(here)
setwd(here())

## note that this file depends on a recent successful covidData `make all`

## function to load one week of data for a particular target variable as of a certain date
load_one_as_of <- function(as_of, target_var, hub = "US") {
  if (hub == "US") {
    spatial_resolution <- "state"
    geography <- "US"
    all_locations <- covidData::fips_codes %>%
      dplyr::filter(nchar(location) == 2, location != "US") %>%
      dplyr::pull(location)
  } else {
    spatial_resolution <- "national"
    geography <- "global"
    all_locations <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR",
      "ES", "FR", "HR", "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT",
      "PL", "PT", "RO", "SI", "SK", "FI", "SE", "GB", "IS", "LI", "NO", "CH")
  }
  
  load_data(
    as_of = as_of,
    spatial_resolution = spatial_resolution,
    temporal_resolution = "weekly",
    measure = target_var,
    geography = geography) %>%
    dplyr::mutate(as_of = lubridate::ymd(as_of)) %>%
    dplyr::filter(location %in% all_locations)
}

# define Sundays to use for as_of dates
first_as_of <- as.Date("2020-04-26")
#last_as_of <- lubridate::floor_date(Sys.Date(), unit = "week")
last_as_of <- as.Date("2021-12-05")

#for (hub in c("US", "EU")) {
for (hub in "US") {
#for (hub in "EU") {
  # for (target_var in c("cases", "deaths")) {
  # for (target_var in "cases") {
  for (target_var in "deaths") {
    as_ofs <- seq.Date(
      from = first_as_of,
      to = last_as_of,
      by = 7
    )

    ## get all data for each target_var
    weekly_inc <- plyr::ldply(as_ofs,
    load_one_as_of,
    target_var = target_var,
    hub = hub)  #combine revisions into 1 dataframe

    ## identify and compute the revisions
    revisions <- suppressMessages(purrr::map_dfr(
      as_ofs[-1], # remove first issue which has no earlier issues to revise 
      function(as_of) {
        updates <- weekly_inc %>%
          ## filtering to only include first report of each week's value
          dplyr::group_by(location, date) %>%
          dplyr::slice_min(as_of, n = 1) %>%
          dplyr::select(-as_of, -cum) %>%
          ## joining this week's obs
          dplyr::inner_join(
            weekly_inc %>%
              dplyr::filter(as_of == UQ(as_of)) %>%
              dplyr::select(-cum),
              by = c("location", "date")
          ) %>%
          ## only keeping rows where obs are not the same
          dplyr::filter(inc.x != inc.y)
        }) %>%
      dplyr::left_join(covidData::fips_codes) %>%
      dplyr::rename(
        issue_date = as_of,
        orig_obs = inc.x,         ## inc.x comes from the df filtered to first reported observation
        revised_obs = inc.y) %>%  ## inc.y comes from the df with most recent obs
      dplyr::mutate(
        real_diff = revised_obs - orig_obs,
        relative_diff_orig_base = ifelse(
          orig_obs == 0,
          revised_obs,
          real_diff / abs(orig_obs)),
        relative_diff_revised_base = ifelse(
          orig_obs == 0,
          revised_obs,
          real_diff / abs(revised_obs))
      ) %>%
      dplyr::select(location, location_name, date, orig_obs, issue_date,
        real_diff, relative_diff_orig_base, relative_diff_revised_base)
    )

    write_csv(
      revisions,
      file=paste0("code/data-anomalies/revisions-inc-", target_var, "-", hub, ".csv"),
      append = FALSE)
  }
}
