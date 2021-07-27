library(tidyverse)
library(covidData)
library(here)
setwd(here())

## note that this file depends on a recent successful covidData `make all`

## function to load one week of data for a particular target variable as of a certain date
load_one_as_of <- function(as_of, target_var) {
  all_locations <- covidData::fips_codes %>%
    dplyr::filter(nchar(location) == 2, location != "US") %>%
    dplyr::pull(location)
  
  load_data(
    as_of = as_of,
    spatial_resolution = "state",
    temporal_resolution = "weekly",
    measure = target_var) %>%
    mutate(as_of = lubridate::ymd(as_of)) #add column listing as_of date
}

# define Sundays to use for as_of dates
most_recent_sunday <- lubridate::floor_date(Sys.Date(), unit = "week")

get_next_sunday <- function(csv, default, start_from_scratch = FALSE) {
  if (file.exists(csv) & !start_from_scratch) {
    suppressMessages(read_csv(csv)) %>% 
    summarise(max(issue_date))%>% 
    pull(1)
  } else {
    lubridate::ymd(default)
  }
}

first_as_of <- as.Date("2020-04-26")

for (target_var in c("cases", "deaths")) {
  as_ofs <- seq.Date(
    from = first_as_of,
    to = most_recent_sunday,
    by = 7
  )

  ## get all data for each target_var
  weekly_inc <- plyr::ldply(as_ofs,
   load_one_as_of,
   target_var = target_var)  #combine revisions into 1 dataframe


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
    file=paste0("code/data-anomalies/revisions-inc-", target_var, ".csv"),
    append = FALSE)
}
