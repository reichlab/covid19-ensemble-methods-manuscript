# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

spatial_scale <- "state"
response_vars <- c("inc_death", "inc_case")

subset_scores_to_common <- function(scores) {
  id_counts_by_model <- scores %>%
    dplyr::mutate(
      combined_id = paste0(location, forecast_date, target_end_date)
    ) %>%
    dplyr::count(model_brief, combined_id) %>%
    tidyr::pivot_wider(names_from = "model_brief", values_from = "n")
  combined_ids_to_drop <- id_counts_by_model[["combined_id"]][
    id_counts_by_model %>%
      dplyr::select(-combined_id) %>%
      as.matrix() %>%
      apply(1, function(x) { any(is.na(x)) })
  ]
  scores <- scores %>%
    dplyr::filter(
      !(paste0(location, forecast_date, target_end_date) %in% combined_ids_to_drop)
    )
  return(scores)
}

# read in scores
all_scores <- dplyr::bind_rows(
  readRDS("code/scores/retrospective_scores-state-inc_death.rds"),
  readRDS("code/scores/retrospective_scores-state-inc_case.rds")
) %>%
  dplyr::filter(
    true_value >= 0,
    horizon_group == "All Horizons", quantile_groups == "Per Model",
    drop_anomalies == FALSE,
    (top_models == "Top 10" & window_size == "Trained on 12 weeks" & combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) |
    (top_models == "All Models" & window_size == "Untrained" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median"))
  ) %>%
  dplyr::mutate(
    combine_method = factor(
      combine_method,
      levels = c("Equal Weighted Mean", "Equal Weighted Median", "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")),
    model_brief = combine_method,
    phase = ifelse(forecast_date >= "2021-05-03", "Prospective Evaluation: US", "Model Development: US")
  )

# ensure that we have the same scores for all model variations
orig_score_count <- nrow(all_scores)
all_scores <- subset_scores_to_common(all_scores)
common_score_count <- nrow(all_scores)
orig_score_count == common_score_count


# overall mean scores by horizon and target variable
overall_means <- all_scores %>%
  dplyr::group_by(phase, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::contains("coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(
    nominal_coverage = as.numeric(nominal_coverage),
    empirical_coverage_diff = empirical_coverage - nominal_coverage
  ) %>%
  dplyr::filter(!is.na(empirical_coverage))


# mean scores after dropping anomalies:
# - target end date is an outlier
# - forecast date is on or shortly after a date for which a substantial revision was made
outliers <- dplyr::bind_rows(
  readr::read_csv("code/data-anomalies/outliers-inc-cases-US.csv") %>%
    dplyr::mutate(target_variable = "Cases"),
  readr::read_csv("code/data-anomalies/outliers-inc-deaths-US.csv") %>%
    dplyr::mutate(target_variable = "Deaths")
) %>%
  dplyr::transmute(
    location = location,
    target_end_date = date,
    target_variable = target_variable)

revisions <- dplyr::bind_rows(
  readr::read_csv("code/data-anomalies/revisions-to-drop-inc-cases-US.csv") %>%
    dplyr::mutate(target_variable = "Cases"),
  readr::read_csv("code/data-anomalies/revisions-to-drop-inc-deaths-US.csv") %>%
    dplyr::mutate(target_variable = "Deaths")
) %>%
  dplyr::transmute(
    location = location,
    forecast_date = date,
    target_variable = target_variable)

non_anomalous_scores <- all_scores %>%
  dplyr::anti_join(
    outliers,
    by = c("location", "target_end_date", "target_variable")
  ) %>%
  dplyr::anti_join(
    revisions,
    by = c("location", "forecast_date", "target_variable")
  )

# non-anomalous overall mean scores by horizon and target variable
non_anomalous_overall_means <- non_anomalous_scores %>%
  dplyr::group_by(phase, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::contains("coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(
    nominal_coverage = as.numeric(nominal_coverage),
    empirical_coverage_diff = empirical_coverage - nominal_coverage
  ) %>%
  dplyr::filter(!is.na(empirical_coverage))




all_scores_europe <- dplyr::bind_rows(
  readRDS("code/scores/retrospective_scores-euro_countries-inc_death.rds"),
  readRDS("code/scores/retrospective_scores-euro_countries-inc_case.rds")
) %>%
  dplyr::filter(
    true_value >= 0,
    horizon_group == "All Horizons", quantile_groups == "Per Model",
    (top_models == "Top 10" & window_size == "Trained on 12 weeks" & combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) |
    (top_models == "All Models" & window_size == "Untrained" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median"))
  ) %>%
  dplyr::mutate(
    combine_method = factor(
      combine_method,
      levels = c("Equal Weighted Mean", "Equal Weighted Median", "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")),
    model_brief = combine_method,
    phase = "Prospective Evaluation: Europe"
  )

# ensure that we have the same scores for all model variations
orig_score_count <- nrow(all_scores_europe)
all_scores_europe <- subset_scores_to_common(all_scores_europe)
common_score_count <- nrow(all_scores_europe)
orig_score_count == common_score_count



# mean scores after dropping anomalies:
# - target end date is an outlier
# - forecast date is on or shortly after a date for which a substantial revision was made
outliers <- dplyr::bind_rows(
  readr::read_csv("code/data-anomalies/outliers-inc-cases-EU.csv") %>%
    dplyr::mutate(target_variable = "Cases"),
  readr::read_csv("code/data-anomalies/outliers-inc-deaths-EU.csv") %>%
    dplyr::mutate(target_variable = "Deaths")
) %>%
  dplyr::transmute(
    location = location,
    target_end_date = date,
    target_variable = target_variable)

revisions <- dplyr::bind_rows(
  readr::read_csv("code/data-anomalies/revisions-to-drop-inc-cases-EU.csv") %>%
    dplyr::mutate(target_variable = "Cases"),
  readr::read_csv("code/data-anomalies/revisions-to-drop-inc-deaths-EU.csv") %>%
    dplyr::mutate(target_variable = "Deaths")
) %>%
  dplyr::transmute(
    location = location,
    forecast_date = date,
    target_variable = target_variable)

non_anomalous_scores_europe <- all_scores_europe %>%
  dplyr::anti_join(
    outliers,
    by = c("location", "target_end_date", "target_variable")
  )
# no revisions to drop in Europe
  # %>%
#  dplyr::anti_join(
#    revisions,
#    by = c("location", "forecast_date", "target_variable")
#  )


# overall mean scores by horizon and target variable
overall_means_europe <- all_scores_europe %>%
  dplyr::group_by(phase, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::contains("coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(
    nominal_coverage = as.numeric(nominal_coverage),
    empirical_coverage_diff = empirical_coverage - nominal_coverage
  ) %>%
  dplyr::filter(!is.na(empirical_coverage))

overall_means <- dplyr::bind_rows(
  overall_means, overall_means_europe
) %>%
  dplyr::mutate(
    phase = factor(phase, levels = c("Model Development: US", "Prospective Evaluation: US", "Prospective Evaluation: Europe"))
  )

# non-anomalous overall mean scores by horizon and target variable
non_anomalous_overall_means_europe <- non_anomalous_scores_europe %>%
  dplyr::group_by(phase, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::contains("coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(
    nominal_coverage = as.numeric(nominal_coverage),
    empirical_coverage_diff = empirical_coverage - nominal_coverage
  ) %>%
  dplyr::filter(!is.na(empirical_coverage))

non_anomalous_overall_means <- dplyr::bind_rows(
  non_anomalous_overall_means, non_anomalous_overall_means_europe
) %>%
  dplyr::mutate(
    phase = factor(phase, levels = c("Model Development: US", "Prospective Evaluation: US", "Prospective Evaluation: Europe"))
  )


p_coverage <- ggplot(
  data = overall_means %>%
    dplyr::filter(top_models %in% c("All Models", "Top 10"))
  ) +
  geom_line(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, linetype = combine_method, group = paste0(combine_method, top_models))) +
  geom_point(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, shape = combine_method)) +
#  facet_wrap( ~ target_variable) +
  facet_grid(phase ~ target_variable) +
  geom_abline(intercept = 0, slope = 0) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Equal Weighted Mean" = "#ef8a62",
      "Equal Weighted Median" = "#67a9cf",
      "Rel. WIS Weighted Mean" = "#b2182b",
      "Rel. WIS Weighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Equal Weighted Mean" = 4,
      "Equal Weighted Median" = 5,
      "Rel. WIS Weighted Mean" = 2,
      "Rel. WIS Weighted Median" = 1)
  ) +
  scale_shape_discrete("Combination Method") +
  # ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  xlab("Nominal Quantile Level") +
  ylab("Empirical Coverage Rate Minus Nominal Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf("manuscript/figures/quantile_coverage_by_phase.pdf", width = 8, height = 8)
print(p_coverage)
dev.off()


# coverage after dropping things affected by data anomalies

p_coverage <- ggplot(
  data = non_anomalous_overall_means %>%
    dplyr::filter(top_models %in% c("All Models", "Top 10"))
  ) +
  geom_line(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, linetype = combine_method, group = paste0(combine_method, top_models))) +
  geom_point(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, shape = combine_method)) +
#  facet_wrap( ~ target_variable) +
  facet_grid(phase ~ target_variable) +
  geom_abline(intercept = 0, slope = 0) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Equal Weighted Mean" = "#ef8a62",
      "Equal Weighted Median" = "#67a9cf",
      "Rel. WIS Weighted Mean" = "#b2182b",
      "Rel. WIS Weighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Equal Weighted Mean" = 4,
      "Equal Weighted Median" = 5,
      "Rel. WIS Weighted Mean" = 2,
      "Rel. WIS Weighted Median" = 1)
  ) +
  scale_shape_discrete("Combination Method") +
  # ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  xlab("Nominal Quantile Level") +
  ylab("Empirical Coverage Rate Minus Nominal Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf("manuscript/figures/quantile_coverage_by_phase_drop_data_anomalies.pdf", width = 8, height = 8)
print(p_coverage)
dev.off()


# how many scores were dropped due to anomalies?
table(all_scores$phase)
table(non_anomalous_scores$phase)
table(all_scores_europe$phase)
table(non_anomalous_scores_europe$phase)
