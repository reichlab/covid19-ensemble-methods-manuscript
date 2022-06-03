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
    horizon_group == "All Horizons",
    is.na(max_weight),
    # Keep subset of models for model development set
    forecast_date < "2021-05-03",
    # (top_models == "All Models" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median")) |
    (top_models == "Top 10" & combine_method %in% c("Weighted Mean", "Rel. WIS Weighted Median"))) %>%
    # (top_models == "Top 10" & combine_method == "Rel. WIS Weighted Median")) %>%
  dplyr::mutate(
    window_size = factor(
      window_size,
      levels = c("Untrained", "Trained on 4 weeks", "Trained on 8 weeks", "Trained on 12 weeks", "Trained on full history")
    ),
    model_brief = paste0(combine_method, "-",
      quantile_groups, "-",
      window_size, "-",
      top_models, "-",
      horizon_group)
  )

# ensure that we have the same scores for all model variations
orig_score_count <- nrow(all_scores)
all_scores <- subset_scores_to_common(all_scores)
common_score_count <- nrow(all_scores)
orig_score_count == common_score_count


# overall mean scores by horizon and target variable
overall_means <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::starts_with("quantile_coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(
    nominal_coverage = as.numeric(nominal_coverage),
    coverage_diff = empirical_coverage - nominal_coverage
  ) %>%
  dplyr::filter(!is.na(empirical_coverage))

p_coverage <- ggplot(data = overall_means) +
  geom_line(mapping = aes(x = nominal_coverage,
                          y = coverage_diff,
                          color = combine_method,
                          linetype = quantile_groups,
                          group = paste0(combine_method, quantile_groups))) +
  geom_point(mapping = aes(x = nominal_coverage,
                           y = coverage_diff,
                           color = combine_method,
                           shape = quantile_groups)) +
  facet_grid(window_size ~ target_variable) + #, scales = "free_x") +
  geom_hline(yintercept = 0) +
  scale_color_manual(
    "Combination Method",
    values = c(
      # "Equal Weighted Mean" = "#ef8a62",
      # "Equal Weighted Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Rel. WIS Weighted Median" = "#2166ac")
  ) +
  scale_shape_discrete("Parameter Sharing\nAcross Quantile Levels") +
  scale_linetype_discrete("Parameter Sharing\nAcross Quantile Levels") +
  # ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  xlab("Nominal Quantile Level") +
  ylab("Empirical Coverage Rate Minus Nominal Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf("manuscript/figures/quantile_coverage_quantile_group.pdf", width = 8, height = 8)
print(p_coverage)
dev.off()


# code below may be out of date and is not used in the manuscript

by_date_means <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable, forecast_date, horizon) %>%
  dplyr::summarize(
    dplyr::across(tidyselect::starts_with("quantile_coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage)) %>%
  dplyr::filter(!is.na(empirical_coverage))

p_coverage <- ggplot(
    data = by_date_means %>%
      dplyr::filter(
        nominal_coverage %in% c(0.75, 0.9),
        target_variable == "Cases",
        window_size == "Trained on full history",
        combine_method == "Weighted Median") %>%
      dplyr::mutate(
        horizon = paste0("Horizon ", horizon)
      )
    ) +
  geom_line(mapping = aes(x = forecast_date, y = empirical_coverage, color = factor(nominal_coverage), linetype = quantile_groups, group = paste0(nominal_coverage, quantile_groups))) +
  geom_point(mapping = aes(x = forecast_date, y = empirical_coverage, color = factor(nominal_coverage), shape = quantile_groups)) +
  geom_hline(
    data = tidyr::expand_grid(
      nominal_coverage = c(0.75, 0.9),
      horizon = paste0("Horizon ", 1:4)
    ),
    mapping = aes(yintercept = nominal_coverage, color = factor(nominal_coverage))
  ) +
  facet_wrap(~ horizon, ncol = 1) + #, scales = "free_x") +
  scale_color_discrete("Nominal Quantile\nCoverage Level") +
  scale_shape_discrete("Parameter Sharing\nAcross Quantile Levels") +
  scale_linetype_discrete("Parameter Sharing\nAcross Quantile Levels") +
  ylab("Empirical Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf("manuscript/figures/quantile_coverage_quantile_group_by_date_Cases.pdf", width = 7, height = 8)
print(p_coverage)
dev.off()



