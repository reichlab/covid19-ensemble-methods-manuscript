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
    horizon_group == "All Horizons", quantile_groups == "Per Model",
    combine_method %in% c("Mean", "Median", "Weighted Mean", "Weighted Median")) %>%
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
    dplyr::across(tidyselect::contains("coverage"), mean)
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("quantile_coverage"),
    names_to = "nominal_coverage",
    names_prefix = "quantile_coverage_",
    values_to = "empirical_coverage") %>%
  dplyr::mutate(nominal_coverage = as.numeric(nominal_coverage)) %>%
  dplyr::filter(!is.na(empirical_coverage))

p_coverage <- ggplot(
  data = overall_means %>%
    dplyr::filter(top_models %in% c("All Models", "Top 10"))
  ) +
  geom_line(mapping = aes(x = nominal_coverage, y = empirical_coverage, color = combine_method, linetype = top_models, group = paste0(combine_method, top_models))) +
  geom_point(mapping = aes(x = nominal_coverage, y = empirical_coverage, color = combine_method, shape = top_models)) +
  facet_grid(window_size ~ target_variable) + #, scales = "free_x") +
  geom_abline(intercept = 0, slope = 1) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Mean" = "#ef8a62",
      "Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Weighted Median" = "#2166ac")
  ) +
  scale_shape_discrete("Number of\nComponent\nForecasters") +
  scale_linetype_discrete("Number of\nComponent\nForecasters") +
  ylim(c(0, 1)) +
  xlim(c(0, 1)) +
  xlab("Nominal Quantile Level") +
  ylab("Empirical Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

pdf("manuscript/figures/quantile_coverage_main.pdf", width = 7, height = 8)
print(p_coverage)
dev.off()



# second version of the coverage plot with difference in empirical and 
# nominal coverage
# ------------------------------------------------------------------------------
overall_means %>%
  dplyr::mutate(difference = empirical_coverage - nominal_coverage) %>%
  dplyr::filter(top_models %in% c("All Models", "Top 10")) %>%
  ggplot() +
  geom_line(mapping = aes(x = nominal_coverage, y = difference, 
                          color = combine_method, linetype = top_models, 
                          group = paste0(combine_method, top_models))) +
  geom_point(mapping = aes(x = nominal_coverage, y = difference, 
                           color = combine_method, shape = top_models)) +
  facet_grid(window_size ~ target_variable) + #, scales = "free_x") +
  geom_abline(intercept = 0, slope = 0) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Mean" = "#ef8a62",
      "Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Weighted Median" = "#2166ac")
  ) +
  scale_shape_discrete("Number of\nComponent\nForecasters") +
  scale_linetype_discrete("Number of\nComponent\nForecasters") +
  xlim(c(0, 1)) +
  xlab("Nominal Quantile Level") +
  ylab("Empirical Coverage Rate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  
  
