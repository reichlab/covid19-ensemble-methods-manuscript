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
    horizon_group == "All Horizons", quantile_groups == "Per Model") %>%
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
    mae = mean(abs_error),
    mwis = mean(wis)
  )

# overall means for the equally weighted median method
overall_means_base <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, base_mwis = mwis)

# differences in means relative to equally weighted median method
overall_means_others <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, target_variable, mwis) %>%
  dplyr::left_join(overall_means_base, by = c("target_variable")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
  )

# by horizon mean scores by horizon and target variable
by_horizon_means <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size,
    top_models, horizon_group, horizon, target_variable) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis)
  )

# by horizon means for the equally weighted median method
by_horizon_means_base <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, horizon, base_mwis = mwis)

# differences in by horizon means relative to equally weighted median method
by_horizon_means_others <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, target_variable, horizon, mwis) %>%
  dplyr::left_join(by_horizon_means_base, by = c("target_variable", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
  )


# mean across locations for each forecast date and horizon
summarized_scores <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size,
    top_models, horizon_group, target_variable, forecast_date, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis)
  )

scores_base <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
  )

plot_upper_bound <- scores_others %>%
  dplyr::filter(combine_method != "Mean") %>%
  dplyr::group_by(target_variable) %>%
  dplyr::summarize(max_mwis = max(wis_diff_unweighted_median))

p_wis_boxplots <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(
    data = scores_others %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
      dplyr::mutate(
        wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
        value_censored = (wis_diff_unweighted_median > max_mwis)
      ) %>%
      dplyr::filter(!value_censored),
    mapping = aes(
      x = combine_method,
      y = wis_diff_censored,
  #    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
      color = top_models)) +
  geom_point(
    data = scores_others %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
      dplyr::mutate(
        wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
        value_censored = (wis_diff_unweighted_median > max_mwis)
      ) %>%
      dplyr::filter(value_censored),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
      color = top_models,
      group = top_models),
    shape = 0,
    size = 2,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  geom_point(
    data = overall_means_others %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable"),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
      group = top_models),
    shape = "+", size = 5,
    position = position_dodge(width = 0.75)
  ) +
  scale_color_discrete(
    "Number of\nComponent\nForecasters"
  ) +
  facet_grid(target_variable ~ window_size, scales = "free") +
  xlab("Combination Method") +
  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

pdf(
  'manuscript/figures/wis_boxplots_all_combine_methods.pdf',
  width=9, height=8)
print(p_wis_boxplots)
dev.off()
