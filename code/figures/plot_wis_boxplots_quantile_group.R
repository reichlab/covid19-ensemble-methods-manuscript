# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(knitr)
library(here)

setwd(here())

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
    horizon_group == "All Horizons",
    combine_method %in% c("Weighted Mean", "Weighted Median")) %>%
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
common_scores <- subset_scores_to_common(all_scores)
common_score_count <- nrow(common_scores)
orig_score_count == common_score_count


# by horizon mean scores by horizon and target variable
by_horizon_means <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size,
    top_models, horizon_group, horizon, target_variable) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis)
  )

by_horizon_means_base <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(quantile_groups == "Per Model") %>%
  dplyr::transmute(combine_method, window_size, top_models, horizon, target_variable, base_mwis = mwis)

by_horizon_means_others <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, horizon, target_variable, mwis) %>%
  dplyr::left_join(by_horizon_means_base, by = c("combine_method", "window_size", "top_models", "horizon", "target_variable")) %>%
  dplyr::mutate(
    wis_diff_per_model = mwis - base_mwis,
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
  dplyr::filter(quantile_groups == "Per Model") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, combine_method, top_models, window_size, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon", "combine_method", "top_models", "window_size")) %>%
  dplyr::mutate(
    wis_diff_per_model = mwis - base_mwis,
  )



plot_upper_bound <- scores_others %>%
  dplyr::group_by(target_variable) %>%
  dplyr::summarize(max_mwis = max(wis_diff_per_model))


# plots for cases and deaths separately, facetting by horizon
for (target_var in c("Cases", "Deaths")) {
  p_wis_boxplots <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(
      data = scores_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::mutate(
          wis_diff_censored = ifelse(wis_diff_per_model > max_mwis, max_mwis, wis_diff_per_model),
          value_censored = (wis_diff_per_model > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          !value_censored,
          target_variable == target_var,
          top_models == "Top 10"
        ),
      mapping = aes(
        x = combine_method,
        y = wis_diff_censored,
        color = quantile_groups)) +
    geom_point(
      data = scores_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::mutate(
          wis_diff_censored = ifelse(wis_diff_per_model > max_mwis, max_mwis, wis_diff_per_model),
          value_censored = (wis_diff_per_model > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          value_censored,
          target_variable == target_var,
          top_models == "Top 10"
        ),
      mapping = aes(
        x = combine_method,
        y = scales::oob_squish(wis_diff_per_model, range = c(-Inf, max_mwis)),
        color = quantile_groups,
        group = quantile_groups),
      shape = 0,
      size = 2,
      position = position_dodge(width = 0.75),
      show.legend = FALSE
    ) +
    geom_point(
      data = by_horizon_means_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::filter(target_variable == target_var, top_models == "Top 10") %>%
        dplyr::mutate(horizon = paste0("Horizon ", horizon)),
      mapping = aes(
        x = combine_method,
        y = scales::oob_squish(wis_diff_per_model, range = c(-Inf, max_mwis)),
        group = quantile_groups),
      shape = "+", size = 5,
      position = position_dodge(width = 0.75)
    ) +
    scale_color_discrete(
      "Parameter Sharing\nAcross Quantile Levels"
    ) +
    facet_grid(horizon ~ window_size, scales = "free") +
    xlab("Combination Method") +
    ylab("Mean WIS for Method - Mean WIS for All Horizons") +
    ggtitle(target_var) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

  pdf(
    paste0('manuscript/figures/wis_boxplots_quantile_grouping_',
      target_var,
      '.pdf'),
    width=9, height=8)
  print(p_wis_boxplots)
  dev.off()
}


