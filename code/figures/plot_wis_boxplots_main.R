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
  'manuscript/figures/wis_boxplots_main.pdf',
  width=9, height=8)
print(p_wis_boxplots)
dev.off()


# plots for cases and deaths separately, facetting by horizon
for (target_var in c("Cases", "Deaths")) {
  p_wis_boxplots <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(
      data = scores_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::mutate(
          wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
          value_censored = (wis_diff_unweighted_median > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          !value_censored,
          target_variable == target_var
        ),
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
          value_censored = (wis_diff_unweighted_median > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          value_censored,
          target_variable == target_var
        ),
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
      data = by_horizon_means_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::filter(target_variable == target_var) %>%
        dplyr::mutate(horizon = paste0("Horizon ", horizon)),
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
    facet_grid(horizon ~ window_size, scales = "free") +
    xlab("Combination Method") +
    ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
    ggtitle(target_var) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

  pdf(
    paste0('manuscript/figures/wis_boxplots_main_by_horizon_',
      target_var,
      '.pdf'),
    width=9, height=8)
  print(p_wis_boxplots)
  dev.off()
}





# investigate effects of per quantile estimation
overall_means_base <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(quantile_groups == "per_model") %>%
  dplyr::transmute(combine_method, window_size, top_models, horizon_group, horizon, target_variable, base_mwis = mwis)

overall_means_others <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, horizon, target_variable, mwis) %>%
  dplyr::left_join(overall_means_base, by = c("combine_method", "horizon_group", "window_size", "top_models", "horizon", "target_variable")) %>%
  dplyr::mutate(
    wis_diff_per_model = mwis - base_mwis,
#        wis_diff_unweighted_median = ifelse(wis_diff_unweighted_median > 100000, Inf, wis_diff_unweighted_median),
    window_size = factor(window_size, levels = c("0", "4", "8", "12", "full_history")),
    combine_method = factor(
      ifelse(combine_method == "ew",
        "mean",
        ifelse(combine_method == "convex",
          "weighted_mean",
          combine_method)),
      levels = c("mean", "median", "weighted_mean", "mean_weights_weighted_median", "rel_wis_weighted_median"))
  )

scores_base <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(quantile_groups == "per_model") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, combine_method, horizon_group, top_models, window_size, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon", "combine_method", "horizon_group", "top_models", "window_size")) %>%
  dplyr::mutate(
    wis_diff_per_model = mwis - base_mwis,
    window_size = factor(window_size, levels = c("0", "4", "8", "12", "full_history")),
    combine_method = factor(
      ifelse(combine_method == "ew",
        "mean",
        ifelse(combine_method == "convex",
          "weighted_mean",
          combine_method)),
      levels = c("mean", "median", "weighted_mean", "mean_weights_weighted_median", "rel_wis_weighted_median"))
  )

p_wis_boxplots <- ggplot(
    data = scores_others %>%
      dplyr::filter(
        top_models == "top_10", combine_method != "mean_weights_weighted_median",
        horizon_group == "all"
      )
  ) +
  geom_hline(yintercept = 0) +
  geom_boxplot(mapping = aes(
    x = combine_method,
    y = scales::oob_squish(wis_diff_per_model, range = c(-Inf, plot_upper_bound)),
    color = quantile_groups)) +
  geom_point(
    data = overall_means_others %>%
      dplyr::filter(
        top_models == "top_10", combine_method != "mean_weights_weighted_median",
        horizon_group == "all"
      ),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_per_model, range = c(-Inf, plot_upper_bound)),
      group = quantile_groups),
    shape = "+", size = 5,
    position = position_dodge(width = 0.75)
  ) +
#      scale_y_log10() +
#      facet_wrap( ~ window_size, nrow = 1, scales = "free_x") +
  facet_grid(horizon ~ window_size, scales = "free") +
  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
  ggtitle(paste0(response_var, ", ", spatial_scale)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))

pdf(
  paste0(
    'code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_boxplots',
    "_", response_var, "_", spatial_scale, '_quantile_groups.pdf'),
  width=14, height=10)
print(p_wis_boxplots)
dev.off()


# mean weights weighted median method
overall_means <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, horizon, target_variable) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

overall_means_base <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "median-per_model-0-all-all") %>%
  dplyr::transmute(target_variable, horizon, base_mwis = mwis)

overall_means_others <- overall_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, target_variable, horizon, mwis) %>%
  dplyr::left_join(overall_means_base, by = c("target_variable", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
#        wis_diff_unweighted_median = ifelse(wis_diff_unweighted_median > 100000, Inf, wis_diff_unweighted_median),
    window_size = factor(window_size, levels = c("0", "4", "8", "12", "full_history")),
    combine_method = factor(
      ifelse(combine_method == "ew",
        "mean",
        ifelse(combine_method == "convex",
          "weighted_mean",
          combine_method)),
      levels = c("mean", "median", "weighted_mean", "mean_weights_weighted_median", "rel_wis_weighted_median"))
  )
if (spatial_scale == "county") {
  overall_means_others <- overall_means_others %>%
    dplyr::filter(window_size %in% c("0", "4"))
}


summarized_scores <- all_scores %>%
  dplyr::group_by(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group,
    target_variable, forecast_date, horizon) %>%
  dplyr::summarize(
    mae = mean(abs_error),
    mwis = mean(wis),
    coverage_50 = mean(coverage_50),
    coverage_95 = mean(coverage_95)
  )

scores_base <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "median-per_model-0-all-all") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
    window_size = factor(window_size, levels = c("0", "4", "8", "12", "full_history")),
    combine_method = factor(
      ifelse(combine_method == "ew",
        "mean",
        ifelse(combine_method == "convex",
          "weighted_mean",
          combine_method)),
      levels = c("mean", "median", "weighted_mean", "mean_weights_weighted_median", "rel_wis_weighted_median"))
  )
if (spatial_scale == "county") {
  scores_others <- scores_others %>%
    dplyr::filter(window_size %in% c("0", "4"))
}

plot_upper_bound <- scores_others %>%
  filter(combine_method != "mean") %>%
  pull(mwis) %>%
  max()

p_wis_boxplots <- ggplot(
    data = scores_others %>%
      dplyr::filter(
        top_models != "top_15",
        horizon_group == "all", quantile_groups == "per_model"
      )
  ) +
  geom_hline(yintercept = 0) +
  geom_boxplot(mapping = aes(
    x = combine_method,
    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, plot_upper_bound)),
    color = top_models)) +
  geom_point(
    data = overall_means_others %>%
      dplyr::filter(
        top_models != "top_15",
        horizon_group == "all", quantile_groups == "per_model"
      ),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, plot_upper_bound)),
      group = top_models),
    shape = "+", size = 5,
    position = position_dodge(width = 0.75)
  ) +
  facet_grid(horizon ~ window_size, scales = "free") +
  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
  ggtitle(paste0(response_var, ", ", spatial_scale)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
pdf(
  paste0(
    'code/application/retrospective-qra-comparison/analyses/retrospective-eval-weighted-median-renormalization/wis_boxplots',
    "_", response_var, "_", spatial_scale, '_mean_weights_weighted_median.pdf'),
  width=14, height=10)
print(p_wis_boxplots)
dev.off()
