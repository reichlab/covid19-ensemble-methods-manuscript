# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(gridExtra)
library(grid)
library(ggpubr)
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
    quantile_groups == "Per Model",
    combine_method == "Rel. WIS Weighted Median",
    top_models == "Top 10",
    is.na(max_weight),
    # Keep subset of models for model development set
    forecast_date < "2021-05-03"
  ) %>%
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

all_scores %>%
  filter(
    model %in% c(
      "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_full_history-top_models_10-drop_anomalies_FALSE-horizon_group_3-estimation_scale_state",
      "combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_full_history-top_models_10-drop_anomalies_FALSE-horizon_group_4-estimation_scale_state")
  ) %>%
  count(horizon, location) %>%
  pivot_wider(names_from = "horizon", values_from = "n") %>%
  filter(`3` != `4`) %>%
  as.data.frame()

# ensure that we have the same scores for all model variations
orig_score_count <- nrow(all_scores)
common_scores <- subset_scores_to_common(all_scores)
common_score_count <- nrow(common_scores)

dropped_scores <- dplyr::anti_join(
  all_scores, common_scores,
  by = colnames(all_scores)
)
dropped_scores %>%
  dplyr::count(location, forecast_date, target_variable)
covidData::fips_codes %>% filter(location == "60")
nrow(dropped_scores)

# proceed with the subset of scores for forecasts made in common
all_scores <- common_scores

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
  dplyr::filter(horizon_group == "All Horizons") %>%
  dplyr::transmute(combine_method, quantile_groups, window_size, top_models, horizon, target_variable, base_mwis = mwis)

by_horizon_means_others <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, horizon, target_variable, mwis) %>%
  dplyr::left_join(by_horizon_means_base, by = c("combine_method", "quantile_groups", "window_size", "top_models", "horizon", "target_variable")) %>%
  dplyr::mutate(
    wis_diff_all_horizons = mwis - base_mwis,
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
  dplyr::filter(horizon_group == "All Horizons") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, combine_method, quantile_groups, top_models, window_size, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon", "combine_method", "quantile_groups", "top_models", "window_size")) %>%
  dplyr::mutate(
    wis_diff_all_horizons = mwis - base_mwis,
  )



plot_upper_bound <- scores_others %>%
  dplyr::filter(combine_method != "Mean") %>%
  dplyr::group_by(target_variable) %>%
  dplyr::summarize(max_mwis = max(wis_diff_all_horizons))


# plots for cases and deaths separately, facetting by horizon
#for (target_var in c("Cases", "Deaths")) {
get_boxplots_one_target_var <- function(target_var) {
  p_wis_boxplots <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(
      data = scores_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::mutate(
          wis_diff_censored = ifelse(wis_diff_all_horizons > max_mwis, max_mwis, wis_diff_all_horizons),
          value_censored = (wis_diff_all_horizons > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          !value_censored,
          target_variable == target_var,
          top_models == "Top 10"
        ),
      mapping = aes(
        x = window_size,
        y = wis_diff_censored,
        color = horizon_group)) +
    geom_point(
      data = scores_others %>%
        dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
        dplyr::mutate(
          wis_diff_censored = ifelse(wis_diff_all_horizons > max_mwis, max_mwis, wis_diff_all_horizons),
          value_censored = (wis_diff_all_horizons > max_mwis),
          horizon = paste0("Horizon ", horizon)
        ) %>%
        dplyr::filter(
          value_censored,
          target_variable == target_var,
          top_models == "Top 10"
        ),
      mapping = aes(
        x = window_size,
#        y = scales::oob_squish(wis_diff_all_horizons, range = c(-Inf, max_mwis)),
        y = wis_diff_all_horizons,
        color = horizon_group,
        group = horizon_group),
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
        x = window_size,
#        y = scales::oob_squish(wis_diff_all_horizons, range = c(-Inf, max_mwis)),
        y = wis_diff_all_horizons,
        group = horizon_group),
      shape = "+", size = 5,
      position = position_dodge(width = 0.75)
    ) +
    scale_color_discrete(
      "Parameter Sharing\nAcross Horizons"
    ) +
    facet_wrap( ~ horizon, scales = "free_y", ncol = 1) +
    xlab("Combination Method") +
    ylab("Mean WIS for Method - Mean WIS for All Horizons") +
    ggtitle(target_var) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
  
  return(p_wis_boxplots)
}

p_cases <- get_boxplots_one_target_var("Cases")
p_deaths <- get_boxplots_one_target_var("Deaths")

legend <- ggpubr::get_legend(p_cases, position = "bottom")
p_cases <- p_cases +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5))
p_deaths <- p_deaths +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5))

pdf('manuscript/figures/wis_boxplots_horizon_grouping.pdf', width = 8, height = 8)
plot_layout <- grid.layout(
  nrow = 4, ncol = 3,
  widths = unit(c(1, 1, 1), c("lines", rep("null", 2))),
  heights = unit(c(1, 1, 1, 2), c("null", "lines", "lines", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(as_ggplot(legend), vp = viewport(layout.pos.row = 4, layout.pos.col = 2:3))
print(p_cases, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(p_deaths, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))

grid.text("Training Set Size",
  just = "center",
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 2, layout.pos.col = 2:3))
grid.text("                   Mean WIS, Per Horizon Weights - Mean WIS, Weights Shared Across Horizons",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
dev.off()
