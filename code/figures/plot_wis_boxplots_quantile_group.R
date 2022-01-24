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
    horizon_group == "All Horizons",
    combine_method %in% c("Rel. WIS Weighted Median", "Weighted Mean"),
    top_models == "Top 10") %>%
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

dropped_scores <- dplyr::anti_join(
  all_scores, common_scores,
  by = colnames(all_scores)
)
unique(dropped_scores$forecast_date) # dropping prospective evaluation set scores

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
  dplyr::filter(quantile_groups == "Per Model") %>%
  dplyr::transmute(combine_method, window_size, top_models, horizon, target_variable, base_mwis = mwis)

by_horizon_means_others <- by_horizon_means %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, horizon, target_variable, mwis) %>%
  dplyr::left_join(by_horizon_means_base, by = c("combine_method", "window_size", "top_models", "horizon", "target_variable")) %>%
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
  dplyr::filter(quantile_groups == "Per Model") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, combine_method, top_models, window_size, base_mwis = mwis)

scores_others <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon", "combine_method", "top_models", "window_size")) %>%
  dplyr::mutate(
    wis_diff_all_horizons = mwis - base_mwis,
  )


# plots for cases and deaths separately, facetting by horizon
get_boxplots_one_target_var_and_combine_method <- function(target_var, combine_method) {
  p_wis_boxplots <- ggplot() +
    geom_hline(yintercept = 0) +
    geom_boxplot(
      data = scores_others %>%
        dplyr::filter(
          target_variable == target_var,
          combine_method == UQ(combine_method)
        ) %>%
        dplyr::mutate(horizon = paste0("Horizon ", horizon)),
      mapping = aes(
        x = window_size,
        y = wis_diff_all_horizons,
        color = quantile_groups)) +
    geom_point(
      data = by_horizon_means_others %>%
        dplyr::filter(
          target_variable == target_var,
          combine_method == UQ(combine_method)
        ) %>%
        dplyr::mutate(horizon = paste0("Horizon ", horizon)),
      mapping = aes(
        x = window_size,
#        y = scales::oob_squish(wis_diff_all_horizons, range = c(-Inf, max_mwis)),
        y = wis_diff_all_horizons,
        group = quantile_groups),
      shape = "+", size = 5,
      position = position_dodge(width = 0.75)
    ) +
    scale_color_discrete(
      "Parameter Sharing\nAcross Quantiles"
    ) +
    facet_wrap( ~ horizon, scales = "free_y", ncol = 1) +
    ggtitle(target_var) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))
  
  return(p_wis_boxplots)
}

for (method in c("Rel. WIS Weighted Median", "Weighted Mean")) {
  p_cases <- get_boxplots_one_target_var_and_combine_method("Cases", method)
  p_deaths <- get_boxplots_one_target_var_and_combine_method("Deaths", method)

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

  pdf(paste0('manuscript/figures/wis_boxplots_quantile_grouping_', method, '.pdf'),
    width = 8, height = 8)
  plot_layout <- grid.layout(
    nrow = 5, ncol = 3,
    widths = unit(c(1, 1, 1), c("lines", rep("null", 2))),
    heights = unit(c(1, 1, 1, 1, 2), c("lines", "null", "lines", "lines", "lines")))

  grid.newpage()
  pushViewport(viewport(layout = plot_layout))

  print(as_ggplot(legend), vp = viewport(layout.pos.row = 5, layout.pos.col = 2:3))
  print(p_cases, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(p_deaths, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))

  grid.text(paste0("Quantile Grouping Strategy: ", method),
    just = "center",
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 1, layout.pos.col = 2:3))
  grid.text("Training Set Size",
    just = "center",
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 3, layout.pos.col = 2:3))
  grid.text("                   Mean WIS, Per Quantile Weights - Mean WIS, Weights Shared Across Quantiles",
    just = "center",
    rot = 90,
    gp = gpar(fontsize = 11),
    vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  dev.off()
}
