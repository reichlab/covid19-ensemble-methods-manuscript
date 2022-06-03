library(tidyverse)
library(grid)
library(dplyr)
library(ggplot2)
library(grid)
library(ggpubr)
library(scales)
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(plotly)

# target_var <- "inc_case"
# spatial_resolution <- "state"


# read in scores and subset to a comparable set for this analysis
case_scores <- readRDS("code/scores/retrospective_scores-state-inc_case.rds")
death_scores <- readRDS("code/scores/retrospective_scores-state-inc_death.rds")

combined_scores <- dplyr::bind_rows(case_scores, death_scores) %>%
  dplyr::filter(
    # grepl("combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_0-drop_anomalies_FALSE-horizon_group_all", model) |
    grepl("combine_method_rel_wis_weighted_median-quantile_groups_per_model-window_size_12-top_models_10-drop_anomalies_FALSE-horizon_group_all", model)
  ) %>%
  dplyr::mutate(
    max_weight = ifelse(is.na(max_weight), "1.0", max_weight)
  ) %>%
  dplyr::filter(
    target_variable == "Deaths" |
      (target_variable == "Cases" & forecast_date >= "2020-09-14"),
    forecast_date <= "2022-01-03"
  )

# double checking that we have a consistent set of scored forecasts for all models
combined_scores %>%
  dplyr::count(
    max_weight,
    top_models,
    target_variable#,
#    forecast_date
  ) %>%
  tidyr::pivot_wider(names_from = "target_variable", values_from = "n") %>%
  as.data.frame()

# summarize scores to overall means
mean_scores <- combined_scores %>%
  dplyr::group_by(max_weight, top_models, target_variable) %>%
  dplyr::summarise(
    wis = mean(wis),
    mae = mean(abs_error),
    coverage_95 = mean(coverage_95)
  ) %>%
  dplyr::ungroup()

p_wis <- ggplot(
    data = mean_scores,
    mapping = aes(x = as.numeric(max_weight), y = wis)
  ) +
  geom_point() +
  geom_line() +
  facet_wrap( ~ target_variable, scales = "free_y") +
  ylab("Mean Weighted Interval Score") +
  xlab("Maximum Weight Limit") +
  theme_bw()

p_coverage <- ggplot(
    data = mean_scores,
    mapping = aes(x = as.numeric(max_weight), y = coverage_95)
  ) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = 2) +
  # geom_line(mapping = aes(linetype = top_models)) +
  # geom_point(shape = aes(linetype = top_models)) +
  facet_wrap( ~ target_variable, scales="free_y") +
  ylim(0.0, 1.0) +
  ylab("95% Prediction Interval Coverage") +
  xlab("Maximum Weight Limit") +
  theme_bw()


pdf(
  'manuscript/figures/compare_max_weight_limits_overall.pdf',
  width = 6,
  height = 7)

plot_layout <- grid.layout(
  nrow = 4, ncol = 1,
  heights = unit(c(2, 1, 2, 1), c("lines", "null", "lines", "null")))
grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text(" (a) Ensemble WIS by maximum weight limit",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1))

grid.text(" (b) Ensemble 95% interval coverage rate by maximum weight limit",
  x = unit(0.0, "npc"),
  just = "left",
  gp = gpar(fontsize = 12),
  vp = viewport(layout.pos.row = 3, layout.pos.col = 1))

print(p_wis,
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_coverage,
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1))

dev.off()


# mean wis by date
mean_scores_by_date <- combined_scores %>%
  dplyr::group_by(model, top_models, max_weight, target_variable, forecast_date) %>%
  dplyr::summarise(wis = mean(wis))

pdf(
  'manuscript/figures/compare_max_weight_limits_by_date.pdf',
  width = 8,
  height = 9)

ggplot(
  data = mean_scores_by_date %>%
    dplyr::filter(top_models == "Top 10")) +
  geom_line(mapping = aes(x = forecast_date, y = wis, color = as.numeric(max_weight), linetype = max_weight)) +
  scale_color_continuous("Maximum Weight Limit") +
  scale_linetype("Maximum Weight Limit") +
  facet_wrap(~ target_variable, ncol = 1, scales = "free_y") +
  ylab("Mean Weighted Interval Score") +
  xlab("Forecast Date") +
  theme_bw()

dev.off()
