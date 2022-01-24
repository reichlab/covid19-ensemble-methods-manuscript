# load packages
library(covidData)
library(covidHubUtils)
library(covidEnsembles)
library(tidyverse)
library(ggpubr)
library(grid)
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
    true_value >= 0,
    horizon_group == "All Horizons",
    quantile_groups == "Per Model",
    drop_anomalies == "FALSE",
#    combine_method %in% c("Mean", "Median", "Weighted Mean", "Weighted Median"),
    window_size %in% c("Untrained", "Trained on 12 weeks"),
    !(combine_method == "Equal Weighted Mean" & window_size == "Trained on 12 weeks" &
      top_models == "All Models"),
    !(combine_method == "Equal Weighted Median" & window_size == "Trained on 12 weeks" &
      top_models == "All Models")) %>%
  dplyr::mutate(
    # window_size = factor(
    #   window_size,
    #   levels = c("Untrained", "Trained on 4 weeks", "Trained on 8 weeks", "Trained on 12 weeks", "Trained on full history")
    # ),
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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, base_mwis = mwis)

# differences in means relative to equally weighted median method
overall_means_others_us <- overall_means %>%
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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, base_mwis = mwis)

scores_others_us <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
  )

plot_upper_bound <- scores_others %>%
  dplyr::filter(combine_method != "Equal Weighted Mean") %>%
  dplyr::group_by(target_variable) %>%
  dplyr::summarize(max_mwis = max(wis_diff_unweighted_median))

# scores_others <- scores_others %>%
#   dplyr::mutate(
#     effective_model = dplyr::case_when(
#       combine_method == "Mean" & window_size == "Untrained" ~ "Equal-Weighted Mean of All Models",
#       combine_method == "Median" & window_size == "Untrained" ~ "Equal-Weighted Median of All Models",
#       combine_method == ""
#     )
#   )


# p_wis_boxplots_us <- ggplot() +
#   geom_hline(yintercept = 0) +
#   geom_boxplot(
#     data = scores_others_us %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
#       dplyr::mutate(
#         wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
#         value_censored = (wis_diff_unweighted_median > max_mwis)
#       ) %>%
#       dplyr::filter(!value_censored),
#     mapping = aes(
#       x = combine_method,
#       y = wis_diff_censored,
#   #    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
#       color = top_models)) +
#   geom_point(
#     data = scores_others_us %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
#       dplyr::mutate(
#         wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, NA),
#         value_censored = (wis_diff_unweighted_median > max_mwis)
#       ), # %>%
# #      dplyr::filter(value_censored),
#     mapping = aes(
#       x = combine_method,
#       y = wis_diff_censored,
#       color = top_models,
#       group = top_models),
#     shape = 0,
#     size = 2,
#     position = position_dodge(width = 0.75),
#     show.legend = FALSE
#   ) +
#   geom_point(
#     data = overall_means_others_us %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable"),
#     mapping = aes(
#       x = combine_method,
#       y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
#       group = top_models),
#     shape = "+", size = 5,
#     position = position_dodge(width = 0.75)
#   ) +
#   # scale_color_viridis_d(
#   #   "Number of\nComponent\nForecasters",
#   #   end = 0.8
#   # ) +
#   scale_color_discrete(
#     "Number of\nComponent\nForecasters",
#     type = c("All Models" = "#440154FF",
#       "Top 10" = "#2A788EFF",
#       "Top 5" = "#7AD151FF")
#   ) +
#   facet_wrap( ~ target_variable, scales = "free", nrow = 1) +
#   xlab("Combination Method") +
#   ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))



# plot of scores for Europe
# read in scores
all_scores <- dplyr::bind_rows(
  readRDS("code/scores/retrospective_scores-euro_countries-inc_death.rds"),
  readRDS("code/scores/retrospective_scores-euro_countries-inc_case.rds")
) %>%
  dplyr::filter(true_value >= 0) %>%
  dplyr::mutate(
    # window_size = factor(
    #   window_size,
    #   levels = c("Untrained", "Trained on 4 weeks", "Trained on 8 weeks", "Trained on 12 weeks", "Trained on full history")
    # ),
    model_brief = paste0(combine_method, "-",
      quantile_groups, "-",
      window_size, "-",
      top_models, "-",
      horizon_group)
  )


# all_scores %>%
#   count(model_brief, target_variable, forecast_date) %>%
#   pivot_wider(names_from = "model_brief", values_from = "n") %>%
#   as.data.frame()

# drop target end dates that are outliers
# outliers <- dplyr::bind_rows(
#   readr::read_csv(
#     "code/data-anomalies/outliers-inc-cases-euro.csv",
#     col_types = cols(
#       location = col_character(),
#       location_abbreviation = col_character(),
#       date = col_date(format = ""),
#       start_issue_date = col_date(format = ""),
#       end_issue_date = col_date(format = "")
#     )) %>%
#     dplyr::mutate(target_variable = "Cases"),
#   readr::read_csv(
#     "code/data-anomalies/outliers-inc-deaths-euro.csv",
#     col_types = cols(
#       location = col_character(),
#       location_abbreviation = col_character(),
#       date = col_date(format = ""),
#       start_issue_date = col_date(format = ""),
#       end_issue_date = col_date(format = "")
#     )) %>%
#     dplyr::mutate(target_variable = "Deaths")
# ) %>%
#   dplyr::filter(end_issue_date == "2021-07-04")

# all_scores <- all_scores %>%
#   dplyr::anti_join(outliers,
#     by = c("location", "target_end_date" = "date", "target_variable"))

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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, base_mwis = mwis)

# differences in means relative to equally weighted median method
overall_means_others_euro <- overall_means %>%
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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
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
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, forecast_date, horizon, base_mwis = mwis)

scores_others_euro <- summarized_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
  dplyr::left_join(scores_base, by = c("target_variable", "forecast_date", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = mwis - base_mwis,
  )

# differences from base for every forecast
all_scores_base <- all_scores %>%
  dplyr::ungroup() %>%
  dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
  dplyr::transmute(target_variable, location, forecast_date, target_end_date, horizon, base_wis = wis)

all_scores_others <- all_scores %>%
  dplyr::ungroup() %>%
  dplyr::select(model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, location, forecast_date, target_end_date, horizon, wis) %>%
  dplyr::left_join(all_scores_base, by = c("target_variable", "location", "forecast_date", "target_end_date", "horizon")) %>%
  dplyr::mutate(
    wis_diff_unweighted_median = wis - base_wis,
  )

overall_means_others <- dplyr::bind_rows(
  overall_means_others_us %>%
    dplyr::mutate(geography = "United States"),
  overall_means_others_euro %>%
    dplyr::mutate(geography = "Europe")
) %>%
  dplyr::mutate(
    geography = factor(geography, levels = c("United States", "Europe"))
  )

# scores_others <- dplyr::bind_rows(
#   scores_others_us %>%
#     dplyr::mutate(geography = "United States"),
#   scores_others_euro %>%
#     dplyr::mutate(geography = "Europe")
# ) %>%
#   dplyr::mutate(
#     geography = factor(geography, levels = c("United States", "Europe"))
#   )

# plot just US results in this figure
scores_others <- scores_others_us

plot_upper_bound <- scores_others %>%
  dplyr::filter(combine_method != "Equal Weighted Mean") %>%
  dplyr::group_by(target_variable) %>%
  dplyr::summarize(max_mwis = max(wis_diff_unweighted_median))

p_wis_boxplots_cases <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(
    data = scores_others %>%
      dplyr::filter(target_variable == "Cases") %>%
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
      dplyr::filter(target_variable == "Cases") %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
      dplyr::mutate(
        wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, NA_real_),
        value_censored = (wis_diff_unweighted_median > max_mwis)
      ), # %>%
#      dplyr::filter(value_censored),
    mapping = aes(
      x = combine_method,
      y = wis_diff_censored,
      color = top_models,
      group = top_models),
    shape = 0,
    size = 2,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  geom_point(
    data = overall_means_others %>%
      dplyr::filter(target_variable == "Cases") %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable"),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
      group = top_models),
    shape = "+", size = 5,
    position = position_dodge(width = 0.75)
  ) +
  # scale_color_viridis_d(
  #   "Number of\nComponent\nForecasters",
  #   end = 0.8
  # ) +
  scale_color_discrete(
    "Number of\nComponent\nForecasters",
    type = c("All Models" = "#440154FF",
      "Top 10" = "#2A788EFF",
      "Top 5" = "#7AD151FF")
  ) +
  # facet_wrap( ~ geography, scales = "free_y", ncol = 1) +
  xlab("Combination Method") +
#  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
  ylab("") +
  ggtitle("Cases") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust= 0.5))


p_wis_boxplots_deaths <- ggplot() +
  geom_hline(yintercept = 0) +
  geom_boxplot(
    data = scores_others %>%
      dplyr::filter(target_variable == "Deaths") %>%
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
      dplyr::filter(target_variable == "Deaths") %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
      dplyr::mutate(
        wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, NA_real_),
        value_censored = (wis_diff_unweighted_median > max_mwis)
      ), # %>%
#      dplyr::filter(value_censored),
    mapping = aes(
      x = combine_method,
      y = wis_diff_censored,
      color = top_models,
      group = top_models),
    shape = 0,
    size = 2,
    position = position_dodge(width = 0.75),
    show.legend = FALSE
  ) +
  geom_point(
    data = overall_means_others %>%
      dplyr::filter(target_variable == "Deaths") %>%
      dplyr::left_join(plot_upper_bound, by = "target_variable"),
    mapping = aes(
      x = combine_method,
      y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
      group = top_models),
    shape = "+", size = 5,
    position = position_dodge(width = 0.75)
  ) +
  # scale_color_viridis_d(
  #   "Number of\nComponent\nForecasters",
  #   end = 0.8
  # ) +
  scale_color_discrete(
    "Number of\nComponent\nForecasters",
    type = c("All Models" = "#440154FF",
      "Top 10" = "#2A788EFF",
      "Top 5" = "#7AD151FF")
  ) +
  # facet_wrap( ~ geography, scales = "free_y", ncol = 1) +
  xlab("Combination Method") +
#  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
  ylab("") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust= 0.5))


legend_top_models <- ggpubr::get_legend(p_wis_boxplots_cases)
p_wis_boxplots_cases <- p_wis_boxplots_cases +
  theme(legend.position = "none")
p_wis_boxplots_deaths <- p_wis_boxplots_deaths +
  theme(legend.position = "none")

# p_wis_boxplots_euro <- ggplot() +
#   geom_hline(yintercept = 0) +
#   geom_boxplot(
#     data = scores_others %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
#       dplyr::mutate(
#         wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
#         value_censored = (wis_diff_unweighted_median > max_mwis)
#       ) %>%
#       dplyr::filter(!value_censored),
#     mapping = aes(
#       x = combine_method,
#       y = wis_diff_censored,
#   #    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
#       color = top_models)) +
#   geom_point(
#     data = scores_others %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable") %>%
#       dplyr::mutate(
#         wis_diff_censored = ifelse(wis_diff_unweighted_median > max_mwis, max_mwis, wis_diff_unweighted_median),
#         value_censored = (wis_diff_unweighted_median > max_mwis)
#       ) %>%
#       dplyr::filter(value_censored),
#     mapping = aes(
#       x = combine_method,
#       y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
#       color = top_models,
#       group = top_models),
#     shape = 0,
#     size = 2,
#     position = position_dodge(width = 0.75),
#     show.legend = FALSE
#   ) +
#   geom_point(
#     data = overall_means_others %>%
#       dplyr::left_join(plot_upper_bound, by = "target_variable"),
#     mapping = aes(
#       x = combine_method,
#       y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
#       group = top_models),
#     shape = "+", size = 5,
#     position = position_dodge(width = 0.75)
#   ) +
#   scale_color_discrete(
#     "Number of\nComponent\nForecasters",
#     type = c("All Models" = "#440154FF",
#       "Top 10" = "#2A788EFF",
#       "Top 5" = "#7AD151FF")
#   ) +
#   facet_wrap( ~ target_variable, scales = "free", nrow = 1) +
#   xlab("Combination Method") +
#   ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5))



pdf(
  'manuscript/figures/wis_boxplots_main.pdf',
  width=8, height=6)

num_plot_rows <- 5
plot_layout <- grid.layout(
  nrow = num_plot_rows + 1, ncol = 5,
  widths = unit(c(1, 1, 1, 1, 0.35), c("lines", "lines", rep("null", 3))),
  heights = unit(rep(1, num_plot_rows + 1), c(rep("null", num_plot_rows), "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text(
#  "                                        Mean WIS for Method - Mean WIS for Unweighted Median",
  "                                     Difference in Mean WIS for Ensemble Method",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1:num_plot_rows, layout.pos.col = 1))

grid.text(
#  "                                        Mean WIS for Method - Mean WIS for Unweighted Median",
  "                                   and Equally Weighted Median Ensemble",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1:num_plot_rows, layout.pos.col = 2))



print(p_wis_boxplots_cases, vp = viewport(layout.pos.row = 1:num_plot_rows, layout.pos.col = 3))
print(p_wis_boxplots_deaths, vp = viewport(layout.pos.row = 1:num_plot_rows, layout.pos.col = 4))

print(as_ggplot(legend_top_models),
  vp = viewport(
    y = unit(0, "npc"),
    layout.pos.row = 2:3, layout.pos.col = 5,
    just = c("top")))

grid.text("Combination Method",
  just = "center",
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = num_plot_rows + 1, layout.pos.col = 3:4))
dev.off()
