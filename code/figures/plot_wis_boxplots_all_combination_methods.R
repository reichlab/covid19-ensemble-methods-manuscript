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

# read in scores for US
all_scores_us <- dplyr::bind_rows(
  readRDS("code/scores/retrospective_scores-state-inc_death.rds"),
  readRDS("code/scores/retrospective_scores-state-inc_case.rds")
) %>%
  dplyr::filter(
    true_value >= 0,
    horizon_group == "All Horizons", quantile_groups == "Per Model",
    drop_anomalies == FALSE,
    !(combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median") &
      top_models == "All Models" &
      window_size != "Untrained"),
    is.na(max_weight),
    # trained methods: all variations other than equal weighted, top 10
    # untrained methods: untrained, equal weighted
    # (window_size == "Trained on 12 weeks" &
    #   !(top_models == "All Models" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median"))) | # & combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) |
    # (window_size == "Untrained" &
    #   combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median")),
    # Keep subset of forecasts for model development phase
    forecast_date < "2021-05-03"
  ) %>%
  dplyr::mutate(
    combine_method = factor(
      combine_method,
      levels = c("Equal Weighted Mean", "Equal Weighted Median",
                 "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median",
                 "Weighted Mean", "Mean Weights Weighted Median")),
    window_size = factor(
      window_size,
      levels = c("Untrained", "Trained on 4 weeks", "Trained on 8 weeks",
                 "Trained on 12 weeks", "Trained on full history")
    ),
    top_models = factor(
      top_models,
      levels = c("All Models", "Top 10", "Top 5")
    ),
    model_brief = paste0(combine_method, "-",
      quantile_groups, "-",
      window_size, "-",
      top_models, "-",
      horizon_group),
    phase = ifelse(forecast_date >= "2021-05-03", "Prospective Evaluation: US", "Model Development: US")
  )

# ensure that we have the same scores for all model variations
orig_score_count <- nrow(all_scores_us)
all_scores_us <- dplyr::bind_rows(
  subset_scores_to_common(
    all_scores_us %>% dplyr::filter(phase == "Model Development: US")),
  subset_scores_to_common(
    all_scores_us %>% dplyr::filter(phase == "Prospective Evaluation: US"))
)
common_score_count <- nrow(all_scores_us)
orig_score_count == common_score_count


make_wis_boxplots_by_phase <- function(
  scores_others,
  overall_means_others,
  plot_limits,
  central_only,
  phases
) {
  num_phases <- length(phases)

  p_wis_boxplots <- ggplot() +
    geom_hline(yintercept = 0)

  if (central_only) {
    p_wis_boxplots <- p_wis_boxplots +
      geom_boxplot(
        data = scores_others %>%
          dplyr::filter(phase %in% UQ(phases)) %>%
          dplyr::left_join(plot_limits, by = c("phase", "target_variable")) %>%
          dplyr::mutate(
            wis_diff_censored = dplyr::case_when(
              wis_diff_unweighted_median > max_mwis ~ max_mwis,
              wis_diff_unweighted_median < min_mwis ~ min_mwis,
              TRUE ~ wis_diff_unweighted_median),
            value_censored = (wis_diff_unweighted_median > max_mwis) | (wis_diff_unweighted_median < min_mwis)
          ),
        mapping = aes(
          x = combine_method,
          y = wis_diff_censored,
      #    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
          color = top_models),
        outlier.shape = NA,
        coef = 0)
  } else {
    p_wis_boxplots <- p_wis_boxplots +
      geom_boxplot(
        data = scores_others %>%
          dplyr::filter(phase %in% UQ(phases)) %>%
          dplyr::left_join(plot_limits, by = c("phase", "target_variable")) %>%
          dplyr::mutate(
            wis_diff_censored = dplyr::case_when(
              wis_diff_unweighted_median > max_mwis ~ max_mwis,
              wis_diff_unweighted_median < min_mwis ~ min_mwis,
              TRUE ~ wis_diff_unweighted_median),
            value_censored = (wis_diff_unweighted_median > max_mwis) | (wis_diff_unweighted_median < min_mwis)
          ),
        mapping = aes(
          x = combine_method,
          y = wis_diff_censored,
      #    y = scales::oob_squish(wis_diff_unweighted_median, range = c(-Inf, max_mwis)),
          color = top_models)#,
          # alpha = as.character(value_censored))
      ) +
      geom_point(
        data = scores_others %>%
          dplyr::filter(phase %in% UQ(phases)) %>%
          dplyr::left_join(plot_limits, by = c("phase", "target_variable")) %>%
          dplyr::mutate(
            wis_diff_censored = dplyr::case_when(
              wis_diff_unweighted_median > max_mwis ~ max_mwis,
              wis_diff_unweighted_median < min_mwis ~ min_mwis,
              TRUE ~ NA_real_),
            value_censored = (wis_diff_unweighted_median > max_mwis) | (wis_diff_unweighted_median < min_mwis)
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
      scale_alpha_manual(values = c("TRUE" = 0.0, "FALSE" = 1.0))
      # scale_alpha_continuous(range = c(0, 1))
  }

  # add phantom observations at facet-specific plot limits to ensure full limits are used
  p_wis_boxplots <- p_wis_boxplots +
    geom_point(
      data = overall_means_others %>%
        dplyr::filter(phase %in% UQ(phases)) %>%
        dplyr::left_join(plot_limits, by = c("phase", "target_variable")),
      mapping = aes(
        x = combine_method,
        y = pmax(min_mwis, wis_diff_unweighted_median)),
      shape = NA, size = 0
    ) +
    geom_point(
      data = overall_means_others %>%
        dplyr::filter(phase %in% UQ(phases)) %>%
        dplyr::left_join(plot_limits, by = c("phase", "target_variable")),
      mapping = aes(
        x = combine_method,
        y = pmin(max_mwis, wis_diff_unweighted_median)),
      shape = NA, size = 0
    )

  p_wis_boxplots <- p_wis_boxplots +
    geom_point(
      data = overall_means_others %>%
        dplyr::filter(phase %in% UQ(phases)) %>%
        dplyr::left_join(plot_limits, by = c("phase", "target_variable")),
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
      "Number of\nComponent Forecasters",
      type = c("All Models" = "#3CBB75FF",
        "Top 10" = "#2A788EFF",
        "Top 5" = "#440154FF")
    ) +
  #  facet_grid(phase ~ target_variable, scales = "free_y") +
    # facet_wrap( ~ geography, scales = "free_y", ncol = 1) +
    xlab("Combination Method") +
  #  ylab("Mean WIS for Method - Mean WIS for Unweighted Median") +
    ylab("") +
  #  ggtitle("Cases") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      # axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0.5))

  p_wis_boxplots <- p_wis_boxplots +
    facet_grid(rows = vars(target_variable),
               cols = vars(window_size),
               scales = "free_y")

  return(p_wis_boxplots)
}


# make a plot based on a given set of scores
# will be called with all_scores and non_anomalous_scores
make_wis_boxplots_calibration_one_setting <- function(
  all_scores,
  plot_limits,
  central_only = FALSE,
  include_wis = TRUE,
  include_calibration = FALSE,
  save_path,
  size_for_poster = FALSE
) {
  # how many phases? orientation of the facets depends
  num_phases <- length(unique(all_scores$phase))

  # Calculations for WIS

  # overall mean scores by horizon and target variable
  overall_means <- all_scores %>%
    dplyr::group_by(phase, model_brief, combine_method, quantile_groups, window_size,
      top_models, horizon_group, target_variable) %>%
    dplyr::summarize(
      mae = mean(abs_error),
      mwis = mean(wis)
    )

  # overall means for the equally weighted median method
  overall_means_base <- overall_means %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
    dplyr::transmute(phase, target_variable, base_mwis = mwis)

  # differences in means relative to equally weighted median method
  overall_means_others <- overall_means %>%
    dplyr::ungroup() %>%
    dplyr::select(phase, model_brief, combine_method, quantile_groups, window_size, top_models, horizon_group, target_variable, mwis) %>%
    dplyr::left_join(overall_means_base, by = c("phase", "target_variable")) %>%
    dplyr::mutate(
      wis_diff_unweighted_median = mwis - base_mwis,
    )

  # mean across locations for each forecast date and horizon
  summarized_scores <- all_scores %>%
    dplyr::group_by(phase, model_brief, combine_method, quantile_groups, window_size,
      top_models, horizon_group, target_variable, forecast_date, horizon) %>%
    dplyr::summarize(
      mae = mean(abs_error),
      mwis = mean(wis)
    )

  scores_base <- summarized_scores %>%
    dplyr::ungroup() %>%
    dplyr::filter(model_brief == "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons") %>%
    dplyr::transmute(phase, target_variable, forecast_date, horizon, base_mwis = mwis)

  scores_others <- summarized_scores %>%
    dplyr::ungroup() %>%
    dplyr::select(phase, model_brief, combine_method, quantile_groups, top_models, window_size, horizon_group, target_variable, forecast_date, horizon, mwis) %>%
    dplyr::left_join(scores_base, by = c("phase", "target_variable", "forecast_date", "horizon")) %>%
    dplyr::mutate(
      wis_diff_unweighted_median = mwis - base_mwis,
    )

  p_wis_boxplots <- make_wis_boxplots_by_phase(
    scores_others = scores_others, #%>%
      # dplyr::mutate(
      #   combine_method = dplyr::case_when(
      #     combine_method == "Equal Weighted Mean" ~ "Equal Weighted\nMean",
      #     combine_method == "Equal Weighted Median" ~ "Equal Weighted\nMedian",
      #     combine_method == "Rel. WIS Weighted Mean" ~ "Rel. WIS Weighted\nMean",
      #     combine_method == "Rel. WIS Weighted Median" ~ "Rel. WIS Weighted\nMedian",
      #   )
      # ),
    overall_means_others = overall_means_others, #%>%
      # dplyr::mutate(
      #   combine_method = dplyr::case_when(
      #     combine_method == "Equal Weighted Mean" ~ "Equal Weighted\nMean",
      #     combine_method == "Equal Weighted Median" ~ "Equal Weighted\nMedian",
      #     combine_method == "Rel. WIS Weighted Mean" ~ "Rel. WIS Weighted\nMean",
      #     combine_method == "Rel. WIS Weighted Median" ~ "Rel. WIS Weighted\nMedian",
      #   )
      # ),
    plot_limits = plot_limits,
    central_only = central_only,
    phases = unique(scores_others$phase)
  )

  legend_top_models <- ggpubr::get_legend(
    p_wis_boxplots,
    position = "bottom")
  p_wis_boxplots <- p_wis_boxplots +
    theme(legend.position = "none")
  # p_wis_boxplots <- p_wis_boxplots +
  #   theme(legend.position = "right")

  # Calculations for coverage
  overall_means_coverage <- all_scores %>%
    dplyr::filter(
      model_brief %in%
        c(
          "Equal Weighted Mean-Per Model-Untrained-All Models-All Horizons",
          "Equal Weighted Median-Per Model-Untrained-All Models-All Horizons",
          "Rel. WIS Weighted Mean-Per Model-Trained on 12 weeks-Top 10-All Horizons",
          "Rel. WIS Weighted Median-Per Model-Trained on 12 weeks-Top 10-All Horizons"
        )
    ) %>%
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

  p_coverage <- ggplot(
    data = overall_means_coverage
    ) +
    geom_line(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, linetype = combine_method, group = paste0(combine_method, top_models))) +
    geom_point(mapping = aes(x = nominal_coverage, y = empirical_coverage_diff, color = combine_method, shape = combine_method)) +
  #  facet_wrap( ~ target_variable) +
    # facet_grid(phase ~ target_variable) +
    # facet_grid(target_variable ~ phase) +
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
    # ylab("Empirical Coverage Rate Minus Nominal Coverage Rate") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1),
      axis.title.y = element_blank(),
      legend.position = "right")

  if (num_phases > 1) {
    p_coverage <- p_coverage +
      facet_grid(rows = vars(target_variable), cols = vars(phase), scales = "free_y")
  } else {
    p_coverage <- p_coverage +
      facet_wrap(facets = vars(target_variable), scales = "free_y")
  }

  legend_combine_methods <- ggpubr::get_legend(
    p_coverage,
    position = "bottom")
  p_coverage <- p_coverage +
    theme(legend.position = "none")

  if (num_phases > 1) {
    if (size_for_poster) {
      fig_height <- 4
      last_row_height <- 0.85
    } else {
      fig_height <- 8.5
      last_row_height <- 0.85
    }
  } else {
    fig_height <- 9
    last_row_height <- 0.75
  }

  if (include_calibration && include_wis) {
    row_units <- unit(c(1, 0.8, 0.2, 1, last_row_height), c("lines", "null", "null", "lines", "null"))
  } else {
    row_units <- unit(c(0.8, 0.2, 2), c("null", "null", "lines"))
  }

  pdf(save_path, width = 8, height = fig_height)
  plot_layout <- grid.layout(
    nrow = length(row_units),
    ncol = 4,
    widths = unit(c(1, 1, 0.02, 1), c("lines", "lines", rep("null", 2))),
    heights = row_units)

  grid.newpage()
  pushViewport(viewport(layout = plot_layout))

  if (include_calibration && include_wis) {
    grid.text("(a) Weighted Interval Scores",
      x = unit(0.0, "npc"),
      y = unit(0.5, "npc"),
      just = c("left", "center"),
      gp = gpar(fontsize = 12),
      vp = viewport(
        layout.pos.row = 1,
        layout.pos.col = 1:3))
  }

  if (include_wis) {
    if (num_phases > 1) {
      extra_spaces <- "               "
    } else {
      extra_spaces <- ""
    }
    grid.text(
      paste0(extra_spaces, "                                    Ensemble Method Mean WIS Minus"),
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = 1:2 + include_calibration, layout.pos.col = 1))

    grid.text(
      paste0(extra_spaces, "                                    Equally Weighted Median Mean WIS"),
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = 1:2 + include_calibration, layout.pos.col = 2))


    print(p_wis_boxplots,
      vp = viewport(layout.pos.row = 1:2 + include_calibration, layout.pos.col = 3:4))

    print(as_ggplot(legend_top_models),
      vp = viewport(
        y = unit(0, "npc"),
        layout.pos.row = 3,
        layout.pos.col = 3:4,
        just = c("top")))
  }

  if (include_calibration) {
    if (include_wis) {
      grid.text("(b) One-sided Quantile Coverage Rates",
        x = unit(0.0, "npc"),
        y = unit(0.5, "npc"),
        just = c("left", "center"),
        gp = gpar(fontsize = 12),
        vp = viewport(
          layout.pos.row = 4,
          layout.pos.col = 1:3))
      plot_rows <- 5
      legend_rows <- 5
    } else {
      plot_rows <- 1:2
      legend_rows <- 1
    }

    grid.text(
    #  "                                        Mean WIS for Method - Mean WIS for Unweighted Median",
      "        Empirical Coverage Minus",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = plot_rows, layout.pos.col = 1))

    grid.text(
    #  "                                        Mean WIS for Method - Mean WIS for Unweighted Median",
      "            Nominal Coverage",
      just = "center",
      rot = 90,
      gp = gpar(fontsize = 11),
      vp = viewport(layout.pos.row = plot_rows, layout.pos.col = 2))

    # grid.text(
    # #  "                                        Mean WIS for Method - Mean WIS for Unweighted Median",
    #   "Empirical Coverage Rate Minus Nominal Coverage Rate",
    #   just = "center",
    #   rot = 90,
    #   gp = gpar(fontsize = 11),
    #   vp = viewport(layout.pos.row = 5, layout.pos.col = 2))

    print(p_coverage,
      vp = viewport(
        layout.pos.row = plot_rows,
        layout.pos.col = 4
      ))

    print(as_ggplot(legend_combine_methods),
      vp = viewport(
        y = unit(0, "npc"),
        layout.pos.row = legend_rows,
        layout.pos.col = 5,
        just = c("top")))
  }

  dev.off()
}


make_wis_boxplots_calibration_one_setting(
  all_scores = all_scores_us,
  plot_limits = data.frame(
    phase = rep(c("Model Development: US", "Prospective Evaluation: US", "Prospective Evaluation: EU"), each = 2),
    target_variable = rep(c("Cases", "Deaths"), times = 3),
    min_mwis = c(-1000, -10, -1000, -10, -10000, -100000),
    max_mwis = c( 1000,  10,  1000,  10,  10000,  100000)
  ),
  central_only = TRUE,
  include_calibration = FALSE,
  save_path = 'manuscript/figures/wis_boxplots_all_combos_central_only.pdf')

make_wis_boxplots_calibration_one_setting(
  all_scores = all_scores_us,
  plot_limits = data.frame(
    phase = rep(c("Model Development: US", "Prospective Evaluation: US", "Prospective Evaluation: EU"), each = 2),
    target_variable = rep(c("Cases", "Deaths"), times = 3),
    min_mwis = c(-Inf, -Inf, -Inf, -Inf, -Inf, -Inf),
    max_mwis = c( Inf,  Inf,  Inf,  Inf,  Inf,  Inf)
  ),
  central_only = FALSE,
  include_calibration = FALSE,
  save_path = 'manuscript/figures/wis_boxplots_all_combos_all_points.pdf')
