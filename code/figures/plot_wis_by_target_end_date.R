library(dplyr)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(ggnewscale)
library(scales)

all_scores <- dplyr::bind_rows(
  readRDS("code/scores/retrospective_scores-state-inc_death.rds"),
  readRDS("code/scores/retrospective_scores-state-inc_case.rds")
) %>%
  dplyr::filter(
    horizon_group == "All Horizons", quantile_groups == "Per Model",
    drop_anomalies == FALSE,
    (top_models == "Top 10" & window_size == "Trained on 12 weeks" & combine_method %in% c("Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")) |
    (top_models == "All Models" & window_size == "Untrained" & combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median"))
  ) %>%
  dplyr::mutate(
    combine_method = factor(
      combine_method,
      levels = c("Equal Weighted Mean", "Equal Weighted Median", "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median"))
  )

# Verify all models have the same counts of scores
all_scores %>%
  dplyr::filter(forecast_date >= "2020-07-27") %>%
  dplyr::mutate(
    top_models = ifelse(top_models == "top_0", "all", top_models),
    model_brief = paste0(combine_method, "-", quantile_groups, "-", window_size, "-", top_models)
  ) %>%
  dplyr::count(model_brief, target_variable) %>%
  tidyr::pivot_wider(names_from = "target_variable", values_from = "n") %>%
  as.data.frame()


plot_data <- dplyr::bind_rows(
  covidData::load_data(
#    as_of = "2021-02-14",
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "cases") %>%
    dplyr::transmute(
      forecast_date = date,
      target_end_date = date,
      value = inc,
      quantity = "National Data",
      model_brief = "Reported Incidence",
      target_variable = "Cases"
    ),
  covidData::load_data(
#    as_of = "2021-02-14",
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "deaths") %>%
    dplyr::transmute(
      forecast_date = date,
      target_end_date = date,
      value = inc,
      quantity = "National Data",
      model_brief = "Reported Incidence",
      target_variable = "Deaths"
    ),
  all_scores %>%
    dplyr::mutate(
      quantity = paste0("Forecast Horizon: ", horizon, ifelse(horizon == 1, " week", " weeks"))
    ) %>%
    dplyr::group_by(target_variable, forecast_date, target_end_date, quantity, combine_method) %>%
    dplyr::summarize(wis = mean(wis)) %>%
    dplyr::transmute(
      forecast_date, target_end_date, value = wis, combine_method, quantity
    )
)

plot_data <- plot_data %>%
  dplyr::mutate(
    quantity = factor(quantity,
      levels = c("National Data", paste0("Forecast Horizon: ", 1:4, ifelse(1:4 == 1, " week", " weeks")))
    )
  )

p_cases <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Cases", model_brief == "Reported Incidence", forecast_date >= "2020-06-22"
      ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Cases", forecast_date >= "2020-07-27",
        combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median",
          "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
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
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
    limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
    expand = expansion()) +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Cases") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.05),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
    legend.position = "none")

p_deaths <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Deaths", model_brief == "Reported Incidence", forecast_date >= "2020-07-27"
      ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Deaths", forecast_date >= "2020-07-27",
        combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median",
          "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  geom_vline(xintercept = as.Date("2021-05-01"), linetype = 2) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
  scale_color_manual(
    "Combination\nMethod",
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
      "Rel. WIS Weighted Median" = 1
    )
  ) +
  # scale_color_manual(
  #   "Model",
  #   values = c(
  #     "Median" = "black",
  #     "Mean" = "orange",
  #     "Weighted Median" = blues[6],
  #     "Trained, Full History" = blues[8])
  # ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y",
    limits = c(as.Date("2020-07-25"), as.Date("2021-11-15")),
    expand = expansion()) +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.05),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
    legend.position = "none")



p_data_temp <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Deaths", model_brief == "Reported Incidence", forecast_date >= "2020-07-27"
      ), # %>%
      # dplyr::mutate(
      #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
      # ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  theme_bw()
legend_data <- ggpubr::get_legend(p_data_temp)

p_wis_temp <- ggplot() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Deaths", forecast_date >= "2020-07-27",
        combine_method %in% c("Equal Weighted Mean", "Equal Weighted Median",
          "Rel. WIS Weighted Mean", "Rel. WIS Weighted Median")
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Equal\nWeighted Mean" = "#ef8a62",
      "Equal\nWeighted Median" = "#67a9cf",
      "Rel. WIS\nWeighted Mean" = "#b2182b",
      "Rel. WIS\nWeighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Equal\nWeighted Mean" = 4,
      "Equal\nWeighted Median" = 5,
      "Rel. WIS\nWeighted Mean" = 2,
      "Rel. WIS\nWeighted Median" = 1
    )
  ) +
  theme_bw()

legend_wis_horiz <- ggpubr::get_legend(p_wis_temp, position = "bottom")



#png("manuscript/figures/scores_by_week.png", width = 8, height = 8, units = "in", res = 600)
pdf("manuscript/figures/scores_by_week.pdf", width = 8, height = 10)
plot_layout <- grid.layout(
  nrow = 9, ncol = 4,
  widths = unit(c(2, 0.925, 0.9, 0.01), c("lines", rep("null", 3))),
  heights = unit(c(0.1, 1.75, rep(1, 5), 1.5, 2), c("lines", "lines", rep("null", 5), "lines", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(as_ggplot(legend_wis_horiz), vp = viewport(layout.pos.row = 8, layout.pos.col = 2:3))
print(p_cases, vp = viewport(layout.pos.row = 1 + 1:6, layout.pos.col = 2))
print(p_deaths, vp = viewport(layout.pos.row = 1 + 1:6, layout.pos.col = 3))
grid.text("Forecast Creation Date",
  just = "center",
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1 + 8, layout.pos.col = 2:3))
grid.text("    Weekly\n    Cases or Deaths",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1 + 2, layout.pos.col = 1))

print(
  ggplot() +
    geom_line(
      data = data.frame(x = c(1, 1), y = c(0.095, 0.97)),
      mapping = aes(x = x, y = y)) +
    xlim(0, 1) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(0, 0)) +
    theme_void(),
  vp = viewport(layout.pos.row = 1 + 3:6, layout.pos.col = 1)
)
grid.text("                 Mean WIS",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 1 + 3:6, layout.pos.col = 1))

dev.off()

# load baseline forecasts and get relative wIS
