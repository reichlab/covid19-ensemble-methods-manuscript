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
    horizon_group == "all", quantile_groups == "per_model",
    (top_models == "top_10" & window_size == "12" & combine_method %in% c("convex", "rel_wis_weighted_median")) |
    (top_models == "all" & window_size == "0" & combine_method %in% c("ew", "median"))
  ) %>%
  dplyr::mutate(
    combine_method = factor(
      dplyr::case_when(
        combine_method == "ew" ~ "Mean",
        combine_method == "median" ~ "Median",
        combine_method == "convex" ~ "Weighted Mean",
        combine_method == "rel_wis_weighted_median" ~ "Weighted Median"
      ),
      levels = c("Mean", "Median", "Weighted Mean", "Weighted Median")),
    target_variable = dplyr::case_when(
      target_variable == "inc case" ~ "Cases",
      target_variable == "inc death" ~ "Deaths",
      TRUE ~ NA_character_
    )
  )


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
      ), # %>%
      # dplyr::mutate(
      #   model_brief = factor(model_brief, levels = c("Mean", "Median", "Weighted, Window Size 4", "Weighted, Full History"))
      # ),
    mapping = aes(x = forecast_date, y = value, color = model_brief, group = model_brief)) +
  scale_color_manual("National Data", values = "black") +
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Cases", forecast_date >= "2020-07-27",
        combine_method %in% c("Mean", "Median", "Weighted Mean", "Weighted Median")
      ) %>%
      dplyr::mutate(
        combine_method = factor(combine_method, levels = c("Mean", "Median", "Weighted Mean", "Weighted Median"))
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Mean" = "#ef8a62",
      "Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Weighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Mean" = 4,
      "Median" = 5,
      "Weighted Mean" = 2,
      "Weighted Median" = 1)
  ) +
#  scale_color_discrete("Combination\nMethod") +
  # scale_color_manual(
  #   "Model",
  #   values = c(
  #     "Median" = "black",
  #     "Mean" = "orange",
  #     "Weighted Mean" = blues[6],
  #     "Weighted Median" = blues[8])
  # ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Cases") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(0, 0, -0.5, -0.25, "cm"),
    legend.position = "none")

p_deaths <- ggplot() +
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
  ggnewscale::new_scale_color() +
  geom_line(
    data = plot_data %>%
      dplyr::filter(
        target_variable == "Deaths", forecast_date >= "2020-07-27",
        combine_method %in% c("Mean", "Median", "Weighted Mean", "Weighted Median")
      ) %>%
      dplyr::mutate(
        combine_method = factor(combine_method, levels = c("Mean", "Median", "Weighted Mean", "Weighted Median"))
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  facet_wrap( ~ quantity, scales = "free_y", ncol = 1) +
  scale_color_manual(
    "Combination\nMethod",
    values = c(
      "Mean" = "#ef8a62",
      "Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Weighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Mean" = 4,
      "Median" = 5,
      "Weighted Mean" = 2,
      "Weighted Median" = 1
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
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  ylab("") +
  xlab("") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
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
        combine_method %in% c("Mean", "Median", "Weighted Mean", "Weighted Median")
      ) %>%
      dplyr::mutate(
        combine_method = factor(combine_method, levels = c("Mean", "Median", "Weighted Mean", "Weighted Median"))
      ),
    mapping = aes(x = forecast_date, y = value, color = combine_method, linetype = combine_method, group = combine_method)) +
  scale_color_manual(
    "Combination Method",
    values = c(
      "Mean" = "#ef8a62",
      "Median" = "#67a9cf",
      "Weighted Mean" = "#b2182b",
      "Weighted Median" = "#2166ac")
  ) +
  scale_linetype_manual(
    "Combination Method",
    values = c(
      "Mean" = 4,
      "Median" = 5,
      "Weighted Mean" = 2,
      "Weighted Median" = 1
    )
  ) +
  theme_bw()

legend_wis_horiz <- ggpubr::get_legend(p_wis_temp, position = "bottom")



#png("manuscript/figures/scores_by_week.png", width = 8, height = 8, units = "in", res = 600)
pdf("manuscript/figures/scores_by_week.pdf", width = 8, height = 8)
plot_layout <- grid.layout(
  nrow = 8, ncol = 4,
  widths = unit(c(2, 0.925, 0.9, 0.01), c("lines", rep("null", 3))),
  heights = unit(c(1.5, rep(1, 5), 1.5, 3), c("lines", rep("null", 5), "lines", "lines")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

print(as_ggplot(legend_wis_horiz), vp = viewport(layout.pos.row = 8, layout.pos.col = 2:3))
print(p_cases, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 2))
print(p_deaths, vp = viewport(layout.pos.row = 1:6, layout.pos.col = 3))
grid.text("Forecast Creation Date",
  just = "center",
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 7, layout.pos.col = 2:3))
grid.text("    Weekly\n    Cases or Deaths",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 2, layout.pos.col = 1))

print(
  ggplot() +
    geom_line(
      data = data.frame(x = c(1, 1), y = c(0.095, 0.97)),
      mapping = aes(x = x, y = y)) +
    xlim(0, 1) +
    scale_y_continuous(limits = c(0, 1), expand = expansion(0, 0)) +
    theme_void(),
  vp = viewport(layout.pos.row = 3:6, layout.pos.col = 1)
)
grid.text("                 Mean WIS",
  just = "center",
  rot = 90,
  gp = gpar(fontsize = 11),
  vp = viewport(layout.pos.row = 3:6, layout.pos.col = 1))

dev.off()
