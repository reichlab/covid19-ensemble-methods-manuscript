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
library(scales)

setwd(here())

us_locations <- c("06", "26")
euro_locations <- c("GB", "PL")

us_data <- dplyr::bind_rows(
  covidData::load_data(
    spatial_resolution = "state",
    temporal_resolution= "weekly",
    measure = "cases") %>%
    dplyr::mutate(target_variable = "Cases") %>%
    dplyr::filter(location %in% us_locations),
  covidData::load_data(
    spatial_resolution = "state",
    temporal_resolution= "weekly",
    measure = "deaths") %>%
    dplyr::mutate(target_variable = "Deaths") %>%
    dplyr::filter(location %in% us_locations)
) %>%
  dplyr::left_join(covidData::fips_codes, by = "location")

euro_data <- dplyr::bind_rows(
  covidData::load_data(
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "cases",
    geography = "global") %>%
    dplyr::mutate(target_variable = "Cases") %>%
    dplyr::filter(location %in% euro_locations),
  covidData::load_data(
    spatial_resolution = "national",
    temporal_resolution = "weekly",
    measure = "deaths",
    geography = "global") %>%
    dplyr::mutate(target_variable = "Deaths") %>%
    dplyr::filter(location %in% euro_locations)
) %>%
  dplyr::left_join(covidData::global_locations, by = "location")

p_cases_us <- ggplot(
    data = us_data %>%
      dplyr::filter(
        target_variable == "Cases",
        date >= as.Date("2020-04-01"))) +
  geom_line(mapping = aes(x = date, y = inc)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ location_name, ncol = 1) +
  ggtitle("Cases") +
  xlab("Date") +
  ylab("Weekly Reported Cases") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_deaths_us <- ggplot(
    data = us_data %>%
      dplyr::filter(
        target_variable == "Deaths",
        date >= as.Date("2020-04-01"))) +
  geom_line(mapping = aes(x = date, y = inc)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ location_name, ncol = 1) +
  ggtitle("Deaths") +
  xlab("Date") +
  ylab("Weekly Reported Deaths") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_cases_euro <- ggplot(
    data = euro_data %>%
      dplyr::filter(
        target_variable == "Cases",
        date >= as.Date("2020-04-01"))) +
  geom_line(mapping = aes(x = date, y = inc)) +
  geom_vline(xintercept = as.Date("2021-05-03"), linetype = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ location_name, ncol = 1) +
  xlab("Date") +
  ylab("Weekly Reported Cases") +
  ggtitle("Cases") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

p_deaths_euro <- ggplot(
    data = euro_data %>%
      dplyr::filter(
        target_variable == "Deaths",
        date >= as.Date("2020-04-01"))) +
  geom_line(mapping = aes(x = date, y = inc)) +
  geom_vline(xintercept = as.Date("2021-05-03"), linetype = 2) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ location_name, ncol = 1) +
  xlab("Date") +
  ylab("Weekly Reported Deaths") +
  ggtitle("Deaths") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


pdf("manuscript/figures/data_eval_phases.pdf", width = 7, height = 8)
plot_layout <- grid.layout(
  nrow = 5, ncol = 2,
  widths = unit(rep(1, 2), rep("null", 2)),
  heights = unit(c(1.5, 1, 1, 1.5, 1), c("lines", "null", "lines", "lines", "null")))

grid.newpage()
pushViewport(viewport(layout = plot_layout))

grid.text("(a) United States",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 14),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))

print(p_cases_us, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_deaths_us, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))

grid.text("(b) Europe",
  x = unit(0.0, "npc"),
  y = unit(0.5, "npc"),
  just = c("left", "center"),
  gp = gpar(fontsize = 14),
  vp = viewport(layout.pos.row = 4, layout.pos.col = 1:2))

print(p_cases_euro, vp = viewport(layout.pos.row = 5, layout.pos.col = 1))
print(p_deaths_euro, vp = viewport(layout.pos.row = 5, layout.pos.col = 2))

dev.off()
