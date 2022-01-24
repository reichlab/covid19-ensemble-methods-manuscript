library(ggplot2)

color_values <- c(
  "#bd0026",
  "#d6510e",
  "#fd9d59",
  #"#ff9041",
  "#fed976",
  # "#fc4e2a",
  # "#feb24c",
  # "#fed976",
  "#67a9cf",
  "#2166ac",
  "#5aae61"#,
)

names(color_values) <- color_values

p <- ggplot(data = data.frame(
    color = factor(color_values, levels = color_values),
    x = 0)) +
  geom_raster(mapping = aes(y = color, x = 0, fill = color)) +
  scale_fill_manual(values = color_values) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank()
  )

png("color_choices.png")
print(p)
dev.off()
