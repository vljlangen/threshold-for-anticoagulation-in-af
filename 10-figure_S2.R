# Load required libraries
library(pacman)
p_load(ggplot2, showtext, magick, grid)

save.prefix <- "figures/figure_S2"

# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()

# Linear regression for major bleeding
majorBleeding_coef <- lm(
  c(1.14, 1.0, 0.7, 0.51, 0.7, 0.7, 2.1, 1.8, 1.4, 2.0) ~ 
    c(1.05, 3, 0.7, 0.18, 0.44, 0.7, 4.9, 3.4, 4.9, 3.7)
)$coefficients

majorBleed <- function(x) {
  as.numeric(majorBleeding_coef[1] + x * majorBleeding_coef[2]) / 100
}

# Linear regression for mortality
mortality_coef <- lm(
  c(3.75, 3.7, 1.2, 3.2, 8.0, 13.8, 3.0, 4.4, 4.4, 3.7) ~ 
    c(1.05, 1.1, 0.4, 0.8, 1.7, 3.1, 1.49, 1.83, 1.95, 4)
)$coefficients

mortality <- function(x) {
  as.numeric(mortality_coef[1] + x * mortality_coef[2]) / 100
}

# Generate data
x_vals <- seq(0, 10, length.out = 100)
data <- data.frame(
  IschemicStrokeRate = rep(x_vals, 2),
  AnnualRate = c(sapply(x_vals, majorBleed), sapply(x_vals, mortality)),
  Outcome = rep(c("Major Bleeding", "Mortality"), each = length(x_vals))
)

# For PDF with proper font handling
pdf(paste0(save.prefix, ".pdf"), width = 8, height = 6)
showtext_begin()  # Explicitly begin showtext

# Plot
p <- ggplot(data, aes(x = IschemicStrokeRate, y = AnnualRate * 100, color = Outcome)) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("Major Bleeding" = "steelblue", "Mortality" = "darkred")) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1),
    labels = function(x) format(x, nsmall = 0),
    expand = c(0.03, 0)
  ) +
  scale_y_continuous(
    breaks = c(0, seq(2, 20, by = 2)),
    labels = function(y) format(y, nsmall = 0),
    limits = c(0, 20),
    expand = c(0.03, 0)
  ) +
  labs(
    x = "Annual ischemic stroke rate (%)",
    y = "Annual rate (%)",
    color = ""
  ) +
  theme_classic(base_family = "rosario") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 20, 20),
    axis.title.x = element_text(face = "bold", margin = margin(t = 15, unit = "pt")),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10, unit = "pt")),
    legend.position = "none"
  ) +
  
  annotate("text", x = 7, y = 16.1, label = "Mortality",
           size = 5, color = "black", hjust = 0.5,
           family = "rosario") +
  
  annotate("text", x = 7, y = 4.1, label = "Major Bleeding",
           size = 5, color = "black", hjust = 0.5,
           family = "rosario")

# Turn off clipping and draw
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

showtext_end()  # End showtext
dev.off()

# Create PNG version
pdf_image <- magick::image_read_pdf(paste0(save.prefix, ".pdf"), density = 300)
magick::image_write(pdf_image,
                    path = paste0(save.prefix, ".png"),
                    format = "png",
                    density = 300)
