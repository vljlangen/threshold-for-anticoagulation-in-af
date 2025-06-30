
###########################################################################
###########################################################################
###                                                                     ###
###         MEAN 20-YEAR CUMULATIVE QALYS BY ANNUAL STROKE RISK         ###
###              IN THE PROBABILISTIC SENSITIVITY ANALYSIS              ###
###                                                                     ###
###########################################################################
###########################################################################




#################################################################
##                          Load data                          ##
#################################################################

df <- readRDS("data/figure_S6_and_S7.rds")

save.prefix <- "figures/figure_s4"



df$mean_diff <- df$mean_qaly_with_noac - df$mean_qaly_without_noac
df$benefit <- df$mean_diff > 0


#################################################################
##                        Load packages                        ##
#################################################################


library(pacman)



#Load rest of packages with pacman
p_load(ggplot2, ggthemes, dplyr, showtext, magick, grid, gt)
       


confidences.df <- df %>% group_by(isRisk) %>%
  summarise(prob = sum(benefit)/1000)

tipping.point.lm <- lm(confidences.df$prob[1:25] ~ confidences.df$isRisk[1:25])
#check the tipping point
#tipping.point.lm$coefficients[2]*0.65 + tipping.point.lm$coefficients[1]


# Create a data frame for the fitted line across the full x range
x_vals <- data.frame(x = seq(0, 3.1, length.out = 30))
x_vals$y <- tipping.point.lm$coefficients[2]*seq(0, 3.1, length.out = 30) + tipping.point.lm$coefficients[1]



############################################################################
##                                Figure:                                 ##
##            Probability that NOAC therapy leads to more QALYs           ##
##  than withholding treatment in the probabilistic sensitivity analysis  ##
############################################################################



# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()

# For PDF with proper font handling
pdf(paste0(save.prefix, ".pdf"), width = 8, height = 5)
showtext_begin()  # Explicitly begin showtext

p <- ggplot(confidences.df, aes(x = isRisk, y = prob)) +
  geom_point(color = "#E69F3D", size = 2.5) +
  ggtitle("Probability that NOAC therapy leads to more QALYs\nthan withholding treatment in the probabilistic sensitivity analysis\n") +
  theme_classic(base_family = "rosario") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 40, 20),
    axis.title.x = element_text(face = "bold", margin = margin(t = 10, unit = "pt")),
    axis.title.y = element_text(face = "bold", margin = margin(r = 4, unit = "pt")),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Annual untreated stroke risk (%)",
    y = "Probability (%)"
  ) +
  scale_y_continuous(
    breaks = seq(0.4, 1, by = 0.1), 
    limits = c(0.35, 1), 
    expand = c(0, 0.02),
    labels = function(x) paste0(x * 100, "")
  ) +
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10), expand = c(0.03, 0)) +
  geom_vline(xintercept = 0.65, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  # Add annotations that will appear outside plot area
  annotation_custom(
    grob = textGrob("\u2191", gp = gpar(fontsize = 18)),
    xmin = 0.65, xmax = 0.65,
    ymin = 0.26, ymax = 0.32
  ) +
  annotation_custom(
    grob = textGrob("0.65", gp = gpar(fontsize = 14, fontfamily = "rosario")),
    xmin = 0.65, xmax = 0.65,
    ymin = 0.19, ymax = 0.25
  )

# Turn off clipping and draw
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

showtext_end()  # End showtext
dev.off()

# Create PNG version
pdf_image <- magick::image_read_pdf(paste0(save.prefix, ".pdf"), density = 300)
image_write(pdf_image,
            path = paste0(save.prefix, ".png"),
            format = "png",
            density = 300)





 