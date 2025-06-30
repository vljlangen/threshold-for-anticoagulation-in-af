
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

save.prefix <- "figures/figure_s5"



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






##################################################################
##                         Draw a table                         ##
##################################################################


# Create and save the table
gt_table <- gt(confidences.df) %>%
  tab_header(
    title = "probability sensitivity analysis summary"
  ) %>%
  cols_label(
    prob = "Probability NOAC treatment is beneficial in terms of QALY",
    isRisk = "Stroke Risk"
  )



prob.df <- df %>% group_by(isRisk) %>%
  summarise(mean.with = sum(mean_qaly_with_noac)/1000, 
            mean.without = sum(mean_qaly_without_noac)/1000)
  




#the eckmann plot
#the tipping point
qaly.tipping.point.50 <- 0.65
qaly.tipping.point.90 <- 2.6

# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# For PDF
pdf(paste0(save.prefix, ".pdf"), width = 8, height = 5)
showtext_begin()  # Start showtext for proper font handling

p <- ggplot(prob.df, aes(x = isRisk)) +
  ggtitle("Mean 20-year cumulative QALYs by annual stroke risk\nin the probabilistic sensitivity analysis") +
  theme_classic(base_family = "rosario") +
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 40, 20),
    axis.title.x = element_text(face = "bold", margin = margin(t = 18, unit = "pt")),
    axis.title.y = element_text(face = "bold", margin = margin(r = 18, unit = "pt")),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(y = "Cumulative QALYs",
       x = "Annual untreated stroke risk (%)") +
  geom_vline(xintercept = qaly.tipping.point.50, linetype = "dashed", color = "black") +
  scale_y_continuous(expand = c(0, 0),
                   breaks = seq(3, 13, by = 1),
                   limits = c(3, 13)) +
  scale_x_continuous(expand = c(0, 0),
                   breaks = seq(0, 10, by = 1),
                   limits = c(0,10)) +
  annotate("text", x = 5.5, y = 8.4, label = "With NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  annotate("text", x = 6.2, y = 4.2, label = "Without NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  geom_line(aes(y=mean.with), color='#E6A03C', linewidth = 1) +
  geom_line(aes(y=mean.without), color='#517EC1', linewidth = 1) +
  # Add annotations that will appear outside plot area
  annotation_custom(
    grob = textGrob("\u2191", gp = gpar(fontsize = 18)),
    xmin = 0.65, xmax = 0.65,
    ymin = 2.0, ymax = 2.8
  ) +
  annotation_custom(
    grob = textGrob("0.65", gp = gpar(fontsize = 14, fontfamily = "rosario")),
    xmin = 0.65, xmax = 0.65,
    ymin = 0.6, ymax = 2.4
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


