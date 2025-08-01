#Tipping point for OAC medication main script
#by: Aleksi Kristian Winsten
#contact: alkrwi@utu.fi
#date: 7.06.2025
#University of Turku, Finland


load("data/main-analysis-70-year-olds.RData")



#################################################################
##                        Load packages                        ##
#################################################################

library(pacman)
p_load(ggplot2, ggstream, showtext, magick, grid)


##################################################################
##                           Figure 1                           ##
##################################################################

save.prefix <- "figures/figure_S3"


#############################################
## draw a plot from the mean diff p-values ##
#############################################
#
mean_diff_values <- vector(mode = "numeric", length = 101)
for (i in 1:101) {
  mean_diff_values[i] <- mean(sapply(simulated_cumqalys_with_noac[[i]], function(x) x$QALY) - sapply(simulated_cumqalys_without_noac[[i]], function(x) x$QALY))
}
#the eckmann plot
#the tipping point
qaly.tipping.point <- (min(which(mean_diff_values >= 0)) -1 ) / 10 - 0.05
sprintf( "the QALY tipping point is: %.2f", qaly.tipping.point)

std.err <- function(x) sd(x)/sqrt(length(x))
eckmann_with_noac_df <- do.call(rbind,
                                lapply(simulated_cumqalys_with_noac, 
                                       function(isRisk.list) {
                                         x <- sapply(isRisk.list, function(x) x$QALY)
                                         c(mean(x), mean(x) - 1.96*std.err(x), mean(x) + 1.96*std.err(x))
                                       }
                                )
)
colnames(eckmann_with_noac_df) <- c("mean.with", "lower.with", "upper.with")

eckmann_without_noac_df <- do.call(rbind,
                                   lapply(simulated_cumqalys_without_noac, 
                                          function(isRisk.list) {
                                            x <- sapply(isRisk.list, function(x) x$QALY)
                                            c(mean(x), mean(x) - 1.96*std.err(x), mean(x) + 1.96*std.err(x))
                                          }
                                   )
)
colnames(eckmann_without_noac_df) <- c("mean.without", "lower.without", "upper.without")
df <- cbind(ISrisk = seq(0,10,by=0.1), eckmann_with_noac_df)
df <- cbind(df, eckmann_without_noac_df)
df <- as.data.frame(df)




# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()

# For PDF with proper font handling
pdf(paste0(save.prefix, ".pdf"), width = 8, height = 6)
showtext_begin()  # Explicitly begin showtext

p <- ggplot(df, aes(x = ISrisk)) +
  
  ggtitle("20-year cumulative QALY expectancy by annual stroke risk") +
  
  theme_classic(base_family = "rosario") +
  
  theme(#plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 40, 20),
    axis.title.x = element_text(face = "bold"), 
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  
  theme(axis.title.x = element_text(
    margin = margin(t = 12, unit = "pt")),  # Adjust margin for x-axis title
    axis.title.y = element_text(
      margin = margin(r = 12, unit = "pt"))) +  # Adjust margin for y-axis title
  
  labs(y = "Cumulative QALYs",
       x =  "Annual untreated stroke risk (%)") +
  
  geom_vline(xintercept = qaly.tipping.point, linetype = "dashed", color = "black") +
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(3, 13, by = 1),
                     limits=c(3, 13),
  ) +
  
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 10, by = 1),
                     limits=c(0,10)
  ) +
  
  annotate("text", x = 5.5, y = 8.3, label = "With DOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  annotate("text", x = 6.2, y = 4.3, label = "Without DOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  geom_line(aes(y=mean.with), color='#E6A03C', linewidth = 1) +
  geom_line(aes(y=mean.without), color='#517EC1', linewidth = 1) +
  # Add annotations that will appear outside plot area
  annotation_custom(
    grob = textGrob("\u2191", gp = gpar(fontsize = 18)),
    xmin = qaly.tipping.point, xmax = qaly.tipping.point,
    ymin = 2.4, ymax = 2.7
  ) +
  annotation_custom(
    grob = textGrob(as.character(qaly.tipping.point), gp = gpar(fontsize = 14, fontfamily = "rosario")),
    xmin = qaly.tipping.point, xmax = qaly.tipping.point,
    ymin = 2.0, ymax = 1.9
  )

# Turn off clipping and draw
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)

showtext_end()  # End showtext
dev.off()

# Create TIFF version
pdf_image <- magick::image_read_pdf(paste0(save.prefix, ".pdf"), density = 1200)
image_write(pdf_image,
            path = paste0(save.prefix, ".tiff"),
            format = "tiff",
            density = 1200,
            compression = "LZW"
)
