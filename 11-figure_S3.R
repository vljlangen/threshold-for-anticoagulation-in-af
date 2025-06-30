#project: eckmann revisited
#code by: A. Winstén, V. Langén
#date: 25.6.2025

#############################################################
## Cumulative life years without severely disabling events ##
#############################################################


#load("data/main-analysis-70-year-olds.RData")


#################################################################
##                        Load packages                        ##
#################################################################

library(pacman)
p_load(ggplot2, ggstream, showtext, magick, grid)



###############
## Figure S3 ##
###############

save.prefix <- "figures/figure_s3"




mean_goodLife_values <- vector(mode = "numeric", length = 101)
for (i in 1:101) {
  mean_goodLife_values[i] <- mean(sapply(simulated_cumqalys_with_noac[[i]], function(x) x$goodLife) - sapply(simulated_cumqalys_without_noac[[i]], function(x) x$goodLife))
}

# the tipping point
goodLife.tipping.point <- (min(which(mean_goodLife_values >= 0)) - 1 ) / 10 - 0.05
sprintf( "the good life tipping point is: %.2f", goodLife.tipping.point)



#the good life plot
std.err <- function(x) sd(x)/sqrt(length(x))
goodLife_with_noac_df <- do.call(rbind,
                                 lapply(simulated_cumqalys_with_noac, 
                                        function(isRisk.list) {
                                          x <- sapply(isRisk.list, function(x) x$goodLife)
                                          c(mean(x), mean(x) - 1.96*std.err(x), mean(x) + 1.96*std.err(x))/12
                                        }
                                 )
)
colnames(goodLife_with_noac_df) <- c("mean.with", "lower.with", "upper.with")

goodLife_without_noac_df <- do.call(rbind,
                                    lapply(simulated_cumqalys_without_noac, 
                                           function(isRisk.list) {
                                             x <- sapply(isRisk.list, function(x) x$goodLife)
                                             c(mean(x), mean(x) - 1.96*std.err(x), mean(x) + 1.96*std.err(x))/12
                                           }
                                    )
)
colnames(goodLife_without_noac_df) <- c("mean.without", "lower.without", "upper.without")
df <- cbind(ISrisk = seq(0,10,by=0.1), goodLife_with_noac_df)
df <- cbind(df, goodLife_without_noac_df)
df <- as.data.frame(df)




# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")
showtext_auto()

# For PDF with proper font handling
pdf(paste0(save.prefix, ".pdf"), width = 8, height = 6)
showtext_begin()  # Explicitly begin showtext

p <- ggplot(df, aes(x = ISrisk)) +
  
  ggtitle("Cumulative life years without severely disabling events by annual stroke risk") +
  
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
  
  labs(y = "Years",
       x =  "Annual untreated stroke risk (%)") +
  
  geom_vline(xintercept = goodLife.tipping.point, linetype = "dashed", color = "black") +
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(3, 17, by = 1),
                     limits=c(3, 17),
  ) +
  
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 10, by = 1),
                     limits=c(0,10)
  ) +
  
  annotate("text", x = 5.5, y = 10.8, label = "With NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  annotate("text", x = 6.2, y = 6.0, label = "Without NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  geom_line(aes(y=mean.with), color='#E6A03C', linewidth = 1) +
  geom_line(aes(y=mean.without), color='#517EC1', linewidth = 1) +
  # Add annotations that will appear outside plot area
  annotation_custom(
    grob = textGrob("\u2191", gp = gpar(fontsize = 18)),
    xmin = goodLife.tipping.point, xmax = goodLife.tipping.point,
    ymin = 2.1, ymax = 2.7
  ) +
  annotation_custom(
    grob = textGrob(as.character(goodLife.tipping.point), gp = gpar(fontsize = 14, fontfamily = "rosario")),
    xmin = goodLife.tipping.point, xmax = goodLife.tipping.point,
    ymin = 1.2, ymax = 1.9
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


