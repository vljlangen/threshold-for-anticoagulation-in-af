
############################
## draw the GoodLife plot ##
############################
#############################################
## draw a plot from the mean diff p-values ##
#############################################

# Run first main_analyses.R



mean_goodLife_pvalues <- vector(mode = "numeric", length = 101)
for (i in 1:101) {
  mean_goodLife_pvalues[i] <- t.test(sapply(simulated_cumqalys_with_noac[[i]], function(x) x$goodLife), 
                                     sapply(simulated_cumqalys_without_noac[[i]], function(x) x$goodLife),
                                     alternative = "less")$p.value
}
# the tipping point
goodLife.tipping.point <- (min(which(mean_goodLife_pvalues >= 0.50)) - 1 ) / 10 - 0.05
sprintf( "the good life tipping point is: %.2f", goodLife.tipping.point)

ggplot(data.frame(ISrisk = seq(0,10,by=0.1), 
                  Probability = mean_goodLife_pvalues), 
       aes(x = ISrisk, y = Probability)) + 
  geom_point() #+ geom_smooth()

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

# Invoke showtext
showtext_auto()



ggplot(df, aes(x = ISrisk)) +
  
  ggtitle("Cumulative life years without severely disabling events by annual stroke risk") +
  
  theme_classic(base_family = "rosario") +
  
  theme(#plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 17),
    plot.margin = margin(20, 20, 20, 20),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  
  theme(axis.title.x = element_text(
    margin = margin(t = 19, unit = "pt")),  # Adjust margin for x-axis title
    axis.title.y = element_text(
      margin = margin(r = 23, unit = "pt"))) +  # Adjust margin for y-axis title
  
  labs(y = "Years",
       x =  "Annual untreated stroke risk (%)") +
  
  geom_vline(xintercept = goodLife.tipping.point, linetype = "dashed", color = "black") +
  
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(3, 17, by = 1),
                     limits=c(3, 17),
                     #labels=c("0", "3", "4", "5", "6", "7")
  ) +
  
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 10, by = 1),
                     limits=c(0,10)#,
                     #labels=c("0", "2.5", "5.0", "7.5", "10")
  ) +
  
  annotate("text", x = 5.5, y = 10.7, label = "With NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  annotate("text", x = 6.2, y = 5.7, label = "Without NOAC",
           size = 5, color="black", hjust = 1.0,
           family = "rosario") +
  
  annotate("text", x = 1, y = 4, label = as.character(goodLife.tipping.point), size = 5, family = "rosario") +
  
  geom_line(aes(y=mean.with), color='#E6A03C', linewidth = 1) +
  #geom_smooth(aes(y=lower.with), color='lightblue') +
  #geom_smooth(aes(y=upper.with), color='lightblue') +
  geom_line(aes(y=mean.without), color='#517EC1', linewidth = 1)
#geom_smooth(aes(y=lower.without), color='pink') +
#geom_smooth(aes(y=upper.without), color='pink')


# EHRA instructions max. 1920 x 1080

# # # Save as PDF with dpi specified
ggsave("figures/goodLife_expectancy_by_stroke_risk.pdf", dpi = 170,
       bg = "white", width = 8, height = 6)



# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/goodLife_expectancy_by_stroke_risk.pdf", density = 170)

# Save it as PNG
image_write(pdf_image,
            path = "figures/goodLife_expectancy_by_stroke_risk.png",
            format = "png",
            density = 170)


# Save it as TIFF
image_write(pdf_image,
            path = "figures/goodLife_expectancy_by_stroke_risk.tiff",
            format = "tiff",
            density = 170,
            compression = "LZW")

# Save it as JPEG
image_write(pdf_image,
            path = "figures/goodLife_expectancy_by_stroke_risk.jpeg",
            format = "jpeg",
            density = 170,
            quality = 100)
