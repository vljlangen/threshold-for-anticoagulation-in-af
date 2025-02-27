 

###########################################################################
###########################################################################
###                                                                     ###
###                           CREATE FIGURE 2                           ###
###                                                                     ###
###########################################################################
###########################################################################

# Run first main_analyses.R


library(pacman)

#Load rest of packages with pacman
p_load(ggplot2, ggthemes, tibble, dplyr, showtext, magick,
       tidyr, forcats, circlize, patchwork, gt,
       ggstream, cowplot, pdftools, foreach, doParallel, doRNG, interp,
       stringr)



# Do preparations

cum_observations_with_noac <- lapply(simulated_cumqalys_with_noac, function(isRisk.list) {
  tempvec <- colSums(do.call(rbind, lapply(isRisk.list, function(x) x$Observations)))
  tempvec <- tempvec[c(1,3,4,5,6)]
  #tempvec <- tempvec / sum(tempvec)
  names(tempvec) <- c("1", 
                      #"2", 
                      "3", 
                      "4", 
                      "5", 
                      "6")
  return(tempvec)
}
)
cum_observations_without_noac <- lapply(simulated_cumqalys_without_noac, function(isRisk.list) {
  tempvec <- colSums(do.call(rbind, lapply(isRisk.list, function(x) x$Observations)))
  tempvec <- tempvec[c(1,3,4,5,6)]
  #tempvec <- tempvec / sum(tempvec)
  names(tempvec) <- c("1", 
                      #"2", 
                      "3", 
                      "4", 
                      "5", 
                      "6")
  return(tempvec)
}
)
# total observed morbidities after 20 year followup by stroke risk


obs.names <- c("dead",
               #"susceptible",
               "ischemic stroke",
               "intracranial bleeding",
               "other intracranial bleeding",
               "extracranial bleeding")
stream_data_with_noac <- data.frame(stroke_risk = NULL, obs = NULL, prop = NULL)
for (i in seq(0,10, by = 0.1)) {
  stream_data_with_noac <- rbind(stream_data_with_noac, data.frame(stroke_risk = i, obs = obs.names, prop = cum_observations_with_noac[[i*10+1]]))
}

stream_data_without_noac <- data.frame(stroke_risk = NULL, obs = NULL, prop = NULL)
for (i in seq(0,10, by = 0.1)) {
  stream_data_without_noac <- rbind(stream_data_without_noac, data.frame(stroke_risk = i, obs = obs.names, prop = cum_observations_without_noac[[i*10+1]]))
}



#################################################################
##           Prepare stylistic settings of the graph           ##
#################################################################


# Load specific font from Google Fonts
font_add_google("Rosario", family = "rosario")

# Invoke showtext
showtext_auto()

# Define constants
base_size_constant <- 28
x_margin <- 18
y_margin <- 22
title_size <- 22

# Define gradient 
pal=c(
  "#00405d",  # Deep navy blue  
  "#a15296",  # Medium purple  
  "#E388AE",  # Soft pink  
  "#F4B69C",  # Peachy-orange
  "#FFDBA8" # Soft golden-yellow
)


# Set the ylim value
ylim_custom <- 16000


# Stacking order for Plots 1 & 2
order <- c("Death",
           "Ischemic stroke",
           "Extracranial bleeding",
           "Intracranial bleeding",
           "Other intracranial bleeding")


# Remodel plot data
with_noac_plot_data <- stream_data_with_noac 
with_noac_plot_data <- with_noac_plot_data %>% rename(Count = prop,
                                                      `Outcome events` = obs)

without_noac_plot_data <- stream_data_without_noac
without_noac_plot_data <- without_noac_plot_data %>% rename(Count = prop,
                                                      `Outcome events` = obs)

# Remodel disability values
with_noac_plot_data <- with_noac_plot_data %>%
  mutate(`Outcome events` = case_when(
    `Outcome events` == "dead" ~ "Death",  # Rename "dead" to "Death"
    TRUE ~  str_to_sentence(`Outcome events`)   # Capitalize first letter of all other values
  ))


without_noac_plot_data <- without_noac_plot_data %>%
  mutate(`Outcome events` = case_when(
    `Outcome events` == "dead" ~ "Death",  # Rename "dead" to "Death"
    TRUE ~  str_to_sentence(`Outcome events`)   # Capitalize first letter of all other values
  ))




# Plot 1
p1 <- with_noac_plot_data %>% 
  mutate(`Outcome events` = factor(`Outcome events`, levels=order)) %>% 
  ggplot(aes(stroke_risk, Count, fill = `Outcome events`, label = `Outcome events`, color = `Outcome events`)) +
  geom_stream(type = "ridge", bw=1.1) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  coord_cartesian(clip = "off") +
  theme_classic(base_size = base_size_constant, base_family = "rosario") +
  labs(x = "Annual untreated\nstroke risk (%)") +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, ylim_custom, 4000), limits = c(0, ylim_custom), expand = c(0, 0)) +
  
  theme(plot.title = element_text(size = title_size, face = "bold"),
        axis.line = element_line(linewidth = 0.5),     # Set axis line width
        axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
        axis.title.x = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(t = x_margin, unit = "pt")),
        axis.title.y = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(r = y_margin, unit = "pt"))) +
  ggtitle("With\nNOAC medication")

  

# Plot 2
p2 <- without_noac_plot_data %>% 
  mutate(`Outcome events` = factor(`Outcome events`, levels=order)) %>% 
  ggplot(aes(stroke_risk, Count, fill = `Outcome events`, label = `Outcome events`, color = `Outcome events`)) +
  geom_stream(type = "ridge", bw=1.1) +
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  theme_classic(base_size = base_size_constant, base_family = "rosario") +
  labs(x = "Annual untreated\nstroke risk (%)") +
  scale_x_continuous(breaks = seq(0, 10, 2), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, ylim_custom, 4000), limits = c(0, ylim_custom), expand = c(0, 0)) +
  theme(plot.title = element_text(size = title_size, face = "bold"),
        axis.line = element_line(linewidth = 0.5),     # Set axis line width
        axis.ticks = element_line(linewidth = 0.5),    # Set axis tick width
        axis.title.x = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(t = x_margin, unit = "pt")),
        axis.title.y = element_text(size = base_size_constant,
                                    face = "bold",
                                    margin = margin(r = y_margin, unit = "pt"))) +
  ggtitle("Without\nNOAC medication")
  





############################################################################
############################################################################
###                                                                      ###
###                           CREATE THE PANEL                           ###
###                                                                      ###
############################################################################
############################################################################



# Extract the legend from one of the plots
legend <- get_legend(
  p1 + theme(legend.box.margin = margin(0, 0, 0, 0)))

# Create the panel with a legend
panel_fig2 <- plot_grid(p1 + theme(legend.position="none"),
                        
                        NULL,
                        
                        p2 + theme(legend.position="none"),
                        
                        NULL,
                        
                        legend,
                        
                        rel_widths = c(1, 0.28, 1, 0.28, 1),
                        nrow = 1)

# Display the panel
print(panel_fig2)
 

###########################################################################
###########################################################################
###                                                                     ###
###                           EXPORT FIGURE 2                           ###
###                                                                     ###
###########################################################################
###########################################################################


# When showtext() is used to change font(s), plots have to be saved as pdf files,
# otherwise proportions get distorted.

# Save as PDF with dpi specified
ggsave("figures/figure2.pdf", width = 15, height =8, dpi = 600)

# Load that pdf file with the magick package
pdf_image <- magick::image_read_pdf("figures/figure2.pdf", density = 600)

# Save it as PNG
image_write(pdf_image,
            path = "figures/figure2.png",
            format = "png",
            density = 600)


