# Introduction ------------------------------------------------------------
# In this script we will make a couple scatter plots comparing average tube 
# length with average percent burst at 34C and average pistil length.

library(dplyr)
library(ggplot2)
library(ggrepel)

# Running the other scripts so we have their data
source(file.path(getwd(), "R", "tube_measurments.R"))
source(file.path(getwd(), "R", "bursting.R"))
source(file.path(getwd(), "R", "make_flower_plots.R"))


# Preparing the data for scatterplots -------------------------------------
# Lets make a nice simple data frame for plotting the relevant data.
burst_summary <- bursting %>%
  filter(temperature == 34) %>%
  group_by(accession_id) %>%
  summarize(mean_burst = mean(percent_burst))

tube_lengths$tube_length <- tube_lengths$tube_length * 0.7619
tube_summary <- tube_lengths %>%
  group_by(accession_id) %>%
  summarize(mean_tube_length = mean(tube_length))

pistil_summary <- chosen_accessions %>%
  group_by(accession_id) %>%
  summarize(mean_pistil_length = mean(pistil_length))

# Combining them
plot_df <- full_join(burst_summary, tube_summary)
plot_df <- full_join(plot_df, pistil_summary)


# Tube length v.  percent burst -------------------------------------------
tube_length_v_percent_burst <- ggplot(plot_df, aes(x = mean_tube_length, 
                                                   y = mean_burst,
                                                   label = accession_id)) +
  geom_smooth(method = 'lm', se = FALSE, size = 1, color = "red") +
  geom_point(size = 4) +
  geom_text_repel(size = 5, fontface = 'bold', box.padding = 0.5, min.segment.length = Inf) +
  labs(title = "Tube length versus percent burst",
       x = "Mean tube length at 26ºC (µm)",
       y = "Mean % burst at 34ºC") +
    scale_y_continuous(breaks = seq(0, 1, 0.2),
                       labels = seq(0, 100, 20),
                       limits = c(0, 1)) +
    scale_x_continuous(breaks = seq(275, 400, 25),
                       labels = seq(275, 400, 25),
                       limits = c(275, 410)) +
    theme_bw() +
    theme(axis.title = element_text(size = 26, face = 'bold'),
          axis.text = element_text(size = 22, face = 'bold', color = 'black'),
          axis.text.x = element_text(size = 20, face = 'bold', color = 'black'),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
          plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 20, 0)),
          panel.border = element_blank(),
          axis.line = element_line(size = 1, color = 'black'),
          axis.ticks = element_line(size = 1, color = 'black'),
          axis.ticks.length = unit(8, 'pt'),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
          panel.grid = element_blank(),
          legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "tube_length_v_percent_burst.png"),
       device = 'png',
       width = 9,
       height = 9,
       dpi = 400,
       units = 'in')

# Stats
lm_tube_length_percent_burst <- lm(mean_burst ~ mean_tube_length, data = plot_df)
summary(lm_tube_length_percent_burst)


# Tube length versus pistil length ----------------------------------------
tube_length_v_pistil_length <- ggplot(plot_df, aes(x = mean_pistil_length,
                                                   y = mean_tube_length,
                                                   label = accession_id)) +
  geom_smooth(method = 'lm', se = FALSE, size = 1, color = "red") +
  geom_point(size = 4) +
  geom_text_repel(size = 5, fontface = 'bold', box.padding = 0.5, min.segment.length = Inf) +
  labs(title = "Pistil length versus tube length",
       x = "Mean pistil length (mm)",
       y = "Mean tube length at 26ºC (µm)") +
  scale_x_continuous(breaks = seq(6, 10, 1),
                     labels = seq(6, 10, 1),
                     limits = c(6, 10.3)) +
  scale_y_continuous(breaks = seq(275, 400, 25),
                     labels = seq(275, 400, 25),
                     limits = c(275, 405)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, face = 'bold', color = 'black'),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 20, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "tube_length_v_pistil_length.png"),
       device = 'png',
       width = 9,
       height = 9,
       dpi = 400,
       units = 'in')

# Stats
lm_pistil_length_tube_length <- lm(mean_tube_length ~ mean_pistil_length, data = plot_df)
summary(lm_pistil_length_tube_length)
