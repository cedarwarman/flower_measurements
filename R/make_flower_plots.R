# Introduction ------------------------------------------------------------
# This script will make a plots for visualizations of the flower (anther and 
# pistil) measurements.

library(tidyverse)
library(googlesheets4)

# Sheet is public
gs4_deauth()


# Tidying -----------------------------------------------------------------
# Loading the sheet
df <- read_sheet(ss = "1YAbstZeZfTu6bItHQXVr02WrD1JmNNvn4dd-m88omLY",
                 sheet = "wave_1")
df <- df[complete.cases(df), ]

df <- df %>% 
  group_by(accession_id) %>%
  filter(n() == 12)

df$anther_over_pistil <- df$anther_length / df$pistil_length


# Plotting ----------------------------------------------------------------
# Boxplot of anther lengths
ggplot(data = df, aes(x = reorder(accession_id, anther_length, median), 
                      y = anther_length)) +
  geom_boxplot(size = 1, color = "black") +
  labs(title = "Anther cone lengths by accession",
       y = "Length (mm)") +
  scale_y_continuous(breaks = seq(5, 12, 1),
                     labels = seq(5, 12, 1),
                     limits = c(5, 12.3)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, face = 'bold', color = 'black'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "anther_lengths.png"),
       device = 'png',
       width = 9,
       height = 9,
       dpi = 400,
       units = 'in')

# Boxplot of pistil lengths
ggplot(data = df, aes(x = reorder(accession_id, pistil_length, median), 
                      y = pistil_length)) +
  geom_boxplot(size = 1, color = "black") +
  labs(title = "Pistil lengths by accession",
       y = "Length (mm)") +
  scale_y_continuous(breaks = seq(5, 12, 1),
                     labels = seq(5, 12, 1),
                     limits = c(5, 12.3)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, face = 'bold', color = 'black'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "pistil_lengths.png"),
       device = 'png',
       width = 9,
       height = 9,
       dpi = 400,
       units = 'in')

# Boxplot of ratio
ggplot(data = df, aes(x = reorder(accession_id, anther_over_pistil, median), 
                      y = anther_over_pistil)) +
  geom_boxplot(size = 1, color = "black") +
  labs(title = "Anther and pistil lengths by accession",
       y = "Ratio of AL/PL") +
  scale_y_continuous(breaks = seq(0.8, 1.4, 0.1),
                     labels = seq(0.8, 1.4, 0.1),
                     limits = c(0.8, 1.4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1, face = 'bold', color = 'black'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "anther_pistil_ratio.png"),
       device = 'png',
       width = 9,
       height = 9,
       dpi = 400,
       units = 'in')

