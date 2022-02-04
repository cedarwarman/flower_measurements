# Introduction ------------------------------------------------------------
# Plotting and doing statistics with Xander's tube measurements.

library(googlesheets4)
library(dplyr)
library(ggplot2)
library(dunn.test)


# Importing and adding metadata -------------------------------------------
# The tube measurements:
gs4_deauth()
tube_lengths <- read_sheet("113Ky66QnrF7j2tEly51sgi32Jshh77i3SQSIBCo-WpU")

# The key to associate image names with accession identifier
image_key <- read.table(file = file.path(getwd(), "data", "image_key.txt"))

# Adding the accession info
tube_lengths <- left_join(tube_lengths, image_key,
                          by = c("file_name" = "V1"))
colnames(tube_lengths)[3] <- "accession_id"

tube_lengths$accession_id <- factor(tube_lengths$accession_id, 
                                    levels = c("CW0081", "CW0054", "CW0042", "CW0041",
                                               "CW0060", "CW0056", "CW0065", "CW0064"))

tube_lengths$short_or_long <- NA
tube_lengths$short_or_long[tube_lengths$accession_id %in% c("CW0081", "CW0054", "CW0042", "CW0041")] <- "short"
tube_lengths$short_or_long[tube_lengths$accession_id %in% c("CW0060", "CW0056", "CW0065", "CW0064")] <- "long"

# Finding n of tube
tube_lengths %>%
  group_by(accession_id) %>%
  summarize(n = n())

# Making an initial plot of the lengths -----------------------------------
# Color-coding the same as other plots
plot_colors <- c("#009292", "#d1214d")

ggplot(data = tube_lengths, aes(x = accession_id, 
                                y = tube_length,
                                fill = short_or_long)) + 
  geom_boxplot(size = 1, color = "black") +
  # geom_violin(size = 1, color = "black") +
  scale_fill_manual(values = plot_colors) +
  labs(title = "Tube lengths",
       y = "Length (pixels)") +
  # scale_y_continuous(breaks = seq(5, 12, 1),
  #                    labels = seq(5, 12, 1),
  #                    limits = c(5, 12)) +
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

ggsave(filename = file.path(getwd(), "plots", "tube_lengths.png"),
       device = 'png',
       width = 7,
       height = 9,
       dpi = 400,
       units = 'in')


# Statistics --------------------------------------------------------------
# Checking normality
tube_lengths %>% ggplot(aes(x = tube_length)) +
  geom_histogram(binwidth = 10) +
  theme_classic()

# By accession
tube_lengths %>% ggplot(aes(x = tube_length)) +
  geom_histogram(binwidth = 20) +
  facet_wrap(~accession_id) +
  theme_classic()

# Kruskal-Wallis test
kruskal.test(tube_length ~ accession_id, data = tube_lengths)

# Dunn's test
dunn.test(tube_lengths$tube_length, 
          tube_lengths$accession_id,
          method = "bonferroni")
