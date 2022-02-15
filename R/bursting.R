# Introduction ------------------------------------------------------------
# Measuring bursting
 
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(dunn.test)

# Importing and adding metadata -------------------------------------------
# The bursting measurements:
gs4_deauth()
bursting_26 <- read_sheet("1zwpLN657F0NfHP-tTfhIzFI_6tiAOoQ_UaQ0IQoKAjE")
bursting_26$temperature <- 26

bursting_34 <- read_sheet("1UqZed3rQqFQgXJhTOgDbqZ6w4rvAGnFPORil7UD9A98")
bursting_34 <- bursting_34[ , c("file_name", "burst", "not_burst")]
bursting_34$temperature <- 34

bursting <- rbind(bursting_26, bursting_34)

# The key to associate image names with accession identifier
image_key <- read.table(file = file.path(getwd(), "data", "image_key.txt"))

# Adding the accession info
bursting <- left_join(bursting, image_key,
                          by = c("file_name" = "V1"))
colnames(bursting)[5] <- "accession_id"

# Adding factor order
bursting$accession_id <- factor(bursting$accession_id, 
                                    levels = c("CW0081", "CW0054", "CW0042", "CW0041",
                                               "CW0060", "CW0056", "CW0065", "CW0064"))

# Adding column for short or long pistil
bursting$short_or_long <- NA
bursting$short_or_long[bursting$accession_id %in% c("CW0081", "CW0054", "CW0042", "CW0041")] <- "short"
bursting$short_or_long[bursting$accession_id %in% c("CW0060", "CW0056", "CW0065", "CW0064")] <- "long"


# Calculating bursting ----------------------------------------------------
bursting$percent_burst <- bursting$burst / (bursting$burst + bursting$not_burst)



# Plotting ----------------------------------------------------------------
# Bursting at 26C and 34C
ggplot(data = bursting, aes(x = accession_id, 
                            y = percent_burst,
                            fill = factor(temperature))) +
  # geom_boxplot(size = 1, color = "black") +
  geom_boxplot(size = 1) +
  scale_fill_manual(values = c("#78e6ff", "#ff7878")) +
  labs(title = "Bursting at 26 ºC and 34 ºC",
       y = "% burst") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = seq(0, 100, 20),
                     limits = c(0, 1)) +
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

ggsave(filename = file.path(getwd(), "plots", "bursting_26_34.png"),
       device = 'png',
       width = 7,
       height = 9,
       dpi = 400,
       units = 'in')


# Bursting at 34C
ggplot(data = bursting[bursting$temperature == 34, ], aes(x = accession_id, 
                            y = percent_burst,
                            # fill = factor(temperature))) +
                            fill = factor(short_or_long))) +
  # geom_boxplot(size = 1, color = "black") +
  geom_boxplot(size = 1) +
  scale_fill_manual(values = c("#78e6ff", "#ff7878")) +
  labs(title = "Bursting at 34ºC",
       y = "% burst") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = seq(0, 100, 20),
                     limits = c(0, 1)) +
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

ggsave(filename = file.path(getwd(), "plots", "bursting_34.png"),
       device = 'png',
       width = 7,
       height = 9,
       dpi = 400,
       units = 'in')

# Bursting at 34C ordered by percent burst
ggplot(data = bursting[bursting$temperature == 34, ], aes(x = reorder(accession_id, percent_burst, median), 
                                                          y = percent_burst,
                                                          # fill = factor(temperature))) +
                                                          fill = factor(short_or_long))) +
  # geom_boxplot(size = 1, color = "black") +
  geom_boxplot(size = 1) +
  scale_fill_manual(values = c("#78e6ff", "#ff7878")) +
  labs(title = "Bursting at 34ºC",
       y = "% burst") +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = seq(0, 100, 20),
                     limits = c(0, 1)) +
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

ggsave(filename = file.path(getwd(), "plots", "bursting_34_ordered.png"),
       device = 'png',
       width = 7,
       height = 9,
       dpi = 400,
       units = 'in')


# Stats -------------------------------------------------------------------
# Checking normality
bursting %>% ggplot(aes(x = percent_burst)) +
  geom_histogram(binwidth = 0.05) +
  theme_classic()

# By accession
bursting %>% ggplot(aes(x = percent_burst)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~accession_id) +
  theme_classic()

# Kruskal-Wallis test at 26
kruskal.test(percent_burst ~ accession_id, data = bursting[bursting$temperature == 26, ])

# Dunn's test at 26
dunn.test(bursting[bursting$temperature == 26, ]$percent_burst, 
          bursting[bursting$temperature == 26, ]$accession_id,
          method = "bonferroni")

# Kruskal-Wallis test at 34 
kruskal.test(percent_burst ~ accession_id, data = bursting[bursting$temperature == 34, ])

# Dunn's test at 34 
dunn.test(bursting[bursting$temperature == 34, ]$percent_burst, 
          bursting[bursting$temperature == 34, ]$accession_id,
          method = "bonferroni")



