# Introduction ------------------------------------------------------------
# This script will make a plots for visualizations of the flower (anther and 
# pistil) measurements.

library(tidyverse)
library(googlesheets4)

# Sheet is public
#gs4_deauth()

# Adding my Google service account credentials
gs4_auth(path = "~/.credentials/google_sheets_api/service_account.json")


# Tidying -----------------------------------------------------------------
# Loading the sheet
sheet_url <- "1YAbstZeZfTu6bItHQXVr02WrD1JmNNvn4dd-m88omLY"
df <- bind_rows(lapply(sheet_names(sheet_url), function(x){
  read_sheet(sheet_url, sheet = x)
}))

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
  scale_y_continuous(breaks = seq(5, 13, 1),
                     labels = seq(5, 13, 1),
                     limits = c(5, 13)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = 'bold', color = 'black'),
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
       width = 15,
       height = 9,
       dpi = 400,
       units = 'in')

# Boxplot of pistil lengths
ggplot(data = df, aes(x = reorder(accession_id, pistil_length, median), 
                      y = pistil_length)) +
  geom_boxplot(size = 1, color = "black") +
  labs(title = "Pistil lengths by accession",
       y = "Length (mm)") +
  scale_y_continuous(breaks = seq(5, 13, 1),
                     labels = seq(5, 13, 1),
                     limits = c(5, 13)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = 'bold', color = 'black'),
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
       width = 15,
       height = 9,
       dpi = 400,
       units = 'in')

# Boxplot of ratio
ggplot(data = df, aes(x = reorder(accession_id, anther_over_pistil, median), 
                      y = anther_over_pistil)) +
  geom_boxplot(size = 1, color = "black") +
  labs(title = "Anther and pistil lengths by accession",
       y = "Ratio of AL/PL") +
  scale_y_continuous(breaks = seq(0.7, 1.4, 0.1),
                     labels = seq(0.7, 1.4, 0.1),
                     limits = c(0.7, 1.4)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = 'bold', color = 'black'),
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
       width = 15,
       height = 9,
       dpi = 400,
       units = 'in')


# Selecting accessions for Xander to measure ------------------------------
# First we need to narrow down accessions to those with completed image 
# sequences. We'll get that information from the app Google sheet. 
app_info <- read_sheet(ss = "15oanRivQrhWl0EFmv4zxZqsIB1pLp9InEP43pjqkfGs")

# Pulling out the accessions that have completed image sequences at both 
# temperatures.
app_info <- app_info[app_info$good_run_count_26 >= 8 &
                     app_info$good_run_count_34 >= 8, ]

passing_accession_list <- unlist(app_info$accession)

# Adding a factor column for passing accessions, to make a pretty plot. 
df$accession_passing <- NA
df$accession_passing[df$accession_id %in% passing_accession_list] <- "Images complete"
df$accession_passing[is.na(df$accession_passing)] <- "Images incomplete"

# Making a plot with passing accessions highlighted
ggplot(data = df, aes(x = reorder(accession_id, pistil_length, median), 
                      y = pistil_length,
                      fill = accession_passing)) +
  geom_boxplot(size = 1, color = "black") +
  scale_fill_manual(values = c("green", "white")) +
  labs(title = "Pistil lengths by accession",
       y = "Length (mm)") +
  scale_y_continuous(breaks = seq(5, 13, 1),
                     labels = seq(5, 13, 1),
                     limits = c(5, 13)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = 'bold', color = 'black'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "pistil_lengths_complete_accessions.png"),
       device = 'png',
       width = 15,
       height = 9,
       dpi = 400,
       units = 'in')

# Choosing the top and bottom 4 accessions by pistil length
mean_pistils <- df %>%
  filter(accession_passing == "Images complete") %>%
  # Removing CW1008 because it's pimpinellifolium, just to keep the species consisten
  filter(accession_id != "CW1008") %>%
  summarize(mean_pistils = mean(pistil_length)) %>%
  arrange(mean_pistils)
  
shortest_pistils <- mean_pistils$accession_id[1:4]
longest_pistils <- mean_pistils$accession_id[(nrow(mean_pistils) - 3):nrow(mean_pistils)]

pistil_factor_list <- c(shortest_pistils, longest_pistils)

# Adding the factor to the original data frame
df$chosen_pistils <- NA
df$chosen_pistils[df$accession_id %in% pistil_factor_list] <- "Chosen"
df$chosen_pistils[is.na(df$chosen_pistils)] <- "Not chosen"

# Making a plot with passing accessions highlighted
ggplot(data = df, aes(x = reorder(accession_id, pistil_length, median), 
                      y = pistil_length,
                      fill = chosen_pistils)) +
  geom_boxplot(size = 1, color = "black") +
  scale_fill_manual(values = c("violet", "white")) +
  labs(title = "Pistil lengths by accession",
       y = "Length (mm)") +
  scale_y_continuous(breaks = seq(5, 13, 1),
                     labels = seq(5, 13, 1),
                     limits = c(5, 13)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1, face = 'bold', color = 'black'),
        plot.title = element_text(size = 28, face = 'bold', margin = margin(0, 0, 10, 0)),
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = file.path(getwd(), "plots", "pistil_lengths_chosen_accessions.png"),
       device = 'png',
       width = 15,
       height = 9,
       dpi = 400,
       units = 'in')

# Plot only chosen accessions
chosen_accessions <- df[df$chosen_pistils == "Chosen", ]
chosen_accessions$short_or_long <- NA
chosen_accessions$short_or_long[chosen_accessions$accession_id %in% shortest_pistils] <- "Short"
chosen_accessions$short_or_long[chosen_accessions$accession_id %in% longest_pistils] <- "Long"

ggplot(data = chosen_accessions, aes(x = reorder(accession_id, pistil_length, median), 
                      y = pistil_length,
                      fill = short_or_long)) +
  geom_boxplot(size = 1, color = "black") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Chosen accessions",
       y = "Pistil length (mm)") +
  scale_y_continuous(breaks = seq(5, 12, 1),
                     labels = seq(5, 12, 1),
                     limits = c(5, 12)) +
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

ggsave(filename = file.path(getwd(), "plots", "only_chosen_accessions.png"),
       device = 'png',
       width = 7,
       height = 9,
       dpi = 400,
       units = 'in')






