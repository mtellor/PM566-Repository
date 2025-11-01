# Load libraries
library(tidyverse)
library(ggplot2)

# Load data
cranes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv")

# Filter to first and last count days only
cranes_season_edges <- cranes %>%
  filter(comment %in% c("First count of season", "Last count of season")) %>%
  mutate(comment = factor(comment, levels = c("First count of season", "Last count of season")))

# Plot: crane counts on first vs. last count days
ggplot(cranes_season_edges, aes(x = comment, y = observations, fill = comment)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.6) +
  scale_fill_manual(values = c("First count of season" = "dodgerblue2", "Last count of season" = "red2")) +
  labs(
    title = "Crane Counts on First vs. Last Count Days",
    subtitle = "Lake Hornborgasjön, Sweden (1994–2024)",
    x = "Season Marker",
    y = "Number of Cranes Observed"
  ) +
  theme_bw() +
  theme(legend.position = "none")
