# Load libraries
library(tidyverse)
library(ggrepel)

# Load data
euroleague_basketball <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-07/euroleague_basketball.csv")

# Clean and filter: keep teams with at least one Final Four appearance
euro_clean <- euroleague_basketball %>%
  filter(!is.na(FinalFour_Appearances), FinalFour_Appearances > 0)

# Plot: Final Four appearances vs. Titles won
ggplot(euro_clean, aes(x = FinalFour_Appearances, y = Titles_Won)) +
  geom_point(color = "dodgerblue3", size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = Team), size = 3.5, max.overlaps = 20) +
  labs(
    title = "EuroLeague Final Four Appearances vs. Titles Won",
    subtitle = "Each point represents a team with at least one Final Four appearance",
    x = "Final Four Appearances",
    y = "Titles Won"
  ) +
  theme_bw()
