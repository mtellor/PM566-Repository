# Load libraries
library(tidyverse)
library(scales)

# Load dataset
prizes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-10-28/prizes.csv")

# Prepare gender proportions by genre
gender_genre <- prizes %>%
  filter(gender %in% c("man", "woman")) %>%
  group_by(prize_genre, gender) %>%
  summarize(n = n(), .groups = "drop") %>%
  group_by(prize_genre) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()

# Extract proportion of women per genre for ascending ranking
ranked_genres <- gender_genre %>%
  filter(gender == "woman") %>%
  arrange(pct) %>%
  pull(prize_genre)

# Reorder genre factor by ascending proportion of women
gender_genre <- gender_genre %>%
  mutate(prize_genre = factor(prize_genre, levels = ranked_genres))

# Plot stacked flipped bar chart
ggplot(gender_genre, aes(x = pct, y = prize_genre, fill = gender)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("woman" = "pink1", "man" = "dodgerblue")) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Gender Representation by Genre in British Literary Prizes",
    subtitle = "Stacked proportions of men and women among recipients (1990–2022)",
    x = "Proportion",
    y = "Genre",
    fill = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    axis.text.y = element_text(face = "bold")
  )
