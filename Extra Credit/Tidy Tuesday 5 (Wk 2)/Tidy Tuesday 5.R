# Week 2: Australian Frogs

# Load libraries
library(tidyverse)
library(lubridate)
library(scales)

# Load data
frogID_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv")

# Extract month and simplify species names
frog_calls_by_month <- frogID_data %>%
  mutate(month = month(eventDate, label = TRUE, abbr = TRUE)) %>%
  count(scientificName, month) %>%
  group_by(scientificName) %>%
  mutate(total = sum(n),
         prop = n / total) %>%
  ungroup()

# Pick 6 species with the most total calls for clarity
top_species <- frog_calls_by_month %>%
  group_by(scientificName) %>%
  summarise(total = sum(n)) %>%
  slice_max(total, n = 6) %>%
  pull(scientificName)

# Filter to top species
frog_calls_top <- frog_calls_by_month %>%
  filter(scientificName %in% top_species)

# Plot: seasonal calling pattern
ggplot(frog_calls_top, aes(x = month, y = prop, group = scientificName, color = scientificName)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Seasonal Calling Patterns of Australian Frogs (2023)",
    subtitle = "Proportion of total calls by month for six common species",
    x = "Month",
    y = "Proportion of Annual Calls",
    color = "Species"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")



# Define custom colors
custom_colors <- c(
  "red",
  "orange2",
  "grey50",
  "green3",
  "dodgerblue",
  "magenta3")

# Assign names to match the species order
species_order <- frog_calls_top %>%
  distinct(scientificName) %>%
  arrange(scientificName) %>%
  pull()

names(custom_colors) <- species_order

# Plot with manual colors
ggplot(frog_calls_top, aes(x = month, y = prop, group = scientificName, color = scientificName)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Seasonal Calling Patterns of Australian Frogs (2023)",
    subtitle = "Proportion of total calls by month for six common species",
    x = "Month",
    y = "Proportion of Annual Calls",
    color = "Species"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

