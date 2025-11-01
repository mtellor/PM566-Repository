library(tidyverse)
library(ggplot2)

# Load datasets
flint_mdeq <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_mdeq.csv")
flint_vt <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-04/flint_vt.csv")

# Bin MDEQ lead2 values
mdeq_bins <- flint_mdeq %>%
  filter(!is.na(lead2), lead2 > 0) %>%
  mutate(source = "MDEQ",
         category = case_when(
           lead2 < 5 ~ "<5 ppb",
           lead2 <= 15 ~ "5–15 ppb",
           lead2 > 15 ~ ">15 ppb"
         ))

# Bin VT lead values
vt_bins <- flint_vt %>%
  filter(!is.na(lead), lead > 0) %>%
  mutate(source = "Virginia Tech",
         category = case_when(
           lead < 5 ~ "<5 ppb",
           lead <= 15 ~ "5–15 ppb",
           lead > 15 ~ ">15 ppb"
         ))

# Combine and count
lead_categories <- bind_rows(mdeq_bins, vt_bins) %>%
  count(source, category)

# Plot grouped bar chart
# Set category order
lead_categories$category <- factor(
  lead_categories$category,
  levels = c("<5 ppb", "5–15 ppb", ">15 ppb")
)

# Plot grouped bar chart with ordered categories
ggplot(lead_categories, aes(x = category, y = n, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Lead Level Categories: MDEQ vs Virginia Tech",
    subtitle = "EPA action level = 15 ppb",
    x = "Lead Category",
    y = "Number of Samples"
  ) +
  scale_fill_manual(values = c("MDEQ" = "firebrick", "Virginia Tech" = "dodgerblue3")) +
  theme_bw()
