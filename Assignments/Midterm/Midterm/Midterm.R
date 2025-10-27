# Set-up
## Packages ----
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(countrycode)
library(rnaturalearth)
library(sf)



## Data ----
neo <- read.csv("~/Library/CloudStorage/GoogleDrive-tellorin@usc.edu/.shortcut-targets-by-id/10yI1Vp2x44iBX7T-_NfWNeL7go8kwnUH/2. College - USC/1. Degree/1. Courses/Y4 Senior/Fall 2025/PM 566/PM566-Repository/Assignments/Midterm/Midterm/data/neo.csv")


## Other Environment ----
names(neo) <- c("indicator", "year", "abbrev", "country", "global", "region",
                "income", "age", "cause", "source_short",
                "source_long", "value_num", "value_string")

# Clean region names
neo <- mutate(neo, region = tolower(trimws(region)))

# Classify data type
neo <- mutate(neo,
              data_type = case_when(
                region == "global" ~ "global",
                !is.na(country) & country != "" ~ "country",
                TRUE ~ "region"
              ))

table(neo$data_type)
# neat version:
as.data.frame(table(neo$data_type)) %>%
  kable(col.names = c("Data Type", "Count"))



# Cat for cause type
unique(neo$cause)
# neater view:
data.frame(Cause = sort(unique(neo$cause))) %>%
  kable()

infectious <- c(
  "HIV/AIDS", "Diarrhoeal diseases", "Tetanus", "Measles",
  "Meningitis/encephalitis", "Malaria", "Acute lower respiratory infections",
  "Sepsis and other infectious conditions of the newborn", "Tuberculosis")

neo <- neo %>%
  mutate(cause_type = case_when(
    cause %in% infectious ~ "infectious",
    cause == "Other Group 1 and Other noncommunicable (neonatal and under-5 only)" ~ "mixed",
    TRUE ~ "non-infectious"))


# Split datasets
country <- filter(neo, data_type == "country")
region  <- filter(neo, data_type == "region")
global  <- filter(neo, data_type == "global")

data.frame(
  Dataset = c("Country", "Region", "Global"),
  Observations = c(nrow(country), nrow(region), nrow(global))) %>%
  kable()


# Confirm sizes
nrow(country)
nrow(region)
nrow(global)

dim(country)
dim(region)
dim(global)


data.frame(
  Dataset = c("Country", "Region", "Global"),
  Rows = c(dim(country)[1], dim(region)[1], dim(global)[1]),
  Columns = c(dim(country)[2], dim(region)[2], dim(global)[2])
) %>%
  kable()



# Country-level preview
head(country) %>%
  kable(caption = "First 6 Rows of Country-Level Data")
tail(country) %>%
  kable(caption = "Last 6 Rows of Country-Level Data")

# Regional-level preview
head(region) %>%
  kable(caption = "First 6 Rows of Regional-Level Data")
tail(region) %>%
  kable(caption = "Last 6 Rows of Regional-Level Data")

# Global-level preview
head(global) %>%
  kable(caption = "First 6 Rows of Global-Level Data")
tail(global) %>%
  kable(caption = "Last 6 Rows of Global-Level Data")


str(country)
str(region)
str(global)


# Environment Cont'd ----
# top 5 causes (x) ----
top_causes <- country %>%
  group_by(cause) %>%
  summarise(total = sum(value_num, na.rm = TRUE)) %>%
  slice_max(order_by = total, n = 5) %>%
  pull(cause)

# capitalize WHO region names (x) ----
neo <- neo %>% mutate(region = stringr::str_to_title(region))

# top 10 ID deaths (x) ----
top_10_countries <- country %>%
  filter(cause_type == "infectious") %>%
  group_by(country) %>%
  summarise(total = sum(value_num, na.rm = TRUE)) %>%
  slice_max(order_by = total, n = 10) %>%
  pull(country)

# Function for map data for given year ----
get_map_data <- function(year) {
  deaths <- country %>%
    filter(cause_type == "infectious", year == !!year) %>%
    group_by(country) %>%
    summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  left_join(world, deaths, by = c("iso_a3" = "iso3"))
}

# Function to create map
make_map <- function(map_data, year_label) {
  ggplot(map_data) +
    geom_sf(aes(fill = total_deaths), color = "white", size = 0.1) +
    scale_fill_gradient(low = "lavender", high = "red", na.value = "gray90", labels = scales::comma) +
    labs(title = paste("Infectious Neonatal Deaths in", year_label), fill = "Total Deaths") +
    theme_bw()
}


# top infections by country
top_infectious <- country %>%
  filter(cause_type == "infectious") %>%
  group_by(cause) %>%
  summarise(total = sum(value_num, na.rm = TRUE)) %>%
  slice_max(order_by = total, n = 5) %>%
  pull(cause)


# Rank causes by total deaths in 2000 and 2020
ranks_2000 <- country %>%
  filter(cause_type == "infectious", year == 2000) %>%
  group_by(cause) %>%
  summarise(deaths_2000 = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(rank_2000 = rank(-deaths_2000))

ranks_2020 <- country %>%
  filter(cause_type == "infectious", year == 2020) %>%
  group_by(cause) %>%
  summarise(deaths_2020 = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(rank_2020 = rank(-deaths_2020))

# Combine and filter for causes present in both years
slope_data <- inner_join(ranks_2000, ranks_2020, by = "cause") %>%
  select(cause, rank_2000, rank_2020) %>%
  pivot_longer(cols = starts_with("rank"), names_to = "year", values_to = "rank") %>%
  mutate(year = ifelse(year == "rank_2000", 2000, 2020))

# Create label positions
label_data <- slope_data %>%
  filter(year %in% c(2000, 2020)) %>%
  mutate(hjust = ifelse(year == 2000, 1.1, -0.1))

# Label only 2020
label_data <- slope_data %>%
  filter(year == 2020)


# Identify top 7 infectious causes by total deaths
top7 <- country %>%
  filter(cause_type == "infectious") %>%
  group_by(cause) %>%
  summarise(total = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total, n = 7) %>%
  pull(cause)

custom_colors <- c(
  "Acute lower respiratory infections" = "red2",
  "Diarrhoeal diseases" = "yellow3",
  "Malaria" = "orange1",
  "HIV/AIDS" = "green3",
  "Tuberculosis" = "dodgerblue2",
  "Measles" = "purple3",
  "Meningitis/encephalitis" = "black"
)









# EDA 2 & 3 ----
str(country)
glimpse(country)

str(region)
glimpse(region)

str(global)
glimpse(global)


data.frame(
  Dataset = c("Country-level", "Regional-level", "Global-level"),
  Min_Year = c(min(country$year), min(region$year), min(global$year)),
  Max_Year = c(max(country$year), max(region$year), max(global$year))
) %>%
  kable(caption = "Year Range for Each Dataset")

# Country-level histogram
ggplot(country, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue3", color = "black") +
  labs(title = "Country-Level: Observations per Year", x = "Year", y = "Count") +
  theme_bw()

# Regional-level histogram
ggplot(region, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue3", color = "black") +
  labs(title = "Regional-Level: Observations per Year", x = "Year", y = "Count") +
  theme_bw()

# Global-level histogram
ggplot(global, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue3", color = "black") +
  labs(title = "Global-Level: Observations per Year", x = "Year", y = "Count") +
  theme_bw()


data.frame(
  Dataset = c("Country-level", "Regional-level", "Global-level"),
  Min_Value = c(min(country$value_num, na.rm = TRUE),
                min(region$value_num, na.rm = TRUE),
                min(global$value_num, na.rm = TRUE)),
  Max_Value = c(max(country$value_num, na.rm = TRUE),
                max(region$value_num, na.rm = TRUE),
                max(global$value_num, na.rm = TRUE))) %>%
  kable(caption = "Range of value_num for Each Dataset")





# EDA 4 ----
head(country, 5)
tail(country, 5)

head(region, 5)
tail(region, 5)

head(global, 5)
tail(global, 5)

# Cleaning ----
colSums(is.na(neo))

unique(neo$cause)

data.frame(
  Dataset = c("Country-level", "Regional-level", "Global-level"),
  Unique_Entities = c(n_distinct(country$country),
                      n_distinct(region$region),
                      nrow(global))) %>%
  kable(caption = "Entity Counts Across Datasets")

unique(country$country)


data.frame(
  Dataset = c("Country-level", "Regional-level", "Global-level"),
  NA_Count = c(sum(is.na(country)),
               sum(is.na(region)),
               sum(is.na(global)))
) %>%
  kable(caption = "Total NA Values in Each Dataset")


# EDA 5 ----
## total deaths over time (global) ----
country %>%
  group_by(year) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_deaths)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line(color = "dodgerblue4", size = 1.2) +
  labs(title = "Total Neonatal Deaths Over Time",
       x = "Year", y = "Total Deaths") +
  theme_minimal()


## deaths over time by cause type ----
country %>%
  group_by(year, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_deaths, color = cause_type)) +
  scale_y_continuous(labels = scales::comma) +
  geom_line(size = 1.2) +
  labs(title = "Neonatal Deaths Over Time by Cause Type",
       x = "Year", y = "Total Deaths", color = "Cause Type") +
  theme_minimal()

country %>%
  filter(year %in% c(2000, 2021)) %>%
  group_by(year, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = total_deaths, fill = cause_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Neonatal Deaths by Cause Type in 2000 vs 2021",
       x = "Year", y = "Total Deaths", fill = "Cause Type") +
  theme_bw()




## top 5 causes over time ----
top_causes <- country %>%
  group_by(cause) %>%
  summarise(total = sum(value_num, na.rm = TRUE)) %>%
  slice_max(order_by = total, n = 5) %>%
  pull(cause)

### line chart ----
country %>%
  filter(cause %in% top_causes) %>%
  group_by(year, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_deaths, color = cause)) +
  geom_line(size = 1.2) +
  scale_y_continuous(breaks = seq(0, 2500000, by = 250000), labels = scales::comma) +
  labs(title = "Top 5 Neonatal Causes Over Time",
       x = "Year", y = "Total Deaths", color = "Cause") +
  theme_minimal()

### stacked line chart ----
country %>%
  filter(cause %in% top_causes) %>%
  group_by(year, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = total_deaths, fill = cause)) +
  geom_area(position = "stack", alpha = 0.8) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 5 Neonatal Causes Over Time (Stacked)",
       x = "Year", y = "Total Deaths", fill = "Cause") +
  theme_minimal()


# ID vs NonID deaths by region (all time) ----
country %>%
  group_by(region, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  group_by(region) %>%
  mutate(prop = total_deaths / sum(total_deaths)) %>%
  ungroup() %>%
  mutate(region = str_to_title(region)) %>%
  ggplot(aes(x = region, y = prop, fill = cause_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Neonatal Deaths by Cause Type and Region",
       x = "WHO Region", y = "Proportion", fill = "Cause Type") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  scale_fill_manual(values = c("infectious" = "dodgerblue4", "non-infectious" = "red3", "mixed" = "yellow3"))



# regional trends over time by cause type ----
country %>%
  group_by(region, year, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = str_to_title(region)) %>%
  ggplot(aes(x = year, y = total_deaths, color = cause_type)) +
  geom_line(size = 1.1) +
  facet_wrap(~region, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("infectious" = "dodgerblue4", "non-infectious" = "red3", "mixed" = "yellow3")) +
  labs(
    title = "Neonatal Deaths Over Time by Region and Cause Type",
    x = "Year", y = "Total Deaths", color = "Cause Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9))

country %>%
  filter(year %in% c(2000, 2021)) %>%
  group_by(region, year, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = str_to_title(region)) %>%
  pivot_wider(names_from = year, values_from = total_deaths) %>%
  mutate(
    `2000` = scales::comma(`2000`),
    `2021` = scales::comma(`2021`)
  ) %>%
  arrange(region, cause_type) %>%
  kable(caption = "Regional Neonatal Deaths by Cause Type: 2000 vs 2021")




# Neonatal Deaths Over Time by Income Group and Cause Type ----
country %>%
  filter(income != "") %>%
  group_by(income, year, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total_deaths, color = cause_type)) +
  geom_line(size = 1.1) +
  facet_wrap(~income, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = c("infectious" = "dodgerblue4", "non-infectious" = "red3", "mixed" = "yellow3")) +
  labs(
    title = "Neonatal Deaths Over Time by Income Group and Cause Type",
    x = "Year", y = "Total Deaths", color = "Cause Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9))


# Proportion of Neonatal Deaths by Cause Type and Income Group ----
country %>%
  filter(income != "") %>%
  group_by(income, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(income) %>%
  mutate(prop = total_deaths / sum(total_deaths)) %>%
  ggplot(aes(x = income, y = prop, fill = cause_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("infectious" = "dodgerblue4", "non-infectious" = "red3", "mixed" = "yellow3")) +
  labs(
    title = "Proportion of Neonatal Deaths by Cause Type and Income Group",
    x = "Income Group", y = "Proportion", fill = "Cause Type"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10))



# barchart: top 10 countries w/ neonatal deaths overall ----
country %>%
  filter(year %in% c(2000, 2021)) %>%
  group_by(country, year) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_deaths, n = 10) %>%
  ggplot(aes(x = total_deaths, y = reorder(country, total_deaths), fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2000" = "dodgerblue3", "2021" = "red3")) +
  labs(x = "Total Deaths", y = "Country", fill = "Year") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10))


# barchart: top 10 countries w/ non-ID deaths ----
country %>%
  filter(cause_type == "non-infectious", year %in% c(2000, 2021)) %>%
  group_by(country, year) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_deaths, n = 10) %>%
  ggplot(aes(x = total_deaths, y = reorder(country, total_deaths), fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2000" = "dodgerblue3", "2021" = "red3")) +
  labs(x = "Total Deaths", y = "Country", fill = "Year") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10))


# barchart: top 10 countries w/ ID neonatal deaths ----
country %>%
  filter(cause_type == "infectious", year %in% c(2000, 2021)) %>%
  group_by(country, year) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  slice_max(order_by = total_deaths, n = 10) %>%
  ggplot(aes(x = total_deaths, y = reorder(country, total_deaths), fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("2000" = "dodgerblue3", "2021" = "red3")) +
  labs(x = "Total Deaths", y = "Country", fill = "Year") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 10))


# Total neonatal deaths by country by year ----
get_map_data_total <- function(year) {
  deaths <- country %>%
    filter(year == !!year) %>%
    group_by(country) %>%
    summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  left_join(world, deaths, by = c("iso_a3" = "iso3"))
}

make_map_total <- function(map_data, year_label) {
  ggplot(map_data) +
    geom_sf(aes(fill = total_deaths), color = "white", size = 0.1) +
    scale_fill_gradient(low = "lavender", high = "red", na.value = "gray90", labels = scales::comma) +
    labs(title = paste("Total Neonatal Deaths in", year_label), fill = "Total Deaths") +
    theme_bw()
}

make_map_total(get_map_data_total(2000), 2000)
make_map_total(get_map_data_total(2010), 2010)
make_map_total(get_map_data_total(2020), 2020)


# Non-ID neonatal deaths by country by year ----
get_map_data_nonid <- function(year) {
  deaths <- country %>%
    filter(cause_type == "non-infectious", year == !!year) %>%
    group_by(country) %>%
    summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  left_join(world, deaths, by = c("iso_a3" = "iso3"))
}

make_map_nonid <- function(map_data, year_label) {
  ggplot(map_data) +
    geom_sf(aes(fill = total_deaths), color = "white", size = 0.1) +
    scale_fill_gradient(low = "lavender", high = "red", na.value = "gray90", labels = scales::comma) +
    labs(title = paste("Non-Infectious Neonatal Deaths in", year_label), fill = "Total Deaths") +
    theme_bw()
}

make_map_nonid(get_map_data_nonid(2000), 2000)
make_map_nonid(get_map_data_nonid(2010), 2010)
make_map_nonid(get_map_data_nonid(2020), 2020)


# ID neonatal deaths by country by year ----
get_map_data <- function(year) {
  deaths <- country %>%
    filter(cause_type == "infectious", year == !!year) %>%
    group_by(country) %>%
    summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
    mutate(iso3 = countrycode(country, "country.name", "iso3c"))
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  left_join(world, deaths, by = c("iso_a3" = "iso3"))
}

# Function to create map
make_map <- function(map_data, year_label) {
  ggplot(map_data) +
    geom_sf(aes(fill = total_deaths), color = "white", size = 0.1) +
    scale_fill_gradient(low = "lavender", high = "red", na.value = "gray90", labels = scales::comma) +
    labs(title = paste("Infectious Neonatal Deaths in", year_label), fill = "Total Deaths") +
    theme_bw()
}

make_map(get_map_data(2000), 2000)
make_map(get_map_data(2010), 2010)
make_map(get_map_data(2020), 2020)


# Top 5 Non-Infectious Neonatal Causes by Region ----
top_noninfectious <- country %>%
  filter(cause_type == "non-infectious") %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_deaths, n = 5) %>%
  pull(cause)

country %>%
  filter(cause %in% top_noninfectious, cause_type == "non-infectious") %>%
  mutate(region = str_to_title(region)) %>%
  group_by(region, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths), fill = cause)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~region, scales = "free_x", ncol=2) +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Total Deaths", y = "Cause") +
  theme_bw()


# Top 5 Infectious Neonatal Causes by Region ----
top_infectious <- country %>%
  filter(cause_type == "infectious") %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_deaths, n = 5) %>%
  pull(cause)

country %>%
  filter(cause %in% top_infectious, cause_type == "infectious") %>%
  mutate(region = stringr::str_to_title(region)) %>%
  group_by(region, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths), fill = cause)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~region, scales = "free_x", ncol = 2) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Top 5 Infectious Neonatal Causes by Region",
    x = "Total Deaths", y = "Cause"
  ) +
  theme_bw()



# Overall Infectious vs Non-Infectious Neonatal Deaths by Region ----
country %>%
  filter(cause_type %in% c("infectious", "non-infectious")) %>%
  group_by(region, cause_type) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = total_deaths, y = reorder(region, total_deaths), fill = cause_type)) +
  geom_col(position = "dodge") +
  scale_x_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("infectious" = "dodgerblue4", "non-infectious" = "red3")) +
  labs(
    title = "Infectious vs Non-Infectious Neonatal Deaths by Region",
    x = "Total Deaths", y = "WHO Region", fill = "Cause Type"
  ) +
  theme_bw()


# ID by cause and income group ----
country %>%
  filter(cause_type == "infectious", income != "") %>%
  group_by(income, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths), fill = income)) +
  geom_col() +
  facet_wrap(~income, scales = "free_x") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Infectious Neonatal Deaths by Cause and Income Group",
    x = "Total Deaths", y = "Cause", fill = "Income Group"
  ) +
  theme_bw()


# ID by cause and region ----
country %>%
  filter(cause_type == "infectious", region != "") %>%
  group_by(region, cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths), fill = region)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~region, scales = "free_x") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Infectious Neonatal Deaths by Cause and Region",
    x = "Total Deaths", y = "Cause"
  ) +
  theme_bw()


# ID by cause and year ----
country %>%
  filter(cause_type == "infectious") %>%
  group_by(cause, year) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = reorder(cause, -total_deaths), fill = total_deaths)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lavender", high = "red2", labels = scales::comma) +
  labs(
    title = "Heatmap of Infectious Neonatal Deaths by Cause and Year",
    x = "Year", y = "Cause", fill = "Total Deaths"
  ) +
  theme_bw()


# Rank top country by WHO region ----
top_total <- country %>%
  filter(year == 2021, !is.na(region), !is.na(country)) %>%
  group_by(region, country) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  group_by(region) %>%
  slice_max(order_by = total_deaths, n = 1)

breakdown <- country %>%
  filter(year == 2021, !is.na(region), !is.na(country)) %>%
  semi_join(top_total, by = c("region", "country")) %>%
  group_by(region, country, cause_type) %>%
  summarise(deaths = sum(value_num, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = cause_type, values_from = deaths, values_fill = 0)

final_table <- top_total %>%
  left_join(breakdown, by = c("region", "country")) %>%
  mutate(region = str_to_title(region)) %>%
  left_join(
    country %>% select(country, income) %>% distinct(),
    by = "country"
  ) %>%
  select(
    Region = region,
    Country = country,
    Income = income,
    `Total Neonatal Deaths` = total_deaths,
    `Infectious Deaths` = infectious,
    `Non-Infectious Deaths` = `non-infectious`
  ) %>%
  arrange(desc(`Total Neonatal Deaths`))

kable(final_table, align = "l")




# Top country by region: NON-infectious deaths/causes ----
## Nigera
nigeria <- country %>% filter(country == "Nigeria", cause_type == "non-infectious")
nigeria %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "darkgreen") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in Nigeria", x = "Total Deaths", y = "Cause") +
  theme_bw()

## India
india <- country %>% filter(country == "India", cause_type == "non-infectious")
india %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "darkorange") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in India", x = "Total Deaths", y = "Cause") +
  theme_bw()

## Pakistan
pakistan <- country %>% filter(country == "Pakistan", cause_type == "non-infectious")
pakistan %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "forestgreen") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in Pakistan", x = "Total Deaths", y = "Cause") +
  theme_bw()

## Indonesia
indonesia <- country %>% filter(country == "Indonesia", cause_type == "non-infectious")
indonesia %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "firebrick") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in Indonesia", x = "Total Deaths", y = "Cause") +
  theme_bw()


## Haiti
haiti <- country %>% filter(country == "Haiti", cause_type == "non-infectious")
haiti %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in Haiti", x = "Total Deaths", y = "Cause") +
  theme_bw()

## Tajikistan
tajikistan <- country %>% filter(country == "Tajikistan", cause_type == "non-infectious")
tajikistan %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "tomato") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Non-Infectious Neonatal Causes in Tajikistan", x = "Total Deaths", y = "Cause") +
  theme_bw()




# Top country by region: Infectious deaths/causes ----
## Nigeria
nigeria <- country %>% filter(country == "Nigeria", cause_type == "infectious")
nigeria %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "green4") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in Nigeria", x = "Total Deaths", y = "Cause") +
  theme_bw()

## India
india <- country %>% filter(country == "India", cause_type == "infectious")
india %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "orange2") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in India", x = "Total Deaths", y = "Cause") +
  theme_bw()

## Pakistan
pakistan <- country %>% filter(country == "Pakistan", cause_type == "infectious")
pakistan %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "green4") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in Pakistan", x = "Total Deaths", y = "Cause") +
  theme_bw()

# Indonesia
indonesia <- country %>% filter(country == "Indonesia", cause_type == "infectious")
indonesia %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "red2") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in Indonesia", x = "Total Deaths", y = "Cause") +
  theme_bw()

# Haiti
haiti <- country %>% filter(country == "Haiti", cause_type == "infectious")
haiti %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "dodgerblue4") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in Haiti", x = "Total Deaths", y = "Cause") +
  theme_bw()

# Tajikistan
tajikistan <- country %>% filter(country == "Tajikistan", cause_type == "infectious")
tajikistan %>%
  group_by(cause) %>%
  summarise(total_deaths = sum(value_num, na.rm = TRUE)) %>%
  arrange(desc(total_deaths)) %>%
  slice_head(n = 5) %>%
  ggplot(aes(x = total_deaths, y = reorder(cause, total_deaths))) +
  geom_col(fill = "red1") +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Top 5 Infectious Neonatal Causes in Tajikistan", x = "Total Deaths", y = "Cause") +
  theme_bw()



