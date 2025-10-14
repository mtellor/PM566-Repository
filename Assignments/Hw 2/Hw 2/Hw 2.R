# Set-up ----
## Packages ----
library(nycflights13)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)

## Data sets ----
airlines <- airlines
airports <- airports
flights <- flights
planes <- planes
weather <- weather

## Other things ----
top_destinations <- flights

hh_mm <- function(time) {
  ifelse(is.na(time), NA, floor(time / 100))
}

flights <- flights %>%
  mutate(
    dep_hour = hh_mm(dep_time),
    arr_hour = hh_mm(arr_time),
    
    dep_period = case_when(
      dep_hour >= 0 & dep_hour < 6 ~ "early morning",
      dep_hour >= 6 & dep_hour < 12 ~ "morning",
      dep_hour >= 12 & dep_hour < 18 ~ "afternoon",
      dep_hour >= 18 & dep_hour <= 23 ~ "evening",
      TRUE ~ NA_character_
    ),
    
    arr_period = case_when(
      arr_hour >= 0 & arr_hour < 6 ~ "early morning",
      arr_hour >= 6 & arr_hour < 12 ~ "morning",
      arr_hour >= 12 & arr_hour < 18 ~ "afternoon",
      arr_hour >= 18 & arr_hour <= 23 ~ "evening",
      TRUE ~ NA_character_
    )
  )

dep_counts <- flights %>%
  count(period = dep_period) %>%
  mutate(type = "Departure")

arr_counts <- flights %>%
  count(period = arr_period) %>%
  mutate(type = "Arrival")

combined_counts <- bind_rows(dep_counts, arr_counts)

max_count <- max(combined_counts$n)

red_eye <- flights %>%
  filter(dep_period %in% c("afternoon", "evening"),
         arr_period %in% c("early morning", "morning"))

multi_airlines <- flights %>%
  filter(!is.na(tailnum), !is.na(carrier)) %>%
  distinct(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(n > 1)

num_planes <- nrow(multi_airlines)

planes_airlines <- flights %>%
  filter(tailnum %in% multi_airlines$tailnum) %>%
  distinct(tailnum, carrier) %>%
  arrange(tailnum)

multi_airlines <- flights %>%
  filter(!is.na(tailnum), !is.na(carrier)) %>%
  distinct(tailnum, carrier) %>%
  group_by(tailnum) %>%
  summarise(airlines = paste(sort(unique(carrier)), collapse = ", "),
            num_airlines = n()) %>%
  filter(num_airlines > 1)

flights_weather <- flights %>%
  left_join(weather, by = c("time_hour", "origin"))

hourly_origin_avg <- flights_weather %>%
  group_by(origin, time_hour) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

hourly_origin_sorted <- hourly_origin_avg %>%
  arrange(desc(avg_dep_delay))

avg_delay_dest <- flights_weather %>%
  group_by(dest) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

airport_delays <- airports %>%
  inner_join(avg_delay_dest, by = c("faa" = "dest"))

delay_pal <- colorNumeric(
  palette = colorRampPalette(c("green", "yellow2", "red"))(100),
  domain = airport_delays$avg_dep_delay
)

combo_counts <- multi_airlines %>%
  count(airlines)

weather <- weather %>%
  mutate(weather_id = paste(year, month, day, hour, origin, sep = "_"))

flights_weather <- flights %>%
  left_join(weather, by = c("time_hour", "origin"))

weather <- weather %>%
  mutate(weather_id = paste(year, month, day, hour, origin, sep = "_"))

daily_avg <- flights_weather %>%
  group_by(year.x, month.x, day.x) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

daily_avg_sorted <- daily_avg %>%
  arrange(desc(avg_dep_delay))

daily_origin_avg <- flights_weather %>%
  group_by(origin, year.x, month.x, day.x) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

daily_origin_sorted <- daily_origin_avg %>%
  arrange(desc(avg_dep_delay))

hourly_origin_avg <- flights_weather %>%
  group_by(origin, time_hour) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

hourly_origin_sorted <- hourly_origin_avg %>%
  arrange(desc(avg_dep_delay))

avg_delay_dest <- flights_weather %>%
  group_by(dest) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

airport_delays <- airports %>%
  inner_join(avg_delay_dest, by = c("faa" = "dest"))


# Question 1 ----

## tibble
top_destinations <- flights |>
  count(dest, sort = TRUE) |>
  top_n(10) |>
  print()

## bar plot
ggplot(top_destinations, aes(x = reorder(dest, n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_text(aes(label = n), vjust = -0.7, size = 3.5) +
  expand_limits(y = max(top_destinations$n) * 1.1) +
  labs(title = "Top 10 Destinations from NYC in 2013",
       x = "Destination Airport",
       y = "Number of Flights") +
  theme_minimal()


# Question 2 ----

## Convert HHMM time to hour
hh_mm <- function(time) {
  ifelse(is.na(time), NA, floor(time / 100))
}

## New categorical variables
flights <- flights %>%
  mutate(
    dep_hour = hh_mm(dep_time),
    arr_hour = hh_mm(arr_time),
    
    dep_period = case_when(
      dep_hour >= 0 & dep_hour < 6 ~ "early morning",
      dep_hour >= 6 & dep_hour < 12 ~ "morning",
      dep_hour >= 12 & dep_hour < 18 ~ "afternoon",
      dep_hour >= 18 & dep_hour <= 23 ~ "evening",
      TRUE ~ NA_character_
    ),
    
    arr_period = case_when(
      arr_hour >= 0 & arr_hour < 6 ~ "early morning",
      arr_hour >= 6 & arr_hour < 12 ~ "morning",
      arr_hour >= 12 & arr_hour < 18 ~ "afternoon",
      arr_hour >= 18 & arr_hour <= 23 ~ "evening",
      TRUE ~ NA_character_
    )
  )



## Count flights by dep_period and arr_period
dep_counts <- flights %>%
  count(period = dep_period) %>%
  mutate(type = "Departure")

arr_counts <- flights %>%
  count(period = arr_period) %>%
  mutate(type = "Arrival")

## Combine both into one data frame
combined_counts <- bind_rows(dep_counts, arr_counts)

## Find max count for consistent y-axis and label spacing
max_count <- max(combined_counts$n)

## Faceted barplot
ggplot(combined_counts, aes(x = period, y = n)) +
  geom_bar(stat = "identity", aes(fill = type), show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  expand_limits(y = max_count * 1.1) +
  facet_wrap(~ type) +
  scale_fill_manual(values = c("dodgerblue4", "#990000")) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Flights by Time of Day",
       x = "Time Period",
       y = "Number of Flights") +
  theme_minimal() +
  theme(strip.text = element_text(size = 13, face = "bold"))



## Defining red eyes
red_eye <- flights %>%
  filter(dep_period %in% c("afternoon", "evening"),
         arr_period %in% c("early morning", "morning"))

## Percentage of red eyes
red_eye_percent <- nrow(red_eye) / nrow(flights) * 100

print(paste("Percentage of red eye flights:", round(red_eye_percent, 2), "%"))

# Question 3 ----

## Count how many unique airlines each plane flew for
multi_airlines <- flights %>%
  filter(!is.na(tailnum), !is.na(carrier)) %>%
  distinct(tailnum, carrier) %>%
  count(tailnum) %>%
  filter(n > 1)

## How many planes flew for multiple airlines
num_planes <- nrow(multi_airlines)

print(paste("Number of planes that flew for multiple airlines:", num_planes))

## Which airlines did they fly for
planes_airlines <- flights %>%
  filter(tailnum %in% multi_airlines$tailnum) %>%
  distinct(tailnum, carrier) %>%
  arrange(tailnum)

print(planes_airlines, n = 34)


## Identify planes that flew for multiple airlines
multi_airlines <- flights %>%
  filter(!is.na(tailnum), !is.na(carrier)) %>%
  distinct(tailnum, carrier) %>%
  group_by(tailnum) %>%
  summarise(airlines = paste(sort(unique(carrier)), collapse = ", "),
            num_airlines = n()) %>%
  filter(num_airlines > 1)

## Count how many planes flew for each airline combo
combo_counts <- multi_airlines %>%
  count(airlines)

## Plot the airline combinations
ggplot(combo_counts, aes(x = reorder(airlines, -n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue4") +
  geom_text(aes(label = n), vjust = -1, size = 3.5) +
  scale_y_continuous(breaks = pretty_breaks(n = 5)) +
  expand_limits(y = 11) +
  labs(title = "Planes Flying for Multiple Airlines",
       x = "Airlines Sharing Planes",
       y = "Number of Planes") +
  theme_minimal()


# Question 4 ----

weather %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  select(origin, name, time_hour, temp, wind_speed, visib)


# Question 5 ----

weather <- weather %>%
  mutate(weather_id = paste(year, month, day, hour, origin, sep = "_"))

sum(duplicated(weather$weather_id))

flights_weather <- flights %>%
  left_join(weather, by = c("time_hour", "origin"))

flights_weather %>% select(flight, origin, time_hour, temp, wind_speed, visib) %>%
  head()

weather <- weather %>%
  mutate(weather_id = paste(year, month, day, hour, origin, sep = "_"))

# Duplicated entries
weather %>%
  filter(duplicated(weather_id) | duplicated(weather_id, fromLast = TRUE)) %>%
  select(origin, year, month, day, hour, time_hour, temp, wind_speed, visib) %>%
  arrange(origin, year, month, day, hour)

# Question 6 ----

## EDA Checklist
### Step 2
dim(flights_weather)

### Step 3
str(flights_weather)

### Step 4
head(flights_weather)
tail(flights_weather)

### Step 5
# Departure delay
ggplot(flights_weather, aes(x = dep_delay)) +
  geom_histogram(binwidth = 10, fill = "dodgerblue4") +
  geom_vline(xintercept = max(flights_weather$dep_delay, na.rm = TRUE), 
             color = "#990000", linetype = "dashed", size = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Departure Delays", x = "Delay (min)", y = "Count") +
  theme_minimal()

max(flights_weather$dep_delay, na.rm = TRUE)

# Wind speed
ggplot(flights_weather, aes(x = wind_speed)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue4") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Wind Speed", x = "Wind Speed (mph)", y = "Count") +
  theme_minimal()

# Visibility
ggplot(flights_weather, aes(x = visib)) +
  geom_histogram(binwidth = 1, fill = "dodgerblue4") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Distribution of Visibility", x = "Visibility (miles)", y = "Count") +
  theme_minimal()


# Question 7 ----

## Avg daily dep delay
daily_avg <- flights_weather %>%
  group_by(year.x, month.x, day.x) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

### Worst day overall
daily_avg %>% slice(1)

daily_avg_sorted <- daily_avg %>%
  arrange(desc(avg_dep_delay)) %>%
  print(n=1)

### Graph
daily_avg %>%
  ggplot(aes(x = as.Date(paste(year.x, month.x, day.x, sep = "-")), y = avg_dep_delay)) +
  geom_line(color = "dodgerblue4") +
  labs(title = "Average Departure Delay by Day", x = "Date", y = "Avg Delay (min)") +
  theme_minimal()


## Avg dep delay by day and origin
daily_origin_avg <- flights_weather %>%
  group_by(origin, year.x, month.x, day.x) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

### Worst single day at any airport
daily_origin_avg %>% slice(1)

daily_origin_sorted <- daily_origin_avg %>%
  arrange(desc(avg_dep_delay)) %>%
  print(n=1)


### Graph
daily_origin_avg %>%
  mutate(date = as.Date(paste(year.x, month.x, day.x, sep = "-"))) %>%
  ggplot(aes(x = date, y = avg_dep_delay, color = origin)) +
  geom_smooth(se = FALSE, size = 1.2) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks") +
  labs(title = "Smoothed Average Departure Delay by Day and Airport",
       x = "Date",
       y = "Average Delay (min)",
       color = "Airport") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Avg dep delay by hour and origin
hourly_origin_avg <- flights_weather %>%
  group_by(origin, time_hour) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(avg_dep_delay))

### Worst single hour at any airport
hourly_origin_avg %>% slice(1)

hourly_origin_sorted <- hourly_origin_avg %>%
  arrange(desc(avg_dep_delay)) %>%
  print(n=1)

### Graph
hourly_origin_avg %>%
  ggplot(aes(x = time_hour, y = avg_dep_delay)) +
  geom_smooth(se = FALSE, size = 1, color = "dodgerblue4") +
  facet_wrap(~ origin, ncol = 1) +
  labs(title = "Smoothed Average Departure Delay by Hour",
       x = "Scheduled Hour",
       y = "Average Delay (min)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Question 8 ----

# Average departure delay by destination
avg_delay_dest <- flights_weather %>%
  group_by(dest) %>%
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

# Merge with airports data
airport_delays <- airports %>%
  inner_join(avg_delay_dest, by = c("faa" = "dest"))

# Map
delay_pal <- colorNumeric(
  palette = colorRampPalette(c("green", "yellow2", "red"))(100),
  domain = airport_delays$avg_dep_delay
)

leaflet(data = airport_delays) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = 7,
    color = ~delay_pal(avg_dep_delay),
    fillOpacity = 2,
    label = ~paste0(name, "\nAvg Delay: ", round(avg_dep_delay, 1), " min")
  ) %>%
  addLegend(
    position = "bottomright",
    pal = delay_pal,
    values = ~avg_dep_delay,
    title = "Avg Departure Delay (min)"
  )


# Question 9 ----

## Scatterplot: departure delay vs. wind speed
ggplot(flights_weather, aes(x = wind_speed, y = dep_delay)) +
  geom_point(color = "dodgerblue4", alpha = 0.2) +
  labs(title = "Departure Delay vs. Wind Speed",
       x = "Wind Speed (mph)",
       y = "Departure Delay (min)")


## Boxplot: dep delay by rain cat.
flights_weather$precip_bin <- cut(flights_weather$precip,
                                  breaks = c(0, 0.01, 0.1, 0.5, 1, Inf),
                                  labels = c("None", "Light", "Moderate", "Heavy", "Extreme"),
                                  right = FALSE
)

ggplot(flights_weather, aes(x = precip_bin, y = dep_delay)) +
  geom_boxplot(fill = "dodgerblue4", alpha = 0.6) +
  labs(title = "Departure Delay by Precipitation Level",
       x = "Precipitation Category",
       y = "Departure Delay (min)")


## Boxplot: dep delay by visibility cat.
flights_weather$visib_bin <- cut(flights_weather$visib,
                                 breaks = c(0, 1, 5, 10, Inf),
                                 labels = c("Low", "Moderate", "High", "Clear"),
                                 right = FALSE
)

ggplot(flights_weather, aes(x = visib_bin, y = dep_delay)) +
  geom_boxplot(fill = "dodgerblue4", alpha = 0.6) +
  labs(title = "Departure Delay by Visibility Level",
       x = "Visibility Category",
       y = "Departure Delay (min)")


## Scatterplot: dep delays vs. temperature
ggplot(flights_weather, aes(x = temp, y = dep_delay)) +
  geom_point(color = "dodgerblue4", alpha = 0.2) +
  geom_smooth(se = FALSE, color = "red2") +
  labs(title = "Departure Delay vs. Temperature",
       x = "Temperature (°F)",
       y = "Departure Delay (min)")


## Boxplot: dep delay by hour
flights_weather$dep_hour <- as.factor(flights_weather$dep_hour)

ggplot(flights_weather, aes(x = dep_hour, y = dep_delay)) +
  geom_boxplot(fill = "dodgerblue4", alpha = 0.6) +
  labs(title = "Departure Delay by Hour",
       x = "Scheduled Departure Hour",
       y = "Departure Delay (min)")


## Faceted boxplot: departure delay by hour and precipitation level
ggplot(flights_weather, aes(x = dep_hour, y = dep_delay)) +
  geom_boxplot(fill = "dodgerblue4", alpha = 0.6, outlier.size = 0.5) +
  facet_wrap(~ precip_bin, ncol = 3) +
  labs(title = "Departure Delay by Hour and Precipitation Level",
       x = "Scheduled Departure Hour",
       y = "Departure Delay (min)")


## Faceted boxplot: dep delay by hour and visibility level
ggplot(flights_weather, aes(x = dep_hour, y = dep_delay)) +
  geom_boxplot(fill = "dodgerblue4", alpha = 0.6, outlier.size = 0.5) +
  facet_wrap(~ visib_bin, ncol = 3) +
  labs(title = "Departure Delay by Hour and Visibility Level",
       x = "Scheduled Departure Hour",
       y = "Departure Delay (min)")


## Barplot: median delay by visibility and precipitation
delay_summary <- flights_weather %>%
  group_by(visib_bin, precip_bin) %>%
  summarize(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  ungroup()

ggplot(delay_summary, aes(x = visib_bin, y = median_delay, fill = precip_bin)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median Departure Delay by Visibility and Precipitation",
       x = "Visibility Level",
       y = "Median Departure Delay (min)",
       fill = "Precipitation Level") +
  theme_minimal()


## Heatmap: median departure delay by visbility and precipitation
heatmap_data <- flights_weather %>%
  group_by(visib_bin, precip_bin) %>%
  summarize(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  ungroup()

ggplot(heatmap_data, aes(x = precip_bin, y = visib_bin, fill = median_delay)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "black",
    mid = "yellow",
    high = "red",
    midpoint = 30,
    na.value = "white"
  ) +
labs(title = "Median Departure Delay by Visibility and Precipitation",
       x = "Precipitation Level",
       y = "Visibility Level",
       fill = "Median Delay (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Summary stats: median delay by weather variable
# Median delay by precipitation
precip_summary <- flights_weather %>%
  group_by(precip_bin) %>%
  summarize(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  print()

# Median delay by visibility
visib_summary <- flights_weather %>%
  group_by(visib_bin) %>%
  summarize(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  print()

# Median delay by wind speed
flights_weather <- flights_weather %>%
  mutate(wind_bin = case_when(
    wind_speed < 5 ~ "Calm",
    wind_speed < 10 ~ "Moderate",
    wind_speed < 20 ~ "Strong",
    TRUE ~ "Extreme"
  ))

wind_summary <- flights_weather %>%
  group_by(wind_bin) %>%
  summarize(median_delay = median(dep_delay, na.rm = TRUE)) %>%
  print()

# Comparison table
bind_rows(
  precip_summary %>% mutate(variable = "Precipitation", level = precip_bin),
  visib_summary %>% mutate(variable = "Visibility", level = visib_bin),
  wind_summary %>% mutate(variable = "Wind Speed", level = wind_bin)
) %>%
  select(variable, level, median_delay) %>%
  arrange(desc(median_delay))


## Comparison table: multiple weather variables
combo_summary <- flights_weather %>%
  group_by(visib_bin, precip_bin, wind_bin) %>%
  summarize(
    median_delay = median(dep_delay, na.rm = TRUE),
    iqr_delay = IQR(dep_delay, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(median_delay))

combo_summary %>%
  slice_max(median_delay, n = 10) %>%
  arrange(desc(median_delay))







