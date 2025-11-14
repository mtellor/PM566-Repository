# Setup ----
# Load required libraries
library(tidyverse)
library(plotly)
library(zoo)
library(tidyr)

# Set ggplot theme
theme_set(theme_bw())


# Q1 ----
# COVID state-level data from NYT
cv_states <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

# State population data
state_pops <- read.csv("https://raw.githubusercontent.com/COVID19Tracking/associated-data/master/us_census_data/us_census_2018_population_estimates_states.csv")

# Column names
state_pops$abb <- state_pops$state
state_pops$state <- state_pops$state_name
state_pops$state_name <- NULL

# Merge datasets
cv_states <- merge(cv_states, state_pops, by = "state")


# Q2 ----
dim(cv_states)
head(cv_states)
tail(cv_states)
str(cv_states)

# Q3 ----
# Format the date
cv_states$date <- as.Date(cv_states$date, format="%Y-%m-%d")

# Format the state and state abbreviation (abb) variables
state_list <- unique(cv_states$state)
cv_states$state <- factor(cv_states$state, levels = state_list)
abb_list <- unique(cv_states$abb)
cv_states$abb <- factor(cv_states$abb, levels = abb_list)

# Order the data first by state, second by date
cv_states <- cv_states[order(cv_states$state, cv_states$date),]

# Confirm the variables are now correctly formatted
str(cv_states)
head(cv_states)
tail(cv_states)

# Inspect the range values for each variable
head(cv_states)
summary(cv_states)
min(cv_states$date)
max(cv_states$date)

# Q4 ----
# Add variables for new_cases and new_deaths:
for (i in 1:length(state_list)) {
  cv_subset <- subset(cv_states, state == state_list[i])
  cv_subset <- cv_subset[order(cv_subset$date),]
  
  # add starting level for new cases and deaths
  cv_subset$new_cases <- cv_subset$cases[1]
  cv_subset$new_deaths <- cv_subset$deaths[1]
  
  for (j in 2:nrow(cv_subset)) {
    cv_subset$new_cases[j] <- cv_subset$cases[j] - cv_subset$cases[j-1]
    cv_subset$new_deaths[j] <- cv_subset$deaths[j] - cv_subset$deaths[j-1]
  }
  
  # include in main dataset
  cv_states$new_cases[cv_states$state==state_list[i]] <- cv_subset$new_cases
  cv_states$new_deaths[cv_states$state==state_list[i]] <- cv_subset$new_deaths
}

# Focus on recent dates
cv_states <- cv_states |> dplyr::filter(date >= "2021-06-01")

# Inspect outliers in new_cases using plotly
p1 <- ggplot(cv_states, aes(x = date, y = new_cases, color = state)) + geom_point(size = .5, alpha = 0.5)
ggplotly(p1)

p2 <- ggplot(cv_states, aes(x = date, y = new_deaths, color = state)) + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)

# Set negative new case or death counts to 0
cv_states$new_cases[cv_states$new_cases < 0] <- 0
cv_states$new_deaths[cv_states$new_deaths < 0] <- 0

# Re-calculate cases and deaths as cumulative sum of updated new_cases and new_deaths
for (i in 1:length(state_list)) {
  cv_subset <- subset(cv_states, state == state_list[i])
  
  # add starting level for new cases and deaths
  cv_subset$cases <- cv_subset$cases[1]
  cv_subset$deaths <- cv_subset$deaths[1]
  
  for (j in 2:nrow(cv_subset)) {
    cv_subset$cases[j] <- cv_subset$new_cases[j] + cv_subset$cases[j-1]
    cv_subset$deaths[j] <- cv_subset$new_deaths[j] + cv_subset$deaths[j-1]
  }
  
  # include in main dataset
  cv_states$cases[cv_states$state==state_list[i]] <- cv_subset$cases
  cv_states$deaths[cv_states$state==state_list[i]] <- cv_subset$deaths
}

# Smooth new counts
cv_states$new_cases <- zoo::rollmean(cv_states$new_cases, k=7, fill=NA, align='right') |> round(digits = 0)
cv_states$new_deaths <- zoo::rollmean(cv_states$new_deaths, k=7, fill=NA, align='right') |> round(digits = 0)

# Inspect data again interactively
p2 <- ggplot(cv_states, aes(x = date, y = new_deaths, color = state)) + geom_line() + geom_point(size = .5, alpha = 0.5)
ggplotly(p2)


# Q5 ----
# Add population normalized (by 100,000) counts for each variable
cv_states$per100k <- as.numeric(format(round(cv_states$cases/(cv_states$population/100000),1), nsmall=1))
cv_states$newper100k <- as.numeric(format(round(cv_states$new_cases/(cv_states$population/100000),1), nsmall=1))
cv_states$deathsper100k <- as.numeric(format(round(cv_states$deaths/(cv_states$population/100000),1), nsmall=1))
cv_states$newdeathsper100k <- as.numeric(format(round(cv_states$new_deaths/(cv_states$population/100000),1), nsmall=1))

# Add a naive_CFR variable = deaths / cases
cv_states <- cv_states |> mutate(naive_CFR = round((deaths * 100 / cases), 2))

# Create a `cv_states_today` variable
cv_states_today <- subset(cv_states, date == max(cv_states$date))

# Q6 ----
# pop_density vs. cases
cv_states_today |> 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# Filter out "District of Columbia"
cv_states_today_filter <- cv_states_today |> filter(state != "District of Columbia")

# pop_density vs. cases after filtering
cv_states_today_filter |> 
  plot_ly(x = ~pop_density, y = ~cases, 
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# pop_density vs. deathsper100k
cv_states_today_filter |> 
  plot_ly(x = ~pop_density, y = ~deathsper100k,
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5))

# Adding hoverinfo
cv_states_today_filter |> 
  plot_ly(x = ~pop_density, y = ~deathsper100k,
          type = 'scatter', mode = 'markers', color = ~state,
          size = ~population, sizes = c(5, 70), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste(
            paste(state, ":", sep=""),
            paste(" Cases per 100k: ", per100k, sep=""),
            paste(" Deaths per 100k: ", deathsper100k, sep=""),
            sep = "<br>")
  ) |>
  layout(
    title = "Population-normalized COVID-19 deaths (per 100k) vs. population density for US states",
    yaxis = list(title = "Deaths per 100k"),
    xaxis = list(title = "Population Density"),
    hovermode = "compare"
  )


# Q7 ----
# Scatterplot with trend line
p <- ggplot(cv_states_today_filter, aes(x = pop_density, y = deathsper100k, size = population)) +
  geom_point() +
  geom_smooth()

ggplotly(p)


# Q8 ----
# Line chart for naive_CFR for all states over time using `plot_ly()`
plot_ly(cv_states, x = ~date, y = ~naive_CFR, color = ~state, type = "scatter", mode = "lines")

# Line chart for Florida showing new_cases and new_deaths together (two lines)
cv_states |> filter(state == "Florida") |> 
  plot_ly(x = ~date, y = ~new_cases, type = "scatter", mode = "lines", name = "New Cases") |> 
  add_lines(x = ~date, y = ~new_deaths, type = "scatter", mode = "lines", name = "New Deaths")


# Q9 ----
# Map state, date, and new_cases to a matrix
library(tidyr)
cv_states_mat <- cv_states |> select(state, date, new_cases) |> dplyr::filter(date > as.Date("2021-06-15"))
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = new_cases))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)

# Create a heatmap using plot_ly()
plot_ly(x = colnames(cv_states_mat2), y = rownames(cv_states_mat2),
        z = ~cv_states_mat2,
        type = "heatmap",
        showscale = TRUE)

# Repeat with newper100k
cv_states_mat <- cv_states |> select(state, date, newper100k) |> dplyr::filter(date > as.Date("2021-06-15"))
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = newper100k))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)

plot_ly(x = colnames(cv_states_mat2), y = rownames(cv_states_mat2),
        z = ~cv_states_mat2,
        type = "heatmap",
        showscale = TRUE)

# Create a second heatmap after filtering to only include dates every other week
filter_dates <- seq(as.Date("2021-06-15"), as.Date("2021-11-01"), by = 14)

cv_states_mat <- cv_states |> select(state, date, newper100k) |> filter(date %in% filter_dates)
cv_states_mat2 <- as.data.frame(pivot_wider(cv_states_mat, names_from = state, values_from = newper100k))
rownames(cv_states_mat2) <- cv_states_mat2$date
cv_states_mat2$date <- NULL
cv_states_mat2 <- as.matrix(cv_states_mat2)

# Create a heatmap using plot_ly()
plot_ly(x = colnames(cv_states_mat2), y = rownames(cv_states_mat2),
        z = ~cv_states_mat2,
        type = "heatmap",
        showscale = TRUE)


# Q 10 ----
### For specified date
pick.date <- "2021-10-15"

# Extract the data for each state by its abbreviation
cv_per100 <- cv_states |> filter(date==pick.date) |> select(state, abb, newper100k, cases, deaths) # select data
cv_per100$state_name <- cv_per100$state
cv_per100$state <- cv_per100$abb
cv_per100$abb <- NULL

# Create hover text
cv_per100$hover <- with(cv_per100, paste(state_name, '<br>', "Cases per 100k: ", newper100k, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Make sure both maps are on the same color scale
shadeLimit <- 125

# Create the map
fig <- plot_geo(cv_per100, locationmode = 'USA-states') |> 
  add_trace(
    z = ~newper100k, text = ~hover, locations = ~state,
    color = ~newper100k, colors = 'Purples'
  )
fig <- fig |> colorbar(title = paste0("Cases per 100k: ", pick.date), limits = c(0,shadeLimit))
fig <- fig |> layout(
  title = paste('Cases per 100k by State as of ', pick.date, '<br>(Hover for value)'),
  geo = set_map_details
)
fig_pick.date <- fig

### Map for today's date

# Extract the data for each state by its abbreviation
cv_per100 <- cv_states_today |>  select(state, abb, newper100k, cases, deaths) # select data
cv_per100$state_name <- cv_per100$state
cv_per100$state <- cv_per100$abb
cv_per100$abb <- NULL

# Create hover text
cv_per100$hover <- with(cv_per100, paste(state_name, '<br>', "Cases per 100k: ", newper100k, '<br>', "Cases: ", cases, '<br>', "Deaths: ", deaths))

# Set up mapping details
set_map_details <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# Create the map
fig <- plot_geo(cv_per100, locationmode = 'USA-states') |> 
  add_trace(
    z = ~newper100k, text = ~hover, locations = ~state,
    color = ~newper100k, colors = 'Purples'
  )
fig <- fig |> colorbar(title = paste0("Cases per 100k: ", Sys.Date()), limits = c(0,shadeLimit))
fig <- fig |> layout(
  title = paste('Cases per 100k by State as of', Sys.Date(), '<br>(Hover for value)'),
  geo = set_map_details
)
fig_Today <- fig

### Plot together 
subplot(fig_pick.date, fig_Today, nrows = 2, margin = 0.05)
