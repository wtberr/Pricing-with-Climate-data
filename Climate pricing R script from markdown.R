
```{r}
market <- read.csv("C:/Users/will/Downloads/Pricedata_forWill.csv")
rain <- read.csv("C:/Users/will/Downloads/Kenya_Uganda_rain_use.csv")
temp <- read.csv("C:/Users/will/Downloads/Kenya_Uganda_temp_use.csv")

# Convert the date format using as.Date
market$price_date <- as.Date(market$price_date, format="%m/%d/%Y")  # Specify the current format (mm/dd/yyyy)

# Reformat the date to the desired format (YYYYMMDD)
market$price_date <- format(market$price_date, "%Y%m%d")
```

```{r}
library(dplyr)
print(nrow(rain))
print(nrow(temp))

# Filter 'rain' to include only rows with dateRain present in market$price_date
rain_filtered <- rain %>%
  filter(dateRain %in% market$price_date)

# Filter 'rain' to include only rows with dateRain present in market$price_date
temp_filtered <- temp %>%
  filter(dateTemp %in% market$price_date)

print(nrow(rain_filtered))
print(nrow(temp_filtered))
#we can now deal with a smaller, simpler dataframe for rain and temp with only applicable dates
```
```{r}
market$lon <- market$coord_long
market$lat <- market$coord_lat
```

```{r}
#testing
#rain_filtered <- rain_filtered %>%
#filter(precipitation != 0)
```

```{r}
#market <- market %>%
#sample_n(size = 1000)
```

```{r}
#rain_filtered <- rain %>%
#filter(dateRain %in% market$price_date)
#nrow(rain_filtered)
#nrow(market)
```
```{r}
library(RANN)

# Normalize function to handle potential edge cases
normalize <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Avoid division by zero in normalization
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Step 1: Pre-filter `rain_filtered` and `market`
rain_filtered <- rain_filtered %>%
  filter(!is.na(lon) & !is.na(lat) & !is.na(dateRain))  # Remove rows with invalid coordinates

market <- market %>%
  filter(!is.na(lon) & !is.na(lat) & !is.na(price_date))  # Remove rows with invalid coordinates

# Step 2: Normalize date and combine with spatial coordinates
market <- market %>%
  mutate(
    norm_date = normalize(as.numeric(price_date)),
    norm_lon = lon,
    norm_lat = lat
  )

rain_filtered <- rain_filtered %>%
  mutate(
    norm_date = normalize(as.numeric(dateRain)),
    norm_lon = lon,
    norm_lat = lat
  )

# Step 3: Create matrices for KNN
market_coords <- as.matrix(market[, c("norm_lon", "norm_lat", "norm_date")])
rain_coords <- as.matrix(rain_filtered[, c("norm_lon", "norm_lat", "norm_date")])

# Ensure no NA/NaN/Inf values in matrices
if (any(is.na(market_coords)) || any(is.nan(market_coords)) || any(is.infinite(market_coords))) {
  stop("market_coords contains invalid values (NA/NaN/Inf). Please inspect the data.")
}
if (any(is.na(rain_coords)) || any(is.nan(rain_coords)) || any(is.infinite(rain_coords))) {
  stop("rain_coords contains invalid values (NA/NaN/Inf). Please inspect the data.")
}

# Step 4: Perform KNN
knn_result <- nn2(rain_coords, market_coords, k = 1)

# Step 5: Assign precipitation values with exact date matching
market$closest_precipitation <- sapply(1:nrow(market), function(i) {
  nn_idx <- knn_result$nn.idx[i, 1]  # Nearest neighbor index
  # Ensure the date matches
  if (market$price_date[i] == rain_filtered$dateRain[nn_idx]) {
    return(rain_filtered$precipitation[nn_idx])
  } else {
    return(NA)
  }
})

# Step 6: Check results
summary(market$closest_precipitation)

```
```{r}
library(dplyr)
library(RANN)

# Normalize function to handle potential edge cases
normalize <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Avoid division by zero in normalization
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Step 1: Pre-filter `temp_filtered` and `market`
temp_filtered <- temp_filtered %>%
  filter(!is.na(lon) & !is.na(lat) & !is.na(dateTemp))  # Remove rows with invalid coordinates

market <- market %>%
  filter(!is.na(lon) & !is.na(lat) & !is.na(price_date))  # Remove rows with invalid coordinates

# Step 2: Normalize date and combine with spatial coordinates
market <- market %>%
  mutate(
    norm_date = normalize(as.numeric(price_date)),
    norm_lon = lon,
    norm_lat = lat
  )

temp_filtered <- temp_filtered %>%
  mutate(
    norm_date = normalize(as.numeric(dateTemp)),
    norm_lon = lon,
    norm_lat = lat
  )

# Step 3: Create matrices for KNN
market_coords <- as.matrix(market[, c("norm_lon", "norm_lat", "norm_date")])
temp_coords <- as.matrix(temp_filtered[, c("norm_lon", "norm_lat", "norm_date")])

# Ensure no NA/NaN/Inf values in matrices
if (any(is.na(market_coords)) || any(is.nan(market_coords)) || any(is.infinite(market_coords))) {
  stop("market_coords contains invalid values (NA/NaN/Inf). Please inspect the data.")
}
if (any(is.na(temp_coords)) || any(is.nan(temp_coords)) || any(is.infinite(temp_coords))) {
  stop("temp_coords contains invalid values (NA/NaN/Inf). Please inspect the data.")
}

# Step 4: Perform KNN
knn_result <- nn2(temp_coords, market_coords, k = 1)

# Step 5: Assign temperature values with exact date matching
market$closest_temp <- sapply(1:nrow(market), function(i) {
  nn_idx <- knn_result$nn.idx[i, 1]  # Nearest neighbor index
  # Ensure the date matches
  if (market$price_date[i] == temp_filtered$dateTemp[nn_idx]) {
    return(temp_filtered$temp_2m_cel[nn_idx])  # Use temp_2m_cel for temperature values
  } else {
    return(NA)
  }
})

# Step 6: Check results
summary(market$closest_temp)

```
```{r}
# Create a data frame for Kenya (KEN)
market_ken <- market %>%
  filter(country == "KEN")

# Create a data frame for Uganda (UGA)
market_uga <- market %>%
  filter(country == "UGA")

```


```{r}
# Calculate the 1st and 99th percentiles for each variable
pricer_percentiles <- quantile(market_uga$price_r, probs = c(0.01, 0.99), na.rm = TRUE)
pricew_percentiles <- quantile(market_uga$price_w, probs = c(0.01, 0.99), na.rm = TRUE)

# Filter data above and below the percentiles
price_r_below <- market_uga$price_r[market_uga$price_r <= pricer_percentiles[1]]
price_r_above <- market_uga$price_r[market_uga$price_r >= pricer_percentiles[2]]

price_w_below <- market_uga$price_w[market_uga$price_w <= pricew_percentiles[1]]
price_w_above <- market_uga$price_w[market_uga$price_w >= pricew_percentiles[2]]

# Calculate means
mean_total_profit <- mean(market_uga$price_r, na.rm = TRUE)

mean_total_sales <- mean(market_uga$price_w, na.rm = TRUE)




market_uga <- market_uga %>% filter(is.na(price_r) | price_r <= pricer_percentiles[2])
market_uga <- market_uga %>% filter(is.na(price_w) | price_w <= pricew_percentiles[2])
nrow(market_uga)
```



```{r}
# Calculate the 1st and 99th percentiles for each variable
pricer_percentiles <- quantile(market_ken$price_r, probs = c(0.01, 0.99), na.rm = TRUE)
pricew_percentiles <- quantile(market_ken$price_w, probs = c(0.01, 0.99), na.rm = TRUE)

# Filter data above and below the percentiles
price_r_below <- market_ken$price_r[market_ken$price_r <= pricer_percentiles[1]]
price_r_above <- market_ken$price_r[market_ken$price_r >= pricer_percentiles[2]]

price_w_below <- market_ken$price_w[market_ken$price_w <= pricew_percentiles[1]]
price_w_above <- market_ken$price_w[market_ken$price_w >= pricew_percentiles[2]]

# Calculate means
mean_total_profit <- mean(market_ken$price_r, na.rm = TRUE)

mean_total_sales <- mean(market_ken$price_w, na.rm = TRUE)




market_ken <- market_ken %>% filter(is.na(price_r) | price_r <= pricer_percentiles[2])
market_ken <- market_ken %>% filter(is.na(price_w) | price_w <= pricew_percentiles[2])

```
```{r}
#picking cooking banana as commodity, many observations in both df

ken_banana <- market_ken %>%
  filter(mp_product_nicename == "Cooking Bananas")

ken_sp <- market_ken %>%
  filter(mp_product_nicename == "Sweet Potatoes")


uga_banana <- market_uga %>%
  filter(mp_product_nicename == "Cooking Bananas")

uga_sp <- market_uga %>%
  filter(mp_product_nicename == "Sweet Potatoes")

banana <- market %>%
  filter(mp_product_nicename == "Cooking Bananas")
```

```{r}
market_filtered <- ken_sp %>%
  filter(
    year %in% c("2019", "2020", "2021", "2022"),
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename
  )

market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_r, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Kenya Avg Retail Sweet Potatoes Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Retail Price",
    fill = "Market"
  )
```
```{r}
market_filtered <- uga_sp %>%
  filter(
    year %in% c("2019", "2020", "2021", "2022"),
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename
  )

market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_r, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Uganda Avg Retail Sweet Potatoes Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Retail Price",
    fill = "Market"
  )
```
```{r}
market_filtered <- uga_sp %>%
  filter(
    year %in% c("2019", "2020", "2021", "2022"),
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename
  )

market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_w, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Uganda Avg Wholesale Sweet Potatoes Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Retail Price",
    fill = "Market"
  )
```

```{r}
market_filtered <- ken_sp %>%
  filter(
    year %in% c("2019", "2020", "2021", "2022"),
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename
  )

market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_w, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Kenya Avg Wholesale Sweet Potatoes Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Retail Price",
    fill = "Market"
  )
```
```{r}
library(ggplot2)

# Step 1: Count observations by location
top_locations <- ken_sp %>%
  count(mp_marketplaces_nicename, sort = TRUE) %>%
  slice_max(n, n = 3)  # Select the top 5 locations

# Step 2: Filter the data for the top 5 locations
market_top <- ken_sp %>%
  filter(mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename)

# Step 3: Create the grouped line plot
ggplot(market_top, aes(x = price_date, y = price_r, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Kenyan Sweet Potato Retail Price 2021-2022",
    x = "Time",
    y = "Price",
    color = "Location"
  )

# Step 1: Count observations by location
top_locations <- ken_sp %>%
  count(mp_marketplaces_nicename, sort = TRUE) %>%
  slice_max(n, n = 3)  # Select the top 5 locations

# Step 2: Filter the data for the top 5 locations
market_top <- ken_sp %>%
  filter(mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename)

# Step 3: Create the grouped line plot
ggplot(market_top, aes(x = price_date, y = price_w, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Kenyan Sweet Potato Wholesale Price 2021-2022",
    x = "Time",
    y = "Price",
    color = "Location"
  )
```
```{r}

# Step 1: Count observations by location and slice ties
top_locations <- uga_sp %>%
  count(mp_marketplaces_nicename, sort = TRUE) %>%
  slice_max(n, n = 5, with_ties = TRUE) %>%  # Include ties
  slice_sample(n = 5)  # Randomly select 5 from tied locations

# Step 2: Filter the data for the sampled top 5 locations
market_top <- uga_sp %>%
  filter(mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename)

# Step 3: Create the grouped line plot for retail price
ggplot(market_top, aes(x = price_date, y = price_r, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Ugandan Sweet Potato Retail Price 2021-2022",
    x = "Time",
    y = "Price",
    color = "Location"
  )

# Step 4: Create the grouped line plot for wholesale price
ggplot(market_top, aes(x = price_date, y = price_w, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Ugandan Sweet Potato Wholesale Price 2021-2022",
    x = "Time",
    y = "Price",
    color = "Location"
  )

```
```{r}


# Re-merge market_ken and market_uga
market_combined <- bind_rows(market_ken, market_uga)

# View the combined data frame
write.csv(market_combined, file = "C:/Users/will/Downloads/Market_climate_data_trimmed.csv")
```


```{r}

# Step 1: Randomly select 10 locations
set.seed(123)  # For reproducibility
selected_locations <- sample(unique(market$mp_marketplaces_nicename), size = 5)

# Step 2: Filter the data
market_subset <- market %>%
  filter(mp_marketplaces_nicename %in% selected_locations)

# Step 3: Create the grouped line plot
ggplot(market_subset, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Selected Locations",
    x = "Time",
    y = "Precipitation (mm)",
    color = "Location"
  )
```

```{r}

# Step 1: Count observations by location
top_locations <- market %>%
  count(mp_marketplaces_nicename, sort = TRUE) %>%
  slice_max(n, n = 5)  # Select the top 5 locations

# Step 2: Filter the data for the top 5 locations
market_top <- market %>%
  filter(mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename)

# Step 3: Create the grouped line plot
ggplot(market_top, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Top 5 Locations",
    x = "Time",
    y = "Precipitation (mm)",
    color = "Location"
  )
```



```{r}

# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20190101
end_date <- 20191231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Top 5 Locations 2019",
    x = "Date",
    y = "Precipitation (mm)",
    color = "Location"
  )

```

```{r}

# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20200101
end_date <- 20201231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Top 5 Locations 2020",
    x = "Date",
    y = "Precipitation (mm)",
    color = "Location"
  )

```

```{r}

# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20210101
end_date <- 20211231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Top 5 Locations 2021",
    x = "Date",
    y = "Precipitation (mm)",
    color = "Location"
  )

```
```{r}

# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20220101
end_date <- 20221231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_precipitation, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Precipitation Over Time for Top 5 Locations 2022",
    x = "Date",
    y = "Precipitation (mm)",
    color = "Location"
  )

```

```{r}
# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20190101
end_date <- 20191231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_temp, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Temperature Over Time for Top 5 Locations 2019",
    x = "Date",
    y = "Temperature (Celsius)",
    color = "Location"
  )
```
```{r}
# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20200101
end_date <- 20201231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_temp, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Temperature Over Time for Top 5 Locations 2020",
    x = "Date",
    y = "Temperature (Celsius)",
    color = "Location"
  )
```
```{r}
# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20210101
end_date <- 20211231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_temp, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Temperature Over Time for Top 5 Locations 2021",
    x = "Date",
    y = "Temperature (Celsius)",
    color = "Location"
  )
```

```{r}
# Define the time frame (e.g., from "2022-01-01" to "2022-12-31")
start_date <- 20220101
end_date <- 20221231

# Filter the data for the top 5 locations and the selected time frame
market_zoomed <- market %>%
  filter(
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename,
    price_date >= start_date,
    price_date <= end_date
  )

# Create the grouped line plot
ggplot(market_zoomed, aes(x = price_date, y = closest_temp, color = mp_marketplaces_nicename, group = mp_marketplaces_nicename)) +
  geom_line() +
  theme_minimal() +
  labs(
    title = "Temperature Over Time for Top 5 Locations 2022",
    x = "Date",
    y = "Temperature (Celsius)",
    color = "Location"
  )
```

```{r}


# Step 1: Extract year from price_date
market <- market %>%
  mutate(year = substr(price_date, 1, 4))  # Extract first 4 digits as year

# Step 2: Filter for years 2019-2022 and top 5 markets
market_filtered <- market %>%
  filter(
    year %in% c("2019", "2020", "2021", "2022"),
    mp_marketplaces_nicename %in% top_locations$mp_marketplaces_nicename
  )

# Step 3: Calculate average temperatures
market_avg_temp <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_temp = mean(closest_temp, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_temp, aes(x = year, y = avg_temp, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Average Temperature by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Temperature (°C)",
    fill = "Market"
  )

```
```{r}
market_avg_precip <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_precip = mean(closest_precipitation, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_precip, aes(x = year, y = avg_precip, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Average Precipitation by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Precipitation (mm)",
    fill = "Market"
  )
```

```{r}
market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_r, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Average Retail Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Retail Price",
    fill = "Market"
  )
```

```{r}
market_avg_price <- market_filtered %>%
  group_by(mp_marketplaces_nicename, year) %>%
  summarize(avg_price = mean(price_w, na.rm = TRUE), .groups = "drop")

# Step 4: Create the grouped bar graph
ggplot(market_avg_price, aes(x = year, y = avg_price, fill = mp_marketplaces_nicename)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(
    title = "Average Wholesale Price by Year (2019-2022) for Top 5 Markets",
    x = "Year",
    y = "Average Wholesale Price",
    fill = "Market"
  )
```






```{r}
write.csv(market, file = "C:/Users/will/Downloads/Market_climate_data.csv")
```

