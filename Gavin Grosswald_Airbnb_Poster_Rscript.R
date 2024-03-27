#
# Author: Gavin Grosswald
# Purpose: Analysis on NYC Airbnb Data
#

# NYC Airbnb data set
# Data as of January 05, 2024
# Link to data set - https://www.kaggle.com/datasets/vrindakallu/new-york-dataset

# Package library
#install.packages("leaflet")
library(leaflet)
#install.packages("maps")
library(maps)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("rnaturalearth")
library(rnaturalearth)
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)
#install.packages("dplyr")
library(dplyr)
#install.packages("tidyr")
library(tidyr) 
#install.packages("readr")
library(readr)
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("cluster")
library(cluster)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("e1071")
library(e1071)
#install.packages("tcltk2")
library(tcltk2)

# Color library
"#606C38"
"#283618"
"#6F1D1B"
"#FEFAE0"
"#FFE6A7"
"#DDA15E"
"#BC6C25"
"#BB9457"
"#99582A"
"#432818"

# Read CSV
airbnb_raw <- read.csv("C:\\Users\\gsgro\\OneDrive\\Desktop\\Syr_MSBA\\Term 3\\Information Visualization\\Final Project\\new_york_listings_2024.csv"
                       , stringsAsFactors = TRUE
                       , header = TRUE)

# Data set size
dimensions <- dim(airbnb_raw)
num_rows <- dimensions[1]
num_col <- dimensions[2]
dataSize <- (num_col*4)*(num_rows/100)

# Analyze raw data set
View(airbnb_raw)
summary(airbnb_raw)
str(airbnb_raw)
colnames(airbnb_raw)
na_count_per_column <- colSums(is.na(airbnb_raw))

# Log of price
airbnb_raw$log_price <- log10(airbnb_raw$price)

# Single Dimensional Plots
#par(mfrow = c(2,2))

barplot(table(airbnb_raw$neighbourhood_group)
        , main = "Count of Airbnb Listings by Boruogh"
        , col = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b")
        , xlab = "NYC Borough"
        , ylab = "Count")

pie(table(airbnb_raw$neighbourhood_group),
    col = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b"),
    main = "Count of Airbnb Listings by Borough",
    labels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island"))

hist(airbnb_raw$beds, col = "#6F1D1B"
        , main = "Histogram of Bed Distribution"
        , xlab = "Number of Beds")

hist(airbnb_raw$log_price, col = "tan"
        , main = "Histogram of Price Distribution"
        , xlab = "Price (log10)")

plot(airbnb_raw$longitude, airbnb_raw$latitude
        , col = rgb(.4, .6, .2, alpha = 0.3)
        , main = "Airbnb Listing Locations"
        , xlab = "Longitude"
        , ylab = "Latitude"
        , axes = FALSE
        , frame.plot = FALSE
        , ann = FALSE)


# Multi-dimensional Plots

average_price <- aggregate(price ~ neighbourhood_group, data = airbnb_raw, FUN = mean)
ggplot(data = average_price, aes(x = neighbourhood_group, y = price, fill = neighbourhood_group)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Price of Airbnb Listings by Borough") +
  xlab("Borough") +
  ylab("Average Price") +
  scale_fill_manual(values = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b")) +
  theme_minimal() +
  theme(panel.grid = element_blank())


ggplot(airbnb_raw, aes(x = neighbourhood_group, y = log_price)) +
  geom_boxplot(fill = c("#995928", "#fce6a7", "#432818", "#626e38", "#6f1d1b"), color = "black") +
  labs(title = "Boxplot of Price by Borough", x = "Borough", y = "Price (log10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


ggplot(airbnb_raw, aes(x = neighbourhood_group, fill = room_type)) +
  geom_bar() +
  labs(title = "Stacked Bar Chart of Room Type by Neighborhood", x = "Neighborhood", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Density Map
myColors <- c("Manhattan" = "#432818", 
              "Brooklyn" = "#fce6a7", 
              "Queens" = "#626e38", 
              "Bronx" = "#995928", 
              "Staten Island" = "#6f1d1b")

plot(airbnb_raw$longitude, airbnb_raw$latitude
     , col = myColors[as.numeric(airbnb_raw$neighbourhood_group)]
     , main = "Airbnb Listing Location by Borough"
     , xlab = ""
     , ylab = ""
     , axes = FALSE
     , frame.plot = FALSE
     , ann = FALSE)

# Count the occurrences of each neighborhood
neighborhood_counts <- airbnb_raw %>%
  group_by(neighbourhood) %>%
  summarize(count = n())

# Filter neighborhoods with a count of 50 or greater
neighborhoods_filtered <- neighborhood_counts %>%
  filter(count >= 250) %>%
  pull(neighbourhood)

# Filter the original dataset to include only neighborhoods with a count of 500 or greater
airbnb_filtered <- airbnb_raw %>%
  filter(neighbourhood %in% neighborhoods_filtered)

# Reorder the levels of the 'neighbourhood' variable based on count in reverse order
airbnb_filtered$neighbourhood <- factor(airbnb_filtered$neighbourhood, levels = neighborhood_counts$neighbourhood[order(neighborhood_counts$count)])

# Plot the chart with filtered and sorted neighborhoods
ggplot(data = airbnb_filtered, aes(x = neighbourhood, fill = neighbourhood_group)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
  ggtitle("Count of Neighborhoods in NYC Dataset") +
  xlab("Neighborhood") +
  ylab("Count") +
  coord_flip() +
  scale_fill_manual(values = c("Manhattan" = "#432818", 
                               "Brooklyn" = "#fce6a7", 
                               "Queens" = "#626e38", 
                               "Bronx" = "#995928", 
                               "Staten Island" = "#6f1d1b")) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# NYC Borough Statistics
# Rating analysis
airbnb_raw$rating[airbnb_raw$rating == "No rating"] <- NA
# Convert the factor column to numeric
airbnb_raw$rating <- as.numeric(as.character(airbnb_raw$rating))
# Calculate average rating excluding NA values
average_rating <- mean(airbnb_raw$rating, na.rm = TRUE)

manhattan_data <- subset(airbnb_raw, neighbourhood_group == "Manhattan")
brooklyn_data <- subset(airbnb_raw, neighbourhood_group == "Brooklyn")
bronx_data <- subset(airbnb_raw, neighbourhood_group == "Bronx")
queens_data <- subset(airbnb_raw, neighbourhood_group == "Queens")
statenIsland_data <- subset(airbnb_raw, neighbourhood_group == "Staten Island")

summary(manhattan_data$price)
summary(brooklyn_data$price)
summary(bronx_data$price)
summary(queens_data$price)
summary(statenIsland_data$price)

nrow(manhattan_data)
nrow(brooklyn_data)
nrow(bronx_data)
nrow(queens_data)
nrow(statenIsland_data)

## Scatter Plot
airbnb_raw$rating[airbnb_raw$rating == "No rating"] <- NA

# Filter out rows with over 300 reviews in ltm
airbnb_filtered <- airbnb_raw[airbnb_raw$number_of_reviews_ltm <= 100, ]

# Create a scatter plot with the filtered data
ggplot(airbnb_filtered, aes(x = rating, y = number_of_reviews_ltm, color = neighbourhood_group)) +
  geom_point() +
  labs(title = "Average Rating vs. Number of Reviews in Last Twelve Months by Borough (Excluding Outliers)",
       x = "Average Rating",
       y = "Number of Reviews (LTM)",
       color = "Borough") +
  scale_color_manual(values = c("Manhattan" = "#432818", 
                                "Brooklyn" = "#fce6a7", 
                                "Queens" = "#626e38", 
                                "Bronx" = "#995928", 
                                "Staten Island" = "#6f1d1b")) +
  theme_minimal() +
  theme(panel.grid = element_blank())
