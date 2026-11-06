# DEP CA1 - Dublin Airbnb Dataset
# Loading libraries 
# 1. Load Required Libraries
library(tidyverse)
library(fastDummies)

# Step 1: Data Import and Initial Exploration

# Importing the Airbnb dataset
airbnb <- read.csv("listings.csv",
                   stringsAsFactors = FALSE)

# Checking dataset dimensions
dim(airbnb)

# Previewing the dataset
head(airbnb)

# Examining all column names
names(airbnb)

# Inspecting important variables
str(airbnb[, c("id", "room_type",
               "price", "bedrooms")])

# Checking the price data type
class(airbnb$price)

# Step 2: Initial Data Quality Check

# Check missing values in selected columns
sum(is.na(airbnb$price))
sum(is.na(airbnb$bedrooms))
sum(is.na(airbnb$review_scores_rating))

# Checking for duplicate listing IDs
sum(duplicated(airbnb$id))

# Checking empty strings in the price column as it retrured 0 dublicates
sum(airbnb$price == "")

# Displaying the first 6 price values
head(airbnb$price)


# initializing a clear data set version 
airbnb_clean <- airbnb

# Replacing empty price strings with NA
airbnb_clean$price[airbnb_clean$price == ""] <- NA

# Verifying missing prices
sum(is.na(airbnb_clean$price))

#converting prices to numerical values
# Removing $ and commas
airbnb_clean$price_numeric <- as.numeric(
  gsub("[$,]", "", airbnb_clean$price)
)

# Verifying the new data type
class(airbnb_clean$price_numeric)

# Verifying missing values after conversion
sum(is.na(airbnb_clean$price_numeric))


# Investigating missing price patterns
# Counting missing prices by room type
price_missing_by_room <- table(
  airbnb_clean$room_type,
  is.na(airbnb_clean$price_numeric)
)

price_missing_by_room
# Calculating percentages within each room type
prop.table(price_missing_by_room, margin = 1) * 100


#Price Distribution and Potential Outliers
# Examining descriptive statistics for numerical prices
summary(airbnb_clean$price_numeric)

#Visualising the price distribution
# Histogram of Airbnb prices
ggplot(airbnb_clean, aes(x = price_numeric)) +
  geom_histogram(bins = 30,
                 fill = "lightblue",
                 color = "black") +
  labs(title = "Distribution of Airbnb Prices in Dublin",
       x = "Listed Price",
       y = "Number of Listings") +
  theme_minimal()


# Focused Price Distribution

ggplot(airbnb_clean, aes(x = price_numeric)) +
  geom_histogram(binwidth = 50,
                 fill = "lightblue",
                 color = "black",
                 na.rm = TRUE) +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(title = "Dublin Airbnb Prices: Focused Distribution",
       x = "Listed Price ($)",
       y = "Number of Listings")

# Count listings with prices above 1000
sum(airbnb_clean$price_numeric > 1000, na.rm = TRUE)

# Detecting outliers
# Focused boxplot to better visualise price outliers
ggplot(airbnb_clean, aes(x = "All Listings", y = price_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(title = "Dublin Airbnb Prices: Focused Boxplot",
       x = "",
       y = "Listed Price ($)")

#IQR Outlier Detection
# Calculating the first and third quartiles
q1 <- quantile(airbnb_clean$price_numeric, 0.25, na.rm = TRUE)
q3 <- quantile(airbnb_clean$price_numeric, 0.75, na.rm = TRUE)

# Calculating the interquartile range
price_iqr <- IQR(airbnb_clean$price_numeric, na.rm = TRUE)

# Calculating the lower and upper outlier thresholds
lower_limit <- q1 - 1.5 * price_iqr
upper_limit <- q3 + 1.5 * price_iqr

# Displaying the results
q1
q3
price_iqr
lower_limit
upper_limit

# Counting potential price outliers

# Count prices above the upper IQR threshold
sum(airbnb_clean$price_numeric > upper_limit, na.rm = TRUE)

# Count prices below the lower IQR threshold
sum(airbnb_clean$price_numeric < lower_limit, na.rm = TRUE)


# Investigating potential price outliers
# Select listings above the upper IQR threshold
price_outliers <- filter(airbnb_clean, price_numeric > upper_limit)

# Examine selected characteristics of these listings
head(select(price_outliers, id, room_type, property_type,
            accommodates, bedrooms, price_numeric))

# Examine descriptive statistics of the outlier prices
summary(price_outliers$price_numeric)