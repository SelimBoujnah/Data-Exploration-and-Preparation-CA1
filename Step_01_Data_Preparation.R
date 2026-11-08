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
       y = "Number of Listings") + theme_minimal()

# Counting listings with prices above 1000
sum(airbnb_clean$price_numeric > 1000, na.rm = TRUE)

# Detecting outliers
# Focused boxplot to better visualise price outliers
ggplot(airbnb_clean, aes(x = "All Listings", y = price_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(title = "Dublin Airbnb Prices: Focused Boxplot",
       x = "",
       y = "Listed Price ($)")+ theme_minimal()

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

# Counting prices above the upper IQR threshold
sum(airbnb_clean$price_numeric > upper_limit, na.rm = TRUE)

# Count prices below the lower IQR threshold
sum(airbnb_clean$price_numeric < lower_limit, na.rm = TRUE)


# Investigating potential price outliers
# Selecting listings above the upper IQR threshold
price_outliers <- filter(airbnb_clean, price_numeric > upper_limit)

# Examining selected characteristics of these listings
head(select(price_outliers, id, room_type, property_type,
            accommodates, bedrooms, price_numeric))

# Examining descriptive statistics of the outlier prices
summary(price_outliers$price_numeric)


# Investigating the highest-priced listing
# Finding the maximum recorded price
highest_price <- max(airbnb_clean$price_numeric, na.rm = TRUE)

# Selecting listing(s) with the highest price
highest_listing <- filter(airbnb_clean, price_numeric == highest_price)

# Examining their characteristics
select(highest_listing, id, name, room_type, property_type,
       accommodates, bedrooms, minimum_nights, price_numeric)


# Inspecting other numerical variables
# Examining descriptive statistics and missing values
summary(airbnb_clean[, c(
  "accommodates",
  "bedrooms",
  "beds",
  "minimum_nights",
  "availability_365",
  "number_of_reviews"
)])

# Investigating unusual minimum night values
# Counting listings with a minimum stay of 999 nights
sum(airbnb_clean$minimum_nights == 999, na.rm = TRUE)

# Counting listings requiring more than 30 nights
sum(airbnb_clean$minimum_nights > 30, na.rm = TRUE)

# Counting listings with zero bedrooms
sum(airbnb_clean$bedrooms == 0, na.rm = TRUE)


# Investigating unusual accommodation values
# Examining the listing requiring 999 minimum nights
filter(airbnb_clean, minimum_nights == 999) %>%
  select(id, name, room_type, property_type,
         minimum_nights, availability_365)

# Examining property types of listings with zero bedrooms
table(airbnb_clean$property_type[airbnb_clean$bedrooms == 0])

# Checking missing values in selected features
# Selecting relevant variables for further analysis
selected_features <- airbnb_clean[, c(
  "price_numeric",
  "room_type",
  "property_type",
  "neighbourhood_cleansed",
  "accommodates",
  "bedrooms",
  "beds",
  "minimum_nights",
  "availability_365",
  "number_of_reviews"
)]

# Checking missing values in each selected feature
colSums(is.na(selected_features))


# Checking the dimensions
dim(selected_features)

#trying to detect the empty fields
# Checking empty strings 
sum(airbnb$price == "")

# Checking room type
sum(selected_features$room_type == "", na.rm = TRUE)

# Checking property type
sum(selected_features$property_type == "", na.rm = TRUE)

# Checking neighbourhood
sum(selected_features$neighbourhood_cleansed == "", na.rm = TRUE)

# Detecting empty strings across all character columns
empty_counts <- sapply(airbnb, function(x) {
  if (is.character(x)) {
    sum(trimws(x) == "", na.rm = TRUE)
  } else {
    0
  }
})

# Display only columns containing empty strings
empty_counts[empty_counts > 0]



# Preparing the dataset for price-related EDA
# Selecting listings with recorded prices
airbnb_price_eda <- filter(selected_features, !is.na(price_numeric))

# Verifying the dimensions
dim(airbnb_price_eda)

# Verifying there are no missing prices
sum(is.na(airbnb_price_eda$price_numeric))

# Final Dataset Validation
# Checking remaining missing values in the price EDA dataset
colSums(is.na(airbnb_price_eda))

# Verifying the price data type
class(airbnb_price_eda$price_numeric)

# Final Data Cleaning Decisions

# 1. Missing Price Values
# The dataset contains 1,706 missing prices out of 6,748 listings.
# These values were converted from empty strings to NA.
# Missing prices were kept in the main dataset because the
# listings still contain useful information about other variables.
# A separate dataset of 5,042 listings was created for price analysis.
# Mean imputation was avoided because it could change price distributions.


# 2. Missing Bedrooms, Beds and Minimum Nights
# The dataset contains 7 missing bedroom values, 133 missing bed
# values and 25 missing minimum-night values.
# These observations were kept because the missing values do not
# necessarily affect analyses involving other variables.
# Missing values will be handled when the relevant features are used.


# 3. Potential Price Outliers
# The IQR method identified 296 prices above the upper threshold of 576.79.
# These observations were retained because unusually expensive listings
# may represent genuine luxury or larger accommodation properties.
# Removing them without evidence could exclude useful information.
# Their influence on statistical results will be considered during analysis.


# 4. Unusual Accommodation Values
# A total of 150 listings reported zero bedrooms.
# These were retained because some properties may be studio apartments.
# One listing had a minimum stay of 999 nights.
# This value was flagged as unusual but not automatically removed,
# because there is insufficient evidence to confirm it is incorrect.


# 5. Limitations of the Cleaning Approach
# Approximately 25% of listings have missing prices.
# Excluding these listings from price analysis may introduce bias,
# especially because missing-price rates differ between room types.
# Retaining price outliers may influence averages and other statistics.
# Additional missing-value treatment may be required for PCA.



