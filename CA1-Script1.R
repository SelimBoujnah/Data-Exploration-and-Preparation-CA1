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

