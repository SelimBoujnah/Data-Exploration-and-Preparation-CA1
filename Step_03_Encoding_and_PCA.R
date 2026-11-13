# DEP CA1 - Dublin Airbnb Dataset
# Step 3: Encoding and Principal Component Analysis


# Loading the required package
library(fastDummies)

# Inspecting categorical variables before encoding
table(selected_features$room_type)

table(selected_features$neighbourhood_cleansed)

# Checking missing values in the two categorical variables
colSums(is.na(selected_features[, c(
  "room_type",
  "neighbourhood_cleansed"
)]))