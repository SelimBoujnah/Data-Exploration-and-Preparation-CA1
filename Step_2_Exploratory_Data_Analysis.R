# DEP CA1 - Dublin Airbnb Dataset
# Step 3: Exploratory Data Analysis (EDA)



# Question 1: How do recorded Airbnb prices vary across different accommodation room types in Dublin?
# Exploring Accommodation Room Types
# Counting listings by room type
table(airbnb_price_eda$room_type)

# Comparing Price Distributions Across Room Types
# Boxplot comparing Airbnb prices by room type
ggplot(airbnb_price_eda, aes(x = room_type, y = price_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    title = "Dublin Airbnb Prices by Room Type",
    x = "Accommodation Room Type",
    y = "Listed Price"
  ) + theme_minimal()

# Descriptive Price Statistics by Room Type
# Entire home/apt
summary(
  filter(airbnb_price_eda, room_type == "Entire home/apt")$price_numeric
)

# Private room
summary(
  filter(airbnb_price_eda, room_type == "Private room")$price_numeric
)

# Shared room
summary(
  filter(airbnb_price_eda, room_type == "Shared room")$price_numeric
)

# Hotel room
summary(
  filter(airbnb_price_eda, room_type == "Hotel room")$price_numeric
)

# Question 2: How do recorded Airbnb prices vary across Dublin neighbourhoods?
# Exploring Neighbourhood Frequencies

# Counting listings by neighbourhood
table(airbnb_price_eda$neighbourhood_cleansed)

# Comparing Price Distributions Across Dublin Areas
# Boxplot comparing Airbnb prices by local authority area
ggplot(airbnb_price_eda,
       aes(x = neighbourhood_cleansed, y = price_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    title = "Dublin Airbnb Prices by Local Authority Area",
    x = "Local Authority Area",
    y = "Listed Price"
  ) + theme_minimal()


# Descriptive Price Statistics by Local Authority Area
# Dublin City
summary(
  filter(airbnb_price_eda, neighbourhood_cleansed == "Dublin City")$price_numeric
)

# Dn Laoghaire-Rathdown
summary(
  filter(airbnb_price_eda, neighbourhood_cleansed == "Dn Laoghaire-Rathdown")$price_numeric
)

# Fingal
summary(
  filter(airbnb_price_eda, neighbourhood_cleansed == "Fingal")$price_numeric
)

# South Dublin
summary(
  filter(airbnb_price_eda, neighbourhood_cleansed == "South Dublin")$price_numeric
)


# Question 3: Is accommodation capacity associated with recorded Airbnb prices?

# Scatterplot comparing guest capacity and listed prices
ggplot(airbnb_price_eda,
       aes(x = accommodates, y = price_numeric)) +
  geom_point(color = "darkblue", alpha = 0.3) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(
    title = "Dublin Airbnb Prices vs Accommodation Capacity",
    x = "Number of Guests Accommodated",
    y = "Listed Price"
  ) +
  theme_minimal()

# Correlation Between Accommodation Capacity and Price
# Pearson correlation
cor(airbnb_price_eda$accommodates,
    airbnb_price_eda$price_numeric)

# Spearman correlation
cor(airbnb_price_eda$accommodates,
    airbnb_price_eda$price_numeric,
    method = "spearman")




# Question 4:How is Airbnb listing availability distributed across Dublin?
# Histogram of listing availability
ggplot(selected_features, aes(x = availability_365)) +
  geom_histogram(
    binwidth = 30,
    fill = "darkblue",
    color = "white"
  ) +
  labs(
    title = "Distribution of Airbnb Availability in Dublin",
    x = "Available Days in the Next 365 Days",
    y = "Number of Listings"
  ) +
  theme_minimal()

# Descriptive Statistics for Airbnb Availability
summary(selected_features$availability_365)

# Counting listings with zero available days
sum(selected_features$availability_365 == 0)

# Counting listings with 300 or more available days
sum(selected_features$availability_365 >= 300)


# Question 5: How does Airbnb room-type composition vary across Dublin areas?
# 100% Stacked Bar Chart
ggplot(selected_features,
       aes(x = neighbourhood_cleansed, fill = room_type)) +
  geom_bar(position = "fill", color = "white") +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  labs(
    title = "Airbnb Room Types Across Dublin Areas",
    x = "Local Authority Area",
    y = "Proportion of Listings",
    fill = "Room Type"
  ) +
  theme_minimal()

# Calculating Room Type Percentages by Local Authority Area

# Count each room type within each area
room_type_by_area <- table(
  selected_features$neighbourhood_cleansed,
  selected_features$room_type
)

# Display counts
room_type_by_area

# Calculate percentages within each area
round(prop.table(room_type_by_area, margin = 1) * 100, 1)