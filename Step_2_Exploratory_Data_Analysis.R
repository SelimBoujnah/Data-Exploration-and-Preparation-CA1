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

# Research Question 2: How do recorded Airbnb prices vary across Dublin neighbourhoods?
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