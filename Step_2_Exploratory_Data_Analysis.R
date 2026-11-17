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

# Question 2: Median Airbnb Price by Local Authority Area

median_prices <- airbnb_price_eda %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(
    median_price = median(price_numeric, na.rm = TRUE)
  )

ggplot(median_prices,
       aes(x = reorder(neighbourhood_cleansed, median_price),
           y = median_price)) +
  geom_col(fill = "lightblue", color = "black") +
  coord_flip() +
  labs(
    title = "Median Airbnb Price by Local Authority",
    x = "Local Authority Area",
    y = "Median Listed Price (€)"
  ) +
  theme_minimal()


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
    y = "Listed Price (€)"
  ) +
  theme_minimal()

# Correlation Between Accommodation Capacity and Price
# Pearson correlation
cor(airbnb_price_eda$accommodates,
    airbnb_price_eda$price_numeric)



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



# Additional EDA: Numerical Correlation Analysis
# Selecting numerical features for correlation analysis
numeric_eda <- airbnb_price_eda %>%
  select(
    price_numeric,
    accommodates,
    bedrooms,
    beds,
    minimum_nights,
    availability_365,
    number_of_reviews
  )

# Checking missing values
colSums(is.na(numeric_eda))

# Checking the number of complete observations
sum(complete.cases(numeric_eda))

# Creating a dataset with complete numerical observations
numeric_complete <- na.omit(numeric_eda)

# Checking dimensions
dim(numeric_complete)

# Calculating Pearson correlation matrix (Lecture 6)
correlation_matrix <- cor(numeric_complete)

# Displaying correlations rounded to two decimal places
round(correlation_matrix, 2)


# Correlation Heatmap

# Convert correlation matrix into a format suitable for ggplot
correlation_data <- as.data.frame(
  as.table(round(correlation_matrix, 2))
)

# Visualise correlations using a heatmap
ggplot(correlation_data, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 3) +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "darkblue",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlation Heatmap - Dublin Airbnb",
    x = "",
    y = "",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )


### Interpretation of Exploratory Data Analysis

# Question 1: Airbnb Prices by Room Type

# Entire homes/apartments have the highest median price of 258, followed by hotel rooms at 219 and private rooms at 110.
# Shared rooms have the lowest median price of 38.40. This suggests that accommodation type is associated with substantial differences in listed Airbnb prices.
# However, only 10 hotel rooms have recorded prices, these findings should be interpreted with caution.


# Question 2: Airbnb Prices by Local Authority Area

# Dublin City has the highest median listed price of 192.81, followed by Dn Laoghaire-Rathdown at 178.36.
# Fingal has a median price of 148, while South Dublin has the lowest median price of 113.
# This suggests that Airbnb prices vary geographically.
# However, the analysis does not establish that location alone causes these differences, as other factors may influence the prices


# Question 3: Accommodation Capacity and Price

# identified a positive relationship between accommodation capacity and listed price (r = 0.47).
# This indicates a moderate positive linear association.
# Listings accommodating more guests generally tend to have higher prices.
# However, correlation does not establish causation, and extreme prices may skew the results.


# Question 4: Airbnb Availability Distribution

# A total of 1,404 listings have zero available days, while 1,345 listings have 300 or more available days.
# This indicates considerable variation in availability across Dublin Airbnb listings.
# Availability does not necessarily represent occupancy, as unavailable days may have different explanations (clausure for maintenance for example)


# Question 5: Room-Type Composition and Additional Correlation Findings

# Entire homes/apartments represent approximately 61.3% of listings in Dublin City and 62.1% in Dn Laoghaire-Rathdown.
# In contrast, private rooms account for approximately 65.3% of listings in South Dublin and 53.3% in Fingal.
# This suggests that accommodation composition differs across Dublin's local authority areas.
# These percentages include listings with missing prices.

# Additional Pearson correlation analysis identified strong positive relationships
# between accommodates and bedrooms (r = 0.78), accommodates and beds (r = 0.78), and bedrooms and beds (r = 0.68).

# Listed price showed moderate positive correlations with bedrooms (r = 0.56),
# accommodates (r = 0.46) and beds (r = 0.41).
# Minimum nights, availability and number of reviews showed very weak
# linear relationships with listed price.

# The strong correlations between accommodation size variables suggest overlapping information, 
# supporting the use of PCA for dimensionality reduction.
# However, correlation does not establish causation.


# References
# Iqbal, M. (2026) Data Exploration and Preparation: Lecture Notes and Tutorial Materials
# CCT College Dublin. 

# The techniques used throughout this project were studied
# through the module lectures, tutorials and class examples.
# I adapted These techniques and applied them to the
# Dublin Airbnb dataset.