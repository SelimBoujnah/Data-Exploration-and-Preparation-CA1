# DEP CA1 - Dublin Airbnb Dataset
# Step 3: Encoding and Principal Component Analysis


# Loading the required package
library(fastDummies)
library("factoextra")

# Inspecting categorical variables before encoding
table(selected_features$room_type)

table(selected_features$neighbourhood_cleansed)

# Checking missing values in the two categorical variables
colSums(is.na(selected_features[, c(
  "room_type",
  "neighbourhood_cleansed"
)]))


# One-Hot Encoding of Airbnb Categorical Features

airbnb_encoded <- dummy_cols(
  selected_features,
  select_columns = c("room_type", "neighbourhood_cleansed"),
  remove_selected_columns = FALSE
)

# Comparing dataset dimensions before and after encoding
dim(selected_features)
dim(airbnb_encoded)

# Inspecting the new column names
names(airbnb_encoded)

# Preview only the newly created dummy columns
head(airbnb_encoded[, 11:ncol(airbnb_encoded)])



# Preparing Numerical Features for PCA

# Selecting numerical features and category labels
pca_data <- selected_features %>%
  select(
    room_type,
    neighbourhood_cleansed,
    price_numeric,
    accommodates,
    bedrooms,
    beds,
    minimum_nights,
    availability_365,
    number_of_reviews
  )

# Checking missing values before PCA
colSums(is.na(pca_data))

# Creating a complete dataset for PCA
pca_complete <- na.omit(pca_data)

# Selecting only numerical features for PCA
pca_numeric <- pca_complete %>%
  select(
    price_numeric,
    accommodates,
    bedrooms,
    beds,
    minimum_nights,
    availability_365,
    number_of_reviews
  )

# Verifying the final PCA dataset
dim(pca_numeric)
colSums(is.na(pca_numeric))
summary(pca_numeric)


# Log Transformation for Skewed Features
# Preserve the original numerical PCA dataset
pca_transformed <- pca_numeric

# Transform highly skewed numerical features
pca_transformed$price_numeric <- log10(
  pca_transformed$price_numeric
)

pca_transformed$minimum_nights <- log10(
  pca_transformed$minimum_nights
)

# Add 1 because some listings have zero reviews
pca_transformed$number_of_reviews <- log10(
  pca_transformed$number_of_reviews + 1
)

# Inspect the transformed variables
summary(pca_transformed)

# Confirm that all values are finite
colSums(!is.finite(as.matrix(pca_transformed)))



#Principal Component Analysis (PCA)
# Applying PCA to the transformed numerical dataset
pca_result <- prcomp(
  pca_transformed,
  center = TRUE,
  scale. = TRUE
)

# Displaying the PCA result
pca_result

# Examining explained variance
summary(pca_result)

# PCA Scree Plot
fviz_eig(
  pca_result,
  addlabels = TRUE,
  barfill = "steelblue",
  barcolor = "darkblue"
)

#PCA Variable Plot
fviz_pca_var(
  pca_result,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
)

#PCA Biplot
fviz_pca_biplot(
  pca_result,
  repel = TRUE,
  col.var = "#2E9FDF",
  col.ind = "#696969",
  geom.ind = "point",
  label = "var",
  alpha.ind = 0.2
)


# PCA Biplot Focused View as the default biplot is off center

fviz_pca_biplot(
  pca_result,
  repel = TRUE,
  col.var = "#2E9FDF",
  col.ind = "#696969",
  geom.ind = "point",
  label = "var",
  alpha.ind = 0.2
) +
  coord_cartesian(
    xlim = c(-6, 4),
    ylim = c(-4, 5)
  )


# PCA Visualisation by room type Comparing PC1 scores across accommodation categories
pca_room_scores <- data.frame(
  PC1 = pca_result$x[, 1],
  room_type = pca_complete$room_type
)

ggplot(pca_room_scores,
       aes(x = room_type, y = PC1, fill = room_type)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_cartesian(ylim = c(-6, 4)) +
  labs(
    title = "PC1 Scores by Airbnb Room Type (Focused View)",
    x = "Accommodation Type",
    y = "PC1 Score (44% of Total Variance)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

table(pca_complete$room_type)


# PCA Heatmap by Local Authority Area
# Standardise PC scores for comparison
pca_area_scores <- data.frame(
  area = pca_complete$neighbourhood_cleansed,
  scale(pca_result$x[, 1:4])
)

# Calculate average PC scores for each area
pca_area_means <- aggregate(
  pca_area_scores[, c("PC1", "PC2", "PC3", "PC4")],
  by = list(area = pca_area_scores$area),
  FUN = mean
)

# Reshape the data for visualisation
pca_area_long <- reshape2::melt(
  pca_area_means,
  id.vars = "area",
  variable.name = "Component",
  value.name = "MeanScore"
)

# Create the heatmap
ggplot(pca_area_long,
       aes(x = Component, y = area, fill = MeanScore)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(MeanScore, 2)), size = 4) +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "PCA Scores Across Dublin Local Authority Areas",
    x = "Principal Component",
    y = "Local Authority Area",
    fill = "Mean PC Score"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())


# PCA Variable Contributions
# Contributions to Principal Component 1
fviz_contrib(
  pca_result,
  choice = "var",
  axes = 1,
  top = 7
)

# Contributions to Principal Component 2
fviz_contrib(
  pca_result,
  choice = "var",
  axes = 2,
  top = 7
)



# Cumulative Explained Variance
# Supporting PCA Component Selection
variance <- pca_result$sdev^2
cumulative_variance <- cumsum(variance) / sum(variance) * 100

variance_data <- data.frame(
  Component = 1:7,
  CumulativeVariance = cumulative_variance
)

ggplot(variance_data,
       aes(x = Component, y = CumulativeVariance)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "darkblue", size = 3) +
  geom_hline(yintercept = 85, linetype = "dashed",
             color = "red") +
  geom_point(data = subset(variance_data, Component == 4),
             color = "red", size = 4) +
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    title = "Cumulative Explained Variance - Dublin Airbnb",
    subtitle = "Four components retain 86.8% of total variance",
    x = "Number of Principal Components",
    y = "Cumulative Variance Explained (%)"
  ) +
  theme_minimal()

#PCA Dimensionality Reduction

# Selecting the first four principal components
# These components explain approximately 86.8% of total variance
pca_reduced <- as.data.frame(pca_result$x[, 1:4])

# Checking dimensions of the reduced dataset
dim(pca_reduced)

# Checking for missing values
colSums(is.na(pca_reduced))

# Previewing the reduced dataset
head(pca_reduced)

# PCA Individuals Plot
fviz_pca_ind(
  pca_result,
  col.ind = "darkblue",
  geom = "point"
)
### Interpretation of PCA Results

# PCA was applied to seven numerical Airbnb features using 4,925 listings with complete observations.

# The first principal component (PC1) explains approximately 44% of the total variance.
# Price, accommodation capacity, bedrooms and beds have the strongest contributions to this component.
# These variables reflect differences in listing size and price.

# The second principal component (PC2) explains approximately 18.7% of the total variance.
# Number of reviews, minimum nights and availability contribute most strongly to this component.
# PC2 contrasts review activity with minimum-stay requirements and listing availability.

# The first four principal components explain approximately 86.8% of the total variance.

# As a result, I retained four components to reduce dimensionality while preserving most of the information
# represented by the original numerical variables.

# The reduced dataset contains 4,925 observations and four principal components instead of seven numerical features.

# PCA simplifies the numerical dataset and highlights relationships between Airbnb listing characteristics.

# However, some information is lost during dimensionality reduction, and extreme observations may still influence
# the principal components.


# References
# Iqbal, M. (2026) Data Exploration and Preparation: Lecture Notes and Tutorial Materials
# CCT College Dublin. 

# The techniques used throughout this project were studied
# through the module lectures, tutorials and class examples.
# I adapted These techniques and applied them to the
# Dublin Airbnb dataset.
