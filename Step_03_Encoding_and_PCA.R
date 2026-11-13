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


