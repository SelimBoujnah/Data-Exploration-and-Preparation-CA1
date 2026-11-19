# DEP CA1 - Dublin Airbnb Dataset
# Extra Visualisations for dataset investigation
# Run Step 1, Step 2 and Step 3 first.
# I explored these additional plots but did not include them in my poster.

# Extra price histograms
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

# Extra focused price boxplot
# Focused boxplot to better visualise price outliers
ggplot(airbnb_clean, aes(x = "All Listings", y = price_numeric)) +
  geom_boxplot(fill = "lightblue", color = "black", na.rm = TRUE) +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(title = "Dublin Airbnb Prices: Focused Boxplot",
       x = "",
       y = "Listed Price ($)")+ theme_minimal()

# Additional EDA correlation heatmap

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

# Additional PCA visualisations
# PCA Biplot
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

# Additional PCA individuals plot
# PCA Individuals Plot
fviz_pca_ind(
  pca_result,
  col.ind = "darkblue",
  geom = "point"
)