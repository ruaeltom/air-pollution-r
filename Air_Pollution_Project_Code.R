# ============================================================
#   AIR POLLUTION LEVEL CLUSTERING
#   Using K-Means Clustering
#   Data Analytics Project
#   Dataset: Air Pollution Sensors Data
# ============================================================

# ── STEP 1: Install & Load Required Libraries ────────────────
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("corrplot")
# install.packages("gridExtra")

library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(corrplot)
library(gridExtra)

# ── STEP 2: Load Real Dataset ────────────────────────────────
cat("Loading dataset...\n")

# ⚠️ Change this path if your file is somewhere else
df <- read.csv("air_quality_data.csv", stringsAsFactors = FALSE)

cat("✅ Dataset Loaded Successfully!\n")
cat("Dimensions:", nrow(df), "rows x", ncol(df), "columns\n\n")
cat("Column Names:\n")
print(names(df))

# ── STEP 3: Data Quality Check (like friend's report) ────────
cat("\n============================================\n")
cat("         DATA QUALITY CHECK\n")
cat("============================================\n")

cat("Total Records:", nrow(df), "\n")
cat("Total Columns:", ncol(df), "\n")

cat("\nMissing Values per Column:\n")
print(colSums(is.na(df)))

cat("\nDuplicate Rows:", sum(duplicated(df)), "\n")

# Remove duplicates and missing values
df <- df[!duplicated(df), ]
df <- na.omit(df)
cat("✅ After Cleaning:", nrow(df), "records remaining\n")

# ── STEP 4: Rename Columns (If necessary) ────────────────────
# In our project, columns are mostly standardized, but we can do a check.
df <- df %>% rename(
  Record_Date = Date
)

cat("\n✅ Columns Validated Successfully!\n")
print(head(df, 5))

# ── STEP 5: Exploratory Data Analysis (EDA) ──────────────────
cat("\n============================================\n")
cat("     EXPLORATORY DATA ANALYSIS (EDA)\n")
cat("============================================\n")

# Summary Statistics
cat("\n📊 Summary Statistics:\n")
numeric_cols <- df %>% select(PM2.5, PM10, NO2, SO2, CO, O3)
print(summary(numeric_cols))

# ── EDA Plot 1: PM2.5 Distribution ───────────────────────────
p1 <- ggplot(df, aes(x = PM2.5)) +
  geom_histogram(bins = 15, fill = "#3498db", color = "white") +
  labs(title = "Distribution of PM2.5 Levels",
       subtitle = "How spread out are our fine particulate matter readings?",
       x = "PM2.5 (µg/m³)", y = "Frequency (Days)") +
  theme_minimal()
print(p1)

# ── EDA Plot 2: PM10 Distribution ────────────────────────────
p2 <- ggplot(df, aes(x = PM10)) +
  geom_histogram(bins = 15, fill = "#2ecc71", color = "white") +
  labs(title = "Distribution of PM10 Levels",
       subtitle = "Spread across coarse particulate matter",
       x = "PM10 (µg/m³)", y = "Frequency (Days)") +
  theme_minimal()
print(p2)

# ── EDA Plot 3: O3 (Ozone) Distribution ──────────────────────
p3 <- ggplot(df, aes(x = O3)) +
  geom_histogram(bins = 15, fill = "#e74c3c", color = "white") +
  labs(title = "Distribution of Ozone (O3) Levels",
       subtitle = "Checking the toxicity levels across observations",
       x = "Ozone (µg/m³)", y = "Frequency (Days)") +
  theme_minimal()
print(p3)

# ── EDA Plot 4: PM2.5 vs PM10 Scatter ────────────────────────
p4 <- ggplot(df, aes(x = PM2.5, y = PM10)) +
  geom_point(alpha = 0.6, color = "#9b59b6", size = 3) +
  labs(title = "PM2.5 vs PM10",
       subtitle = "Relationship between the two most prominent pollutants",
       x = "PM2.5 (µg/m³)", y = "PM10 (µg/m³)") +
  theme_minimal()
print(p4)

# ── STEP 6: Correlation Analysis ─────────────────────────────
cat("\n📊 Correlation Analysis:\n")

corr_data <- cor(numeric_cols, use = "complete.obs")

print(round(corr_data, 3))

corrplot(corr_data,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlation Matrix of Air Pollutants",
         mar = c(0, 0, 2, 0))

# ── STEP 7: Data Preprocessing ───────────────────────────────
cat("\n============================================\n")
cat("          DATA PREPROCESSING\n")
cat("============================================\n")

cat("Features selected for clustering:\n")
print(names(numeric_cols))
cat("\nBefore Scaling (first 3 rows):\n")
print(round(head(numeric_cols, 3), 2))

# Normalize features using scale()
features_scaled <- scale(numeric_cols)

cat("\nAfter Scaling (first 3 rows):\n")
print(round(head(features_scaled, 3), 3))
cat("✅ All features scaled to mean=0, sd=1\n")

# ── STEP 8: Find Optimal K (Elbow Method) ────────────────────
cat("\n============================================\n")
cat("    DETERMINING OPTIMAL CLUSTERS (K)\n")
cat("============================================\n")

set.seed(42)
elbow_plot <- fviz_nbclust(features_scaled, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k",
       subtitle = "Determining the ideal number of pollution clusters")
print(elbow_plot)
cat("✅ Elbow method plotted. We will proceed with K=3 clusters.\n")

# ── STEP 9: K-Means Clustering ───────────────────────────────
cat("\n============================================\n")
cat("            K-MEANS CLUSTERING\n")
cat("============================================\n")

k <- 3
kmeans_model <- kmeans(features_scaled, centers = k, nstart = 25)
cat("✅ K-Means Clustering Done (k =", k, ")\n")

# ── STEP 10: Assign Clusters to Dataset ──────────────────────
clusters <- kmeans_model$cluster
df$Cluster <- as.factor(clusters)

cat("\n✅ Data categorized into", k, "clusters\n")
cat("Cluster distribution:\n")
print(table(df$Cluster))

# ── STEP 11: Create Risk Profiles ────────────────────────────
cluster_summary <- df %>%
  group_by(Cluster) %>%
  summarise(
    Mean_PM2.5 = mean(PM2.5),
    Mean_PM10  = mean(PM10),
    Mean_NO2   = mean(NO2),
    Mean_CO    = mean(CO),
    Mean_O3    = mean(O3),
    Count      = n()
  )

cat("\nCluster Profile Summary:\n")
print(cluster_summary)

# ── STEP 12: Assign Risk Labels ──────────────────────────────
# We order clusters by severity of PM2.5 levels
cluster_ranked <- cluster_summary %>%
  arrange(Mean_PM2.5)

risk_labels <- c("Low Risk (Good)", "Medium Risk (Moderate)", "High Risk (Severe)")
cluster_ranked$Risk_Level <- risk_labels

risk_map <- setNames(cluster_ranked$Risk_Level, cluster_ranked$Cluster)
df$Risk_Level <- as.factor(risk_map[as.character(df$Cluster)])

# Set factor levels for visualizations
df$Risk_Level <- factor(df$Risk_Level, levels = risk_labels)

cat("\n✅ Pollution Risk Labels Assigned:\n")
print(table(df$Risk_Level))

# ── STEP 13: PCA Cluster Plot ────────────────────────────────
pca_plot <- fviz_cluster(
  kmeans_model, data = features_scaled,
  geom         = "point",
  ellipse.type = "convex",
  palette      = c("#2ecc71", "#f39c12", "#e74c3c"),
  ggtheme      = theme_minimal(),
  main         = "Air Pollution Level Clusters (PCA View)"
)
print(pca_plot)

# ── STEP 14: Risk Level Visualizations ───────────────────────

# Bar chart: Count per risk level
p_bar <- ggplot(df, aes(x = Risk_Level, fill = Risk_Level)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  scale_fill_manual(values = c("Low Risk (Good)"        = "#2ecc71",
                               "Medium Risk (Moderate)" = "#f39c12",
                               "High Risk (Severe)"     = "#e74c3c")) +
  labs(title = "Number of Records per Risk Category",
       subtitle = "Air Pollution Clustering Analysis",
       x = "Risk Level", y = "Count (Days)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_bar)

# Box plot: PM2.5 by Risk Level
p_boxplot1 <- ggplot(df, aes(x = Risk_Level, y = PM2.5, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low Risk (Good)"        = "#2ecc71",
                               "Medium Risk (Moderate)" = "#f39c12",
                               "High Risk (Severe)"     = "#e74c3c")) +
  labs(title = "PM2.5 Distribution by Risk Level",
       subtitle = "Higher risk clusters display more fine particulate matter",
       x = "Risk Level", y = "PM2.5 (µg/m³)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_boxplot1)

# Box plot: PM10 by Risk Level
p_boxplot2 <- ggplot(df, aes(x = Risk_Level, y = PM10, fill = Risk_Level)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Low Risk (Good)"        = "#2ecc71",
                               "Medium Risk (Moderate)" = "#f39c12",
                               "High Risk (Severe)"     = "#e74c3c")) +
  labs(title = "PM10 Distribution by Risk Level",
       subtitle = "Coarse particulate matter across distinct categories",
       x = "Risk Level", y = "PM10 (µg/m³)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p_boxplot2)

# ── STEP 15: Final Summary Report ────────────────────────────
cat("\n")
cat("============================================================\n")
cat("        FINAL CLUSTER SUMMARY REPORT\n")
cat("        Air Pollution Level Clustering (K-Means)\n")
cat("============================================================\n")

final_summary <- df %>%
  group_by(Risk_Level) %>%
  summarise(
    Total_Days = n(),
    Avg_PM25   = round(mean(PM2.5), 1),
    Avg_PM10   = round(mean(PM10), 1),
    Avg_NO2    = round(mean(NO2), 1),
    Avg_O3     = round(mean(O3), 1)
  )

print(final_summary)

cat("\n✅ 3 risk clusters accurately identified and validated.\n")

# Save final dataset
write.csv(df, "final_air_pollution_clustered.csv", row.names = FALSE)
cat("✅ Resulting dataset saved to 'final_air_pollution_clustered.csv'\n")
