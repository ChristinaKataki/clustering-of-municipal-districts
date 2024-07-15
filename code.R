# Necessary libraries.
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(viridis)
library(magrittr)
library(stats) 
library(purrr)  
library(ggfortify)
library(factoextra)
library(gridExtra)
library(reshape2)
library(cluster)

##############################################
##
## Clean the data
##
##############################################

# Load the data.
data <- read.csv('population.csv', header = TRUE, check.names = FALSE)
print(data)

# Identify columns that contain '2000'.
cols_to_remove <- grep("2000", names(data), value = TRUE)

# Remove these columns because they have no records. 
data <- data[ , !(names(data) %in% cols_to_remove)]

# Now remove "2022" from the column names in order to have "cleaner" headers. 
names(data) <- gsub("2022", "", names(data))

# Keep rows where the first column has the word 'Municipality'.
municipality_df <- data[data$'Geographic Group' == 'Municipality', ]

# Remove the first column.
municipality_df <- subset(municipality_df, select = -1)

# Change the name of the first column.
names(municipality_df)[1] <- "Municipality Name"

# Remove commas from all columns in the dataset.
municipality_df <- municipality_df %>%
    mutate_all(~ gsub(",", "", .))

# Convert the age groups columns and the 'Total' column to numeric.  
municipality_df <- municipality_df %>%
    mutate(across(2:ncol(municipality_df), as.numeric))

# Check for NAs.
sum(is.na(municipality_df)) # No missing values in the dataset.

colnames(municipality_df)

# Trim whitespace from column names.
colnames(municipality_df) <- trimws(colnames(municipality_df))


##############################################
##
## Exploratory Data Analysis
##
##############################################

# Summary statistics for numeric columns.
numeric_summary <- summary(municipality_df[, 2:ncol(municipality_df)])

## Plot the distribution based on each age group.
# Remove the first two columns.
first_plot <- municipality_df[, -c(1:2)]

# Sum each column.
column_sums <- colSums(first_plot, na.rm = TRUE)
print(column_sums)

# Convert to a data frame.
sums_df <- data.frame(Age_Group = names(column_sums), Total = column_sums)

# Make the Age_Group column a factor.
sums_df$Age_Group <- factor(sums_df$Age_Group, levels = names(column_sums))

# Plot the distribution.
ggplot(sums_df, aes(x = Age_Group, y = Total)) +
    geom_bar(stat = "identity", fill = "#008080") +
    theme_minimal() +
    labs(title = "Total Population by Age Group", x = "Age Group", y = "Total Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Map plot to show the most common age group in each municipality.
# Remove the second column to exclude the 'Total' column when finding the maximum value in each row.
plot2_data <- subset(municipality_df, select = -2)

# Find the maximum value and the corresponding age group for each row.
results <- apply(plot2_data[, -1], 1, function(row) {
    max_value <- max(row, na.rm = TRUE)
    max_age_group <- colnames(plot2_data[, -1])[which.max(row)]
    c(max_value, max_age_group)
})

# Convert the results to a data frame.
results_df <- as.data.frame(t(results), stringsAsFactors = FALSE)
colnames(results_df) <- c("Max_Value", "Age_Group")

# Add the municipality names.
results_df$Municipality <- plot2_data$Municipality
results_df

# Load the map data.
portugal_map <- st_read('Master AUEB/Statistical Machine Learning/Second project/municipalities-portugal')

portugal_map$con_name_up[portugal_map$con_name_up == "Praia da VitÃ³ria"] <- "Vila da Praia da VitÃ³ria"
portugal_map$con_name_up[portugal_map$con_name_up == "Calheta de SÃ£o Jorge"] <- "Calheta [R.A.A.]"
portugal_map$con_name_up[portugal_map$con_name_up == "Calheta"] <- "Calheta [R.A.M.]"

# Merge the results.
merged_data <- portugal_map %>%
    left_join(results_df, by = c("con_name_up" = "Municipality"))

# The bounding box for mainland Portugal.
xlim <- c(-9.5, -6)  
ylim <- c(36.5, 42)  

ggplot(data = merged_data) +
    geom_sf(aes(fill = Age_Group), color = "white", size = 0.1) +
    scale_fill_viridis_d(option = "viridis", na.value = "grey50") +  
    coord_sf(xlim = xlim, ylim = ylim) +
    theme_minimal() +
    labs(title = "Most Common Age Group in Each Municipality of Portugal",
         fill = "Age Group") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())

## Normalized Heatmap of Age Group Distribution for Top 10 Municipalities.
# Identify the top 10 municipalities by total population.
top_10_municipalities <- municipality_df %>%
    arrange(desc(Total)) %>%
    head(10)

# Normalize the data by dividing each age group population by the total population for that municipality.
normalized_top_10 <- top_10_municipalities %>%
    mutate(across(`(0-5)`:`85 or more`, ~ . / Total))

# Convert data to long format, excluding the 'Total' column.
long_normalized_top_10 <- melt(normalized_top_10, id.vars = "Municipality Name", variable.name = "Age_Group", value.name = "Population")

# Remove the 'Total' row from the age groups.
long_normalized_top_10 <- long_normalized_top_10[long_normalized_top_10$Age_Group != "Total", ]

# Plot the Heatmap.
ggplot(long_normalized_top_10, aes(x = Age_Group, y = `Municipality Name`, fill = Population)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#008080") +
    theme_minimal() +
    labs(title = "Normalized Heatmap of Age Group Distribution for Top 10 Municipalities", x = "Age Group", y = "Municipality") +
    theme(axis.text.y = element_text(size = 6),  # Adjust the size of the municipality names for better readability
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks = element_blank(),
          panel.grid = element_blank())


##############################################
##
##  Cluster Analysis
##
##############################################

# Scale the data.
non_numeric_cols <- municipality_df[, 1, drop = FALSE]
nrow(non_numeric_cols)
numeric_cols <- municipality_df[, -1]
numeric_cols_scaled <- scale(municipality_df[, -1])
municipality_df_scaled <- cbind(non_numeric_cols, scale(numeric_cols))

# Check that we get mean of 0 and sd of 1.
colMeans(numeric_cols_scaled)  # faster version of apply(numeric_cols_scaled, 2, mean)
apply(numeric_cols_scaled, 2, sd)

##############################################
## K-means Clustering
##############################################

set.seed(42)

# Elbow method.
fviz_nbclust(numeric_cols_scaled, kmeans, method = "wss", k.max = 10)

# Silhouette method.
fviz_nbclust(numeric_cols_scaled,
             kmeans, 
             method = "silhouette") 

# Gap Statistic method.
fviz_nbclust(numeric_cols_scaled, 
             kmeans,
             nstart = 25, 
             method = "gap_stat")

# K-means clustering with three clusters.
kmeans_result <- kmeans(numeric_cols_scaled, centers = 3, nstart = 25)

# Cluster Plot.
fviz_cluster(kmeans_result, geom = "point",  data = numeric_cols_scaled) + ggtitle("k = 3")

# Cluster centers.
kmeans_result$centers

# Silhouette score.
sil <- silhouette(kmeans_result$cluster, dist(numeric_cols_scaled))

# Plot silhouette.
fviz_silhouette(sil)

# Average silhouette width.
cat("Average silhouette width:", mean(sil[, 3]))

# Create a new dataset with clustering results.
clustered_data <- data.frame(Municipality = municipality_df_scaled$Municipality, cluster = kmeans_result$cluster)

# Merge clustering results with the geographical data
merged_kmeans <- portugal_map %>%
    left_join(clustered_data, by = c("con_name_up" = "Municipality"))

# The bounding box for mainland Portugal.
xlim <- c(-9.5, -6)  
ylim <- c(36.5, 42)

# Plot the map with ggplot2.
ggplot(data = merged_kmeans) +
    geom_sf(aes(fill = factor(cluster)), color = "black", size = 0.1) +
    scale_fill_viridis_d() +
    coord_sf(xlim = xlim, ylim = ylim) +
    theme_minimal() +
    labs(title = "Clusters in Municipalities of Portugal",
         fill = "Cluster") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())

##############################################
## Hierarchical Clustering
##############################################

# Set seed for reproducibility.
set.seed(42)

# Since all the values here are continuous numerical values, the euclidean distance will be used.
# Calculate the Euclidean distance.
distance <- dist(numeric_cols_scaled, method = 'euclidean')

# List of clustering methods to evaluate.
clustering_methods <- c("average", "ward.D2", "complete", "single")
mean_silhouettes <- numeric(length(clustering_methods))
silhouettes <- list()

# Bounding box for mainland Portugal.
xlim <- c(-9.5, -6)
ylim <- c(36.5, 42)

par(mfrow = c(2,2))
# Î•mpty list to store plots.
plot_list <- list()

# Hierarchical clustering and silhouette analysis for each method.
for (i in seq_along(clustering_methods)) {
    hc <- hclust(distance, method = clustering_methods[i])
    clusters <- cutree(hc, 2)
    sil <- silhouette(clusters, distance)
    silhouettes[[i]] <- sil
    mean_silhouettes[i] <- mean(sil[, 3])

    cat(mean_silhouettes[i], clustering_methods[i], '\n')
    
    # Plot dendrograms.
    dendrogram_plot <- plot(hc, main = paste("Dendrogram (", clustering_methods[i], ")", sep = ""), xlab = "", sub = "")
    
    # Create a data frame with cluster assignments.
    cluster_assignments <- data.frame(
        Municipality = municipality_df_scaled$Municipality,
        Cluster = clusters
    )
    
    # Merge the clustering results with the map data.
    merged_hclust <- portugal_map %>%
        left_join(cluster_assignments, by = c("con_name_up" = "Municipality"))
    
    # Plot map.
    map_plot <- ggplot(data = merged_hclust) +
        geom_sf(aes(fill = factor(Cluster)), color = "black", size = 0.1) +
        scale_fill_viridis_d() +
        coord_sf(xlim = xlim, ylim = ylim) +
        theme_minimal() +
        labs(title = paste("Clusters in Municipalities of Portugal (", clustering_methods[i], ")", sep = ""),
             fill = "Cluster") +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank())
    
    plot_list[[i]] <- map_plot
}

# Arrange the map plots in a grid.
grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)
