############################---- 19 ----#########################
###########---  Create the dataset for K-Means from rfm_data --#########

# Create the new dataset by selecting specific columns
rfm_k_means <- rfm_data %>%
  select(household_key, Recency, Frequency, Monetary)

############################--------#########################

##############---------------- 20 ---------#########################
##--------------- Apply K-Means for RFM --------------------##

# scale the numeric variables before applying k-means
K_means_num_sc <- rfm_k_means
K_means_num_sc[, -1]<- scale(K_means_num_sc[, -1])
str(K_means_num_sc)


######### K-Means

set.seed(123)
max_k <- 20
wss <- numeric(max_k)
for (k in 1:max_k) {
  km <- kmeans(K_means_num_sc[, -1], centers = k, nstart = 100, iter.max = 500, algorithm = "MacQueen")
  wss[k] <- km$tot.withinss
}


##-------Elbow Method
plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)", ylab = "Total within-clusters sum of squares (WSS)",
     main = "Elbow Method for Optimal K")

##---------Validate Clusters
library(NbClust)
nb <- NbClust(data = K_means_num_sc[, -1], diss = NULL, 
              distance = "euclidean", min.nc = 2, max.nc = 10, 
              method = "kmeans", index = "all", alphaBeale = 0.1)

# Extract the number of clusters proposed by each index
cluster_counts <- table(nb$Best.nc["Number_clusters", ])

# Create a bar plot of these counts
barplot(cluster_counts, main = "Number of Clusters Voting", xlab = "Number of Clusters", ylab = "Votes", col = "blue")


##------ Apply K-means clustering method (after selecting optimal k)
set.seed(123)
km.out <- kmeans(K_means_num_sc[, -1], centers = 5 , nstart = 100, iter.max = 500)
print(km.out)


#--------- Visualize clustering

set.seed(123)
km.clusters <- km.out$cluster
K_means_num_sc <- as.data.frame(K_means_num_sc)
rownames(K_means_num_sc) <- K_means_num_sc$household_key
fviz_cluster(list(data = K_means_num_sc, cluster= km.clusters))

#-------- View the distribution of clusters
table(km.out$cluster)

##---------------------------------------------------##


#--------- Visualize clustering 3D PLot

library(plotly)

# Perform PCA to reduce data to 3 dimensions
pca_result <- prcomp(K_means_num_sc[, -1], scale. = TRUE) # Assuming clustering_data_sc[, -1] contains the relevant features
pca_data <- as.data.frame(pca_result$x[, 1:3]) # Select the first 3 principal components
pca_data$Cluster <- factor(km.clusters) # Add cluster assignments as a factor

# 3D Scatter plot using plotly
fig <- plot_ly(
  data = pca_data,
  x = ~PC1, y = ~PC2, z = ~PC3, 
  color = ~Cluster, 
  colors = "Set1", # You can choose a different color palette if desired
  type = "scatter3d", 
  mode = "markers+lines",
  marker = list(size = 4)
)

# Add titles and labels
fig <- fig %>%
  layout(
    scene = list(
      xaxis = list(title = 'PC1'),
      yaxis = list(title = 'PC2'),
      zaxis = list(title = 'PC3')
    ),
    title = "3D Visualization of Clusters"
  )

# Show the plot
fig
##---------------------------------------------------##
##############------------------------------#########################


##############---------------- 21 ---------#########################
#-------------------------------------------------------------------------------
## Unscale and plot the means for RFM
#-------------------------------------------------------------------------------

# Assuming km.out is your kmeans result
rfm_k_means$cluster <- km.out$cluster

# Calculating means for scaled numeric variables
numeric_means <- aggregate(rfm_k_means[, -1], 
                           by = list(cluster = rfm_k_means$cluster), mean)


# Melt the data for plotting
cluster_means_melted <- melt(numeric_means, id.vars = "cluster")


# Get the list of variables to plot
variables_to_plot <- unique(cluster_means_melted$variable)


# Create a list to store plots
plots_list <- list()


# Loop through each variable and create a plot
for (var in variables_to_plot) {
  plot <- ggplot(subset(cluster_means_melted, variable == var), 
                 aes(x = factor(cluster), y = value, fill = factor(cluster))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Cluster", y = "Mean", fill = "Cluster", title = var)
  plots_list[[var]] <- plot
  print(plot)
}


# All in one plot
# Create a single ggplot object with facets for each variable
combined_plot <- ggplot(cluster_means_melted, 
                        aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cluster", y = "Mean", fill = "Cluster")


# Print the combined plot
print(combined_plot)
#############------------------------------#########################


#############---------------- 22 -----------#########################
# First, ensure that your cluster labels are correctly applied
# You can manually define the labels based on the cluster means you've observed

cluster_labels <- c("Big Spenders","Low Engagers","Loyal Enthusiasts",
                    "Dormant Shoppers","Potential Loyalists")

names(cluster_labels) <- 1:5  # Assuming clusters are numbered from 1 to 6

# Map the labels to your data
rfm_k_means$cluster_label <- cluster_labels[as.character(rfm_k_means$cluster)]

# Calculate the proportion of each cluster
cluster_proportions <- prop.table(table(rfm_k_means$cluster_label)) * 100

# Create a data frame for plotting
df_plot <- data.frame(
  Cluster = names(cluster_proportions),
  Percentage = as.numeric(cluster_proportions)
)

# Order the clusters based on some logic or manual preference
df_plot$Cluster <- factor(df_plot$Cluster, levels = c("Big Spenders",
                                                      "Low Engagers","Loyal Enthusiasts",
                                                      "Dormant Shoppers","Potential Loyalists"))


#Pie chart 
ggplot(df_plot, aes(x = "", y = Percentage, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +  # This makes it a pie chart
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "right") +
  labs(fill = "Cluster", 
       title = "Distribution of Customer Clusters") +
  scale_fill_brewer(palette = "Set3")  # Provides distinct colors


# Calculate the cumulative percentages to position labels
df_plot <- df_plot %>%
  arrange(desc(Cluster)) %>%
  mutate(lab.y = cumsum(Percentage) - 0.5 * Percentage)

# Create pie chart and add labels
ggplot(df_plot, aes(x = "", y = Percentage, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +  # This converts the bar chart into a pie chart
  geom_text(aes(label = sprintf("%0.1f%%", Percentage), y = lab.y), color = "black", size = 3.5) +
  theme_void() +
  theme(legend.title = element_blank(), legend.position = "right") +
  labs(fill = "Cluster", 
       title = "Distribution of Customer Clusters") +
  scale_fill_brewer(palette = "Set3")  # Provides distinct colors


library(ggtext)  # For rich text in labels

# Enhanced Pie Chart using updated ggplot2 syntax
ggplot(df_plot, aes(x = "", y = Percentage, fill = Cluster)) +
  geom_bar(width = 1, stat = "identity", color = "white", linewidth = 0.5) +  # Update to 'linewidth'
  coord_polar(theta = "y") +
  geom_text(aes(label = sprintf("%0.1f%%", Percentage), y = cumsum(Percentage) - 0.5 * Percentage),
            color = "black", size = 4, fontface = "bold") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, hjust = 0.5)
  ) +
  labs(fill = "Cluster", title = "Distribution of Clusters") +
  scale_fill_brewer(palette = "Dark2")  # Using a distinct color palette

#########################------------------##########################
