#########################------------------##########################
#--------------K-MEANS ON Low Engagers -------------------#
#########################------------------##########################


#---Further cluster  Recent Moderate Shoppers combined with the other variables 
#------------  STEP 1 
str(rfm_k_means)

# Filter the dataset for "Sleeping Giants"
low_engagers <- rfm_k_means %>%
  filter(cluster_label == "Low Engagers")

# View the result
print(low_engagers)

# Merge sleeping_giants and rfm_data data with household and other keys
low_engagers<- merge(low_engagers, rfm_data, 
                     by = c("household_key","Recency","Frequency","Monetary"), 
                     all.x = TRUE)

#-------------------#


#---Keep only numerical variables for clustering

#------------  STEP 2  

# Keep only the numerical columns
low_engagers_num <- low_engagers %>% select(where(is.numeric))

# View the structure of the resulting dataset
str(low_engagers_num)
names(low_engagers_num)
#-------------------#


#------------  STEP 3

# Specify the columns to remove
columns_to_remove <- c("cluster", "BASKET_ID", "DAY",
                       "QUANTITY","SALES_VALUE", "RETAIL_DISC",
                       "TRANS_TIME", "WEEK_NO", "COUPON_MATCH_DISC", 
                       "days_since_last_purchase","COUPON_DISC",
                       "total_transactions","total_discount_per_household")


# Remove the columns
low_engagers_num <- low_engagers_num[,!(names(low_engagers_num) %in% 
                                          columns_to_remove)]

#-------------------#



#------------  STEP 4

#------- Check the correlation of numerical variables with plot

# Check the structure of the updated dataset
names(low_engagers_num)

cor_matrix <- cor(low_engagers_num[ ,-c(1)])

# Create a correlogram
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"),
           title = "Correlogram of Numerical Variables",
           ggtheme = theme_minimal(),
           hc.order = FALSE, # Disable hierarchical clustering order
           legend.title = "Corr",
           show.legend = TRUE)


#---------- CHECK CORRELATION Linear 
##Linear regression to check the correlation of numerical before K-Means

# Fit the linear regression model
linear_model <- lm(Frequency ~ ., data = low_engagers_num[ ,-c(1)])

# View the summary of the model
summary(linear_model)

vif_values <- vif(linear_model)
print(vif_values)

#---------------------------


#--------------REMOVE IDENTIFIED USELESS VARIABLES FROM LINEAR
# Specify the columns to remove
columns_to_remove <- c("duration_of_unique_camp",
                       "coupons_redeem_prod", "total_paid_per_trans",
                       "avg_paid_per_trans", "avg_disc_per_household",
                       "total_products_purchased")

# Remove the columns
low_engagers_remove <- low_engagers_num[,!(names(low_engagers_num) %in% 
                                             columns_to_remove)]

#---------------------------


#------------  STEP 5

#---------IDENTIFY AND HANDLE OUTLIERS
str(low_engagers_remove)
names(low_engagers_remove)

# Create boxplots for a few selected columns
ggplot(low_engagers_remove, aes(x = "", y = paid_per_product)) +
  geom_boxplot() +
  labs(title = "Boxplot for paid_per_product")


# Define the IQR multiplier (1.5 is standard)
iqr_multiplier <- 1.5

# Function to identify outliers based on IQR
identify_outliers <- function(column) {
  Q1 <- quantile(column, 0.05, na.rm = TRUE)
  Q3 <- quantile(column, 0.95, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - iqr_multiplier * IQR
  upper_bound <- Q3 + iqr_multiplier * IQR
  return(column < lower_bound | column > upper_bound)
}

names(low_engagers_remove)

# Apply the outlier detection function across all numeric columns
outliers_df <- low_engagers_remove %>%
  mutate(across(where(is.numeric), identify_outliers, .names = "outlier_{col}"))

# Count the number of outliers for each row
outlier_summary <- outliers_df %>%
  mutate(total_outliers = rowSums(select(., starts_with("outlier_"))))

# Filter rows with outliers
outlier_rows_iqr <- outlier_summary %>%
  filter(total_outliers > 0)

# Create the new dataset by filtering out rows where total_outliers is greater than 0
low_engagers_clean <- outlier_summary %>% filter(total_outliers == 0)

low_engagers_clean <- low_engagers_clean[, -c(13:25)]

##------------------------------


#------------  STEP 6

#APPLCATION OF K_MEANS

# scale the numeric variables before applying k-means
K_means_num_low_engagers_sc <- low_engagers_clean
K_means_num_low_engagers_sc[, -1]<- scale(K_means_num_low_engagers_sc[, -1])
str(K_means_num_low_engagers_sc)


#K-Means

set.seed(123)
max_k <- 20
wss <- numeric(max_k)
for (k in 1:max_k) {
  km <- kmeans(K_means_num_low_engagers_sc[, -1], centers = k, nstart = 100, 
               iter.max = 500, algorithm = "MacQueen")
  wss[k] <- km$tot.withinss
}


##-------Elbow Method
plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)", ylab = "Total within-clusters sum of squares (WSS)",
     main = "Elbow Method for Optimal K")



##---------Validate Clusters
library(NbClust)
nb <- NbClust(data = K_means_num_low_engagers_sc[, -1], diss = NULL, 
              distance = "euclidean", min.nc = 2, max.nc = 10, 
              method = "kmeans", index = "all", alphaBeale = 0.1)

# Extract the number of clusters proposed by each index
cluster_counts <- table(nb$Best.nc["Number_clusters", ])




##------ Apply K-means clustering method (after selecting optimal k)
set.seed(123)
km.out <- kmeans(K_means_num_low_engagers_sc[, -1], centers = 3 , nstart = 100, 
                 iter.max = 500)
print(km.out)


#--------- Visualize clustering

set.seed(123)
km.clusters <- km.out$cluster
K_means_num_recent_sc <- as.data.frame(K_means_num_low_engagers_sc)
rownames(K_means_num_low_engagers_sc) <- K_means_num_low_engagers_sc$household_key
fviz_cluster(list(data = K_means_num_low_engagers_sc, cluster= km.clusters))

#-------- View the distribution of clusters
table(km.out$cluster)



#------------  STEP 7
#-------------------------------------------------------------------------------
## Unscale and plot the means Low Engagers
#-------------------------------------------------------------------------------

# Assuming km.out is your kmeans result
low_engagers_clean$cluster <- km.out$cluster

# Calculating means for scaled numeric variables
low_engagers_means <- aggregate(low_engagers_clean[, -1], 
                                by = list(cluster = low_engagers_clean$cluster), mean)


# Melt the data for plotting
melted_low_engagers<- melt(low_engagers_means, id.vars = "cluster")


# Get the list of variables to plot
variables_to_plot <- unique(melted_low_engagers$variable)


# Create a list to store plots
plots_list <- list()


# Loop through each variable and create a plot
for (var in variables_to_plot) {
  plot <- ggplot(subset(melted_low_engagers, variable == var), 
                 aes(x = factor(cluster), y = value, fill = factor(cluster))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Cluster", y = "Mean", fill = "Cluster", title = var)
  plots_list[[var]] <- plot
  print(plot)
}


# All in one plot
# Create a single ggplot object with facets for each variable
combined_plot <- ggplot(melted_low_engagers, 
                        aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cluster", y = "Mean", fill = "Cluster")


# Print the combined plot
print(combined_plot)


#------------  STEP 8
#-------------------------------------------------------------------------------
# Radial Plots Low Engagers
#-------------------------------------------------------------------------------
# Visualization of continuous numeric variables

str(low_engagers_clean)
names(low_engagers_clean)

# Assuming cluster is the grouping variable you use
data_continuous_low_engagers <- low_engagers_clean

# Add cluster for grouping
data_continuous_low_engagers$cluster <- low_engagers_clean$cluster

str(data_continuous_low_engagers)


data_continuous_radial_low_engagers <- data_continuous_low_engagers[,-c(1)]
str(data_continuous_radial_low_engagers)


# Normalize the data
#remove "cluster" variable from normalizing
data_continuous_radial_low_engagers[,-13] <- as.data.frame(lapply(data_continuous_radial_low_engagers[,-13], 
                                                                  function(x) (x - min(x)) / (max(x) - min(x))))

str(data_continuous_radial_low_engagers)

# Calculate the means for each cluster
cluster_means_low_engagers <- aggregate(. ~ cluster, data_continuous_radial_low_engagers, mean)


# Create data frame for radar chart
radar_data <- rbind(rep(1, ncol(cluster_means_low_engagers) - 1), rep(0, ncol(cluster_means_low_engagers) - 1), cluster_means_low_engagers[, -1])
radar_data <- cbind(cluster = c("max", "min", as.character(cluster_means_low_engagers$cluster)), radar_data)
rownames(radar_data) <- radar_data$cluster
radar_data$cluster <- NULL


# Set graphical parameters
par(bg = "black", col.axis = "white", col.lab = "white", fg = "white")

# Plotting
for (k in unique(cluster_means_low_engagers$cluster)) {
  cluster_data <- radar_data[c("max", "min", as.character(k)), ]
  radarchart(cluster_data, axistype = 1,
             paxis.labels = names(data_continuous_radial_low_engagers),  # Keep the variable names on the axes
             vlcex = 0.8, 
             cglcol = "white", 
             cglty = 1, 
             axislabcol = "white",
             caxislabels = rep("", 5),  # Hide concentric circle labels by using empty strings
             cglwd = 0.3, 
             pcol = "yellow", 
             plcol = "yellow", 
             plwd = 3)
  title(main = paste("Profile Cluster", k), col.main = "white", cex.main = 2)
}

# Reset graphical parameters
par(bg = "white", col.axis = "black", col.lab = "black", fg = "black")


#############------------------------------#########################



#############---------------- 24 -----------#########################
#--------------K-MEANS ON "Potential Loyalists" -------------------#
#########################------------------##########################


#---Further cluster the Sleeping Giants combined with the other variables 
#------------  STEP 1 
str(rfm_k_means)

# Filter the dataset for "Sleeping Giants"
potential<- rfm_k_means %>%
  filter(cluster_label == "Potential Loyalists")

# View the result
print(potential)

# Merge sleeping_giants and rfm_data data with household and other keys
potential<- merge(potential, rfm_data, 
                  by = c("household_key","Recency", "Frequency",
                         "Monetary"), all.x = TRUE)
#-------------------#


#---Keep only numerical variables for clustering

#------------  STEP 2  

# Keep only the numerical columns
potential_num <- potential %>%
  select(where(is.numeric))

# View the structure of the resulting dataset
str(potential_num)
names(potential_num)
#-------------------#


#------------  STEP 3

# Specify the columns to remove
columns_to_remove <- c("cluster", "BASKET_ID", "DAY",
                       "QUANTITY","SALES_VALUE", "RETAIL_DISC",
                       "TRANS_TIME", "WEEK_NO", "COUPON_MATCH_DISC", 
                       "days_since_last_purchase","COUPON_DISC",
                       "total_transactions","total_discount_per_household")


# Remove the columns
potential_num <- potential_num[,!(names(potential_num) %in% columns_to_remove)]

#-------------------#



#------------  STEP 4

#------- Check the correlation of numerical variables with plot

# Check the structure of the updated dataset
names(potential_num)

cor_matrix <- cor(potential_num[ ,-c(1)])

# Create a correlogram
ggcorrplot(cor_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"),
           title = "Correlogram of Numerical Variables",
           ggtheme = theme_minimal(),
           hc.order = FALSE, # Disable hierarchical clustering order
           legend.title = "Corr",
           show.legend = TRUE)


#---------- CHECK CORRELATION Linear 
##Linear regression to check the correlation of numerical before K-Means

# Fit the linear regression model
linear_model <- lm(Frequency ~ ., data = potential_num[ ,-c(1)])

# View the summary of the model
summary(linear_model)


vif_values <- vif(linear_model)
print(vif_values)


#-------------------#


#--------------REMOVE IDENTIFIED USELESS VARIABLES FROM LINEAR
# Specify the columns to remove
columns_to_remove <- c("duration_of_unique_camp",
                       "coupons_redeem_prod", "total_paid_per_trans",
                       "avg_paid_per_trans", "avg_disc_per_household",
                       "total_products_purchased")

# Remove the columns
potential_num_remove <- potential_num[,!(names(potential_num) %in% columns_to_remove)]

#---------------------------




#------------  STEP 5

#---------IDENTIFY AND HANDLE OUTLIERS
str(potential_num_remove)
names(potential_num_remove)

# Create boxplots for a few selected columns
ggplot(potential_num, aes(x = "", y = paid_per_product)) +
  geom_boxplot() +
  labs(title = "Boxplot for paid_per_product")


# Define the IQR multiplier (1.5 is standard)
iqr_multiplier <- 1.5

# Function to identify outliers based on IQR
identify_outliers <- function(column) {
  Q1 <- quantile(column, 0.05, na.rm = TRUE)
  Q3 <- quantile(column, 0.95, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - iqr_multiplier * IQR
  upper_bound <- Q3 + iqr_multiplier * IQR
  return(column < lower_bound | column > upper_bound)
}

names(potential_num_remove)

# Apply the outlier detection function across all numeric columns
outliers_df <- potential_num_remove %>%
  mutate(across(where(is.numeric), identify_outliers, .names = "outlier_{col}"))

# Count the number of outliers for each row
outlier_summary <- outliers_df %>%
  mutate(total_outliers = rowSums(select(., starts_with("outlier_"))))

# Filter rows with outliers
outlier_rows_iqr <- outlier_summary %>%
  filter(total_outliers > 0)

# Create the new dataset by filtering out rows where total_outliers is greater than 0
potential_num_clean <- outlier_summary %>% filter(total_outliers == 0)

potential_num_clean <- potential_num_clean[, -c(13:25)]

##------------------------------


#------------  STEP 6

#APPLCATION OF K_MEANS

# scale the numeric variables before applying k-means
K_means_potential_num_sc <- potential_num_clean
K_means_potential_num_sc[, -1]<- scale(K_means_potential_num_sc[, -1])
str(K_means_potential_num_sc)


#K-Means

set.seed(123)
max_k <- 20
wss <- numeric(max_k)
for (k in 1:max_k) {
  km <- kmeans(K_means_potential_num_sc[, -1], centers = k, nstart = 100, 
               iter.max = 500, algorithm = "MacQueen")
  wss[k] <- km$tot.withinss
}

##-------Elbow Method
plot(1:max_k, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)", ylab = "Total within-clusters sum of squares (WSS)",
     main = "Elbow Method for Optimal K")



##---------Validate Clusters
library(NbClust)
nb <- NbClust(data = K_means_potential_num_sc[, -1], diss = NULL, 
              distance = "euclidean", min.nc = 2, max.nc = 10, 
              method = "kmeans", index = "all", alphaBeale = 0.1)

# Extract the number of clusters proposed by each index
cluster_counts <- table(nb$Best.nc["Number_clusters", ])




##------ Apply K-means clustering method (after selecting optimal k)
set.seed(123)
km.out <- kmeans(K_means_potential_num_sc[, -1], centers = 3 , nstart = 100, 
                 iter.max = 500)
print(km.out)


#--------- Visualize clustering

set.seed(123)
km.clusters <- km.out$cluster
K_means_potential_num_sc <- as.data.frame(K_means_potential_num_sc)
rownames(K_means_potential_num_sc) <- K_means_potential_num_sc$household_key
fviz_cluster(list(data = K_means_potential_num_sc, cluster= km.clusters))

#-------- View the distribution of clusters
table(km.out$cluster)



#------------  STEP 7
#-------------------------------------------------------------------------------
## Unscale and plot the means Potential Loyalists
#-------------------------------------------------------------------------------

# Assuming km.out is your kmeans result
potential_num_clean$cluster <- km.out$cluster

# Calculating means for scaled numeric variables
potential_numeric_means <- aggregate(potential_num_clean[, -1], 
                                     by = list(cluster = potential_num_clean$cluster), mean)


# Melt the data for plotting
melted_potential <- melt(potential_numeric_means, id.vars = "cluster")


# Get the list of variables to plot
variables_to_plot <- unique(melted_potential$variable)


# Create a list to store plots
plots_list <- list()


# Loop through each variable and create a plot
for (var in variables_to_plot) {
  plot <- ggplot(subset(melted_potential, variable == var), 
                 aes(x = factor(cluster), y = value, fill = factor(cluster))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Cluster", y = "Mean", fill = "Cluster", title = var)
  plots_list[[var]] <- plot
  print(plot)
}


# All in one plot
# Create a single ggplot object with facets for each variable
combined_plot <- ggplot(melted_potential, 
                        aes(x = factor(cluster), y = value, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~variable, scales = "free_y", ncol = 4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Cluster", y = "Mean", fill = "Cluster")


# Print the combined plot
print(combined_plot)


#------------  STEP 8
#-------------------------------------------------------------------------------
# Radial Plots 
#-------------------------------------------------------------------------------
# Visualization of continuous numeric variables

str(potential_num_clean)
names(potential_num_clean)

# Assuming cluster is the grouping variable you use
data_continuous_potential <- potential_num_clean

# Add cluster for grouping
data_continuous_potential$cluster <- potential_num_clean$cluster

str(data_continuous_potential)


data_continuous_potential_radial <- data_continuous_potential[,-c(1)]
str(data_continuous_potential_radial)


# Normalize the data
data_continuous_potential_radial[,-13] <- as.data.frame(lapply(data_continuous_potential_radial[,-13], 
                                                               function(x) (x - min(x)) / (max(x) - min(x))))

str(data_continuous_potential_radial)

# Calculate the means for each cluster
cluster_means_potential <- aggregate(. ~ cluster, data_continuous_potential_radial, mean)


# Create data frame for radar chart
radar_data <- rbind(rep(1, ncol(cluster_means_potential) - 1), rep(0, ncol(cluster_means_potential) - 1), cluster_means_potential[, -1])
radar_data <- cbind(cluster = c("max", "min", as.character(cluster_means_potential$cluster)), radar_data)
rownames(radar_data) <- radar_data$cluster
radar_data$cluster <- NULL


# Set graphical parameters
par(bg = "black", col.axis = "white", col.lab = "white", fg = "white")

# Plotting
for (k in unique(cluster_means_potential$cluster)) {
  cluster_data <- radar_data[c("max", "min", as.character(k)), ]
  radarchart(cluster_data, axistype = 1,
             paxis.labels = names(data_continuous_potential_radial),  # Keep the variable names on the axes
             vlcex = 0.8, 
             cglcol = "white", 
             cglty = 1, 
             axislabcol = "white",
             caxislabels = rep("", 5),  # Hide concentric circle labels by using empty strings
             cglwd = 0.3, 
             pcol = "yellow", 
             plcol = "yellow", 
             plwd = 3)
  title(main = paste("Profile Cluster", k), col.main = "white", cex.main = 2)
}

# Reset graphical parameters
par(bg = "white", col.axis = "black", col.lab = "black", fg = "black")


#############------------------------------#########################