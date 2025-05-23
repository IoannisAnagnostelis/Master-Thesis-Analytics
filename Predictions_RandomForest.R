










#############------------------------------#########################
########----------------PREDICTIONS-----------####################
#############------------------------------#########################

predictions_data <- rfm_data

str(predictions_data)
names(predictions_data)

# Specify the columns to remove
columns_to_remove <- c("BASKET_ID","QUANTITY","SALES_VALUE",
                       "RETAIL_DISC", "DAY", "total_products_purchased",
                       "TRANS_TIME","COUPON_MATCH_DISC","WEEK_NO",
                       "COUPON_DISC", "total_transactions","avg_paid_per_trans",
                       "Recency", "Frequency", "Monetary","avg_products_purchased",
                       "total_paid_per_trans","total_discount_per_household",
                       "DEPART_GROCERY_ESSENTIALS", "duration_of_campaign",
                       "DEPART_PERISHABLES_FRESH_FOODS", "DEPART_HEALTH_WELLNESS",
                       "DEPART_SPECIALTY_MISC", "coupons_redeem_prod",
                       "KID_CATEGORY_DESC", "HOMEOWNER_DESC")


# Remove the columns
predictions_data <- predictions_data[,!(names(predictions_data) %in%  
                                          columns_to_remove)]


#-----------Define Churning
# Create a new column 'Churned'

max(predictions_data$days_since_last_purchase)

predictions_data$Churn_Status <- ifelse(predictions_data$days_since_last_purchase > 21, 
                                        "Churned", "Not_Churned")


predictions_data <- predictions_data[,-12]


###############################################################################



############################################################################

# Renaming variables
predictions_data <- predictions_data %>%
  rename(
    AGE = AGE_DESC,
    MARITAL = MARITAL_STATUS_CODE,
    INCOME = INCOME_DESC,
    HH_MEMBERS = HH_COMP_DESC,
    HH_SIZE = HOUSEHOLD_SIZE_DESC,
    CAMP_TYPE = CAMPAIGN_type,
    TOTAL_CAMP = total_campaigns,
    TOTAL_COUPONS = total_coupons
  )


# Base R approach to recode the variable
predictions_data$MARITAL <- factor(predictions_data$MARITAL,
                                   levels = c("A", "B", "U"),
                                   labels = c("MARRIED", "SINGLE", "UNKNOWN"))

str(predictions_data)
names(predictions_data)


#############------------------------------#########################


##################################################################################

table(predictions_data$AGE)

# List of original and new column names for AGE
old_age_names <- c("AGE.19.24","AGE.25.34","AGE.35.44","AGE.45.54", "AGE.55.64", "AGE.65.")

new_age_names <- c("AGE 19-24","AGE 25-34", "AGE 35-44","AGE 45-54", "AGE 55-64", "AGE 65+")

# Rename columns in data_encoded
names(predictions_data)[names(predictions_data) %in% old_age_names] <- new_age_names

# Check the structure of the renamed dataset
str(predictions_data)




# List of original and new column names for AGE
old_size_names <- c("HH_SIZE1", "HH_SIZE2", "HH_SIZE3", "HH_SIZE4", "HH_SIZE5.")

new_size_names <- c("HH SIZE-1", "HH SIZE-2", "HH SIZE-3", "HH SIZE-4", "HH SIZE-5+")

# Rename columns in data_encoded
names(predictions_data)[names(predictions_data) %in% old_size_names] <- new_size_names

# Check the structure of the renamed dataset
str(predictions_data)



# List of original and new column names
old_names <- c("INCOME.100.124K", "INCOME.125.149K", "INCOME.15.24K", "INCOME.150.174K", 
               "INCOME.175.199K", "INCOME.200.249K", "INCOME.25.34K", "INCOME.250K.", 
               "INCOME.35.49K", "INCOME.50.74K", "INCOME.75.99K","INCOME.Under.15K")

new_names <- c("INCOME 100-124K", "INCOME 125-149K", "INCOME 15-24K", "INCOME 150-174K", 
               "INCOME 175-199K", "INCOME 200-249K", "INCOME 25-34K", "INCOME 250K+", 
               "INCOME 35-49K", "INCOME 50-74K", "INCOME 75-99K","INCOME Under 15K")

# Rename columns in data_encoded
names(predictions_data)[names(predictions_data) %in% old_names] <- new_names


# Check the structure of the renamed dataset
glimpse(predictions_data)

#############------------------------------#########################



#############------------------------------#########################


# Select only the variables you want to encode and keep the others as is
variables_to_encode <- c("AGE", "MARITAL", "INCOME", "HH_SIZE",
                         "HH_MEMBERS", 
                         "CAMP_TYPE")

# Prepare a dummyVars object for encoding only the specified variables
dummies <- dummyVars(~ AGE + MARITAL + INCOME+ HH_SIZE  + HH_MEMBERS + 
                       CAMP_TYPE, 
                     data = predictions_data, fullRank = FALSE)

# Apply the transformation to create dummy variables
encoded_vars <- predict(dummies, newdata = predictions_data)

# Combine the encoded variables with the rest of the original data (excluding the original variables that were encoded)
data_encoded <- cbind(predictions_data[, !names(predictions_data) %in% variables_to_encode], 
                      data.frame(encoded_vars))

# Check the structure of the new dataset
str(data_encoded)

#############------------------------------#########################







#############################################################################

glimpse(predictions_data)

#------------------------Plot of categorical against target -----------#

categorical_vars <- c("AGE", "MARITAL", "INCOME", "HH_MEMBERS", 
                      "HH_SIZE", "CAMP_TYPE")

plots <- lapply(categorical_vars, function(cat_var) {
  ggplot(predictions_data, aes_string(x = "factor(`cat_var`)", fill = "Churn_Status")) +
    geom_bar(position = "stack") +
    labs(title = paste("Bar Plot of", cat_var, "against Churn Status"),
         x = cat_var,
         y = "Count") +
    theme_minimal()
})


# Print the list of plots
print(plots)

# Combine all the plots into one
combined_plot <- grid.arrange(grobs = plots, ncol = 3) 

# Print the combined plot
print(combined_plot)
#------------------------------------------------------------------#

###########################################################################


#############################################################################

glimpse(predictions_data)

#------------------------Plot of numeric against target -----------#
numeric_vars <- c("TOTAL_CAMP", "TOTAL_COUPONS", "avg_disc_per_household")

# Create a list of plots for each numeric variable
numeric_plots <- lapply(numeric_vars, function(numeric_var) {
  ggplot(predictions_data, aes(x = Churn_Status, y = predictions_data[[numeric_var]], fill = Churn_Status)) +
    geom_boxplot() +
    labs(title = paste("Box Plot of", numeric_var, "against churn"),
         x = "Churn_Status",
         y = numeric_var) +
    theme_minimal()
})

# Print the list of numeric plots
print(numeric_plots)

# Combine all the numeric plots into one
combined_numeric_plot <- grid.arrange(grobs = numeric_plots, ncol = 3)  

# Print the combined numeric plot
print(combined_numeric_plot)
#------------------------------------------------------------------#

###########################################################################





#############################################################################

glimpse(predictions_data)


# Convert Churn_Status to binary format
predictions_data$Churn_Status <- ifelse(predictions_data$Churn_Status == "Churned", 1, 0)

# Check the conversion to ensure everything is correct
table(predictions_data$Churn_Status)


data_numerics <- predictions_data[,-c(1,13,12)]

# Numeric Columns Extraction
numerics <- unlist(lapply(data_numerics, is.numeric))
numerics_data <- data_numerics[, numerics]

# Fit the logistic regression model
dummy_model <-glm(Churn_Status ~ .,data = numerics_data,family = "binomial")
print(vif(dummy_model))

# # Let's have a better understanding about each feature through a correlation plot
options(repr.plot.width=10, repr.plot.height=7) 

nums <- select_if(numerics_data, is.numeric)

corr <- round(cor(nums), 1)

ggcorrplot(corr, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="square", 
           colors = c("tomato2", "white", "#01A9DB"), 
           title="Correlogram Employee Attritions", 
           ggtheme=theme_minimal())


####---REMOVE "PAID PER PRODUCT", AVG_DISCOUNT_PER_HOUSEHOLD 

###########################################################################



#############################################################################
########--------CHECK FOR MULTICOLLINEARITY CATEGORICALS----###############
# Check the relationship between the categorical variables(factors)

cramers_v <- function(x, y) {
  table <- table(x, y)
  chi_sq <- chisq.test(table)
  n <- sum(table)
  phi_sq <- chi_sq$statistic / n
  r <- nrow(table) - 1
  k <- ncol(table) - 1
  min <- min(r, k)
  v <- sqrt(phi_sq / (min * (n - 1)))
  p_value <- chi_sq$p.value
  return(c("Cramer's V" = v, "p-value" = p_value))
}

str(predictions_data)

factors_data <- predictions_data[,-c(1,10:14)]

glimpse(factors_data)


# Converting all columns to factors except for 'Churn_Status'
factors_data[] <- lapply(names(factors_data), function(col_name) {
  if (col_name != "Churn_Status") {
    return(as.factor(factors_data[[col_name]]))
  } else {
    return(factors_data[[col_name]])  # Return as is if it's 'Churn_Status'
  }
})


variable.combinations <- combn(names(factors_data)[sapply(factors_data, is.factor)], 2)

# Applying the Function to Variable Combinations
cramers_results <- apply(variable.combinations, 2, function(vars) {
  cat_vars <- lapply(vars, function(var_name) factors_data[[var_name]])
  cramer_v_and_p_value <- do.call(cramers_v, cat_vars)
  return(cramer_v_and_p_value)
})

# Convert the results to a data frame
cramers_results_df <- as.data.frame(t(cramers_results))

# Add meaningful column names
colnames(cramers_results_df) <- c("Cramer's V", "p-value")

# Add variable combinations as row names
rownames(cramers_results_df) <- apply(variable.combinations, 2, paste, 
                                      collapse = " & ")

# Print the results
print(cramers_results_df)

# Convert the row names to a new column for plotting
cramers_results_df$VariableCombination <- rownames(cramers_results_df)

# Reshape the data for plotting

barplot_data <- gather(cramers_results_df, Metric, Value, -VariableCombination)

ggplot(barplot_data, aes(x = VariableCombination, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Cramer's V Bar Plot",
       x = "Variable Combination",
       y = "Value",
       fill = "Metric") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# REMOVE CHILDREN, HOMEOWNERSHIP

##############################################################################

glimpse(data_encoded)

random_forest_data <- data_encoded[ ,-c(1,2,4,5)]



#############------------------------------#########################


set.seed(123)  # For reproducibility

data_balanced <- ovun.sample(Churn_Status ~ ., data = random_forest_data, method = "over", N = 2 * nrow(random_forest_data))$data

sum(is.na(data_balanced))

## shuffle data before splitting.
data_balanced <- data_balanced[sample(nrow(data_balanced)),]

# Splitting data into train and test data
index <- createDataPartition(data_balanced$Churn_Status, p = 0.65, list = FALSE)
train_data <- data_balanced[index, ]
test_data <- data_balanced[-index, ]

glimpse(data_balanced)

train_data$Churn_Status <- as.factor(train_data$Churn_Status)
test_data$Churn_Status <- as.factor(test_data$Churn_Status)

sum(is.na(train_data))
sum(is.na(test_data))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
glimpse(train_data)

# Initial Random Forest model
set.seed(123)
rf_model <- randomForest(Churn_Status ~., data = train_data, mtry = 4,
                         importance = TRUE, ntree = 500, do.trace = TRUE,
                         control = rpart.control(minsplit = 2, cp = 0))

rf_model

# Call:
#   randomForest(formula = ChurnStatus ~ ., data = train_data[, -1],      mtry = 4, importance = TRUE, ntree = 1000, do.trace = TRUE,      control = rpart.control(minsplit = 2, cp = 0)) 
# Type of random forest: classification
# Number of trees: 1000
# No. of variables tried at each split: 4

# OOB estimate of  error rate: 6.85%
# Confusion matrix:
#   Churn No_Churn class.error
# Churn    11747     2582  0.18019401
# No_Churn  1806    47885  0.03634461

# Find OOB error convergence to determine 'best' ntree
err_best.tree <- as.data.frame(rf_model$err.rate)
err_best.tree$ntree <- as.integer(rownames(err_best.tree))

ggplot(err_best.tree, aes(x = ntree, y = OOB))+
  geom_line(col = "black")+
  labs(title = "Random Forest (ntree) bootstrap samples", x = "Number of bootstrap samples",
       y = "OOB error")+
  geom_vline(xintercept = 100, linetype = "dotted", color = "red", size = 1)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        title = element_text(size = 17))
# optimal ntree 100 based on the plot

# Find 'best' mtry
mtry_err <- vector(length = 17)

# Run 17-times random forest model and obtain OOB estimated error per iteration
for(i in 1:17){
  set.seed(123)
  temp <- randomForest(Churn_Status ~., data = train_data, mtry = i,
                       importance = TRUE, ntree = 350, do.trace = TRUE,
                       control = rpart.control(minsplit = 2, cp = 0))
  mtry_err[i] <- temp$err.rate[100,1]
}

# Store all 17 OOB estimated errors in matrix
mtry <- c(1:17)
mtry_mx <- data.frame(cbind(mtry, mtry_err))

min(mtry_mx$mtry_err)
# [1] 0.06262106

# Visualize OOB error per mtry
ggplot(mtry_mx, aes(x = mtry, y = mtry_err))+
  geom_line(col = "black")+
  geom_point()+
  labs(title = "Random Forest (mtry) predictor parameter", x = "Number of (mtry) predictors",
       y = "OOB error")+
  geom_vline(xintercept = 14, linetype = "dotted", color = "red", size = 1)+
  theme(axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        title = element_text(size = 17))
# optimal mtry equals 9 based on the plot

# Final Random Forest model
set.seed(123)
rf_best <- randomForest(Churn_Status ~., data = train_data, mtry = 5,
                        importance = TRUE, ntree = 350, do.trace = TRUE, 
                        control = rpart.control(minsplit = 2, cp = 0))

print(rf_best)

# Call:
#   randomForest(formula = ChurnStatus ~ ., data = train_data[, -1],      mtry = 9, importance = TRUE, ntree = 100, do.trace = TRUE,      control = rpart.control(minsplit = 2, cp = 0)) 
# Type of random forest: classification
# Number of trees: 100
# No. of variables tried at each split: 9

# OOB estimate of  error rate: 6.26%
# Confusion matrix:
#   Churn No_Churn class.error
# Churn    12187     2142   0.1494871
# No_Churn  1867    47824   0.0375722

rf_predict <- predict(rf_best, test_data, type = "class")

confusionMatrix(rf_predict, test_data$Churn_Status)

# Confusion Matrix and Statistics

# Reference
# Prediction Churn No_Churn
# Churn     5260      800
# No_Churn   880    20496

# Accuracy : 0.9388          
# 95% CI : (0.9359, 0.9416)
# No Information Rate : 0.7762          
# P-Value [Acc > NIR] : < 2e-16         

# Kappa : 0.8229          

# Mcnemar's Test P-Value : 0.05393         

#             Sensitivity : 0.8567          
#             Specificity : 0.9624          
#          Pos Pred Value : 0.8680          
#          Neg Pred Value : 0.9588          
#              Prevalence : 0.2238          
#          Detection Rate : 0.1917          
#    Detection Prevalence : 0.2209          
#       Balanced Accuracy : 0.9096          

#        'Positive' Class : Churn


###########################################################################


#####################------ Feature Immportance ----------------###############

# Plot feature importance
plot(rf_best)

varImpPlot(rf_best)


##################################----------------------######################


