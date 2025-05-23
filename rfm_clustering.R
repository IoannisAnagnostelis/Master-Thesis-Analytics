

############################--- 18 -----#########################
##-------------- RFM 

#-STEP 1

#-Calculation of RFM Metrics

rfm_data <- FINAL_DATASET

#Filter Transactions and Aggregate RFM Metrics
# Filter out transactions with non-positive sales value
trans_valid <- rfm_data %>%
  filter(total_paid_per_trans > 0)

# Compute RFM metrics
rfm_data <- trans_valid %>%
  group_by(household_key) %>%
  mutate(
    Recency = 712 - max(DAY),  # Calculates how many days ago was the last purchase from day 712
    Frequency = n_distinct(BASKET_ID),  # Count unique BASKET_ID indicating distinct transactions
    Monetary = sum(total_paid_per_trans)  # Sums all sales values per customer
  )

names(rfm_data)

# Filter rows where specified demographic fields are not NA
rfm_data <- rfm_data %>%
  filter(
    !is.na(AGE),
    !is.na(MARITAL),
    !is.na(INCOME),
    !is.na(HOMEOWNER_DESC),
    !is.na(HH_MEMBERS),
    !is.na(HH_SIZE),
    !is.na(KID_CATEGORY_DESC)
  )

hist(rfm_data$Recency)
hist(rfm_data$Frequency)
hist(rfm_data$Monetary)

#------------#


#-STEP 2

# Keeping only the first unique row for each household_key
rfm_data <- rfm_data %>%
  distinct(household_key, .keep_all = TRUE)


sum(is.na(rfm_data))

# Summary of missing values by column
na_summary <- colSums(is.na(rfm_data))

# Display the columns with missing values
na_summary[na_summary > 0]

# Replacing NA values with 'None' directly in a character column
rfm_data$CAMPAIGN_type[is.na(rfm_data$CAMPAIGN_type)] <- "None"

# Verify the changes
sum(is.na(rfm_data$CAMPAIGN_type))  # Should return 0, indicating no more NAs

names(rfm_data)

# Replace NA, NaN, or Inf values with 0 in specified columns
rfm_data <- rfm_data %>%
  mutate(
    total_campaigns = replace(total_campaigns, is.na(total_campaigns) | is.nan(total_campaigns) | is.infinite(total_campaigns), 0),
    duration_of_unique_camp = replace(duration_of_unique_camp, is.na(duration_of_unique_camp) | is.nan(duration_of_unique_camp) | is.infinite(duration_of_unique_camp), 0),
    coupons_redeem_prod = replace(coupons_redeem_prod, is.na(coupons_redeem_prod) | is.nan(coupons_redeem_prod) | is.infinite(coupons_redeem_prod), 0),
    coupons_redeem = replace(coupons_redeem, is.na(coupons_redeem) | is.nan(coupons_redeem) | is.infinite(coupons_redeem), 0)
  )

sum(is.na(rfm_data))
#------------#

############################----- END OF RFM ---#########################
