
#------------------------------------------------------------------------------
## Data Loading ##
#------------------------------------------------------------------------------
setwd("F:\\ERASMUS-ESE\\Thesis\\datasets\\DUNN")

transaction_data <- read.csv("transaction_data.csv")
hh_demographic <- read.csv("hh_demographic.csv")
product <- read.csv("product.csv")
coupon <- read.csv("coupon.csv")
coupon_redempt <- read.csv("coupon_redempt.csv")
campaign_desc <- read.csv("campaign_desc.csv")
campaign_table <- read.csv("campaign_table.csv")
#-----------------------------------------------------------------------------#

###########################---- MERGING 1 ------###############################

#----- STEP 1 : MERGING

# Merge campaign_desc and campaign_table
merged_campaign <- merge(campaign_desc, campaign_table, 
                         by = c("CAMPAIGN", "DESCRIPTION"), all = TRUE)
#-----------------------------------------


#----- STEP 2 : CALCULATE UNIQUE CAMPAIGNS 

# Check the result
str(merged_campaign)
sum(is.na(merged_campaign))


# Calculate the number of unique campaigns per household
merged_campaign <- merged_campaign %>%
  group_by(household_key) %>%
  mutate(total_campaigns = n_distinct(CAMPAIGN))
#-----------------------------------------


#----- STEP 3 : CALCULATE DURATION OF CAMPAIGN

# Calculate the difference between END_DAY and START_DAY(Duratiion of campaign)
merged_campaign$duration_of_unique_camp <- merged_campaign$END_DAY - merged_campaign$START_DAY
#-----------------------------------------


#----- STEP 4 : REMOVE COLUMNS

# Specify the columns i want to remove
columns_to_remove <- c("START_DAY", "END_DAY")

# Remove the columns
merged_campaign <- merged_campaign[, 
                                   !(names(merged_campaign) %in% columns_to_remove)]
#-----------------------------------------


#----- STEP 5: CLEAN ENVIRONMENT

remove(campaign_desc, campaign_table)
#-----------------------------------------


#----- STEP 6 : RENAME THE COLUMNS

# Check the result
str(merged_campaign)
sum(is.na(merged_campaign))

#rename the column campaign
merged_campaign <- merged_campaign %>%
  rename(CAMPAIGN_id = CAMPAIGN) 

#rename the column campaign
merged_campaign <- merged_campaign %>%
  rename(CAMPAIGN_type = DESCRIPTION) 
#-----------------------------------------
############################---------####################################


###################################---- 2 ------#######################
#----- STEP 1: MERGING

# Merge coupon and coupon_redempt
merged_coupon <- merge(coupon_redempt, coupon, by = c("COUPON_UPC"),
                       all.x = TRUE)
#-----------------------------------------


#----- STEP 2 

# Check the result
str(merged_coupon)
sum(is.na(merged_coupon))

#Calculate the number of unique COUPON_UPC per product
merged_coupon<- merged_coupon %>%
  group_by(PRODUCT_ID) %>%
  mutate(coupons_redeem_prod = n_distinct(COUPON_UPC))
#-----------------------------------------


#----- STEP 3

# Calculate the number of unique COUPON_UPC per household
merged_coupon <- merged_coupon %>%
  group_by(household_key) %>%
  mutate(coupons_redeem = n_distinct(COUPON_UPC))
#-----------------------------------------


#----- STEP 4

# Specify the columns i want to remove
columns_to_remove <- c("COUPON_UPC", "DAY","CAMPAIGN.x",
                       "CAMPAIGN.y","PRODUCT_ID" )

# Remove the columns
merged_coupon <- merged_coupon[,!(names(merged_coupon) %in% 
                                    columns_to_remove)]
#-----------------------------------------


#----- STEP 5

#CLEAN ENVIRONMENT
remove(coupon, coupon_redempt)
#-----------------------------------------
########################---------------###########################


########################---- 3------###############################

#----- STEP 1

# Merge campaign and coupon data with household demographics by household_key
demo_campaign <- merge(hh_demographic, merged_campaign, 
                       by = "household_key", all.x = TRUE)
#-----------------------------------------


#----- STEP 2

# Renaming variables
demo_campaign <- demo_campaign %>%
  rename(
    AGE = AGE_DESC,
    MARITAL = MARITAL_STATUS_CODE,
    INCOME = INCOME_DESC,
    HH_MEMBERS = HH_COMP_DESC,
    HH_SIZE = HOUSEHOLD_SIZE_DESC,
  )


# Base R approach to recode the variable
demo_campaign$MARITAL <- factor(demo_campaign$MARITAL,
                                levels = c("A", "B", "U"),
                                labels = c("MARRIED", "SINGLE", "UNKNOWN"))

str(demo_campaign)
names(demo_campaign)

#-----------------------------------------


#----- STEP 3

str(demo_campaign)

sum(is.na(demo_campaign))

# Identify unique household_keys with NA in total_campaigns
unique_na_households <- demo_campaign %>%
  filter(is.na(total_campaigns)) %>%  # Filter rows where total_campaigns is NA
  distinct(household_key)             # Select unique household_keys

# Count the number of unique household_keys with NA in total_campaigns
num_unique_na_households <- nrow(unique_na_households)

# Print the result
print(num_unique_na_households)

# Count unique household keys
unique_households <- length(unique(demo_campaign$household_key))

# Print the result
print(paste("The number of unique households is:", unique_households))
#-----------------------------------------


#----- STEP 4

# Keep only unique household_keys
demo_campaign<- demo_campaign %>%
  distinct(household_key, .keep_all = TRUE)
#-----------------------------------------


#----- STEP 5

sum(is.na(demo_campaign))

# Summary of missing values by column
na_summary <- colSums(is.na(demo_campaign))

# Display the columns with missing values
na_summary[na_summary > 0]

# Extract rows with any missing values
rows_with_na <- demo_campaign[apply(is.na(demo_campaign), 1, any), ]
#-----------------------------------------


#----- STEP 6

remove(hh_demographic, merged_campaign)
#-----------------------------------------

############################################################################


########################----    4  ------###############################

#----- STEP 1

# Merge campaign and coupon data with household demographics by household_key
demo_campaign_coupon <- merge(demo_campaign, merged_coupon, by= "household_key", 
                              all.x = TRUE)
#-----------------------------------------

#----- STEP 2
str(demo_campaign_coupon)

sum(is.na(demo_campaign_coupon))

# Count unique household keys
unique_households <- length(unique(demo_campaign_coupon$household_key))

# Print the result
print(paste("The number of unique households is:", unique_households))
#-----------------------------------------

#----- STEP 3

# Keep only unique household_keys
demo_campaign_coupon<- demo_campaign_coupon %>%
  distinct(household_key, .keep_all = TRUE)
#-----------------------------------------

#----- STEP 4

sum(is.na(demo_campaign_coupon))

# Summary of missing values by column
na_summary <- colSums(is.na(demo_campaign_coupon))

# Display the columns with missing values
na_summary[na_summary > 0]
#-----------------------------------------

#----- STEP 5

remove(demo_campaign, merged_coupon)
#-----------------------------------------
############################################################################


###############################---- 5 ------#####################################
#----- STEP 1

# Replace NAs with 0 in these columns since they didnt redeemed coupons and 
#were not part of any CAMPAIGN from retailer

demo_campaign_coupon <- demo_campaign_coupon %>%
  mutate(
    total_campaigns = replace_na(total_campaigns, 0),
    duration_of_unique_camp = replace_na(duration_of_unique_camp, 0),
    coupons_redeem_prod = replace_na(coupons_redeem_prod, 0),
    coupons_redeem = replace_na(coupons_redeem, 0),
  )
#-----------------------------------------


#----- STEP 2

sum(is.na(demo_campaign_coupon))

# Replacing NA values with 'None' directly in a character column
demo_campaign_coupon$CAMPAIGN_type[is.na(demo_campaign_coupon$CAMPAIGN_type )] <- "None"

demo_campaign_coupon$CAMPAIGN_id[is.na(demo_campaign_coupon$CAMPAIGN_id)] <- "None"
#-----------------------------------------


#----- STEP 3

# Check if NAs are successfully replaced
na_summary <- colSums(is.na(demo_campaign_coupon))
print(na_summary)
#-----------------------------------------
############################################################################

########################---- 6 ------###############################
#----- STEP 1

# Merge campaign and coupon data with household demographics by household_key
transactions <- merge(transaction_data, demo_campaign_coupon, 
                      by = "household_key", all.x = TRUE)
#-----------------------------------------

#----- STEP 2

sum(is.na(transactions))

# Summary of missing values by column
na_summary <- colSums(is.na(transactions))

# Display the columns with missing values
na_summary[na_summary > 0]
#-----------------------------------------

#----- STEP 3

remove(transaction_data, demo_campaign_coupon)
#-----------------------------------------
############################################################################

########################---- 7 ------###############################
#----- STEP 1
# Merge campaign and coupon data with household demographics by household_key
products_demo_camp_coupon <- merge(transactions, product, 
                                   by = "PRODUCT_ID", all.x = TRUE)
#-----------------------------------------

#----- STEP 2
sum(is.na(products_demo_camp_coupon))

# Summary of missing values by column
na_summary <- colSums(is.na(products_demo_camp_coupon))

# Display the columns with missing values
na_summary[na_summary > 0]
#-----------------------------------------

#----- STEP 3
# Count unique household keys
unique_households <- length(unique(products_demo_camp_coupon$household_key))

# Print the result
print(paste("The number of unique households is:", unique_households))
#-----------------------------------------

#----- STEP 4

remove(transactions, product)
#-----------------------------------------
###########################---------################################


######################-------- 8  -----------#########################
#------SPLIT GROCERY TO SUB-DEPARTMENTS SINCE IT DOMINATES

#----- STEP 1
# Update the DEPARTMENT column based on the COMMODITY_DESC within the GROCERY department
products_demo_camp_coupon_1 <- products_demo_camp_coupon%>%
  mutate(DEPARTMENT = case_when(
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("FLUID MILK PRODUCTS", 
                                                    "CHEESE", "HOT CEREAL", "SUGARS/SWEETNERS", "YOGURT", 
                                                    "COLD CEREAL", "BUTTER", "MARGARINES", 
                                                    "FLOUR & MEALS",
                                                    "MILK BY-PRODUCTS", "EGGS","MISC. DAIRY", 
                                                    "CANNED MILK") ~ "Grocery_Dairy_And_Alternatives",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("SOFT DRINKS", 
                                                    "REFRGRATD JUICES/DRNKS","TEAS", 
                                                    "FRZN JCE CONC/DRNKS", "CANNED JUICES", "COFFEE", 
                                                    "TEA", "JUICE", "ISOTONIC DRINKS", 
                                                    "WATER - CARBONATED/FLVRD DRINK") ~ "Grocery_Beverages",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("BAKED BREAD/BUNS/ROLLS", 
                                                    "BAGELS", "BAKED SWEET GOODS", "CAKE/PASTRIES", 
                                                    "SANDWICH PRODUCTS", "PNT BTR/JELLY/JAMS",
                                                    "BAKING NEEDS", "BAKING MIXES") ~ "Grocery_Bakery",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("FROZEN PIZZA", 
                                                    "FROZEN BREAKFAST FOODS","FRZN MEAT/MEAT DINNERS", 
                                                    "FRZN VEGETABLE/VEG DSH", "FROZEN CHICKEN", 
                                                    "FROZEN BREAD/DOUGH", 
                                                    "FROZEN SNACKS", "FROZEN DAIRY", "FRZN POTATOES", 
                                                    "FRZN ICE CREAM/MILK/SHERBETS", "FROZEN PIE/DESSERTS", 
                                                    "FRZN NOVELTIES/WTR ICE", "FRZN ICE CONC/DRNKS", 
                                                    "FRZN FRUITS", "FRZN SEAFOOD", "FRZN BREAKFAST FOODS",
                                                    "FRZN ICE", "ICE CREAM/MILK/SHERBTS") ~ "Grocery_Frozen_Foods",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("BAG SNACKS", 
                                                    "CRACKERS/MISC BKD FD", "POPCORN", "COOKIES/CONES", 
                                                    "CHIPS", "DRY SNACKS", "FRUIT SNACKS", "SNACK NUTS", 
                                                    "CONVENIENT BRKFST/WHLSM SNACKS", 
                                                    "WAREHOUSE SNACKS") ~ "Grocery_Snacks_And_Convenience_Foods",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("CONDIMENTS/SAUCES", 
                                                    "SPICES & EXTRACTS", "DRY SAUCES/GRAVY",
                                                    "PICKLE/RELISH/PKLD VEG", "SYRUPS/TOPPINGS",
                                                    "MOLASSES/SYRUP/PANCAKE MIXES", 
                                                    "DRY BN/VEG/POTATO/RICE") ~ "Grocery_Condiments_Sauces_And_Spices",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("VEGETABLES - SHELF STABLE",
                                                    "FRUIT - SHELF STABLE", "MEAT - SHELF STABLE", 
                                                    "CANNED MEAT", "REFRGRATD DOUGH PRODUCTS", 
                                                    "SEAFOOD - SHELF STABLE", "BEANS - CANNED GLASS & MW",
                                                    "SUGARS/SWEETENERS", "SOUP", "DRY NOODLES/PASTA", 
                                                    "PASTA SAUCE", "BOTTLE DEPOSITS", "RESTRICTED DIET", 
                                                    "SALD DRSNG/SNDWCH SPRD", 
                                                    "HISPANIC") ~ "Grocery_Canned_And_Shelf_Stable_Goods",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("HOUSEHOLD CLEANG NEEDS", 
                                                    "PAPER TOWELS", "FACIAL TISS/DNR NAPKIN", "SHORTENING/OIL", 
                                                    "BLEACH", "AIR CARE", "LAUNDRY DETERGENTS", 
                                                    "LAUNDRY ADDITIVES", "DISHWASH DETERGENTS", "BATH TISSUES", 
                                                    "PAPER HOUSEWARES", 
                                                    "FD WRAPS/BAGS/TRSH BG") ~ "Grocery_Cleaning",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("DOG FOODS", 
                                                    "CAT FOOD", "PET CARE SUPPLIES", "CAT LITTER", 
                                                    "BIRD SEED") ~ "Grocery_Pet_Supplies",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("DOMESTIC WINE", 
                                                    "IMPORTED WINE", "MISC WINE","LIQUOR", 
                                                    "BEERS/ALES") ~ "Grocery_Alcohol",
    
    DEPARTMENT == "GROCERY" & COMMODITY_DESC %in% c("CANDY - GUM", 
                                                    "PREPARED FOODS","DRY MIX DESSERTS", "DINNER MXS:DRY", 
                                                    "PWDR/CRYSTL DRNK MX",
                                                    "COUPON/MISC ITEMS", "NEW AGE", "OLIVES", "COCOA MIXES",
                                                    "MOLASSES/SYRUP/PANCAKE MIXS") ~ "Grocery_Miscellaneous",
    TRUE ~ DEPARTMENT  # Keep the existing department if no match
  ))
#-----------------------------------------

#----- STEP 2
# View the updated dataset
print(head(products_demo_camp_coupon_1))
table(products_demo_camp_coupon_1$DEPARTMENT)
#-----------------------------------------
############################-------------------#########################

############################-------  9 ------------#########################
#-------IDENTIFY THE EMPTY DEPARTMENT AND FIX IT 

str(products_demo_camp_coupon_1)
table(products_demo_camp_coupon_1$DEPARTMENT)

#-----STEP 1
# Replace empty levels in the DEPARTMENT column with "DEPARTMENT_UNKNOWN"
products_demo_camp_coupon_1 <- products_demo_camp_coupon_1 %>%
  mutate(DEPARTMENT = ifelse(DEPARTMENT == " ", "DEPARTMENT_UNKNOWN", DEPARTMENT))
#-----------------------------------------

#-----STEP 2
# Filter the rows where DEPARTMENT is DEPARTMENT_UNKNOWN
unknown_department_rows <- products_demo_camp_coupon_1 %>%
  filter(DEPARTMENT == "DEPARTMENT_UNKNOWN")

# Get the unique COMMODITY_DESC values corresponding to DEPARTMENT_UNKNOWN
unknown_commodity_desc <- unique(unknown_department_rows$COMMODITY_DESC)

# Print the unique COMMODITY_DESC values
cat("COMMODITY_DESC values corresponding to DEPARTMENT_UNKNOWN:\n")
print(unknown_commodity_desc)
#-----------------------------------------

#-----STEP 3
# Replace empty cells in the COMMODITY_DESC column with "UNKNOWN"
products_demo_camp_coupon_1 <- products_demo_camp_coupon_1 %>%
  mutate(COMMODITY_DESC = ifelse(COMMODITY_DESC == "", "UNKNOWN", 
                                 COMMODITY_DESC))

# Verify the change
table(products_demo_camp_coupon_1$COMMODITY_DESC)
remove(unknown_department_rows)
#-----------------------------------------
############################-------------------#########################


########################---- 10 ------###############################
#THIS IS THE FIRST VERSION OF FINAL DATAEST 
final_dataset <- products_demo_camp_coupon_1
###########################---------###############################


########################---- 11 ------###############################
#-----STEP 1
#-----Calculate Total Prod Purchased and Remove Product ID
str(final_dataset)

# Add total_prod_purchased column to the dataset
final_dataset <- final_dataset %>%
  group_by(household_key) %>%
  mutate(total_products_purchased = sum(!is.na(QUANTITY)))

# Ungroup the data to avoid grouped operations later
final_dataset <- final_dataset %>% ungroup()
#-----------------------------------------

#-----STEP 2
# Specify the columns i want to remove
columns_to_remove <- c("MANUFACTURER", "BRAND", "STORE_ID",
                       "CURR_SIZE_OF_PRODUCT","START_DAY",
                       "END_DAY","SUB_COMMODITY_DESC", "COMMODITY_DESC")

# Remove the columns
final_dataset <- final_dataset[,!(names(final_dataset) %in% columns_to_remove)]
#-----------------------------------------
############################--------#########################

###################---------- 12 ---------------#########################
#-----STEP 1
str(final_dataset)

# Adjusted code to handle zero quantities or other zero values safely
final_dataset <- final_dataset %>%
  mutate(
    paid_per_product = ifelse(
      QUANTITY == 0 | SALES_VALUE == 0,  # Check for zero quantity
      0,              # If quantity is zero, set paid per product to zero
      (SALES_VALUE - COUPON_DISC) / QUANTITY  # Otherwise, perform the calculation
    )
  )
#-----------------------------------------

#-----STEP 2
# Calculate total paid per basket_id per household
final_dataset <- final_dataset %>%
  group_by(BASKET_ID) %>%
  mutate(total_paid_per_trans = sum(paid_per_product)) %>%
  ungroup()
#-----------------------------------------
############################-------------------#########################

###################---------- 13---------------#########################
#-----STEP 1
# Create a new column "total_discount_per_hh" by summing RETAIL_DISC
final_dataset <- final_dataset %>%
  group_by(household_key) %>%
  mutate(total_discount_per_household = sum(COUPON_DISC, na.rm = TRUE)) %>%
  ungroup()
#-----------------------------------------

#-----STEP 2
# Multiply only negative values in the total_discount column by -1
final_dataset <- final_dataset %>% 
  mutate(total_discount_per_household = ifelse(total_discount_per_household < 0, 
                                               total_discount_per_household * -1, 
                                               total_discount_per_household))
#-----------------------------------------

#-----STEP 3
# Calculate important variables 
final_dataset <- final_dataset %>%
  group_by(household_key) %>%
  mutate(
    avg_paid_per_trans = mean(total_paid_per_trans, na.rm = TRUE),
    avg_disc_per_household = mean(total_discount_per_household, na.rm = TRUE)
  ) %>%
  ungroup()  # Remove the grouping
#-----------------------------------------

#-----STEP 4
# calculate the total quantity of products purchased by each unique household 
#for each unique BASKET_ID
final_dataset <- final_dataset %>%
  group_by(household_key) %>%
  # Calculate the number of unique transactions per household
  mutate(total_transactions = n_distinct(BASKET_ID)) %>%
  # Calculate average products purchased per transaction
  mutate(avg_products_purchased = total_products_purchased / total_transactions) %>%
  ungroup()
#-----------------------------------------

#-----STEP 5
# Calculate the days since last purchase for each household
final_dataset <- final_dataset %>%
  group_by(household_key) %>%
  mutate(last_purchase_day = max(DAY, na.rm = TRUE)) %>%  # Find the last purchase day per household
  mutate(days_since_last_purchase = 712 - last_purchase_day) %>%  # Calculate days since last purchase
  ungroup()
#-----------------------------------------

#-----STEP 6
# Specify the columns i want to remove
columns_to_remove <- c("last_purchase_day")

# Remove the columns
final_dataset <- final_dataset[,!(names(final_dataset) %in% columns_to_remove)]
#-----------------------------------------
############################--------#########################


########################---- 14------###############################
#-----STEP 1

# Convert DEPARTMENT into dummy variables
dpt_dummies_data<- final_dataset %>%
  group_by(household_key, DEPARTMENT) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = DEPARTMENT, 
              values_from = Count, 
              values_fill = list(Count = 0)) %>%
  rename_with(~ paste("DEPARTMENT", ., sep = "_"), -household_key)
#-----------------------------------------


#-----STEP 2
# Merge the DEPARTMENT  dummy variables back to the final dataset
final_dataset <- merge(final_dataset, dpt_dummies_data, by = "household_key", 
                       all.x = TRUE)
#-----------------------------------------

#-----STEP 3
# Specify the columns i want to remove
columns_to_remove <- c("DEPARTMENT")

# Remove the columns
final_dataset <- final_dataset[,!(names(final_dataset) %in% columns_to_remove)]
#-----------------------------------------


#-----STEP 4
remove(dpt_dummies_data)
#-----------------------------------------
############################-------------------#########################



###########################---- 15 ----################################
#=------------STEP 1

# Calculate the frequency of each department
department_frequency <- table(products_demo_camp_coupon_1$DEPARTMENT)

# Convert the table to a data frame for easier viewing
department_frequency_df <- as.data.frame(department_frequency)

# Rename the columns for clarity
colnames(department_frequency_df) <- c("Department", "Frequency")

# Sort the data frame by Frequency in descending order
department_frequency_df <- department_frequency_df[order(-department_frequency_df$Frequency), ]


# Print the sorted data frame
print(department_frequency_df)

#=--------------------


#=------------STEP 2

final_dataset_updated <- final_dataset %>%
  mutate(
    DEPT_GRO_DAIRY = `DEPARTMENT_Grocery_Dairy_And_Alternatives`,
    
    DEPT_HEALTH = `DEPARTMENT_DRUG GM` + `DEPARTMENT_COSMETICS` +
      `DEPARTMENT_MISC SALES TRAN` + `DEPARTMENT_HBC` * 2 +
      `DEPARTMENT_PHARMACY SUPPLY` + `DEPARTMENT_RX`,
    
    DEPT_PRODUCE = `DEPARTMENT_PRODUCE`,
    
    DEPT_GRO_SAUCES_SPICES = `DEPARTMENT_Grocery_Condiments_Sauces_And_Spices`,
    
    DEPT_CAN_SHELF_GOODS = `DEPARTMENT_Grocery_Canned_And_Shelf_Stable_Goods`,
    
    DEPT_BEVERAGES = `DEPARTMENT_Grocery_Beverages`,
    
    DEPT_FROZEN_FOODS = `DEPARTMENT_Grocery_Frozen_Foods` +
      `DEPARTMENT_FROZEN GROCERY`,
    
    DEPT_SNACKS_CONV_FOODS = `DEPARTMENT_Grocery_Snacks_And_Convenience_Foods`,
    
    DEPT_BAKERY = `DEPARTMENT_Grocery_Bakery` + `DEPARTMENT_GRO BAKERY`,
    
    DEPT_PERSONAL_CARE = `DEPARTMENT_COSMETICS`,
    
    DEPT_MEAT = `DEPARTMENT_MEAT-PCKGD` + `DEPARTMENT_MEAT` +
      `DEPARTMENT_MEAT-WHSE` + `DEPARTMENT_PORK`,
    
    DEPT_CLEANING = `DEPARTMENT_Grocery_Cleaning` + `DEPARTMENT_HOUSEWARES`,
    
    DEPT_DELI = `DEPARTMENT_DELI` + `DEPARTMENT_DAIRY DELI` + 
      `DEPARTMENT_DELI/SNACK BAR` + 
      `DEPARTMENT_Grocery_Condiments_Sauces_And_Spices`,
    
    DEPT_PASTRY = `DEPARTMENT_PASTRY`,
    
    DEPT_PET_SUPPLIES = `DEPARTMENT_Grocery_Pet_Supplies`,
    
    DEPT_ALCOHOL = `DEPARTMENT_Grocery_Alcohol` + `DEPARTMENT_SPIRITS`,
    
    DEPT_SEAFOOD = `DEPARTMENT_SEAFOOD` + `DEPARTMENT_SEAFOOD-PCKGD`,
    
    DEPT_SALAD_BAR = `DEPARTMENT_SALAD BAR`,
    
    DEPT_KIOSK_GAS = `DEPARTMENT_KIOSK-GAS`,
    
    DEPT_ENT_ELECTR = `DEPARTMENT_PHOTO` + `DEPARTMENT_TOYS` + 
      `DEPARTMENT_VIDEO RENTAL` + `DEPARTMENT_ELECT &PLUMBING` +
      `DEPARTMENT_VIDEO`,
    
    DEPT_FLORAL = `DEPARTMENT_FLORAL` + `DEPARTMENT_GARDEN CENTER`,
    
    DEPT_MISC = `DEPARTMENT_MISC. TRANS.` + `DEPARTMENT_POSTAL CENTER` + 
      `DEPARTMENT_TRAVEL & LEISUR` + `DEPARTMENT_COUP/STR & MFG` + 
      `DEPARTMENT_CHEF SHOPPE` + `DEPARTMENT_RESTAURANT` + 
      `DEPARTMENT_AUTOMOTIVE` + `DEPARTMENT_GM MERCH EXP` +
      `DEPARTMENT_CNTRL/STORE SUP` + `DEPARTMENT_PROD-WHS SALES` + 
      `DEPARTMENT_CHARITABLE CONT` + `DEPARTMENT_POSTAL CENTER` + 
      `DEPARTMENT_DEPARTMENT_UNKNOWN` + `DEPARTMENT_Grocery_Miscellaneous` + 
      `DEPARTMENT_MISC SALES TRAN` + `DEPARTMENT_MISC. TRANS.` + 
      `DEPARTMENT_FLORAL`
  ) %>%
  # Removing the original department columns
  select(-matches("^DEPARTMENT_"))
#=--------------------
############################--------#########################

######################--- 16 --#########################

#-----------FINAL GROUPING
final_dataset_updated <- final_dataset_updated %>%
  mutate(
    # Group 1: Grocery
    DEPART_GROCERY_ESSENTIALS = DEPT_GRO_DAIRY + DEPT_CAN_SHELF_GOODS + 
      DEPT_BEVERAGES + DEPT_SNACKS_CONV_FOODS + DEPT_CLEANING ,
    
    # Group 2: Perishables & Fresh Foods
    DEPART_PERISHABLES_FRESH_FOODS = DEPT_PRODUCE + DEPT_MEAT + DEPT_SEAFOOD + 
      DEPT_DELI + DEPT_BAKERY + DEPT_FROZEN_FOODS + DEPT_PASTRY + 
      DEPT_GRO_SAUCES_SPICES,
    
    # Group 3: Health & Wellness
    DEPART_HEALTH_WELLNESS = DEPT_HEALTH + DEPT_ALCOHOL + DEPT_PERSONAL_CARE,
    
    # Group 4: Specialty & Miscellaneous
    DEPART_SPECIALTY_MISC = DEPT_FLORAL + DEPT_SALAD_BAR + 
      DEPT_KIOSK_GAS + DEPT_ENT_ELECTR + DEPT_MISC + DEPT_PET_SUPPLIES
  ) %>%
  select(-starts_with("DEPT_")) # Optionally remove original DEPT columns if not needed
#=--------------------
############################--------#########################


############################--- 17 -----#########################

#----- Convert department counts to fractions 
# Convert department counts to fractions using total_prod_purchased and 

final_dataset_updated_1 <- final_dataset_updated %>%
  mutate(
    DEPART_GROCERY_ESSENTIALS = ifelse(total_products_purchased == 0, 0,
                                       DEPART_GROCERY_ESSENTIALS / total_products_purchased),
    
    DEPART_PERISHABLES_FRESH_FOODS = ifelse(total_products_purchased == 0, 0,
                                            DEPART_PERISHABLES_FRESH_FOODS / total_products_purchased),
    
    DEPART_HEALTH_WELLNESS = ifelse(total_products_purchased == 0, 0, 
                                    DEPART_HEALTH_WELLNESS / total_products_purchased),
    
    DEPART_SPECIALTY_MISC = ifelse(total_products_purchased == 0, 0, 
                                   DEPART_SPECIALTY_MISC / total_products_purchased)
  )

names(final_dataset_updated_1)

#THIS IS THE FINAL VERSION OF DATASET
FINAL_DATASET <-final_dataset_updated_1
#remove PRODUCT_ID since is useless anymore
FINAL_DATASET <- FINAL_DATASET[,-2]
#=--------------------

############################--------#########################


###############---- CLEAN ENVIRONMENT ----#########################

rm(department_frequency_df, final_dataset, final_dataset_updated,
   products_demo_camp_coupon, products_demo_camp_coupon_1,
   rows_with_na, unique_na_households)

rm(columns_to_remove, department_frequency_df, na_summary,
   num_unique_na_households, department_frequency,
   unique_households, unknown_commodity_desc,final_dataset_updated_1)
############################--------#########################


#####################---- Dataset For Plotting----#########################

plot_data <- FINAL_DATASET
names(plot_data)

# Filter rows where specified demographic fields are not NA
plot_data <- FINAL_DATASET %>%
  filter(
    !is.na(AGE),
    !is.na(MARITAL),
    !is.na(INCOME),
    !is.na(HOMEOWNER_DESC),
    !is.na(HH_MEMBERS),
    !is.na(HH_SIZE),
    !is.na(KID_CATEGORY_DESC)
  )

# Keeping only the first unique row for each household_key
plot_data <- plot_data %>%
  distinct(household_key, .keep_all = TRUE)

############################--------###########################3######

