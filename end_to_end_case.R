library(tidyverse)
library(lubridate)


long <- read_csv('https://www.dropbox.com/s/9luewx3rekalme9/mh_long.csv?dl=1')
wide <- read_csv('https://www.dropbox.com/s/il32xg5lrgza06m/mh_wide.csv?dl=1')


# Your Code: --------------------------------------------------------------------------------------------

#Most duplicate names have the same zip so you can probably remove NAs based on name + zip match
# At least one row has Florida as the state instead of FL. Search for more (1 with Washington but no dupe. Figure out what to do with it)


#Remove duplicate NA columns based on the ID grouping and keeping the non-NA version based on Longitude
# The NA duplicates didn't have a longitude while the outlier Florida and Washington did.
wide_clean <- wide %>% 
  group_by(id) %>% 
  slice(which.max(!is.na(longitude))) %>% 
  mutate(addressRegion = ifelse(addressRegion == 'Florida','FL',ifelse(addressRegion == 'Washington','WA',addressRegion)),
         addressLocality = ifelse(is.na(addressLocality) & addressRegion == 'FL', 'Madison', ifelse(is.na(addressLocality) & addressRegion == 'WA', 'Allyn',addressLocality)),
         postalCode = ifelse(is.na(postalCode) & addressRegion == 'FL', '32340', ifelse(is.na(postalCode) & addressRegion == 'WA', '98524', postalCode))) %>% 
  janitor::clean_names()
  
# Check all columns for NA values (shouldn't be any in wide_clean)
colSums(is.na(wide_clean))


#NEED TO DO:
#3. Deal with duplicate price values in the long data (average if both are numbers, pick 1 if the 2nd value is "call for price")

# Remove non-numeric Price values
remove_calls <- long %>% 
  filter(value != 'Call for Price' | is.na(value))

# With 'Call for Price' removed, average the remaining duplicate values
average_price <- remove_calls %>% 
  filter(key == 'Price') %>% 
  mutate(value = parse_number(value)) %>% 
  group_by(id) %>% 
  summarise(key = 'Price', value= mean(value)) %>%
  mutate(value = as.character(value))

# re-bind the averaged rows resulting in clean Price data
remove_duplicates <- remove_calls %>% 
  filter(key != 'Price') %>% 
  bind_rows(average_price)

# Pivot longer and do all of the remaining cleanings
long_clean_pivoted <- remove_duplicates %>% 
  mutate(value = ifelse(key %in% c('Laundromat','Pool','Club House', 'Handicap Accessible', 'Storage'),1,value)) %>% 
  group_by(id) %>%
  distinct(key, .keep_all = TRUE) %>% ungroup() %>% 
  pivot_wider(
    names_from = key,
    values_from = value
  ) %>%
  janitor::clean_names() %>% 
  rename('has_laundromat'='laundromat',
         'has_pool'='pool' ,
         'has_club_house'='club_house',
         'has_handicap_accessible'='handicap_accessible',
         'has_storage'= 'storage',
         'total_occupancy_rate'='total_occupancy',
         'size_acres'='size',
         'price_usd'='price',
         'average_rent_for_park_owned_homes_usd' = 'average_rent_for_park_owned_homes',
         'average_mh_lot_rent_usd' = 'average_mh_lot_rent',
         'average_rv_lot_rent_usd' = 'average_rv_lot_rent') %>% 
  transform(number_of_mh_lots = as.numeric(number_of_mh_lots),
            singlewide_lots = as.numeric(singlewide_lots),
            number_of_park_owned_homes = as.numeric(number_of_park_owned_homes),
            doublewide_lots = as.numeric(doublewide_lots),
            number_of_rv_lots = as.numeric(number_of_rv_lots),
            posted_on = parse_date_time(posted_on, c('m d y')),
            updated_on = parse_date_time(updated_on, c('m d y'))) %>% 
  mutate(year_built = ifelse(str_detect(year_built,'\\d{5}'),parse_integer(substr(year_built,start = 1, stop = 4)), parse_integer(year_built)),
         price_usd = parse_number(price_usd),
         average_rent_for_park_owned_homes_usd = parse_number(average_rent_for_park_owned_homes_usd),
         average_mh_lot_rent_usd = parse_number(average_mh_lot_rent_usd),
         average_rv_lot_rent_usd = parse_number(average_rv_lot_rent_usd),
         interest_rate = parse_number(interest_rate) / 100,
         total_occupancy_rate = parse_number(total_occupancy_rate) / 100,
         cash = if_else(str_detect(purchase_method,'Cash'), 1, 0),
         new_loan = if_else(str_detect(purchase_method,'New Loan'), 1, 0),
         seller_financing = if_else(str_detect(purchase_method,'Seller Financing'), 1, 0), 
         assumable_loan = if_else(str_detect(purchase_method,'Assumable Loan'), 1, 0),
         size_acres = if_else(str_detect(size_acres, 'hectare'), parse_number(size_acres) * 2.47105381, parse_number(size_acres)))



# Join time!!!
# Join by ID, then add the age_years, price_per_lot_usd, and price_usd columns/fixes and final cleaning
together_clean <- long_clean_pivoted %>% 
  left_join(wide_clean, by = c('id' = 'id')) %>% 
  mutate(year_built = ifelse(year_built < 1000, NA, year_built),
         age_years = parse_integer(format(Sys.Date(), "%Y")) - year_built,
         price_per_lot_usd = price_usd / number_of_mh_lots,
         price_usd = ifelse(price_usd < 5, NA, price_usd))


# Visualizations

plot_1 <- together_clean %>% 
  ggplot(mapping = aes(x = price_usd)) +
  geom_density(alpha = 0.6, fill = 'blue') +
  theme_bw() +
  labs(title = "Density Distribution of Price (USD)",
       x = "Price USD",
       y = "Density")

plot_1

plot_2 <- together_clean %>% 
  ggplot(mapping = aes(x = log(price_usd))) +
  geom_density(alpha = 0.6, fill = 'blue') +
  theme_bw() +
  labs(title = "Density Distribution of Price (USD), Log Scale",
       x = "Price in USD, Log Scale",
       y = "Density") 

plot_2

plot_3 <- together_clean %>% filter(posted_on > "2022-01-01") %>% 
  ggplot(mapping = aes(x = posted_on)) +
  geom_histogram(alpha = 0.6, fill = 'red', bins = 30) +
  theme_bw() +
  labs(title = "Property Listings over Time",
       x = "Date Posted (30 bins)",
       y = "Count of Properties")  

plot_3

plot_4 <- together_clean %>% 
  ggplot(mapping = aes(x = age_years)) +
  geom_histogram(alpha = 0.6, fill = 'green', bins = 30) +
  theme_bw() +
  labs(title = "Property Age in Years",
       x = "Age (30 bins)",
       y = "Count of Properties")  

plot_4

plot_5 <- together_clean %>% 
  mutate(has_purchase_method = if_else(is.na(purchase_method), 'No','Yes')) %>% 
  ggplot(mapping = aes(x = address_region, fill = purchase_method)) +
  geom_bar() +
  facet_wrap(~has_purchase_method, ncol = 1, labeller = label_both) +
  theme_bw() +
  labs(title = "Property Listings by State and Purchase Method",
       x = "US State",
       y = "Count") 
  
plot_5

# Naming Checks -----------------------------------------------------------------------------------------

# To help us avoid object or column name issues, I've included the following tests that will only pass 
# if you have named your objects and columns exactly correct. 

# IMPORTANT: You should not edit any of the code in this section. It should be run as a big block and you 
# should see the test results at the end.

expected_cols_wide_clean <- c(
  'address_locality', 'address_region', 'id', 'latitude', 'longitude', 'postal_code', 
  'property_name', 'street_address'
)

expected_cols_long_clean_pivoted <- c(
  'assumable_loan', 'average_mh_lot_rent_usd', 'average_rent_for_park_owned_homes_usd', 
  'average_rv_lot_rent_usd', 'cash', 'community_type', 'debt_info', 'doublewide_lots', 
  'has_club_house', 'has_handicap_accessible', 'has_laundromat', 'has_pool', 'has_storage', 
  'id', 'interest_rate', 'new_loan', 'number_of_mh_lots', 'number_of_park_owned_homes', 
  'number_of_rv_lots', 'posted_on', 'price_usd', 'purchase_method', 'seller_financing', 
  'sewer', 'singlewide_lots', 'size_acres', 'total_occupancy_rate', 'water', 'water_paid_by', 
  'year_built'
)

expected_cols_together_clean <- c(
  'address_locality','address_region','age_years','assumable_loan','average_mh_lot_rent_usd',
  'average_rent_for_park_owned_homes_usd','average_rv_lot_rent_usd','cash','community_type',
  'debt_info','doublewide_lots','has_club_house','has_handicap_accessible','has_laundromat',
  'has_pool','has_storage','id','interest_rate','latitude','longitude','new_loan','number_of_mh_lots',
  'number_of_park_owned_homes','number_of_rv_lots','postal_code','posted_on','price_per_lot_usd',
  'price_usd','property_name','purchase_method','seller_financing','sewer','singlewide_lots','size_acres',
  'street_address','total_occupancy_rate','water','water_paid_by','year_built'
)

expected_object_names <- c('wide_clean', 'long_clean_pivoted', 'together_clean', 'plot_1', 
                           'plot_2', 'plot_3', 'plot_4', 'plot_5'
)


# Tests: All of these should return `TRUE`
expected_object_names_result <- sum(ls() %in% expected_object_names) == length(expected_object_names)

if(!expected_object_names_result){
  error_content <- paste(expected_object_names[!expected_object_names %in% ls()], collapse = ',')
  stop(paste("Expected objects not found in the environment:",error_content))
}

expected_cols_wide_clean_result <- sum(names(wide_clean) %in% expected_cols_wide_clean) == length(expected_cols_wide_clean)
expected_cols_long_clean_result <- sum(names(long_clean_pivoted) %in% expected_cols_long_clean_pivoted) == length(expected_cols_long_clean_pivoted)
expected_cols_together_clean_result <- sum(names(together_clean) %in% expected_cols_together_clean) == length(expected_cols_together_clean)


if(
  expected_object_names_result &
  expected_cols_wide_clean_result &
  expected_cols_long_clean_result &
  expected_cols_together_clean_result
){
  message('Congratulations. All naming tests passed.')
} else {
  if(!expected_cols_wide_clean_result){
    print(paste("Expected columns not found in wide_clean:"))
    print(expected_cols_wide_clean[!expected_cols_wide_clean %in% names(wide_clean)])
  }
  if(!expected_cols_long_clean_result){
    print(paste("Expected columns not found in long_clean_pivoted:"))
    print(expected_cols_long_clean_pivoted[!expected_cols_long_clean_pivoted %in% names(long_clean_pivoted)])
  }
  if(!expected_cols_together_clean_result){
    print(paste("Expected columns not found in together_clean:"))
    print(expected_cols_together_clean[!expected_cols_together_clean %in% names(together_clean)])
  }
  stop('Uh oh. One or more tests failed.')
}

