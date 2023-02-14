library(tidyverse)
library(lubridate)


long <- read_csv('https://www.dropbox.com/s/9luewx3rekalme9/mh_long.csv?dl=1')
wide <- read_csv('https://www.dropbox.com/s/il32xg5lrgza06m/mh_wide.csv?dl=1')


# Your Code: --------------------------------------------------------------------------------------------




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

