# Load required libraries
library(tidyverse)

# Read the dataset
job_data <- read_csv("https://www.dropbox.com/s/0ka14thbylxumas/data_jobs.csv?dl=1") 

# 1. top_hiring_companies, plot_1

# 2. top_job_titles, plot_2

# 3. skill_counts, plot_3

# 4. top_10_job_locations, plot_4

# 5. plot_5

# 6. industry_salary, plot_6

# 7. top_locations_salary, plot_7

# Naming Checks -----------------------------------------------------------------------------------------

# To help us avoid object or column name issues, I've included the following tests that will only pass 
# if you have named your objects and columns exactly correct. 

# IMPORTANT: You should not edit any of the code in this section. It should be run as a big block and you 
# should see the test results at the end.

expected_object_names <- c(
  "industry_salary",      "job_data",             "plot_1",               "plot_2",              
  "plot_3",               "plot_4",               "plot_5",               "plot_6",              
  "plot_7",               "skill_counts",         "top_10_job_locations", "top_hiring_companies",
  "top_job_titles",       "top_locations_salary"
  )

# Tests
expected_object_names_result <- sum(ls() %in% expected_object_names) == length(expected_object_names)

expected_cols <- read_csv('expected_cols.csv', show_col_types=F) 

if(!expected_object_names_result){
  error_content <- paste(expected_object_names[!expected_object_names %in% ls()], collapse = ',')
  stop(paste("Expected objects not found in the environment:",error_content))
}

in_mem <- lapply(mget(expected_object_names), colnames)

found_cols <- expected_cols %>% 
  left_join(in_mem[!unlist(lapply(in_mem,is.null))] %>%
              enframe() %>%
              unnest(value) %>%
              rename(tibble = name,
                     colname = value) %>% 
              mutate(was_found = 1),
            by = c("tibble" = "tibble", "colname"="colname"))

if(sum(is.na(found_cols$was_found)) == 0){
  message('All naming tests passed. But did you restart your session and run your WHOLE script from beginning to end with no errors??')
} else {
  message("Uh oh. Couldn't find the column(s) below:")
  found_cols %>% 
    filter(is.na(was_found)) %>% 
    select(-was_found) %>% 
    print()
}





