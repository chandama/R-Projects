# Load required libraries
library(tidyverse)

# Read the dataset
job_data <- read_csv("https://www.dropbox.com/s/0ka14thbylxumas/data_jobs.csv?dl=1") 

# 1. top_hiring_companies, plot_1
top_hiring_companies <- job_data %>% 
  select(company) %>% 
  count(company) %>% 
  arrange(desc(n)) %>% 
  slice(1:12)
  

plot_1 <- top_hiring_companies %>% 
  ggplot(mapping = aes(y=(reorder(company, desc(-n))), x=n, fill=company)) +
  ggtitle('Top 10 Hiring Companies') +
  xlab('Number of Job Postings') + ylab('Company') +
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_1

# 2. top_job_titles, plot_2
top_job_titles <- job_data %>% 
  select(job_simpl) %>% 
  count(job_simpl) %>% 
  arrange(desc(n))

plot_2 <- top_job_titles %>% 
  ggplot(mapping = aes(y=(reorder(job_simpl, desc(-n))), x=n, fill=job_simpl)) +
  ggtitle('Jobs by Simplified Title') +
  xlab('Number of Job Postings') + ylab('Job Title') +
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_2

# 3. skill_counts, plot_3
skill_counts <- job_data %>% 
  select(python_yn:machine_learning_yn) %>% 
  pivot_longer(
    cols = python_yn:machine_learning_yn,
    names_to = 'skill',
    values_to = 'n'
  ) %>% 
  filter(n==1) %>% 
  count(skill) %>% 
  arrange(desc(n)) %>% 
  rename("count"="n")

plot_3 <- skill_counts %>% 
  ggplot(mapping = aes(y=(reorder(skill, desc(-count))), x=count, fill=skill)) +
  ggtitle('Skills Required') +
  xlab('Number of Job Postings') + ylab('Skill') +
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_3
  
# 4. top_10_job_locations, plot_4
top_10_job_locations <- job_data %>% 
  select(location) %>% 
  count(location) %>% 
  arrange(desc(n)) %>% 
  slice(1:11) 
  
plot_4 <- top_10_job_locations %>% 
  ggplot(mapping = aes(y=(reorder(location, desc(-n))), x=n, fill=location)) +
  ggtitle('Jobs Postings by Location') +
  xlab('Number of Job Postings') + ylab('Location') +
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_4
# 5. plot_5
plot_5 <- job_data %>% 
  ggplot(mapping = aes(x=salary_estimate, fill=job_simpl)) +
  ggtitle('Distribution of Salary Estimates') +
  xlab('Salary Estimate (USD)') + ylab('Number of Job Postings') +
  scale_x_continuous(labels = label_dollar())+
  geom_histogram() +
  facet_wrap(~job_simpl) +
  theme_bw() +
  theme(legend.position="none")


plot_5

# 6. industry_salary, plot_6
industry_salary <- job_data %>%
  filter(is.na(company_industry) == FALSE) %>% 
  group_by(company_industry) %>% 
  summarise(median_salary = median(salary_estimate),
            mean_salary = mean(salary_estimate))

demo_continuous(c(0,150000), labels = label_dollar())

plot_6 <- industry_salary %>% 
  ggplot(mapping = aes(y=(reorder(company_industry, desc(-median_salary))), x=median_salary, fill=company_industry)) +
  ggtitle('Salary Estimates by Industry') +
  xlab('Median Salary Estimate (USD)') + ylab('Industry') +
  scale_x_continuous(labels = label_dollar())+
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_6

# 7. top_locations_salary, plot_7
top_locations_salary <- job_data %>%
  filter(is.na(location) == FALSE) %>% 
  group_by(location) %>% 
  summarise(median_salary = median(salary_estimate),
            mean_salary = mean(salary_estimate),
            count=n()) %>% 
  arrange(desc(count)) %>% 
  slice(1:11) %>% 
  arrange(location) %>% 
  select(location, median_salary, mean_salary)

plot_7 <- top_locations_salary %>% 
  ggplot(mapping = aes(y=(reorder(location, desc(-median_salary))), x=median_salary, fill=location)) +
  ggtitle('Salary Estimates by Location') +
  xlab('Median Salary Estimate (USD)') + ylab('Location') +
  scale_x_continuous(labels = label_dollar())+
  geom_col() +
  theme_bw() +
  theme(legend.position="none")

plot_7
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





