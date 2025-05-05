# Load the ggplot2 library
library(ggplot2)
library(tidyverse)

bar_data = read.csv(here("data", "jm_22_23.csv"))

#bar_data = read.csv(here("data", "jm_21_22.csv"))

# Assuming you have a data frame named 'bar_data' with a 'Date' column

# Convert 'Date' to Date class
bar_data <- bar_data %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Calculate the date from a year ago today
one_year_ago <- Sys.Date() - lubridate::years(1)

# Create a daily histogram using ggplot and tidyverse
bar_data %>%
  ggplot(aes(x = Date)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Number of Interviews per Day", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  geom_vline(xintercept = as.numeric(one_year_ago), linetype = "dashed", color = "red") 

bar_data %>%
  ggplot(aes(x = Date)) +
  geom_histogram(binwidth = 7, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Number of Interviews per Week", x = "Date", y = "Count") +
  theme(axis.text.x = element_text(size = 8), axis.text.y = element_text(size = 8)) +
  geom_vline(xintercept = as.numeric(one_year_ago), linetype = "dashed", color = "red") 