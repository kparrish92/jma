library(here)
source(here("job_count_scripts", "2023_2024_listings.R"))
# Split the data into lines
lines <- unlist(strsplit(data, "\n"))

# Initialize vectors for each column
uni <- character(length(lines))
job_title <- character(length(lines))
date <- character(length(lines))

# Extract the parts for each line
for (i in seq_along(lines)) {
  parts <- unlist(strsplit(lines[i], ",", fixed = TRUE))
  uni[i] <- parts[1]  # University name
  job_title[i] <- parts[2]  # Job title
  
  # Find the part with the date
  date_part <- parts[grep("DUE|Starts", parts, ignore.case = TRUE)]
  
  # Extract the date
  if (length(date_part) > 0) {
    date[i] <- trimws(gsub(".*(DUE|Starts) *(.*)", "\\2", date_part))
  } else {
    date[i] <- NA
  }
}

# Create a data frame with the extracted columns
result_23_24 <- data.frame(uni = uni, job_title = job_title, stringsAsFactors = FALSE)


# Mutate the data frame to add a new column 'category'
result_23_24$category <- ifelse(grepl("Visiting", result_23_24$job_title, ignore.case = TRUE), 
                                "VAP",
                                ifelse(grepl("NTT|Non-tenure track|non tenure track", result_23_24$job_title, ignore.case = TRUE), 
                                       "NTT",
                                       ifelse(grepl("Tenure Track|tenure-track|TT", result_23_24$job_title, ignore.case = TRUE), 
                                              ifelse(grepl("Linguistics", result_23_24$job_title, ignore.case = TRUE), 
                                                     "Assistant Professor (TT) - Linguistics",
                                                     ifelse(grepl("Literature", result_23_24$job_title, ignore.case = TRUE), 
                                                            "Assistant Professor (TT) - Literature",
                                                            "Assistant Professor (TT) - Unspecified")),
                                              ifelse(grepl("Assistant Professor", result_23_24$job_title, ignore.case = TRUE), 
                                                     "Assistant Professor (TT) - Unspecified",
                                                     ifelse(grepl("Lecturer", result_23_24$job_title, ignore.case = TRUE), 
                                                            "Lecturer", 
                                                            "Other")))))


result_23_24 %>% 
  group_by(category) %>% 
  summarise(n =n())

result_23_24 %>% 
  mutate(cycle = "2023-2024") %>% 
  write.csv(here("job_count_scripts", "all_jobs", "2023_2024.csv"))
