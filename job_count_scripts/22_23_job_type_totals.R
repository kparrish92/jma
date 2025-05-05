source(here("job_count_scripts", "2022_2023_listings.R"))
# Split the data into lines
lines <- unlist(strsplit(data, "\n"))

# Initialize vectors for each column
uni <- character(length(lines))
job_title <- character(length(lines))
deadline <- character(length(lines))

# Extract the parts for each line
for (i in seq_along(lines)) {
  # Extract the university name
  uni[i] <- sub("\\..*", "", lines[i])  # Part before the first period
  
  # Extract the job title between the second and third periods
  job_title[i] <- unlist(strsplit(lines[i], "\\."))[2]
  job_title[i] <- trimws(job_title[i])
  
  # Extract the deadline part
  deadline_part <- sub(".*Deadline: *(.*)", "\\1", lines[i])
  # Check if a deadline was found
  if (nchar(deadline_part) != nchar(lines[i])) {
    deadline[i] <- trimws(unlist(strsplit(deadline_part, "\\."))[1])
  } else {
    deadline[i] <- NA
  }
}

# Create a data frame with the extracted columns
result_22_23 <- data.frame(uni = uni, job_title = job_title, stringsAsFactors = FALSE)


unique(result_22_23$uni)

unique(result_22_23$job_title)


# Mutate the data frame to add a new column 'category'
result_22_23$category <- ifelse(grepl("Visiting", result_22_23$job_title, ignore.case = TRUE), 
                                "VAP",
                                ifelse(grepl("NTT|Non-tenure track|non tenure track", result_22_23$job_title, ignore.case = TRUE), 
                                       "NTT",
                                       ifelse(grepl("Tenure Track|tenure-track|TT", result_22_23$job_title, ignore.case = TRUE), 
                                              ifelse(grepl("Linguistics", result_22_23$job_title, ignore.case = TRUE), 
                                                     "Assistant Professor (TT) - Linguistics",
                                                     ifelse(grepl("Literature", result_22_23$job_title, ignore.case = TRUE), 
                                                            "Assistant Professor (TT) - Literature",
                                                            "Assistant Professor (TT) - Unspecified")),
                                              ifelse(grepl("Assistant Professor", result_22_23$job_title, ignore.case = TRUE), 
                                                     "Assistant Professor (TT) - Unspecified",
                                                     ifelse(grepl("Lecturer", result_22_23$job_title, ignore.case = TRUE), 
                                                            "Lecturer", 
                                                            "Other")))))

# Display the modified data frame

result_22_23 %>% 
  group_by(category) %>% 
  summarize(n = n())


result_22_23 %>% 
  mutate(cycle = "2022-2023") %>% 
  write.csv(here("job_count_scripts", "all_jobs", "2022_2023.csv"))
