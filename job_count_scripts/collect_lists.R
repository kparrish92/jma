library(here)
library(tidyverse)
a=read.csv(here("job_count_scripts", "all_jobs", "2021_2022.csv"))
b=read.csv(here("job_count_scripts", "all_jobs", "2022_2023.csv"))
c=read.csv(here("job_count_scripts", "all_jobs", "2023_2024.csv"))

alljobs = rbind(a,b,c)

alljobs %>% 
  group_by(category) %>% 
  summarize(n = n())


uns = c %>% 
  filter(category == "Assistant Professor (TT) - Unspecified")

unique(uns$job_title)