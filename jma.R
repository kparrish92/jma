### A script to examine when jobs send interviews in relation to their due date 

library(tidyverse)

## year 2022

jobs_2022 = data.frame(due_date = c("2022-10-31", "2022-10-31", "2022-11-01", 
                                    "2022-11-28", "2022-12-05", "2022-12-16", 
                                    "2023-01-15", "2022-12-19", "2023-01-01",
                                    "2023-01-15", "2023-02-14", "2023-02-14", 
                                    "2023-03-01"),
first_word = c("2022-11-07", "2022-11-28", "2022-11-29", "2022-12-05", "2022-12-28", 
               "2023-01-04", "2023-01-19", "2023-01-24", "2023-01-30", "2023-02-02", 
               "2023-02-15", "2023-03-03", "2023-03-10"),
uni_name = c("Oregon State", "Skidmore", "Iowa State", 
             "Bradley University", "UNC Wilmington", "App State", 
             "St Louis University", "East Carolina", "Florida International University",
             "Missouri", "Bowdoin", "Winthrop", 
             "Colby"),
tt_or_not = c("TT", "TT", "TT",
            "TT", "TT", "TT",
            "TT", "TT", "TT", 
            "TT", "NTT", "NTT", 
            "NTT")) %>% 
  mutate(difference = difftime(first_word, due_date))

## Get Standard error of the mean 

mean(jobs_2022$difference)
sd(jobs_2022$difference)

mean = 17
sample_size = 13
sd = 11


## SE formula is sd/sqrt(sample_size)

se = sd/sqrt(sample_size)

## 95 CI

ci = 1.96*se

mean
mean + ci
mean - ci




# so the mean is 24 days (95% CI )

sd(jobs_2022$difference)


## Get 95 percent CI



jobs_2022 %>% 
  ggplot(aes(x = difference, y = due_date, label = paste(uni_name, " ", difference), 
             color = tt_or_not)) + geom_label(position = "jitter") +
  xlim(-50,200)

mean(jobs_2022$difference)
sd(jobs_2022$difference)


library(ggrepel)

jobs_2022 %>% 
  ggplot(aes(x = first_word, y = due_date, label = paste(uni_name, " ", difference), 
             color = tt_or_not)) + geom_label_repel() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


jobs_2022 %>% 
  ggplot(aes(x = first_word, y = due_date, label = paste(uni_name, " ", difference), 
             color = tt_or_not)) + geom_label_repel() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 



jobs_2022 %>% 
  ggplot(aes(y = due_date, x = difference, label = paste(uni_name, " ", difference), 
             color = tt_or_not)) + geom_label_repel() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


jobs_2022 %>% 
  ggplot(aes(y = first_word, x = difference, label = paste(uni_name, " ", difference), 
             color = tt_or_not)) + geom_label_repel() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


jobs_2022 %>% 
  ggplot(aes(x = first_word, y = due_date, 
             color = tt_or_not, group = first_word)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + geom_line()




summary(jobs_2022)
