
library(tidyverse)
library(here)

data.frame(scores = scores_school_a) %>% 
  ggplot(aes(x = scores)) + geom_dotplot(fill = "seagreen") +
  theme_minimal() + 
  geom_vline(xintercept = mean(scores_school_a), color = "seagreen") +
  geom_vline(xintercept = mean(scores_school_a) + sd(scores_school_a), 
             color = "grey") +
  geom_vline(xintercept = mean(scores_school_a) - sd(scores_school_a), 
             color = "grey") + ggtitle("School A scores") 


ggsave(here("slides", "week1", "images", "school_a_low.svg"))
  

data.frame(scores = scores_school_b) %>% 
  ggplot(aes(x = scores)) + geom_dotplot(fill = "skyblue") +
  theme_minimal() + 
  geom_vline(xintercept = mean(scores_school_b), color = "skyblue") +
  geom_vline(xintercept = mean(scores_school_b) + sd(scores_school_b), 
             color = "purple") +
  geom_vline(xintercept = mean(scores_school_b) - sd(scores_school_b), 
             color = "purple") + ggtitle("School B scores") 


ggsave(here("slides", "week1", "images", "school_b_low.svg"))

rbind(data.frame(scores = scores_school_b, school = "school_b"),
      data.frame(scores = scores_school_a, school = "school_a")) %>% 
  ggplot(aes(x = scores, fill = school)) + geom_dotplot() +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "skyblue")) +
  geom_vline(xintercept = mean(scores_school_a), color = "seagreen") +
  geom_vline(xintercept = mean(scores_school_b), color = "skyblue") +
  ggtitle("Both school scores")
  

ggsave(here("slides", "week1", "images", "school_both_low.svg"))
  

library(truncnorm)

school_a_100 <- rtruncnorm(n = 100, a=0, b=100, mean=mean(scores_school_a), sd=sd(scores_school_a))
school_b_100 <- rtruncnorm(n = 100, a=0, b=100, mean=mean(scores_school_b), sd=sd(scores_school_b))


rbind(data.frame(scores = school_b_100, school = "school_b"),
      data.frame(scores = school_a_100, school = "school_a")) %>% 
  ggplot(aes(x = scores, fill = school)) + geom_dotplot() +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "skyblue")) +
  geom_vline(xintercept = mean(school_a_100), color = "seagreen") +
  geom_vline(xintercept = mean(school_b_100), color = "skyblue") +
  ggtitle("Both school scores: true difference is 3")


ggsave(here("slides", "week1", "images", "school_both_high.svg"))

school_a_100 <- rtruncnorm(n = 100, a=0, b=100, mean=mean(scores_school_a), sd=sd(scores_school_a))
school_b_100 <- rtruncnorm(n = 100, a=0, b=100, mean=50, sd=sd(scores_school_b))


rbind(data.frame(scores = school_b_100, school = "school_b"),
      data.frame(scores = school_a_100, school = "school_a")) %>% 
  ggplot(aes(x = scores, fill = school)) + geom_dotplot() +
  theme_minimal() +
  scale_fill_manual(values = c("seagreen", "skyblue")) +
  geom_vline(xintercept = mean(school_a_100), color = "seagreen") +
  geom_vline(xintercept = mean(school_b_100), color = "skyblue") +
  ggtitle("Both school scores: true difference is 25")

ggsave(here("slides", "week1", "images", "school_both_high_eff.svg"))



