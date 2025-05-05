library(here)
library(tidyverse)
library(brms)

job_data = read.csv(here("data", "jm_manual.csv")) 

e = job_data %>% 
  pivot_longer(cols = days_to_interview:days_to_offer,
               names_to = "type",
               values_to = "number") %>% 
  filter(number != "N/A")

e$number = as.numeric(e$number)

mod = brm(number ~ type + (1 | place), data = e,
          file = here("data", "b_mod.brms"))

cef = brms::conditional_effects(mod)

summary(mod)

posterior<- as.matrix(mod)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")

library(bayesplot)
mcmc_areas(posterior,
           pars = c("b_Intercept", 
                    "b_typedays_to_offer",
                    "b_typedays_to_visit"),
           prob = 0.8) + plot_title + theme_minimal()