
## Anonymous
# 

## Script date: 

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("reghelper")){install.packages("reghelper", dependencies = TRUE); require("reghelper")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")}

# Define preprocessing function ------------------------------------------------

dataframe = read.csv('gemma_probs.csv')
ganfd = read.csv('../GANFD/image_lookup.csv')

result <- cbind(dataframe,
                ganfd[match(dataframe$image, ganfd$full_ID), c("set", "race_guess", "gender")]) %>% 
  rename(race = race_guess) %>% 
  mutate(race = factor(race, levels = c('White', 'Black'))) %>% 
  mutate(gender = factor(gender, levels = c('Men', 'Women')))

summary <- result %>%
  group_by(race, gender, set, iter) %>%
  summarize(
    mean_entropy = mean(entropy, na.rm = TRUE),
    mean_perplexity = mean(perplexity, na.rm = TRUE),
    mean_pd = mean(pd, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  )

# Mixed Effect Models (Race) ---------------------------------------------------

race.entropy <- lmer(mean_entropy ~ race + (1|set), 
                     data = summary, 
                     control = lmerControl(optimizer = "bobyqa"))

summary(race.entropy)
logLik(race.entropy) # log likelihood of the race model 

race.perplexity <- lmer(mean_perplexity ~ race + (1|set), 
                        data = summary, 
                        control = lmerControl(optimizer = "bobyqa"))

summary(race.perplexity)
logLik(race.perplexity) # log likelihood of the race model 

race.pd <- lmer(mean_pd ~ race + (1|set), 
                data = summary, 
                control = lmerControl(optimizer = "bobyqa"))

summary(race.pd)
logLik(race.pd) # log likelihood of the race model 

# Mixed-Effects Models (Gender) ------------------------------------------------

gender.entropy <- lmer(mean_entropy ~ gender + (1|set), 
                       data = summary, 
                       control = lmerControl(optimizer = "bobyqa"))

summary(gender.entropy)
logLik(gender.entropy) # log likelihood of the gender model 

gender.perplexity <- lmer(mean_perplexity ~ gender + (1|set), 
                          data = summary, 
                          control = lmerControl(optimizer = "bobyqa"))

summary(gender.perplexity)
logLik(gender.perplexity) # log likelihood of the gender model 

gender.pd <- lmer(mean_pd ~ gender + (1|set), 
                  data = summary, 
                  control = lmerControl(optimizer = "bobyqa"))

summary(gender.pd)
logLik(gender.pd) # log likelihood of the gender model 


# Mixed Effect Models (Race Interactions) --------------------------------------

race.int.entropy <- lmer(entropy ~ race * position + (1|set), 
                         data = result, 
                         control = lmerControl(optimizer = "bobyqa"))

summary(race.int.entropy)
logLik(race.int.entropy)

race.int.perplexity <- lmer(perplexity ~ race * position + (1|set), 
                        data = result, 
                        control = lmerControl(optimizer = "bobyqa"))

summary(race.int.perplexity)
logLik(race.int.perplexity) # log likelihood of the race model 

race.int.pd <- lmer(pd ~ race * position + (1|set), 
                    data = result, 
                    control = lmerControl(optimizer = "bobyqa"))

summary(race.int.pd)
logLik(race.int.pd) # log likelihood of the race model 

# Mixed Effect Models (Gender Interactions) ------------------------------------

gender.int.entropy <- lmer(entropy ~ gender * position + (1|set), 
                           data = result, 
                           control = lmerControl(optimizer = "bobyqa"))

summary(gender.int.entropy)
logLik(gender.int.entropy)

gender.int.perplexity <- lmer(perplexity ~ gender * position + (1|set), 
                              data = result, 
                              control = lmerControl(optimizer = "bobyqa"))

summary(gender.int.perplexity)
logLik(gender.int.perplexity) # log likelihood of the gender model 

gender.int.pd <- lmer(pd ~ gender * position + (1|set), 
                      data = result, 
                      control = lmerControl(optimizer = "bobyqa"))

summary(gender.int.pd)
logLik(gender.int.pd) # log likelihood of the gender model 


# Simple Slopes ----------------------------------------------------------------

race.int.entropy.ss <- simple_slopes(race.int.entropy, 
                                     levels = list(race = c('Black', 'White')), 
                                     confint = TRUE, conf.level = 0.95)

race.int.perplexity.ss <- simple_slopes(race.int.perplexity, 
                                        levels = list(race = c('Black', 'White')), 
                                        confint = TRUE, conf.level = 0.95)

race.int.pd.ss <- simple_slopes(race.int.pd, 
                                levels = list(race = c('Black', 'White')), 
                                confint = TRUE, conf.level = 0.95)

gender.int.entropy.ss <- simple_slopes(gender.int.entropy, 
                                       levels = list(gender = c('Women', 'Men')), 
                                       confint = TRUE, conf.level = 0.95)

gender.int.perplexity.ss <- simple_slopes(gender.int.perplexity, 
                                          levels = list(gender = c('Women', 'Men')), 
                                          confint = TRUE, conf.level = 0.95)

gender.int.pd.ss <- simple_slopes(gender.int.pd, 
                                  levels = list(gender = c('Women', 'Men')), 
                                  confint = TRUE, conf.level = 0.95)
