
## Anonymous
# Prototypicality Affects Stereotyping in Vision-Language Models

## Script date: 8 Nov 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}

# Load data --------------------------------------------------------------------

gpt4turbo_race = read.csv('mpnetbase_race.csv') %>% 
  mutate(pair_id = paste(image_1, '-', image_2)) %>% 
  rename(cosine = cosine_similarity)

gpt4turbo_gender = read.csv('mpnetbase_gender.csv') %>% 
  mutate(pair_id = paste(image_1, '-', image_2)) %>% 
  rename(cosine = cosine_similarity)

# Fit Race models --------------------------------------------------------------

gpt4turbo.race <- lmer(cosine ~ 1 + race + (1|pair_id),
                         data = gpt4turbo_race,
                         control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4turbo.race)
logLik(gpt4turbo.race)

# Fit Race models --------------------------------------------------------------

gpt4turbo.gender <- lmer(cosine ~ 1 + gender + (1|pair_id),
                         data = gpt4turbo_gender,
                         control = lmerControl(optimizer = "nmkbw", calc.derivs = FALSE))

summary(gpt4turbo.gender)
logLik(gpt4turbo.gender)

