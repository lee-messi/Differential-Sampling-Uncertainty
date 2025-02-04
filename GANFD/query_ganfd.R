
## Anonymous
# Skin Tone Homogeneity Bias in Vision Language Models

## Script date: 20 Nov 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Load data --------------------------------------------------------------------

ganfd = read.csv('GANFD_Data_V1.csv')
lookup = read.csv('image_lookup.csv')

# Randomly sample facial stimuli to present in paper ---------------------------

set.seed(1048596)

lookup %>% 
  group_by(race_guess, gender) %>% 
  sample_n(2) %>% 
  select(c(full_ID, race_guess, gender))