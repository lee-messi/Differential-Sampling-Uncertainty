
## Anonymous
# 

## Script date: 

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("patchwork")){install.packages("patchwork", dependencies = TRUE); require("patchwork")}

# Define preprocessing function ------------------------------------------------

dataframe = read.csv('../gemma_probs.csv')
ganfd = read.csv('../../GANFD/image_lookup.csv')

result <- cbind(dataframe,
                ganfd[match(dataframe$image, ganfd$full_ID), c("set", "race_guess", "gender")]) %>% 
  rename(race = race_guess) %>% 
  mutate(race = factor(race, levels = c('White', 'Black'))) %>% 
  mutate(gender = factor(gender, levels = c('Men', 'Women')))

summary <- result %>%
  group_by(race, gender, image, iter) %>%
  summarize(
    mean_entropy = mean(entropy, na.rm = TRUE),
    mean_perplexity = mean(perplexity, na.rm = TRUE),
    mean_pd = mean(pd, na.rm = TRUE),
    .groups = "drop" # Ungroup after summarizing
  )

summary_by_position <- result %>%
  group_by(race, gender, image) %>%
  arrange(iter) %>% # Ensure data is sorted by index
  mutate(
    cum_mean_pd = cummean(pd),
    cum_mean_perplexity = cummean(perplexity),
    cum_mean_entropy = cummean(entropy)
  ) %>%
  ungroup()

# Visualize Race Effects -------------------------------------------------------

visualize_race <- function(data, measure_type, y_label){
  ggplot(data, aes_string(x = "race", y = measure_type, fill = "race")) +
    geom_violin(alpha = 0.6) + 
    labs(
      y = y_label
    ) +
    theme_bw() + 
    theme(legend.position = "top",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank()) + 
    scale_fill_jama() + 
    geom_boxplot(width=0.2)
}

race_entropy = visualize_race(summary, "mean_entropy", "Mean Entropy")
race_perplexity = visualize_race(summary, "mean_perplexity", "Mean Perplexity")
race_pd = visualize_race(summary, "mean_pd", "Mean Probability of Differentiation")

race_entropy + race_perplexity + race_pd + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'top')

ggsave(filename = "gemma_race.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

# Visualize Gender Effects -------------------------------------------------------

visualize_gender <- function(data, measure_type, y_label){
  ggplot(data, aes_string(x = "gender", y = measure_type, fill = "gender")) +
    geom_violin(alpha = 0.6) + 
    labs(
      y = y_label
    ) +
    theme_bw() + 
    theme(legend.position = "top",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.title = element_blank()) + 
    scale_fill_jama() + 
    geom_boxplot(width=0.2)
}

gender_entropy = visualize_gender(summary, "mean_entropy", "Mean Entropy")
gender_perplexity = visualize_gender(summary, "mean_perplexity", "Mean Perplexity")
gender_pd = visualize_gender(summary, "mean_pd", "Mean Probability of Differentiation")

gender_entropy + gender_perplexity + gender_pd + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'top')

ggsave(filename = "gemma_gender.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

# Visualize Measures by Token Position (Race) ----------------------------------

visualize_race_by_position <- function(data, measure_type, y_label){
  ggplot(data, aes_string(x = "position", y = measure_type, color = "race")) +
    geom_smooth() +
    labs(
      x = "Token Position",
      y = y_label
    ) +
    theme_bw() + 
    theme(legend.position = "top",
          legend.title = element_blank()) + 
    scale_color_jama()
}

race_entropy_position = visualize_race_by_position(result, "entropy", "Entropy")
race_perplexity_position = visualize_race_by_position(result, "perplexity", "Perplexity")
race_pd_position = visualize_race_by_position(result, "pd", "Probability of Differentiation")

race_entropy_position + race_perplexity_position + race_pd_position + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'top')

ggsave(filename = "gemma_race_position.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)

# Visualize Measures by Token Position (Gender) --------------------------------

visualize_gender_by_position <- function(data, measure_type, y_label){
  ggplot(data, aes_string(x = "position", y = measure_type, color = "gender")) +
    geom_smooth() +
    labs(
      x = "Token Position",
      y = y_label
    ) +
    theme_bw() + 
    theme(legend.position = "top",
          legend.title = element_blank()) + 
    scale_color_jama()
}

gender_entropy_position = visualize_gender_by_position(result, "entropy", "Entropy")
gender_perplexity_position = visualize_gender_by_position(result, "perplexity", "Perplexity")
gender_pd_position = visualize_gender_by_position(result, "pd", "Probability of Differentiation")

gender_entropy_position + gender_perplexity_position + gender_pd_position + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = 'top')

ggsave(filename = "gemma_gender_position.pdf", width = 10, height = 4, 
       dpi = "retina", device = cairo_pdf)
