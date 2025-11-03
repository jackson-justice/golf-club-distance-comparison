library(tidyverse)

golf <- read_csv("golf_distances.csv")
golf


golf <- golf |>
  mutate(
    club = factor(club, levels = c("7i", "9i", "PW"))
  ) |>
  arrange(trial)

# check to make sure it worked
golf |> 
  count(club)

# summary table
summary(golf$total_distance)


golf |>
  group_by(club) |>
  summarize(
    n = n(),
    mean = mean(total_distance),
    sd = sd(total_distance)
  )


ggplot(golf, aes(x = club, y = total_distance, fill = club)) +
  geom_boxplot(alpha = 0.8, width = 0.6, color = "black") +
  labs(
    x = "Club Type",
    y = "Total Distance (yards)",
    title = "Distribution of Shot Distance by Club"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none"
  )


model <- aov(total_distance ~ club, data = golf)
anova(model)
