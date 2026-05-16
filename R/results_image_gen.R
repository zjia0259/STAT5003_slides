
install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(tidyr)


# rename group & distribution
fi_data <- data.frame(
  variable = c(
    "Leaving Intention",
    "Salary",
    "Unemployed",
    "Years Coding",
    "Work Experience",
    "Career Change Thoughts",
    "Feels AI is a Threat",
    "Work Tools Used",
    "Tech Purchase Influence",
    "Personal Tools Used",
    "Learned via Personal Projects",
    "Strongly Dislikes AI",
    "Unsure About AI Threat",
    "Loves AI",
    "Highly Distrusts AI"
  ),
  importance = c(100, 25, 18, 13, 11, 9, 8, 7, 7, 5, 5, 4, 4, 3, 3),
  group = c(
    "Career",       # Leaving Intention
    "Pay",          # Salary
    "Career",       # Unemployed
    "Experience",   # Years Coding
    "Experience",   # Work Experience
    "Career",       # Career Change Thoughts
    "AI Attitude",  # Feels AI is a Threat
    "Other",        # Work Tools Used
    "Other",        # Tech Purchase Influence
    "Other",        # Personal Tools Used
    "Other",        # Learned via Personal Projects
    "AI Attitude",  # Strongly Dislikes AI
    "AI Attitude",  # Unsure About AI Threat
    "AI Attitude",  # Loves AI
    "AI Attitude"   # Highly Distrusts AI
  )
)

p1 <- ggplot(fi_data, aes(x = reorder(variable, importance),
                          y = importance, fill = group)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Career" = "#1f4e79",
    "Pay" = "#2e86ab",
    "Experience" = "#a8c5e0",
    "AI Attitude" = "#e67e22",
    "Other" = "#cccccc"
  )) +
  labs(x = NULL, y = "Importance",
       title = "What Predicts Job Satisfaction?",
       subtitle = "5 of the Top 15 predictors are AI-related (orange)") +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 26, color = "#1f2a44"),
    plot.subtitle = element_text(size = 20, color = "#6b7280", 
                                 margin = margin(b = 10)),
    axis.text.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    panel.grid.minor = element_blank()
  )


ggsave("images/feature_importance_ai.png", p1,
       width = 10, height =8.5, dpi = 200, bg = "white")

ai_data <- data.frame(
  ai_attitude = c("Loves AI", "Neutral", "Distrusts AI", "Threatened by AI","Strongly Dislikes AI"),
  satisfied = c(74, 69, 69, 59,67),
  neutral = c(21, 25, 22, 30, 22),
  unsatisfied = c(5, 6, 9, 11, 9)
)

ai_long <- ai_data %>%
  tidyr::pivot_longer(cols = -ai_attitude, names_to = "satisfaction", values_to = "pct") %>%
  mutate(
    ai_attitude = factor(ai_attitude,
                         levels = c("Loves AI", "Neutral", "Distrusts AI", "Threatened by AI","Strongly Dislikes AI")),
    satisfaction = factor(satisfaction,
                          levels = c("unsatisfied", "neutral", "satisfied"))
  )

p2 <-ggplot(ai_long, aes(x = ai_attitude, y = pct, fill = satisfaction)) +
  geom_col(width = 0.65) +
  scale_fill_manual(values = c(
    "satisfied" = "#27ae60",
    "neutral" = "#bdc3c7",
    "unsatisfied" = "#c0392b"
  ),
  labels = c("Unsatisfied", "Neutral", "Satisfied")) +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 5) +
  labs(x = NULL, y = "% of Developers",
       title = "Attitude Toward AI Predicts Job Satisfaction") +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 22, color = "#1f2a44"),
    axis.text = element_text(size = 14)
  )



ggsave("images/ai_attitude_satisfaction.png", p2,
       width = 10, height = 5.5, dpi = 200, bg = "white")

cat("All the charts have been generated to the images/ folder\n")
