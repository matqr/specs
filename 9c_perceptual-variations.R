library(ggplot2)
library(tidyr)
library(dplyr)
library(MetBrewer)

setwd("Developer/paper__specs-INTERNAL/")
bg_color <- "white"
font_family <- "Helvetica"
min_threshold <- 4

# load dataset ----
df <- read.csv("data/labels/processed/to_from_scores.csv")
df$Num_comparisons <- as.numeric(as.character(df$Num_comparisons))

# filter min samples
df_plot <- df[df$Num_comparisons >= min_threshold,] # the min sample size with this filter is the same as in 9a analysis

# fix order
ordered_questions = c("safe", "lively", "wealthy", "beautiful", "boring", "depressing", "live nearby", "walk", "cycle", "green")
ordered_countries = c("USA", "Singapore", "Nigeria", "Netherlands", "Chile")

# dataframe wrangling ----
# z-scores per one-country group vs other-country group, and per indicator
df_zscores <- df_plot %>%
  group_by(SVI_from, Question) %>%
  mutate(Z_Score = scale(Score)[,1]) %>%
  ungroup()

df_dumbbell <- df_zscores %>%
  group_by(participants_from, Question) %>%
  summarise(
    avg_from_zscores = mean(Z_Score[SVI_from == participants_from], na.rm = TRUE),
    avg_rest_zscores = mean(Z_Score[SVI_from != participants_from], na.rm = TRUE),
    .groups = "drop" # Avoids unwanted grouping in the result
  )

# calculate change between Others z-cores and Own z-scores
df_dumbbell$avg_from_norm = 1 
# position in y-axis
df_dumbbell$avg_rest_norm = (df_dumbbell$avg_rest_zscores - df_dumbbell$avg_from_zscores) + 1
# actual value to be displayed
df_dumbbell$pctg_score = round(df_dumbbell$avg_rest_zscores - df_dumbbell$avg_from_zscores, 2)

# add offsets to each question so that they are plotted in two parallels lines.
# NOTE: this is only for plotting purposes
i <- 0
for (q in ordered_questions[length(ordered_questions):1]) { # reversing the order so it matches other plots
  df_dumbbell$avg_from_norm[df_dumbbell$Question == q] <- df_dumbbell$avg_from_norm[df_dumbbell$Question == q] + i
  df_dumbbell$avg_rest_norm[df_dumbbell$Question == q] <- df_dumbbell$avg_rest_norm[df_dumbbell$Question == q] + i
  i <- i + 1
}

df_dumbbell$Question <- factor(df_dumbbell$Question, levels = ordered_questions[length(ordered_questions):1])
df_dumbbell$participants_from <- factor(df_dumbbell$participants_from, levels = ordered_countries[length(ordered_countries):1])

# count sample sizes ----
# sample_size <- df_dumbbell %>%
#   group_by(participants_from, Question) %>%
#   count(name = "n")

# slope pivot plot ----
# Reshape the data and add equal spacing for labels only
df_slope_pivot <- df_dumbbell %>%
  pivot_longer(
    cols = c(avg_from_norm, avg_rest_norm),
    names_to = "category",
    values_to = "score"
  ) %>%
  # Add equally spaced positions only for the labels
  group_by(participants_from) %>%
  mutate(
    # Create equally spaced positions only for the y-axis labels
    label_position = match(Question, levels(Question)),
    x_pos = case_when(
      category == "avg_from_norm" ~ 1,
      category == "avg_rest_norm" ~ 2
    ),
    category = factor(category,
                      levels = c("avg_from_norm", "avg_rest_norm"),
                      labels = c("Their country", "Others"))
  )

# Calculate the range needed for y-axis
y_min <- min(df_slope_pivot$score, na.rm = TRUE)
y_max <- max(df_slope_pivot$score, na.rm = TRUE)

p <- df_slope_pivot %>%
  ggplot() +
  geom_line(aes(x = x_pos, y = score, group = Question,
                color = pctg_score),
            size = 2) +
  geom_point(aes(x = x_pos, y = score, color = pctg_score), size = 7) +
  # Add participant labels on the far left at equally spaced positions
  geom_text(data = . %>% filter(category == "Their country"),
            aes(x = 0.5, y = score, label = Question, color = pctg_score),
            size = 10,
            family = font_family) +
  # Add pctg_score with +/- sign for the "Others" point
  geom_text(data = . %>% filter(x_pos == 2),
            aes(x = x_pos + 0.36, y = score,
                label = ifelse(pctg_score > 0, 
                               paste0("+", pctg_score),
                               paste0(pctg_score))),
            size = 10,
            family = font_family) +
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c("Their\ncountry", "Others"),
    limits = c(0, 2.5)
  ) +
  scale_y_continuous(
    limits = c(y_min - 0.5, y_max + 0.5),
  ) +
  theme_minimal(base_family = font_family, base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = font_family, size = 34),
    plot.margin = margin(4, 4, 10, 4),
    legend.position = "none",  # Removed legend since colors match labels
    panel.spacing.x = unit(0.1, "cm"),
  ) +
  facet_wrap(~ participants_from, nrow = 1) +
  labs(x = "Average change in standard deviations of perception Q Scores from participants rating their own country to evaluating others",
       y = "Visual perception dimension") +
  scale_color_gradientn(colors = MetBrewer::met.brewer("Troy"), limits = c(-0.48, 0.48))
p

# save the figure
ggsave(
  "perceptual_variations.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 9,
  width = 35,
  scale = 1,
  dpi = 300,
)

saveRDS(p, "img/9c_p.rds")
