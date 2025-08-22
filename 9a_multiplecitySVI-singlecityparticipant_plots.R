library(extrafont)
library(tidyverse)
library(ggtext)
library(ggdist)
library(glue)
library(rstatix)
library(patchwork)
library(MetBrewer)

setwd("Developer/paper__specs-INTERNAL/")
bg_color <- "white"
font_family <- "Helvetica"
presentation_fig <- TRUE # change this accordingly

# data wrangling ----
df = read.csv("data/labels/processed/human-centric-differences_4.csv")

# only look for indicators used in PP2
df_plot <- df[!df$Question %in% c('walk', 'live nearby', 'cycle', 'green'),]

# y-axis category will change depending if paper or presentation plot
model_y_axis <- ifelse(presentation_fig, "ML model", "ViT-PP2")

# create factors and sort them for plotting purposes
names(df_plot)[names(df_plot) == "Country"] <- "Participants from"
names(df_plot)[names(df_plot) == "Score"] <- "Perception Q scores"
ordered_questions = c("safe", "lively", "wealthy", "beautiful", "boring", "depressing")
df_plot$Question <- factor(df_plot$Question, levels = ordered_questions)
# update value in dataframe to match model_y_axis name
df_plot$`Participants from`[df_plot$`Participants from` == "ViT-PP2"] <- model_y_axis
df_plot$`Participants from` <- factor(df_plot$`Participants from`, levels = c(model_y_axis, "USA", "Singapore", "Nigeria", "Netherlands", "Chile", "All"))

# median calculation ----
# compute median only for PP2 scores across indicators and for All
pp2_median_ind <- df_plot %>%
    filter(`Participants from` == model_y_axis) %>%
  group_by(Question) %>%
  summarise(median_score = median(`Perception Q scores`, na.rm = TRUE))  # Calculate the median Score for each Question

all_median_ind <- df_plot %>%
  filter(`Participants from` == "All") %>%
  group_by(Question) %>%
  summarise(median_score = median(`Perception Q scores`, na.rm = TRUE))  # Calculate the median Score for each Question

# combined pp2 and all median for numerical comparison
combined_median <- left_join(all_median_ind, pp2_median_ind, 
                    by = "Question",
                    suffix = c(".all", ".pp2"))
# change of pp2 compared to all
combined_median$change <- round((combined_median$median_score.pp2 - combined_median$median_score.all)/combined_median$median_score.all*100, 2)


# ANOVA test between All and ViT-PP2 ----
df_all_vitpp2 <- df_plot[df_plot$`Participants from` %in% c("All", model_y_axis), ]
welch_anova_results <- df_all_vitpp2 %>%
    group_by(Question) %>%
    welch_anova_test(`Perception Q scores` ~ `Participants from`) %>%
    ungroup()

# count sample sizes ----
sample_size <- df_plot %>%
  group_by(`Participants from`, Question) %>%
  count(name = "n")

# sample size for df_all_vitpp2 is 400

# plot ----
# same plot but without the coloured distributions within the bar
p <- df_plot %>% 
  ggplot(aes(`Participants from`, `Perception Q scores`)) + # map visual dataset columns and visual elements
  stat_halfeye(
    fill_type = "segments", 
    alpha = 0.7, 
    point_size = 7,
    interval_size = 15,
    point_interval = median_qi) + # half violin plots
  # ViT-PP2 median vertical line
  geom_hline(
    data = pp2_median_ind, 
    aes(yintercept = median_score), 
    col = "darkgreen",# MetBrewer::met.brewer("VanGogh3")[8],
    size = 2,
    lty = "dashed"
  ) +
  geom_text(
    data = pp2_median_ind, 
    aes(x = 0.65, 
        y = median_score + 0.3, 
        label = ifelse(presentation_fig, paste("Median", model_y_axis), "Median ViT-PP2"),
        color = "darkgreen"
        ),
    family = font_family, 
    size = 10, 
    hjust = 0
  ) +
  # All median vertical line
  geom_hline(
    data = all_median_ind, 
    aes(yintercept = median_score),
    col = "#d47400",
    size = 2,
    lty = "dashed"
  ) +
  geom_text(
    data = all_median_ind, 
    aes(x = 7.65, 
        y = median_score - 2.2, 
        label = "Median All", 
        color = "#d47400"
        ), 
    family = font_family, 
    size = 10, 
    hjust = 0
  ) +
  # Significant difference
  geom_text(
    data = welch_anova_results,
    aes(x = 7.5,
        y = 8,
        label = case_when(
          # p < 0.05 ~ "*",
          p < 0.05 ~ paste0("*", as.character(p)),
          TRUE ~ ""
        )),
    family = font_family, 
    size = 8, # used to be 10
    hjust = 0
  ) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 10, 2.5)) +
  scale_color_manual(values = c("#d47400", "darkgreen")) +
  coord_flip(ylim = c(0, 10), clip = "off") + # reverse plot
  guides(col = "none", fill= "none") + # remove legend bar
  theme_minimal(base_family = font_family, base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4)
  ) + 
  facet_wrap(~ Question) +
  labs(x = NULL, 
       y = "Perception Q scores (n = 285)"
  ) 
p

# save the figure and object  ----
ggsave(
  "human-centric-diff-avg.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 7,
  width = 14,
  scale = 1,
  dpi = 300,
)

saveRDS(p, "img/9a_p.rds")

