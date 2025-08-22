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
df = read.csv("data/labels/processed/spatial-differences-22.csv")

# only look for indicators used in PP2
df_plot <-df[!df$Question %in% c('walk', 'live nearby', 'cycle', 'green'),]

# rename values and column names
df_plot$city[df_plot$city == "Santiago"] <- "Santiago de Chile"
names(df_plot)[names(df_plot) == "city"] <- "SVI from"
names(df_plot)[names(df_plot) == "Score"] <- "Perception Q Scores"
names(df_plot)[names(df_plot) == "Dataset"] <- "Responses"

# category will change depending if paper or presentation plot
model_y_axis <- ifelse(presentation_fig, "ML model", "ViT-PP2")
df_plot$Responses[df_plot$Responses == "ViT-PP2"] <- model_y_axis

# create factors and sort them for plotting purposes
ordered_questions = c("safe", "lively", "wealthy", "beautiful", "boring", "depressing")
df_plot$Question <- factor(df_plot$Question, levels = ordered_questions)
df_plot$`SVI from` <- factor(df_plot$`SVI from`, levels = c("San Francisco", "Singapore", "Abuja", "Amsterdam", "Santiago de Chile", "All"))
df_plot$Responses <- factor(df_plot$Responses, levels =c("Survey", model_y_axis))

# colors
response_colours <- c(MetBrewer::met.brewer("VanGogh3")[2], MetBrewer::met.brewer("VanGogh3")[8])
slab_colours <- c(MetBrewer::met.brewer("VanGogh3")[5], MetBrewer::met.brewer("VanGogh3")[8])
names(response_colours) <- c("Survey", model_y_axis) 
names(slab_colours) <- c("Survey", model_y_axis)

# ANOVA test between All and ViT-PP2 ----
welch_anova_results <- df_plot %>%
  group_by(`SVI from`, Question) %>%
  welch_anova_test(`Perception Q Scores` ~ Responses) %>%
  ungroup()

significance_labels <- welch_anova_results %>%
  mutate(
    sig_label = case_when(
      p < 0.05 ~ paste0("*", as.character(p)),
      # p < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# count sample sizes ----
sample_size <- df_plot %>%
  group_by(`SVI from`, Question) %>%
  count(name = "n")

# plot ----
# plot with 'All' line in each facet
p <- df_plot %>% 
  ggplot(aes(`SVI from`, `Perception Q Scores`, fill = Responses)) +
  stat_halfeye(
    aes(color=Responses),
    fill_type = "segments", 
    alpha = 0.7,
    point_size = 7,
    interval_size = 15,
    point_interval = median_qi) +
  scale_x_discrete() +
  scale_y_continuous(breaks = seq(0, 10, 2.5)) +
  # Specify colors for each response
  scale_fill_manual(
    values = response_colours,
    ) +
  # color for the median point and slab borders
  scale_color_manual(
    values = slab_colours,
    ) + 
  # reverse plot
  coord_flip(ylim = c(0, 10), clip = "off") +
  # Significant difference
  geom_text(
    data = significance_labels,
    aes(x = as.numeric(factor(`SVI from`)) + 0.5,
        y = 8, 
        label = sig_label,
        fill = NULL,  # Explicitly set fill to NULL
        color = NULL  # Explicitly set color to NULL
        ),
    family = font_family,
    size = 8, # used to be 10
    hjust = 0,
  ) +
  theme_minimal(base_family = font_family,  base_size = 34) +
  theme(
    plot.background = element_rect(color = NA, fill = bg_color),
    panel.grid = element_blank(),
    axis.text.y = element_text(hjust = 0, margin = margin(r = -10), family = font_family),
    plot.margin = margin(4, 4, 4, 4),
    legend.position = "top",
    legend.direction = "horizontal"
  ) + 
  facet_wrap(~ Question) +
  labs(x = NULL, 
       y = "Perception Q scores (n = 138)"
  ) 

# create the dataframe for the legend (inside plot)
df_for_legend <- subset(df, Question == "wealthy")
df_for_legend <- df_for_legend %>% 
  filter(city == "San Francisco")

p_legend <- df_for_legend %>% 
  ggplot(aes(city, Score)) +
  stat_halfeye(
    fill_type = "segments", 
    alpha = 0.3,
    point_size = 7,
    interval_size = 15,
    ) +
  annotate(
    "richtext",
    x = c(0.8, 1.6, 1.95),
    y = c(4, 6, 9),
    label = c("95% of scores fall within this range", "Median", "Distribution<br>of scores"),
    fill = NA, label.size = NA, family = font_family, size = 10, vjust = 1,
  ) +
  geom_curve(
    data = data.frame(
      # the arrows follow the order of the label above, since the coord are
      # later flip, change x for vertical and y for horizontal
      x = c(0.7,1.3, 1.5),
      xend = c(0.96, 1.1, 1.5), # where the head of the arrows end
      y = c(3.7, 5.3, 7.5),
      yend = c(4.2, 5.2, 5)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 1, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 10), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legend") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = font_family, size = 30,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.2, fill = bg_color))

# save presentation figure ----
ggsave(
  "spatial-differences-presentation.png",
  plot = p,
  path = "img/",
  height = 7,
  width = 14,
  scale = 1,
  dpi = 300,
)

saveRDS(p, "img/9b_p_presentation.rds")

# save manuscript figure ----

# Insert the custom legend into the plot
p <- p + inset_element(p_legend, l = 0.08, r = 0.37,  t = 1.23, b = 1.05, on_top = TRUE)
p

ggsave(
  "spatial-differences.pdf",
  plot = p,
  device = "pdf",
  path = "img/",
  height = 7,
  width = 14,
  scale = 1,
  dpi = 300,
)

saveRDS(p, "img/9b_p.rds")