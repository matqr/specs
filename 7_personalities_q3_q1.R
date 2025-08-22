library(extrafont)
library(tidyverse)
library(ggtext)
library(ggdist)
library(ggplot2)
library(dplyr)
library(MetBrewer)
library(car)
library(rstatix)
library(ggpubr)
library(showtext)
showtext_auto() 

# initial setup ----
setwd("Developer/paper__specs-INTERNAL/")
bg_color <- "white"
font_family <- "Helvetica"
ordered_questions = c("safe", "lively", "wealthy", "beautiful", "boring", "depressing", "live nearby", "walk", "cycle", "green")
ordered_countries = c("USA", "Singapore", "Nigeria", "Netherlands", "Chile", "All")

# ANOVA function 
perform_welchs_anova <- function(data, demographic_col) {
  formula <- as.formula(paste("Score ~", demographic_col))   # Create a formula dynamically
  welch_anova_results <- data %>%
    group_by(country, Question) %>%
    welch_anova_test(formula) %>%
    ungroup()
  
  return(welch_anova_results)
}

# filter significant results
filter_significant <- function (data, demographic_col) {
  anova_results <- perform_welchs_anova(data, demographic_col)
  plot_data <- data %>%
    left_join(anova_results, by = c("country", "Question"))
  
  # Filter the data to only include countries with any p.value < 0.05
  significant_data <- plot_data %>% 
    group_by(country) %>%
    filter(any(p < 0.05)) %>%
    ungroup()
  
  return(significant_data)
}

# filter out entire questions within countries with smaller than n sample size
filter_sample_size <- function(data, demographic_col, n) {
  data_min <- data %>%
    group_by(country, Question, .data[[demographic_col]]) %>%
    # Step 1: Keep groups with sample size >= n
    filter(n() >= n) %>%
    ungroup() %>%
    group_by(country, Question) %>%
    # Step 2: Keep Questions with more than 1 unique group
    filter(n_distinct(.data[[demographic_col]]) > 1) %>%
    ungroup()
  
  return(data_min)
}

# posthoc test function
posthoc_test <- function(data, demographic_col) {
  # Create a formula dynamically
  formula <- as.formula(paste("Score ~", demographic_col))
  
  # run posthoc analysis
  grouped_posthoc <- data %>%
    group_by(country, Question) %>%
    games_howell_test(formula)
  
  # Get countries with at least one significant difference
  sig_countries <- grouped_posthoc %>%
    filter(p.adj.signif != "ns") %>%
    pull(country) %>%
    unique()
  
  # Filter the main data to include only significant countries
  filtered_data_sig <- data %>%
    filter(country %in% sig_countries)
  # Refactor the levels so the brackets locations are computed correctly
  subset_ordered_countries = ordered_countries[ordered_countries %in% unique(filtered_data_sig$country)]
  filtered_data_sig$country <- factor(filtered_data_sig$country, levels = subset_ordered_countries)
  
  # Rerun the posthoc tests on filtered data with correct positioning
  grouped_posthoc <- filtered_data_sig %>%
    group_by(country, Question) %>%
    games_howell_test(formula) %>%
    add_xy_position(x = "country") %>%
    group_by(country, Question) %>%
    # adjust y.position in each facet
    mutate(
      n_comparisons = n(),
      group_row = row_number(),
      y.position = case_when(
        n_comparisons == 1 ~ 9,
        n_comparisons > 1 ~ 9 + (group_row - 1) * 0.5
      )
    ) %>%
    ungroup()
  
  return(list(results_data = filtered_data_sig, results_posthoc = grouped_posthoc))
}

# plotting posthoc results
plot_posthoc <- function(data, posthoc, demographic_col, x_axis_plot, legend_title, last_plot) {
  # only show a single * for p<0.05, replacing **, ***,**** for a single *
  posthoc <- posthoc %>%
    mutate(p.adj.signif = ifelse(p.adj < 0.05, "*", "ns"))
  
  p <- data %>% 
    group_by(country, Question, .data[[demographic_col]]) %>%
    summarize(
      median_score = median(Score),
      q1 = quantile(Score, 0.25),
      q3 = quantile(Score, 0.75)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = country, y = median_score, color = .data[[demographic_col]])) +
    # geom_text(parse = TRUE) + # for proper <= and >=
    geom_point(
      position = position_dodge(width = 0.8), 
      size = 6, 
      shape = 16
    ) +
    geom_linerange(
      aes(ymin = q1, ymax = q3, color = .data[[demographic_col]]),
      position = position_dodge(width = 0.8),
      linewidth = 1
    ) +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 10, 5)) +
    scale_color_manual(
      values = c(MetBrewer::met.brewer("Hokusai1")[c(2,4,6,8)]),
    ) +
    coord_flip(ylim = c(0, 10), clip = "off") + # reverse plot
    theme_minimal(base_family = font_family, base_size = 34) +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      axis.text.x = element_text(family = font_family),
      plot.margin = margin(4, 4, 4, 20),
      legend.position = "top",
      plot.title = element_text(
        hjust = 0.5,
        size = 34,
        margin = margin(b = 8)
      ),
      legend.box.margin = margin(b = -1),  # Reduce space around legend
      legend.box.spacing = unit(-1, "pt"), 
    ) +
    guides(color = guide_legend(title = legend_title)) +
    facet_wrap(~ Question, nrow = 1) + 
    labs(x = NULL, 
         y = ifelse(last_plot == TRUE, 
                    "Perception Q Score median with 95% confidence intervals", 
                    ""), 
         title = x_axis_plot) +
    stat_pvalue_manual(
      data=posthoc,
      label = "p.adj",
      label.size = 8, # original 16
      bracket.size = 1,
      tip.length = 0.07,
      coord.flip = TRUE,
      hide.ns = TRUE,
      vjust = -0.5
    )
  
  return(p)
}


# extraversion ----
df_extraversion_q3q1 = read.csv("data/labels/processed/extraversion_q3_q1_qscores.csv")
df_extraversion_q3q1$Question <- factor(df_extraversion_q3q1$Question, levels = ordered_questions)
df_extraversion_q3q1$country <- factor(df_extraversion_q3q1$country, levels = ordered_countries)
df_extraversion_q3q1$extraversion_q3_q1 <- factor(
  df_extraversion_q3q1$extraversion_q3_q1, 
  levels = c(0, 1), labels = c("≤ Q1", "≥ Q3")
  )

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_agreeableness_q3q1, "agreeableness_q3_q1")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_extraversion_q3q1,"extraversion_q3_q1", 15)
# run Welch's anova and filter significant results again
# Welch's anova and filter significant results
filtered_data <- filter_significant(filtered_data_min, "extraversion_q3_q1")

posthoc_results_list <- posthoc_test(filtered_data, "extraversion_q3_q1")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, extraversion_q3_q1) %>%
  count(name = "n")

# plot significant results
p_extraversion_q3q1 <- plot_posthoc(
  filtered_plot_data, 
  posthoc_values, 
  "extraversion_q3_q1", 
  "Extraversion (n = 15)",
  "Personality score quantile within the country", 
  FALSE)
p_extraversion_q3q1

ggsave(
  "extraversion_q3q1_anova.png",
  plot = last_plot(),
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# agreeableness ----
df_agreeableness_q3q1 = read.csv("data/labels/processed/agreeableness_q3_q1_qscores.csv")
df_agreeableness_q3q1$Question <- factor(df_agreeableness_q3q1$Question, levels = ordered_questions)
df_agreeableness_q3q1$country <- factor(df_agreeableness_q3q1$country, levels = ordered_countries)
df_agreeableness_q3q1$agreeableness_q3_q1 <- factor(
  df_agreeableness_q3q1$agreeableness_q3_q1, 
  levels = c(0, 1), labels = c("≤ Q1", "≥ Q3")
)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_agreeableness_q3q1, "agreeableness_q3_q1")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_agreeableness_q3q1,"agreeableness_q3_q1", 20)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "agreeableness_q3_q1")

posthoc_results_list <- posthoc_test(filtered_data_min, "agreeableness_q3_q1")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, agreeableness_q3_q1) %>%
  count(name = "n")


# plot significant results
p_agreeableness_q3q1 <- plot_posthoc(
  filtered_plot_data, 
  posthoc_values, 
  "agreeableness_q3_q1", 
  "Agreeableness (n = 21)",
  "Personality score quantile within the country", 
  FALSE)
p_agreeableness_q3q1

ggsave(
  "agreeableness_q3q1_anova.png",
  plot = last_plot(),
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# conscientiousness ----
df_conscientiousness_q3q1 = read.csv("data/labels/processed/conscientiousness_q3_q1_qscores.csv")
df_conscientiousness_q3q1$Question <- factor(df_conscientiousness_q3q1$Question, levels = ordered_questions)
df_conscientiousness_q3q1$country <- factor(df_conscientiousness_q3q1$country, levels = ordered_countries)
df_conscientiousness_q3q1$conscientiousness_q3_q1 <- factor(
  df_conscientiousness_q3q1$conscientiousness_q3_q1, 
  levels = c(0, 1), labels = c("≤ Q1", "≥ Q3")
)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_conscientiousness_q3q1, "conscientiousness_q3_q1")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_conscientiousness_q3q1,"conscientiousness_q3_q1", 20)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "conscientiousness_q3_q1")

posthoc_results_list <- posthoc_test(filtered_data_min, "conscientiousness_q3_q1")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, conscientiousness_q3_q1) %>%
  count(name = "n")

# plot significant results
p_conscientiousness_q3q1 <- plot_posthoc(
  filtered_plot_data, 
  posthoc_values, 
  "conscientiousness_q3_q1", 
  "Conscientiousness (n = 21)",
  "Personality score quantile within the country", 
  FALSE)
p_conscientiousness_q3q1

ggsave(
  "conscientiousness_q3q1_anova.png",
  plot = last_plot(),
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# neuroticism (15 valid samples) ----
df_neuroticism_q3q1 = read.csv("data/labels/processed/neuroticism_q3_q1_qscores.csv")
df_neuroticism_q3q1$Question <- factor(df_neuroticism_q3q1$Question, levels = ordered_questions)
df_neuroticism_q3q1$country <- factor(df_neuroticism_q3q1$country, levels = ordered_countries)
df_neuroticism_q3q1$neuroticism_q3_q1 <- factor(
  df_neuroticism_q3q1$neuroticism_q3_q1,
  levels = c(0, 1), labels = c("≤ Q1", "≥ Q3")
)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_neuroticism_q3q1, "neuroticism_q3_q1")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_neuroticism_q3q1,"neuroticism_q3_q1", 15)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "neuroticism_q3_q1")

posthoc_results_list <- posthoc_test(filtered_data, "neuroticism_q3_q1")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, neuroticism_q3_q1) %>%
  count(name = "n")

# plot significant results
p_neuroticism_q3q1 <- plot_posthoc(
  filtered_plot_data,
  posthoc_values,
  "neuroticism_q3_q1",
  "Neuroticism (n = 15)",
  "Personality score quantile within the country",
  FALSE)
p_neuroticism_q3q1

ggsave(
  "neuroticism_q3q1_anova.png",
  plot = last_plot(),
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# openness (only 5 valid samples) ----
df_openness_q3q1 = read.csv("data/labels/processed/openness_q3_q1_qscores.csv")
df_openness_q3q1$Question <- factor(df_openness_q3q1$Question, levels = ordered_questions)
df_openness_q3q1$country <- factor(df_openness_q3q1$country, levels = ordered_countries)
df_openness_q3q1$openness_q3_q1 <- factor(
  df_openness_q3q1$openness_q3_q1,
  levels = c(0, 1), labels = c("≤ Q1", "≥ Q3")
)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_openness_q3q1, "openness_q3_q1")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_openness_q3q1,"openness_q3_q1", 5)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "openness_q3_q1")

posthoc_results_list <- posthoc_test(filtered_data_min, "openness_q3_q1")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, openness_q3_q1) %>%
  count(name = "n")

# plot significant results
p_openness_q3q1 <- plot_posthoc(
  filtered_plot_data,
  posthoc_values,
  "openness_q3_q1",
  "Openness (n = 6)",
  "Personality score quantile within the country",
  TRUE)
p_openness_q3q1

ggsave(
  "openness_q3q1_anova.png",
  plot = last_plot(),
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# merge all plots into one figure ----
figure <- ggarrange(p_extraversion_q3q1,
                    p_agreeableness_q3q1,
                    p_conscientiousness_q3q1,
                    p_neuroticism_q3q1,
                    p_openness_q3q1,
                    font.label = list(size = 26),
                    nrow = 5,
                    align = "v",
                    common.legend = TRUE,
                    legend = "bottom",
                    heights = c(1.2, 1.2, 1.2, 1.2, 1.2)
)

figure <- annotate_figure(
  figure,
  left = text_grob("Participants from", rot = 90, size = 34)
)

ggsave(
  "merged_personalities_q3q1_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 23,
  width = 25,
  scale = 1,
  dpi = 300,
)

