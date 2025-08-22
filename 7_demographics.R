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

# plot significant results function
plot_anova_2_groups <- function(data, demographic_col, x_axis_plot) {
  p <- data %>% 
    group_by(country, Question, .data[[demographic_col]]) %>%
    summarize(
      median_score = median(Score),
      q1 = quantile(Score, 0.25),
      q3 = quantile(Score, 0.75)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = country, y = median_score, color = .data[[demographic_col]])) +
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
    # display significance level
    geom_text(data = data %>% 
                mutate(sig_symbol = case_when(
                  # p < 0.01 ~ "**",
                  p < 0.05 ~ "*",
                  TRUE ~ ""
                )) %>%
                filter(p < 0.05),
              aes(x = country, y = 7, label = sig_symbol),
              size = 14,
              fontface = "bold",
              color = "black"
    ) +
    scale_x_discrete() +
    scale_y_continuous(breaks = seq(0, 10, 5)) +
    scale_color_manual(
      values = c(MetBrewer::met.brewer("Hokusai1")[2], 
                 MetBrewer::met.brewer("Hokusai1")[7])
    ) +
    coord_flip(ylim = c(0, 10), clip = "off") + # reverse plot
    theme_minimal(base_family = font_family, base_size = 34) +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      axis.text.x = element_text(family = font_family),
      plot.margin = margin(4, 4, 4, 4),
      legend.position = "right"
    ) +
    guides(color = guide_legend(title = NULL)) +
    facet_wrap(~ Question, nrow = 1) + 
    labs(x = NULL, y = x_axis_plot)
  
  return(p)
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

  # DEBUG
  missing_questions <- setdiff(levels(data$Question), unique(grouped_posthoc$Question))
  if (length(missing_questions) > 0) {
    message("Missing Questions in posthoc results: ", paste(missing_questions, collapse = ", "))
  }
  
  return(list(results_data = filtered_data_sig, results_posthoc = grouped_posthoc))
}

# plotting posthoc results
plot_posthoc <- function(data, posthoc, demographic_col, x_axis_plot, legend_nrow, last_plot, more_colors = FALSE) {
  # only show a single * for p<0.05, replacing **, ***,**** for a single *
  posthoc <- posthoc %>%
    mutate(p.adj.signif = ifelse(p.adj < 0.05, "*", "ns"))
  
  if (more_colors == TRUE) {
    color_list <- c(MetBrewer::met.brewer("Hokusai1", n = 10),
                MetBrewer::met.brewer("Hokusai2", n = 10),
                MetBrewer::met.brewer("Hokusai3", n = 4))
  } else {
    color_list <- MetBrewer::met.brewer("Hokusai1")[c(2, 4, 6, 8)]
  }
  
  p <- data %>% 
    group_by(country, Question, .data[[demographic_col]]) %>%
    summarize(
      median_score = median(Score),
      q1 = quantile(Score, 0.25),
      q3 = quantile(Score, 0.75)
    ) %>%
    ungroup() %>%
    ggplot(aes(x = country, y = median_score, color = .data[[demographic_col]])) +
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
    scale_color_manual(values = color_list) + # based on boolean parameter
    coord_flip(ylim = c(0, 10), clip = "off", expand = FALSE) + # reverse plot, expand options gets rid of vertical padding
    theme_minimal(base_family = font_family, base_size = 34) +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      axis.text.x = element_text(family = font_family),
      plot.margin = margin(4, 20, 4, 4),
      legend.position = "top",
      plot.title = element_text(
        hjust = 0.5,
        size = 34,
        margin = margin(b = 8)
      ),
      legend.box.margin = margin(b = -1),  # Reduce space around legend
      legend.box.spacing = unit(-1, "pt"), 
      panel.spacing.x = unit(3, "lines") # to compensate for exand=FALSE in coord_flip
    ) +
    guides(color = guide_legend(title = NULL, nrow=legend_nrow)) +
    facet_wrap(~ Question, nrow = 1) + 
    labs(x = NULL, 
         y = ifelse(last_plot == TRUE, 
                    "Perception Q Score median with 95% confidence intervals", 
                    ""), 
         title = x_axis_plot
    ) +
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

plot_posthoc_nested_demo <- function(data, posthoc, demographic_col, x_axis_plot, legend_nrow, last_plot) {
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
      values = c(
        MetBrewer::met.brewer("Hokusai1", n = 10),
        MetBrewer::met.brewer("Hokusai2", n = 10),
        MetBrewer::met.brewer("Hokusai3", n = 4))
    ) +
    coord_flip(ylim = c(0, 10), clip = "off", expand = FALSE) + # reverse plot, expand options gets rid of vertical padding
    theme_minimal(base_family = font_family, base_size = 34) +
    theme(
      plot.background = element_rect(color = NA, fill = bg_color),
      panel.grid = element_blank(),
      axis.text.x = element_text(family = font_family),
      plot.margin = margin(4, 20, 4, 4),
      legend.position = "right",
      plot.title = element_text(
        hjust = 0.5,
        size = 34,
        margin = margin(b = 8)
      ),
      legend.box.margin = margin(b = -1),  # Reduce space around legend
      legend.box.spacing = unit(-1, "pt"),
      legend.spacing.y = unit(1, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.spacing.x = unit(3, "lines") # to compensate for exand=FALSE in coord_flip
    ) +
    guides(color = guide_legend(title = NULL, nrow=legend_nrow, reverse=TRUE)) +
    facet_wrap(~ Question, nrow = 1) + 
    labs(x = NULL, 
         y = ifelse(last_plot == TRUE, 
                    "Perception Q Score median with 95% confidence intervals", 
                    ""), 
         title = x_axis_plot
    ) +
    stat_pvalue_manual(
      data=posthoc,
      label = "p.adj",
      label.size = 12, # original 16
      bracket.size = 1,
      tip.length = 0.07,
      coord.flip = TRUE,
      hide.ns = TRUE,
      vjust = -0.5
    )
  
  return(p)
}

# gender -----
df_gender = read.csv("data/labels/processed/gender_qscores.csv")
df_gender$Question <- factor(df_gender$Question, levels = ordered_questions)
df_gender$country <- factor(df_gender$country, levels = ordered_countries)
df_gender$gender[df_gender$gender == "female"] <- "Female"
df_gender$gender[df_gender$gender == "male"] <- "Male"

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_gender, "gender")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_gender,"gender", 30)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "gender")


# Welch's anova and filter significant results
filtered_data <- filter_significant(df_gender, "gender")

posthoc_results_list <- posthoc_test(filtered_data, "gender")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

# plot significant results
sample_size <- filtered_plot_data %>%
  group_by(country, Question, gender) %>%
  count(name = "n")

p_gender <- plot_posthoc(filtered_plot_data, posthoc_values, "gender", "Gender (n = 84)", 1, FALSE)
p_gender

ggsave(
  "gender_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 4,
  width = 25,
  scale = 1,
  dpi = 300,
)

# age 4 groups ---- 
df_age_group = read.csv("data/labels/processed/age_group_qscores.csv")
df_age_group$Question <- factor(df_age_group$Question, levels = ordered_questions)
df_age_group$country <- factor(df_age_group$country, levels = ordered_countries)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_age_group, "age_group")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_age_group,"age_group", 10)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "age_group")

posthoc_results_list <- posthoc_test(filtered_data_min, "age_group")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, age_group) %>%
  count(name = "n")

p_age4 <- plot_posthoc(filtered_plot_data, posthoc_values, "age_group", "Age group (n = 10)", 1, FALSE)
p_age4

ggsave(
  "age4_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 5,
  width = 25,
  scale = 1,
  dpi = 300,
  )

# ahi 3 groups ----
df_ahi3 = read.csv("data/labels/processed/ahi_3_remapped_qscores.csv")
df_ahi3$Question <- factor(df_ahi3$Question, levels = ordered_questions)
df_ahi3$country <- factor(df_ahi3$country, levels = ordered_countries)
df_ahi3$ahi_3_remapped[df_ahi3$ahi_3_remapped == "below"] <- "Bottom"
df_ahi3$ahi_3_remapped[df_ahi3$ahi_3_remapped == "middle"] <- "Middle"
df_ahi3$ahi_3_remapped[df_ahi3$ahi_3_remapped == "above"] <- "Upper"

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_ahi3, "ahi_3_remapped")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_ahi3,"ahi_3_remapped", 20)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "ahi_3_remapped")

# run post-hoc analysis only if there were any significant differences
posthoc_results_list <- posthoc_test(filtered_data_min, "ahi_3_remapped")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc
   
sample_size <- filtered_plot_data %>%
  group_by(country, Question, ahi_3_remapped) %>%
  count(name = "n")

p_ahi <- plot_posthoc(filtered_plot_data, posthoc_values, "ahi_3_remapped", "Annual household income (AHI) bracket (n = 20)", 1, FALSE)
p_ahi

  ggsave(
    "ahi_anova.pdf",
    plot = last_plot(),
    device = "pdf",
    path = "img/",
    height = 5,
    width = 25,
    scale = 1,
    dpi = 300,
  )


# education level ----
df_edu = read.csv("data/labels/processed/education_level_qscores.csv")
df_edu$Question <- factor(df_edu$Question, levels = ordered_questions)
df_edu$country <- factor(df_edu$country, levels = ordered_countries)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_edu, "education_level")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_edu,"education_level", 15)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "education_level")

# run post-hoc analysis only if there were any significant differences
posthoc_results_list <- posthoc_test(filtered_data_min, "education_level")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, education_level) %>%
  count(name = "n")

p_edu <- plot_posthoc(filtered_plot_data, posthoc_values, "education_level", "Education level (n = 15)", 2, FALSE)
p_edu

ggsave(
    "edu_anova.pdf",
    plot = last_plot(),
    device = "pdf",
    path = "img/",
    height = 5,
    width = 25,
    scale = 1,
    dpi = 300,
  )


# ethnicity ----
df_eth = read.csv("data/labels/processed/race_ethnicity_qscores.csv")
df_eth$Question <- factor(df_eth$Question, levels = ordered_questions)
df_eth$country <- factor(df_eth$country, levels = ordered_countries)

# Welch's anova and filter significant results
# filtered_data <- filter_significant(df_eth, "race_ethnicity")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_eth,"race_ethnicity", 15)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
filtered_data <- filter_significant(filtered_data_min, "race_ethnicity")

# run post-hoc analysis only if there were any significant differences
posthoc_results_list <- posthoc_test(filtered_data_min, "race_ethnicity")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc
  
sample_size <- filtered_plot_data %>%
  group_by(country, Question, race_ethnicity) %>%
  count(name = "n")

p_eth <- plot_posthoc(filtered_plot_data, posthoc_values, "race_ethnicity", "Race and ethnicity (n = 15)", 1, TRUE)
p_eth
  
ggsave(
    "eth_anova.pdf",
    plot = last_plot(),
    device = "pdf",
    path = "img/",
    height = 5,
    width = 25,
    scale = 1,
    dpi = 300,
)


# merge all plots into one figure ----
figure <- ggarrange(p_gender, p_age4, p_ahi,p_edu, p_eth,
                    font.label = list(size = 26),
                    nrow = 5,
                    align = "v",
                    heights = c(1.2, 1.2, 1.2, 1.2, 1.2)
                    )

figure <- annotate_figure(
  figure,
  left = text_grob("Participants from", rot = 90, size = 34)
  )

ggsave(
  "merged_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 32,
  width = 25,
  scale = 1,
  dpi = 300,
)

# size of demographic nested subgroups ----
df_raw = read.csv("data/labels/processed/global_mapped_cleaned_with_ahiremapped.csv")[,-1]

# all five demographics
subgroups_count_all <- df_raw %>% 
  group_by(Question, Country, Left_image, gender, age_group, ahi_3_remapped, education_level_remapped, race_ethnicity) %>%
  summarise(n=n(), .groups ='drop')
summary_stats_all <- subgroups_count_all %>%
  summarise(
    max_rows = max(n),
    min_rows = min(n),
    avg_rows = mean(n),
    total_subgroups = n()
  )

# only 2 demographics
subgroups_count_less <- df_raw %>% 
  group_by(Question, Country, Left_image, gender, age_group) %>%
  summarise(n=n(), .groups ='drop')
summary_stats_less <- subgroups_count_less %>%
  summarise(
    max_rows = max(n),
    min_rows = min(n),
    avg_rows = mean(n),
    total_subgroups = n()
  )

# only 2 demographics
subgroups_count_less <- df_raw %>% 
  group_by(Question, Country, Left_image, age_group, ahi_3_remapped) %>%
  summarise(n=n(), .groups ='drop')
summary_stats_less <- subgroups_count_less %>%
  summarise(
    max_rows = max(n),
    min_rows = min(n),
    avg_rows = mean(n),
    total_subgroups = n()
  )

# only 2 demographics
subgroups_count_less <- df_raw %>% 
  group_by(Question, Country, Left_image, gender, ahi_3_remapped) %>%
  summarise(n=n(), .groups ='drop')
summary_stats_less <- subgroups_count_less %>%
  summarise(
    max_rows = max(n),
    min_rows = min(n),
    avg_rows = mean(n),
    total_subgroups = n()
  )

# only 3 demographics
subgroups_count_less <- df_raw %>% 
  group_by(Question, Country, Left_image, gender, age_group, ahi_3_remapped) %>%
  summarise(n=n(), .groups ='drop')
summary_stats_less <- subgroups_count_less %>%
  summarise(
    max_rows = max(n),
    min_rows = min(n),
    avg_rows = mean(n),
    total_subgroups = n()
  )

# gender x age_group x ahi ----
df_mixed = read.csv("data/labels/processed/gender_age_group_ahi_3_remapped_qscores.csv")
df_mixed$Question <- factor(df_mixed$Question, levels = ordered_questions)
df_mixed$country <- factor(df_mixed$country, levels = ordered_countries)

# replace wording: split, capitalize, and recombine
transform_values <- function(x) {
  parts <- strsplit(x, " x ")
  sapply(parts, function(p) {
    first <- paste0(toupper(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
    last <- paste0(toupper(substr(p[3], 1, 1)), substr(p[3], 2, nchar(p[3])))
    paste(first, p[2], last, sep = " x ")
  })
}

df_mixed$gender_age_group_ahi_3_remapped <- transform_values(df_mixed$gender_age_group_ahi_3_remapped)

# Welch's anova and filter significant results => Not enough observations
#filtered_data <- filter_significant(df_mixed, "gender_age_group")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_mixed,"gender_age_group_ahi_3_remapped", 5)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
# NOTE: skip this step for these demographic interactions 
filtered_data <- filter_significant(filtered_data_min, "gender_age_group_ahi_3_remapped")

# run post-hoc analysis 
posthoc_results_list <- posthoc_test(filtered_data_min, "gender_age_group_ahi_3_remapped")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc
# there is an error in the positioning of the * line
posthoc_values$y.position <- posthoc_values$y.position/2
posthoc_values$xmax[posthoc_values$group1 == "Female x 30-39 x Bottom" & posthoc_values$group2 == "Male x 21-29 x Middle"] <- 1.1

sample_size <- filtered_plot_data %>%
  group_by(country, Question, gender_age_group_ahi_3_remapped) %>%
  count(name = "n")

p_mixed_demo_3 <- plot_posthoc_nested_demo(filtered_plot_data, posthoc_values, "gender_age_group_ahi_3_remapped", "Gender x Age Group x AHI (n = 5)", 24, FALSE)
p_mixed_demo_3

ggsave(
  "gender_age_group_ahi3_remapped_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 10,
  width = 25,
  scale = 1,
  dpi = 300,
)

# gender x age_group ----
df_mixed = read.csv("data/labels/processed/gender_age_group_qscores.csv")
df_mixed$Question <- factor(df_mixed$Question, levels = ordered_questions)
df_mixed$country <- factor(df_mixed$country, levels = ordered_countries)

# replace wording
df_mixed$gender_age_group[df_mixed$gender_age_group == "female_21-29"] <- "Female x 21-29"
df_mixed$gender_age_group[df_mixed$gender_age_group == "female_30-39"] <- "Female x 30-39"
df_mixed$gender_age_group[df_mixed$gender_age_group == "female_40-49"] <- "Female x 40-49"
df_mixed$gender_age_group[df_mixed$gender_age_group == "female_Above 50"] <- "Female x Above 50"
df_mixed$gender_age_group[df_mixed$gender_age_group == "male_21-29"] <- "Male x 21-29"
df_mixed$gender_age_group[df_mixed$gender_age_group == "male_30-39"] <- "Male x 30-39"
df_mixed$gender_age_group[df_mixed$gender_age_group == "male_40-49"] <- "Male x 40-49"
df_mixed$gender_age_group[df_mixed$gender_age_group == "male_Above 50"] <- "Male x Above 50"

# Welch's anova and filter significant results => Not enough observations
#filtered_data <- filter_significant(df_mixed, "gender_age_group")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_mixed,"gender_age_group", 5)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
# NOTE: skip this step for these demographic interactions 
filtered_data <- filter_significant(filtered_data_min, "gender_age_group")

# run post-hoc analysis 
posthoc_results_list <- posthoc_test(filtered_data_min, "gender_age_group")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, gender_age_group) %>%
  count(name = "n")

p_mixed_demo <- plot_posthoc(filtered_plot_data, posthoc_values, "gender_age_group", "Gender x Age Group (n = 5)", 1, FALSE)
p_mixed_demo

ggsave(
  "gender_age_group_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 5,
  width = 25,
  scale = 1,
  dpi = 300,
)

# gender x ahi ----
df_mixed = read.csv("data/labels/processed/gender_ahi_3_remapped_qscores.csv")
df_mixed$Question <- factor(df_mixed$Question, levels = ordered_questions)
df_mixed$country <- factor(df_mixed$country, levels = ordered_countries)

# replace wording
transform_values <- function(x) {
  parts <- strsplit(x, " x ")
  sapply(parts, function(p) {
    first <- paste0(toupper(substr(p[1], 1, 1)), substr(p[1], 2, nchar(p[1])))
    second <- paste0(toupper(substr(p[2], 1, 1)), substr(p[2], 2, nchar(p[2])))
    paste(first, second, sep = " x ")
  })
}
df_mixed$gender_ahi_3_remapped <- transform_values(df_mixed$gender_ahi_3_remapped)

# Welch's anova and filter significant results => Not enough observations
#filtered_data <- filter_significant(df_mixed, "gender_age_group")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_mixed,"gender_ahi_3_remapped", 20)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
# NOTE: skip this step for these demographic interactions 
filtered_data <- filter_significant(filtered_data_min, "gender_ahi_3_remapped")

# run post-hoc analysis 
posthoc_results_list <- posthoc_test(filtered_data_min, "gender_ahi_3_remapped")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, gender_ahi_3_remapped) %>%
  count(name = "n")

p_mixed_demo_gender_ahi <- plot_posthoc(filtered_plot_data, posthoc_values, "gender_ahi_3_remapped", "Gender x AHI (n = 21)", 1, FALSE)
p_mixed_demo_gender_ahi

ggsave(
  "gender_ahi_3_remapped_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 5,
  width = 25,
  scale = 1,
  dpi = 300,
)

# age group x ahi ----
df_mixed = read.csv("data/labels/processed/age_group_ahi_3_remapped_qscores.csv")
df_mixed$Question <- factor(df_mixed$Question, levels = ordered_questions)
df_mixed$country <- factor(df_mixed$country, levels = ordered_countries)

# replace wording
transform_values <- function(x) {
  parts <- strsplit(x, " x ")
  sapply(parts, function(p) {
    first <- p[1]  # Keep first part as is
    second <- paste0(toupper(substr(p[2], 1, 1)), substr(p[2], 2, nchar(p[2])))
    paste(first, second, sep = " x ")
  })
}

df_mixed$age_group_ahi_3_remapped <- transform_values(df_mixed$age_group_ahi_3_remapped)

# Welch's anova and filter significant results => Not enough observations
#filtered_data <- filter_significant(df_mixed, "gender_age_group")

# NOTE: if there are not enough observations above, filter out questions within
# countries with low sample size
filtered_data_min <- filter_sample_size(df_mixed,"age_group_ahi_3_remapped", 20)
# run Welch's anova and filter significant results again
# this is just to make sure the filter was good enough to satisfy stats test
# NOTE: skip this step for these demographic interactions 
filtered_data <- filter_significant(filtered_data_min, "age_group_ahi_3_remapped")

# run post-hoc analysis 
posthoc_results_list <- posthoc_test(filtered_data_min, "age_group_ahi_3_remapped")
filtered_plot_data <- posthoc_results_list$results_data
posthoc_values <- posthoc_results_list$results_posthoc

sample_size <- filtered_plot_data %>%
  group_by(country, Question, age_group_ahi_3_remapped) %>%
  count(name = "n")

p_mixed_demo_age_group_ahi <- plot_posthoc(filtered_plot_data, posthoc_values, "age_group_ahi_3_remapped", "Age group x AHI (n = 20)", 2, TRUE, TRUE)
p_mixed_demo_age_group_ahi

ggsave(
  "age_group_ahi_3_remapped_anova.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 5,
  width = 25,
  scale = 1,
  dpi = 300,
)

# combine demographic interaction plots ----
figure <- ggarrange(p_mixed_demo_3, 
                    p_mixed_demo,
                    p_mixed_demo_gender_ahi, 
                    p_mixed_demo_age_group_ahi,
                    font.label = list(size = 26),
                    nrow = 4,
                    align = "v",
                    heights = c(0.25, 0.10, 0.10, 0.25)
)

figure <- annotate_figure(
  figure,
  left = text_grob("Participants from", rot = 90, size = 34)
)

ggsave(
  "merged_anova_interactions.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  height = 32,
  width = 25,
  scale = 1,
  dpi = 300,
)

