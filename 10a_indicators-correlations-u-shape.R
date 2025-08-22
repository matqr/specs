library(extrafont)
library(tidyverse)
library(ggtext)
library(ggdist)
library(ggplot2)
library(glue)
library(patchwork)
library(dplyr)
library(MetBrewer)
library(GGally)
library(gridExtra)
library(corrplot)
library(cowplot)

setwd("Developer/paper__specs-INTERNAL/")
bg_color <- "white"
font_family <- "Helvetica"
base_size <- 34
min_threshold <- 22

# load dataset ----
df <- read.csv("data/labels/processed/global_mapped_cleaned_qscores.csv")
df$Num_comparisons <- as.numeric(as.character(df$Num_comparisons))

# filter min samples ----
df_min <- df[df$Num_comparisons >= min_threshold,] # the min sample size with this filter is the same as other analyses
ordered_questions <- c("safe", "lively", "wealthy", "beautiful", "boring", "depressing", "live nearby", "walk", "cycle", "green")
df_min$Question <- factor(df_min$Question, levels = ordered_questions)

df_wide <- df_min %>%
  pivot_wider(
    id_cols = Image,
    names_from = Question,
    values_from = Score
  )


# function to find best nonlinear fits
plot_best_nonlinear_fits <- function(df, indicator_var) {
  vars_to_check <- c("safe", "lively", "wealthy", "beautiful", "boring", "depressing")
  plot_list <- list()
  
  linear_color <- "#0571b0"
  best_color <- "#ca0020"
  
  for (var in vars_to_check) {
    # Define formulas
    formula_linear <- as.formula(paste0("`", indicator_var, "` ~ ", var))
    formula_quad <- as.formula(paste0("`", indicator_var, "` ~ ", var, " + I(", var, "^2)"))
    formula_cubic <- as.formula(paste0("`", indicator_var, "` ~ ", var, " + I(", var, "^2) + I(", var, "^3)"))
    
    # Fit models
    model_linear <- lm(formula_linear, data = df)
    model_quad <- lm(formula_quad, data = df)
    model_cubic <- lm(formula_cubic, data = df)
    
    models <- list(linear = model_linear, quadratic = model_quad, cubic = model_cubic)
    aic_values <- sapply(models, AIC)
    r_squared <- sapply(models, function(m) summary(m)$r.squared)
    
    best_model_name <- names(aic_values)[which.min(aic_values)]
    best_model <- models[[best_model_name]]
    
    # Prediction data
    pred_data <- data.frame(x = seq(min(df[[var]], na.rm = TRUE), max(df[[var]], na.rm = TRUE), length.out = 100))
    names(pred_data) <- var
    
    # Generate predictions
    linear_pred <- predict(model_linear, newdata = pred_data)
    best_pred <- if (best_model_name != "linear") predict(best_model, newdata = pred_data) else NULL
    
    # Determine line colors
    linear_line_color <- if (best_model_name == "linear") best_color else linear_color
    
    # Format metric text
    r_val <- sqrt(r_squared["linear"]) * sign(coef(model_linear)[2])
    linear_label <- paste0("R² = ", round(r_squared["linear"], 2), ", R = ", format(round(r_val, 2), nsmall = 2))
    best_label <- paste0("R² = ", round(r_squared[best_model_name], 2))
    if (best_model_name == "quadratic") {
      quad_coef <- coef(model_quad)[paste0("I(", var, "^2)")]
      if (!is.na(quad_coef)) {
        shape_label <- ifelse(quad_coef > 0, "U-shaped", "Inv. U-shaped")
        best_label <- paste0(best_label, " (", shape_label, ")")
      }
    }
    
    # Colored text in subtitle with two lines
    if (best_model_name != "linear") {
      subtitle_text <- paste0(
        "<span style='color:", best_color, "'>Best (", tools::toTitleCase(best_model_name), "): ", best_label, "</span><br>",
        "<span style='color:", linear_color, "'>Linear: ", linear_label, "</span>"
      )
    } else {
      subtitle_text <- paste0(
        "<span style='color:", best_color, "'>Best (Linear): ", linear_label, "</span>"
      )
    }
    
    # Prediction frames
    if (best_model_name != "linear") {
      linear_df <- data.frame(x = pred_data[[var]], y = linear_pred, model = "linear")
      best_df <- data.frame(x = pred_data[[var]], y = best_pred, model = "best")
    } else {
      # When linear is best, only show the best model line
      linear_df <- NULL
      best_df <- data.frame(x = pred_data[[var]], y = linear_pred, model = "best")
    }
    
    # Build plot
    p <- ggplot(df, aes_string(x = var, y = paste0("`", indicator_var, "`"))) +
      geom_point(alpha = 0.3) +
      (if (!is.null(linear_df)) geom_line(data = linear_df, aes(x = x, y = y), color = linear_color, size = 1) else NULL) +
      (if (!is.null(best_df)) geom_line(data = best_df, aes(x = x, y = y), color = best_color, size = 1) else NULL) +
      labs(
        title = paste(indicator_var, "vs", var),
        subtitle = subtitle_text,
        x = var,
        y = indicator_var
      ) +
      theme_minimal(base_family = font_family, base_size = base_size) +
      theme(
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(size = base_size - 2),
        plot.margin = margin(5.5, 60, 5.5, 5.5, "pt")
      ) +
      coord_cartesian(xlim = c(2, 8), ylim = c(2, 8.1))
    
    plot_list[[var]] <- p
  }
  
  combined_plots <- grid.arrange(grobs = plot_list, ncol = 2, 
                                 padding = unit(2.5, "cm"))
  return(list(individual_plots = plot_list, combined_plots = combined_plots))
}

# all plots ----

indicator_var <- c("green")
indicator_var <- c("live nearby", "cycle", "walk","green")

for (ind in indicator_var) {
  plots <- plot_best_nonlinear_fits(df_wide, ind)
  ggsave(
    paste0(ind, "-polynomials.pdf"),
    plot = plots$combined_plots,
    device = "pdf",
    path = "img/",
    height = 15,
    width = 20,
    scale = 1,
    dpi = 400,
  )
}

