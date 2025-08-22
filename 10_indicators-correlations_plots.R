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
library(corrplot)

setwd("Developer/paper__specs-INTERNAL/")

bg_color <- "white"
font_family <- "Helvetica"
min_threshold <- 22

ordered_questions = c("safe", "lively", "wealthy", "beautiful", "boring", "depressing", "live nearby", "walk", "cycle", "green")

# load dataset ----
df <- read.csv("data/labels/processed/global_mapped_cleaned_qscores.csv")
df$Num_comparisons <- as.numeric(as.character(df$Num_comparisons))

# filter min samples
df_min <- df[df$Num_comparisons >= min_threshold,] # the min sample size with this filter is the same as other analyses

df_min$Question <- factor(df_min$Question, levels = ordered_questions)

df_wide <- df_min %>%
  pivot_wider(
    id_cols = Image,
    names_from = Question,
    values_from = Score
  )

# count sample size
sample_size <- df_min %>%
  group_by(Question) %>%
  count(name = "n")

# ggpairs plot ----
p <- df_wide %>%
  select(all_of(ordered_questions)) %>%  # Select columns in specific order
  ggpairs(
    lower = list(continuous = function(data, mapping) {
      ggplot(data = data, mapping = mapping) +
        geom_point(alpha = 0.3) +
        geom_smooth(method = "lm", se = FALSE, color = "#CA0020")
    }),
    upper = list(continuous = wrap("cor", digits = 2))  # Specify digits in the correlation function
  ) +
  theme(plot.margin = margin(4, 4, 4, 4))
p

ggsave(
  "ind-corr-long.pdf",
  plot = last_plot(),
  device = "pdf",
  path = "img/",
  scale = 1,
  height = 12,
  width = 12,
  dpi = 300,
)

# custom 4x6 correlation matrix ----
pdf(file ="img/ind-corr-simpler-nopvalue.pdf", width=8, height=4.5)
color_palette <- colorRampPalette(c("#404040",
                                    "#bababa",
                                    "#ffffff", 
                                    "#f4a582",
                                    "#CA0020"
                                    )
                                  )
corr_all <- cor(df_wide[-1], use='pairwise.complete.obs')
corr_reduced <- data.frame(corr_all) %>% select(safe, lively, wealthy, beautiful, boring, depressing)
corr_reduced <- corr_reduced[c('live nearby', 'walk', 'cycle', 'green'),]
p <- corrplot(as.matrix(corr_reduced), method="color",
              type="full",
              order="original",
              col = color_palette(200),
              col.lim=c(-1,1),
              number.digits=2,
              addCoef.col = "black",
              tl.col = "black",       # Text label color (black)
              tl.srt = 0,             # Text label string rotation (0 for horizontal)
              tl.offset = 0.6,        # increase spacing
              addgrid.col = "white",
              is.corr = FALSE
              )
# x-axis title
mtext("n = 277", side=1, line=4, cex=1.2)  # side=1 is bottom
p
dev.off()

# custom with pvalues ----
pdf(file ="img/ind-corr-simpler.pdf", width=8, height=4.5)
color_palette <- colorRampPalette(c("#404040",
                                    "#bababa",
                                    "#ffffff", 
                                    "#f4a582",
                                    "#CA0020"
)
)
# Compute correlations and p-values
mat <- as.matrix(corr_reduced)
n <- nrow(df_wide)  # sample size
p.mat <- matrix(NA, ncol=ncol(mat), nrow=nrow(mat))

for(i in 1:nrow(mat)) {
  for(j in 1:ncol(mat)) {
    test <- cor.test(df_wide[[rownames(mat)[i]]], df_wide[[colnames(mat)[j]]])
    p.mat[i,j] <- test$p.value
  }
}

# plot
corrplot(mat,
         method="color",
         type="full",
         order="original",
         col=color_palette(200),
         col.lim=c(-1,1),
         tl.col="black",
         tl.srt=0,
         tl.offset=0.6,
         addgrid.col="white",
         is.corr=FALSE,
)

# Add custom labels: R in bold, p-value smaller underneath
nrow_mat <- nrow(mat)
ncol_mat <- ncol(mat)

for(i in 1:nrow_mat){
  for(j in 1:ncol_mat){
    x_pos <- j
    y_pos <- nrow_mat - i + 1  # flip y because corrplot inverts rows
    text(x=x_pos, y=y_pos, labels=sprintf("%.2f", mat[i,j]),
         cex=0.9, font=2)  # bold R value
    text(x=x_pos, y=y_pos-0.2, labels=sprintf("p=%.3g", p.mat[i,j]),
         cex=0.6)         # smaller p-value underneath
  }
}

# x-axis title
mtext("n = 277", side=1, line=4.2, cex=1.2)

dev.off()