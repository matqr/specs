library(ggpubr)
library(extrafont)

setwd("Developer/paper__specs-INTERNAL/")
bg_color <- "white"
font_family <- "Helvetica"

# load existing plots
p_9a <- readRDS("img/9a_p.rds")
p_9b <- readRDS("img/9b_p.rds")
p_9b_presentation <- readRDS("img/9b_p_presentation.rds")
p_9c <- readRDS("img/9c_p.rds")

# merge all plots into one figure ----
figure_manuscript <- ggarrange(p_9a, 
                    p_9b, 
                    p_9c,
                    font.label = list(size = 26),
                    nrow = 3,
                    align = "v",
                    heights = c(1.2, 1.2, 1.2, 1.2, 1.2)
)

ggsave(
  "merged_comparisons.pdf",
  plot = figure_manuscript,
  device = "pdf",
  path = "img/",
  height = 45,
  width = 35,
  scale = 1,
  dpi = 400,
)

# merge some plots for presentations figures ----
figure_presentation <- ggarrange(p_9a, 
                    p_9b_presentation, 
                    font.label = list(size = 26),
                    nrow = 2,
                    align = "v",
                    heights = c(1.2, 1.2, 1.2, 1.2, 1.2)
)

ggsave(
  "merged_comparisons_presentation.png",
  plot = figure_presentation,
  path = "img/",
  height = 45,
  width = 35,
  scale = 1,
  dpi = 400,
)