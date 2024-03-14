library(rstan)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(plotly)
library(mgcv)
library(showtext)
library(flextable)
library(mice)
library(bayesplot)
library(posterior)
library(caret)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
showtext_auto()

rj_custom_theme <- function(){
  theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
          plot.background = element_rect(fill = "grey95", color = NA),
          panel.background = element_rect(fill = "grey98", color = NA),
          axis.title = element_text(size = 24),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.text = element_text(size = 20, color = "black"),
          axis.text.x = element_text(margin = margin(t = 5), angle = 45, hjust = 1),
          legend.title = element_text(size = 20, color = "black"),
          legend.text = element_text(size = 18, color = "black"),
          axis.ticks = element_line(linewidth = 1))
}

rj_custom_table <- function(tbl){
  tbl %>% 
    flextable() %>% 
    bold(part = "header") %>% 
    align(align = "center", part = "all")
}

set_flextable_defaults(
  font.family = "Iosevka Web", font.size = 11, 
  border.color = "gray", big.mark = "", text.align = "center"
)

color_scheme_set("brightblue")