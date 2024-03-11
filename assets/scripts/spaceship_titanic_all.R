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

sst_data <- data.table::fread("data/train.csv")

sst_data_stan <-
  sst_data %>% 
  select(`Age`, `VIP`, `HomePlanet`, `Transported`) %>% 
  mutate(`HomePlanet` = factor(ifelse(`HomePlanet` == "", NA, `HomePlanet`)),
         `VIP` = factor(VIP))

rj_custom_theme <- function(){
  theme_minimal(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5, size = 24, family = "Iosevka", face = "bold"),
          plot.background = element_rect(fill = "grey95", color = NA),
          panel.background = element_rect(fill = "grey98", color = NA),
          axis.title = element_text(size = 24, family = "Iosevka"),
          axis.title.x = element_text(margin = margin(t = 10), family = "Iosevka"),
          axis.title.y = element_text(margin = margin(r = 10), family = "Iosevka"),
          axis.text = element_text(size = 20, color = "black", family = "Iosevka"),
          axis.text.x = element_text(margin = margin(t = 5), angle = 45, hjust = 1, family = "Iosevka"),
          legend.title = element_text(size = 20, color = "black", family = "Iosevka"),
          legend.text = element_text(size = 18, color = "black", family = "Iosevka"),
          axis.ticks = element_line(size = 1))
}

rj_custom_table <- function(tbl){
  tbl %>% 
    flextable() %>% 
    bold(part = "header") %>% 
    align(align = "center", part = "all")
}

set_flextable_defaults(
  font.family = "Iosevka", font.size = 11, 
  border.color = "gray", big.mark = "", text.align = "center"
)

color_scheme_set("brightblue")

imputed_sst <- mice(sst_data_stan, m = 5, printFlag = FALSE)
imputed_sst_data <- complete(imputed_sst, 1)

imputed_sst_data <-
  imputed_sst_data %>% 
  mutate(HomePlanet = factor(HomePlanet, levels = unique(HomePlanet)),
         VIP = factor(VIP, levels = unique(VIP)))

sst_design_matrix <- 
  imputed_sst_data %>% 
  model.matrix(`Transported` ~ `Age` + `VIP` + `HomePlanet`, data = .)

sst_response <- as.integer(imputed_sst_data$Transported)

load("models/sst_model_1.rds")