# Title: setup

# clear environment ------------------------------------------------------------

rm(list = ls())


# load libraries ---------------------------------------------------------------

library(tidyverse)
library(readstata13)
library(janitor)
library(extrafont)
library(pbapply)
library(lubridate)
library(xtable)
library(ggpubr)
library(fixest)
library(broom)
library(margins)
library(qte)
library(quantreg)
library(haven)
library(modelsummary)
library(rsample)
library(purrr)
library(marginaleffects)
library(lme4)
library(performance)
library(Rearrangement)
library(RLRsim)

suppressMessages(loadfonts(device = "win")) # for nice fonts in graphs

# functions, defaults and paths ------------------------------------------------

# number of digits
options(scipen = 99)

# suppress summary info
options(dplyr.summarise.inform = FALSE)

# ggplot theme
theme_set(theme_light(base_family = "Palatino Linotype") +
          theme(plot.title = element_text(hjust = 0.5),
                strip.background = element_rect(color = "grey", fill = "white"),
                strip.text = element_text(color = "black"),
                legend.position = "bottom"))


# set seed for bootstrapping
set.seed(1234)
