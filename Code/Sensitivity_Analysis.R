##################################################################################
### This file contains the code for the sensitivity analyses conducted for the ###
### publication "Association of primary and booster vaccination status with    ###
### COVID-19 associated mortality during the omicron wave - A retrospective    ###
### observational study in the elderly."                                       ###
##################################################################################

## Loading of necessary packages and sourcing of self-defined functions:
library(lubridate)
library(pec)
library(scales)
library(survival)
library(tidyverse)
library(gridExtra)
source("Code/Functions.R")

##################################
### Data Reading & Preparation ###
##################################

## Reading of the data:
# load("Daten_Impfen_2022_09_07.RData")

## Data preparation:
data <- prepare_data(data_output)

########################
### 30-day mortality ###
########################

data_30 <- prepare_data(data, type = "censoring")
data_30_model <- data_30 %>% filter(Geschlecht != "divers") %>% droplevels()

model_30 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data_30_model)
summary(model_30)
# saveRDS(model_30, file = "Models/model_30.rds")

plot_spline_30_english <- spline_plot(model_30)
# ggsave(plot = plot_spline_30_english, filename = "Graphics/eFigure2.jpeg", width = 10,
#       height = 5)

plot_spline_30_german <- spline_plot(model_30, version = "german")
# ggsave(plot = plot_spline_30_german, filename = "Graphics/eFigure2_german.jpeg",
#        width = 10, height = 5)

plot_coef_30_english <- coef_plot(model_30)
# ggsave(plot = plot_coef_30_english, filename = "Graphics/eFigure3.jpeg", width = 10,
#       height = 5)

plot_coef_30_german <- coef_plot(model_30, version = "german")
# ggsave(plot = plot_coef_30_german, filename = "Graphics/eFigure3_german.jpeg",
#       width = 10, height = 5)

#####################################################
### Cases with COVID-19 as leading cause of death ###
#####################################################

data_cause <- prepare_data(data, type = "covid_only")
data_cause_model <- data_cause %>% filter(Geschlecht != "divers") %>%
  droplevels()

model_cause <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                       Geschlecht + Impfstatus_surv +
                       pspline(Alter_surv_num, df = 4) +
                       pspline(Erkrankungsdatum_num, df = 4),
                     x = TRUE, data = data_cause_model)
summary(model_cause)
# saveRDS(model_cause, file = "Models/model_cause.rds")

plot_spline_english <- spline_plot(model_cause)
# ggsave(plot = plot_spline_english, filename = "Graphics/eFigure4.jpeg", width = 10,
#       height = 5)

plot_spline_cause_german <- spline_plot(model_cause, version = "german")
# ggsave(plot = plot_spline_cause_german, filename = "Graphics/eFigure4_german.jpeg",
#        width = 10, height = 5)

plot_coef_cause_english <- coef_plot(model_cause)
# ggsave(plot = plot_coef_cause_english, filename = "Graphics/eFigure5.jpeg", width = 10,
#        height = 5)

plot_coef_cause_german <- coef_plot(model_cause, version = "german")
# ggsave(plot = plot_coef_cause_german, filename = "Graphics/eFigure5_german.jpeg", width = 10,
#        height = 5)





