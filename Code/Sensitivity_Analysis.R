### This file contains the code for the sensitivity analyses conducted for the
### publication "Association of primary and booster vaccination status with 
### COVID-19 associated mortality during the omicron wave - A retrospective
### observational study in elderly Bavarians."

# Loading of necessary packages and sourcing of self-defined functions:
library(tidyverse)
library(pec)
library(survival)
library(ggpubr)
library(lubridate)
library(viridis)
library(RColorBrewer)
library(scales)
source("Functions.R")

# Reading of the data:
Sys.setlocale("LC_ALL", "English")
load("Daten_Impfen_2022_09_07.RData")

# Data preparation:
data <- prepare_data(data_output)

# 1. Restricting our analysis to subjects older than 79 years
data1 <- prepare_data(data, type = "80+")
data1_model <- data1 %>% filter(Geschlecht != "divers") %>% droplevels()
model1 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv + Alter_surv_num +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data1_model)
plot_spline <- spline_plot(model1)
# ggsave(plot = plot_spline, filename = "Graphics/FigureS4.jpeg", width = 10,
#       height = 5)
plot_coef <- coef_plot(model1)
# ggsave(plot = plot_coef, filename = "Graphics/FigureS5.jpeg", width = 10,
#       height = 5)
# rr_sens <- compute_rr_metrics(data = data3_model, model = model3,
#                              type = "80+", confint = TRUE, samples = 1000)
# saveRDS(rr_sens, file = "Models/rr_ü80.rds")

# 2. Analyzing 30-day mortality
data2 <- prepare_data(data, type = "censoring")
data2_model <- data2 %>% filter(Geschlecht != "divers") %>% droplevels()
model2 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data2_model)
plot_spline <- spline_plot(model2)
# ggsave(plot = plot_spline, filename = "Graphics/FigureS6.jpeg", width = 10,
#       height = 5)
plot_coef <- coef_plot(model2)
# ggsave(plot = plot_coef, filename = "Graphics/FigureS7.jpeg", width = 10,
#       height = 5)


# 3. Using a modified definition of the dependent variable only considering
#    cases for whom physicians had certified COVID-19 as direct cause of death. 
#    Cases with an indirect cause of death, or cases whose cause of death had
#    been unknown, were treated as being censored at the date of death.
data3 <- prepare_data(data, type = "covid_only")
data3_model <- data3 %>% filter(Geschlecht != "divers") %>% droplevels()
model3 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data3_model)
plot_spline <- spline_plot(model3)
# ggsave(plot = plot_spline, filename = "Graphics/FigureS8.jpeg", width = 10,
#        height = 5)
plot_coef <- coef_plot(model2)
# ggsave(plot = plot_coef, filename = "Graphics/FigureS9.jpeg", width = 10,
#       height = 5)

# 4. Restricting our analysis to subjects whose vaccination status had been known
data4 <- prepare_data(data, type = "known_vaccination")
data4_model <- data4 %>% filter(Geschlecht != "divers") %>% droplevels()
model4 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data4_model)
plot_spline <- spline_plot(model4)
# ggsave(plot = plot_spline, filename = "Graphics/FigureS10.jpeg", width = 10,
#       height = 5)
plot_coef <- coef_plot(model4, type = "known_vaccinations")
# ggsave(plot = plot_coef, filename = "Graphics/FigureS11.jpeg", width = 10,
#       height = 5)




