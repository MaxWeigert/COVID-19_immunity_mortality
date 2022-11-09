### This file contains the code for the sensitivity analyses conducted for the
### publication "Association of primary and booster vaccination status with 
### COVID-19 associated mortality during the omicron wave - A retrospective
### observational study in elderly Bavarians."

# Loading of necessary packages and sourcing of self-defined functions:
library(pec)
library(tidyverse)
library(dplyr)
library(splines)
library(survival)
library(survminer)
library(gridExtra)
library(ggpubr)
library(lubridate)
source("Functions.R")

# Data preparation:
data <- prepare_data(data_output)

# 1. Analyzing 30-day mortality
data1 <- prepare_data(data, type = "censoring")
data1_model <- data1 %>% filter(Geschlecht != "divers") %>% droplevels()
model1 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                 Geschlecht + Impfstatus_surv +
                 pspline(Alter_surv_num, df = 4) +
                 pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data1_model)
plot_spline <- spline_plot(model1)
ggsave(plot = plot_spline, filename = "Graphics/FigureS5.png", width = 10,
       height = 5)
plot_coef <- coef_plot(model1)
ggsave(plot = plot_coef, filename = "Graphics/FigureS6.png", width = 10,
       height = 5)


# 2. Using a modified definition of the dependent variable only considering cases for whom physicians had certified COVID-19 as direct cause of death. 
# Cases with an indirect cause of death, or cases whose cause of death had been unknown, were treated as being censored at the date of death.
data2 <- prepare_data(data, type = "covid_only")
data2_model <- data2 %>% filter(Geschlecht != "divers") %>% droplevels()
model2 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data2_model)
plot_spline <- spline_plot(model2)
ggsave(plot = plot_spline, filename = "Graphics/FigureS7.png", width = 10,
       height = 5)
plot_coef <- coef_plot(model2)
ggsave(plot = plot_coef, filename = "Graphics/FigureS8.png", width = 10,
       height = 5)

# 3. Restricting our analysis to subjects older than 79 years
data3 <- prepare_data(data, type = "80+")
data3_model <- data3 %>% filter(Geschlecht != "divers") %>% droplevels()
model3 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv + Alter_surv_num +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data3_model)
plot_spline <- spline_plot(model3)
ggsave(plot = plot_spline, filename = "Graphics/FigureS9.png", width = 10,
       height = 5)
plot_coef <- coef_plot(model3)
ggsave(plot = plot_coef, filename = "Graphics/FigureS10.png", width = 10,
       height = 5)

# 4. Restricting our analysis to subjects whose vaccination status had been known
data4 <- prepare_data(data, type = "known_vaccination")
data4_model <- data4 %>% filter(Geschlecht != "divers") %>% droplevels()
model4 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                  Geschlecht + Impfstatus_surv +
                  pspline(Alter_surv_num, df = 4) +
                  pspline(Erkrankungsdatum_num, df = 4),
                x = TRUE, data = data4_model)
plot_spline <- spline_plot(model4)
ggsave(plot = plot_spline, filename = "Graphics/FigureS11.png", width = 10,
       height = 5)
plot_coef <- coef_plot(model4, type = "known_vaccinations")
ggsave(plot = plot_coef, filename = "Graphics/FigureS12.png", width = 10,
       height = 5)




