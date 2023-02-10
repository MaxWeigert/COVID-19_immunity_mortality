###############################################################################
### This is the main code file containing the analyses conducted for the    ###
### publication "Association of primary and booster vaccination status with ###
### COVID-19 associated mortality during the omicron wave - A retrospective ###
### observational study in the elderly."                                    ###
###############################################################################

## Loading of necessary packages and sourcing of self-defined functions:
library(gridExtra)
library(lubridate)
library(pec)
library(readxl)
library(scales)
library(survival)
library(survminer)
library(tidyverse)
source("Code/Functions.R")

##################################
### Data Reading & Preparation ###
##################################

## Reading of the data:
# load("Daten_Impfen_2022_09_07.RData")

## Overview table of infections per age group in Bavaria:
data_pop <- read_excel("Data/Population.xlsx") %>%
  mutate(Alter = 0:(nrow(.) - 1)) %>% dplyr::select(Alter, Bayern) %>%
  mutate(AlterKat = case_when(Alter <= 5 ~ "00-05",
                              Alter > 5 & Alter <= 11 ~ "06-11",
                              Alter > 11 & Alter <= 15 ~ "12-15",
                              Alter > 15 & Alter <= 19 ~ "16-19",
                              Alter > 19 & Alter <= 34 ~ "20-34",
                              Alter > 34 & Alter <= 59 ~ "35-59",
                              Alter > 59 & Alter <= 64 ~ "60-64",
                              Alter > 64 & Alter <= 69 ~ "65-69",
                              Alter > 69 & Alter <= 74 ~ "70-74",
                              Alter > 74 & Alter <= 79 ~ "75-79",
                              Alter > 79 & Alter <= 84 ~ "80-84",
                              Alter > 84 & Alter <= 89 ~ "85-89",
                              Alter == 90 ~ "90+")) %>%
  group_by(AlterKat) %>% dplyr::summarise(pop = sum(Bayern))

data_cases <- data_output %>% filter(!is.na(AlterKat)) %>%
  group_by(AlterKat) %>% dplyr::summarise(cases = n())

data_pop <- full_join(data_pop, data_cases) %>%
  mutate(cases_rel = cases / pop)

sum(data_pop$cases[7:13]) / sum(data_pop$pop[7:13])
sum(data_pop$cases) / sum(data_pop$pop)

## Data preparation:
data <- prepare_data(data_output)

############################
### Descriptive analysis ###
############################

## Summary statistics:
data$Alter2 <- if_else(data$AlterKat %in% c("60-64", "65-69", "70-74", "75-79"),
                       true = "60-79", false = "80+")

table(data$VerstorbenStatus_surv, data$Geschlecht)
prop.table(table(data$VerstorbenStatus_surv, data$Geschlecht), margin = 2)

table(data$VerstorbenStatus_surv, data$Alter2)
prop.table(table(data$VerstorbenStatus_surv, data$Alter2), margin = 2)

table(data$VerstorbenStatus_surv, data$Impfstatus_surv)
prop.table(table(data$VerstorbenStatus_surv, data$Impfstatus_surv), margin = 2)

## Kaplan-Meier plot function stratified by vaccination status
# Adjusting plot settings 
grid.draw.ggsurvplot <- function(x){
  survminer:::print.ggsurvplot(x, newpage = FALSE)
}

plot_km_english <- km_plot(data)
# ggsave(plot = plot_km_english, filename = "Graphics/eFigure1.jpeg", width = 12.3,
#       height = 12.3)

plot_km_german <- km_plot(data, version = "german")
# ggsave(plot = plot_km_german, filename = "Graphics/eFigure1_german.jpeg", width = 12.3,
#       height = 12.3)


##############################################################
### Model-based analysis for all cases older than 59 years ###
##############################################################

## Fitting of the cox model:
data_model <- data %>% filter(Geschlecht != "divers") %>% droplevels()

model <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
               Geschlecht + Impfstatus_surv +
               pspline(Alter_surv_num, df = 4) +
               pspline(Erkrankungsdatum_num, df = 4),
               x = TRUE, data = data_model)
summary(model)
# saveRDS(model, file = "Models/model_all_cases.rds")

## Visualization of results:
plot_spline_english <- spline_plot(model)
# ggsave(plot = plot_spline_english, filename = "Graphics/Figure2.jpeg", width = 10,
#      height = 5)

plot_spline_german <- spline_plot(model, version = "german")
# ggsave(plot = plot_spline_german, filename = "Graphics/Figure2_german.jpeg", width = 10,
#        height = 5)

plot_coef_english <- coef_plot(model)
# ggsave(plot = plot_coef_english, filename = "Graphics/Figure3.jpeg", width = 10,
#       height = 5)

plot_coef_german <- coef_plot(model, version = "german")
# ggsave(plot = plot_coef_german, filename = "Graphics/Figure3_german.jpeg", width = 10,
#       height = 5)


## Computation of risk reduction metrics:
# (Warning: Running the following command is computationally very expensive)
# rr <- compute_rr_metrics(data = data_model, model = model, confint = TRUE,
#                         samples = 1000)
# rr
# saveRDS(rr, file = "Models/rr_all_cases.rds")


##############################################################
### Model-based analysis for all cases older than 79 years ###
##############################################################

## Data preparation:
data_80 <- prepare_data(data, type = "80+")

## Fitting of the Cox model:
data_80_model <- data_80 %>% filter(Geschlecht != "divers") %>% droplevels()

model_80 <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                   Geschlecht + Impfstatus_surv + Alter_surv_num +
                   pspline(Erkrankungsdatum_num, df = 4),
                  x = TRUE, data = data_80_model)
summary(model_80)
# saveRDS(model_80, file = "Models/model_80.rds")

# Computation of risk reduction metrics:
# rr_80 <- compute_rr_metrics(data = data_80_model, model = model_80,
#                              type = "80+", confint = TRUE, samples = 1000)
# saveRDS(rr_80, file = "Models/rr_80.rds")


##################################################################################
### Model-based analysis for all cases whose vaccination status had been known ###
##################################################################################

## Data preparation:
data_known <- prepare_data(data, type = "known_vaccination")

## Fitting of the Cox model:
data_known_model <- data_known %>% filter(Geschlecht != "divers") %>% droplevels()

model_known <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                       Geschlecht + Impfstatus_surv +
                       pspline(Alter_surv_num, df = 4) +
                       pspline(Erkrankungsdatum_num, df = 4),
                     x = TRUE, data = data_known_model)
summary(model_known)
# saveRDS(model, file = "Models/model_known.rds")

## Computation of risk reduction measures:
# (Warning: Running the following command is computationally very expensive)
# rr_known <- compute_rr_metrics(data = data_known_model, model = model_known,
#                               type = "known_vaccinations", confint = TRUE,
#                               samples = 1000)
# saveRDS(rr_known, file = "rr_known.rds")
