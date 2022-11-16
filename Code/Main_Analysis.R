### This is the main code file containing the analyses conducted for the
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
source("Code/Functions.R")


################################################################################

# Reading of the data:
Sys.setlocale("LC_ALL", "English")
load("Daten_Impfen_2022_09_07.RData")

# Data preparation:
data <- prepare_data(data_output)

################################################################################

## Descriptive analysis:

# Summary statistics:
data$Alter2 <- if_else(data$AlterKat %in% c("60-64", "65-69", "70-74", "75-79"),
                       true = "60-79", false = "80+")
table(data$VerstorbenStatus_surv, data$Geschlecht)
prop.table(table(data$VerstorbenStatus_surv, data$Geschlecht), margin = 2)
table(data$VerstorbenStatus_surv, data$Alter2)
prop.table(table(data$VerstorbenStatus_surv, data$Alter2), margin = 2)
table(data$VerstorbenStatus_surv, data$Impfstatus_surv)
prop.table(table(data$VerstorbenStatus_surv, data$Impfstatus_surv), margin = 2)

# Descriptive plots:
# Unknown outcome:
data_unknown <- prepare_data(data_output, type = "unknown_outcome")
plot_outcome <- line_plot(data = data_unknown, type = "unknown_outcome")
# ggsave(plot = plot_outcome, filename = "Graphics/FigureS1.jpeg", width = 10,
#       height = 5)
# Causes of death:
plot_causes <- cause_plot(data)
# ggsave(plot = plot_causes, filename = "Graphics/FigureS2.jpeg", width = 10,
#       height = 5)
# Unknown vaccination status:
plot_vaccine <- line_plot(data = data, type = "unknown_vaccination")
# ggsave(plot = plot_vaccine, filename = "Graphics/FigureS3.jpeg", width = 10,
#       height = 5)

# Kaplan-Meier plot function stratified by vaccination status
plot_km <- km_plot(data)
# ggsave(plot = plot_km, filename = "Graphics/Figure2.jpeg", width = 10,
#       height = 6)


################################################################################

## Model-based analysis:

# Fitting of the cox model:
data_model <- data %>% filter(Geschlecht != "divers") %>% droplevels()
model <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                 Geschlecht + Impfstatus_surv +
                 pspline(Alter_surv_num, df = 4) +
                 pspline(Erkrankungsdatum_num, df = 4),
               x = TRUE, data = data_model)
summary(model)
# saveRDS(model, file = "Models/Main_model.rds")

# Visualization of results:
plot_spline <- spline_plot(model)
# ggsave(plot = plot_spline, filename = "Graphics/Figure3.jpeg", width = 10,
#       height = 5)
plot_coef <- coef_plot(model)
# ggsave(plot = plot_coef, filename = "Graphics/Figure4.jpeg", width = 10,
#       height = 5)

# Computation of risk reduction metrics:
rr <- compute_rr_metrics(data = data_model, model = model, confint = TRUE,
                         samples = 1000)
rr
# saveRDS(rr, file = "Models/rr.rds")










