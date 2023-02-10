######################################################
### File containing functions needed for the paper ###
######################################################

#####################################
### Function for data preparation ###
#####################################

## Function to prepare data for further analyses:
prepare_data <- function(data, type = "main") {
  
  # Labels without umlaut:
  levels(data$Geschlecht)[5] <- "maennlich"
  
  # Create earliest date of documented infection:
  data <- data %>%
    mutate(Erkrankungsdatum_surv = pmin(Erkrankungsbeginn, Meldedatum,
                                        na.rm = TRUE))
  
  # Adjustment of death date and disease recording:
  data <- data %>%
    mutate(Datum_diff = as.numeric(VerstorbenDatum) -
             as.numeric(Erkrankungsdatum_surv))
  med <- median(data$Datum_diff[data$AlterKat %in% c("60-64", "65-69", "70-74",
                                                     "75-79","80-84", "85-89",
                                                     "90+") &
                           data$Geschlecht %in% c("maennlich", "weiblich",
                                                  "divers") &
                             data$Datum_diff >= 0],
                na.rm = TRUE)
  
  # Death date earlier than earliest date of documented infection:
  data <- data %>%
    mutate(Erkrankungsdatum_surv = as.Date(if_else(!is.na(VerstorbenDatum) & 
                                                     (as.numeric(Erkrankungsdatum_surv) > as.numeric(VerstorbenDatum)) &
                                                     VerstorbenStatus == "Ja",
                                                   VerstorbenDatum - med, Erkrankungsdatum_surv)))
  
  # Missing death date:
  data <- data %>%
    mutate(VerstorbenDatum = as.Date(if_else(is.na(VerstorbenDatum) &
                                               VerstorbenStatus == "Ja",
                                             Erkrankungsdatum_surv + med,
                                             VerstorbenDatum)))
  
  # Correct time interval:
  data <- data %>%
    filter(between(Erkrankungsdatum_surv, as.Date("2022-01-01"),
                   as.Date("2022-06-30")))
  
  # Patients are filtered out if they are either: 
  # 1) younger than 60
  # 2) not "männlich", "weiblich" or "divers" 
  # 3) VerstorbenStatus is "nicht erhoben" or "nicht ermittelbar"
  data <- data %>%
    filter(AlterKat %in% c("60-64", "65-69", "70-74", "75-79","80-84", "85-89", "90+")) %>%
    filter(Geschlecht %in% c("maennlich", "weiblich", "divers"))
  
  # Data also with unknown outcome for descriptive plot:
  if (type == "unknown_outcome") {
    return(data)
  }
  
  # Adjustment of level of immunity:
  data <- data %>%
    filter(VerstorbenStatus %in% c("Ja", "Nein")) %>%
    droplevels() %>% 
    mutate(Impfstatus_surv = case_when(impfstatus %in% c("keine Angabe",
                                                         "unplausible oder unzureichende Angaben") |
                                         is.na(impfstatus) ~ "keine Angabe",
                                       TRUE ~ impfstatus))
  
  # Patients with age 80 or older:
  if (type == "80+") {
    data <- data %>% filter(Alter_surv %in% c("80-84", "85-89", "90+")) %>%
      droplevels()
  }
  
  # Death only of covid:
  if (type == "covid_only") {
    data <- data %>%
      mutate(VerstorbenStatus_surv = if_else(VerstorbenGrund != "an der gemeldeten Krankheit",
                                             0, VerstorbenStatus_surv))
  }
  
  # Exclusion of implausible observations:
  data <- data %>% filter(between(Erkrankungsdatum_surv, as.Date("2022-01-01"),
                                  as.Date("2022-06-30")))
  
  # Adjust Impfstatus according to time between Erkrankungsdatum and Impfdatum
  data <- data %>%
    mutate(Impfstatus_surv = case_when(Impfstatus_surv == "geboostert" &
                                         (as.numeric(Erkrankungsdatum_surv) - as.numeric(impfung_datum)) < 7 ~
                                         "grundimmunisiert",
                                       Impfstatus_surv == "grundimmunisiert" &
                                         (as.numeric(Erkrankungsdatum_surv) - as.numeric(impfung_datum)) < 14 ~
                                         "unvollstaendig grundimmunisiert",
                                       TRUE ~ Impfstatus_surv))
  
  # Differentiate between "geboostert" and "grundimmunisiert" if vaccination was within 3 or 6 months since infection
  data <- data %>%
    mutate(Impfstatus_surv = case_when(Impfstatus_surv == "geboostert" & interval(data$impfung_datum, data$Erkrankungsdatum_surv) %/% months(1) <= 3 ~
                                         "geboostert_within_3months",
                                       Impfstatus_surv == "grundimmunisiert" & interval(data$impfung_datum, data$Erkrankungsdatum_surv) %/% months(1) <= 6 ~
                                         "grundimmunisiert_within_6months",
                                       TRUE ~ Impfstatus_surv))
  
  data <- data %>%
    mutate(Alter_surv = as.factor(AlterKat),
           Impfstatus_surv = factor(Impfstatus_surv,
                                    levels = c("ungeimpft", "unvollstaendig grundimmunisiert",
                                               "grundimmunisiert", "grundimmunisiert_within_6months",
                                               "geboostert", "geboostert_within_3months", "keine Angabe")))
  levels(data$Impfstatus_surv) <- c("ungeimpft",
                                    "unvollstaendig grundimmunisiert",
                                    "grundimmunisiert (> 6 Monate)",
                                    "grundimmunisiert (\u2264 6 Monate)",
                                    "geboostert (> 3 Monate)",
                                    "geboostert (\u2264 3 Monate)", "keine Angabe")
  
  # Compute time between infection and death:
  data <- data %>%
    mutate(time = as.numeric(if_else(VerstorbenStatus == "Ja",
                                     VerstorbenDatum - Erkrankungsdatum_surv, NA_real_)))
  data <- data %>%
    mutate(VerstorbenStatus_surv = case_when(VerstorbenStatus == "Ja" ~ 1,
                                             VerstorbenStatus == "Nein" ~ 0))
  data <- data %>%
    mutate(Erkrankungsdatum_num = as.numeric(Erkrankungsdatum_surv) - min(as.numeric(Erkrankungsdatum_surv)))
  
  # Continuous age variable:
  data <- data %>%
    mutate(Alter_surv_num = case_when(Alter_surv == "60-64" ~ 62,
                                      Alter_surv == "65-69" ~ 67,
                                      Alter_surv == "70-74" ~ 72,
                                      Alter_surv == "75-79" ~ 77,
                                      Alter_surv == "80-84" ~ 82,
                                      Alter_surv == "85-89" ~ 87,
                                      Alter_surv == "90+" ~ 92))
  
  # Set NA time to maximum of follow-up time period (60 days):
  data <- data %>%
    mutate(time = if_else(is.na(time), 60, time))
  
  # Censor data to a follow-up time of 60 days. If patient survived longer than 60 days, the person will be treated as a survivor.
  data <- data %>%
    mutate(VerstorbenStatus_surv = if_else(time > 60, 0, VerstorbenStatus_surv)) %>%
    mutate(time = if_else(time > 60, 60 , time))
  
  # Censoring after 30 days:
  if (type == "censoring") {
    data <- data %>%
      mutate(VerstorbenStatus_surv = if_else(time > 30, 0, VerstorbenStatus_surv)) %>%
      mutate(time = if_else(time > 30, 30 , time))
  }
  
  # Restricting analysis to subjects whose vaccination status had been known
  if (type == "known_vaccination") {
    data <- data %>% filter(Impfstatus_surv != "keine Angabe") %>%
      mutate(Impfstatus_surv = droplevels(Impfstatus_surv))
  }
  
  return(data)
}

###################################################
### Function to compute risk reduction measures ###
###################################################

## Function to compute risk reduction measures for a Cox PH model:
compute_rr_metrics <- function(model, data, type = "main",
                               confint = FALSE, samples = 100, seed = 1234) {
  
  # Set random numbers:
  set.seed(seed)
  
  # Compute ARR, RRR and NNT estimates:
  metrics_list <- lapply(X = levels(data$Impfstatus_surv)[2:length(levels(data$Impfstatus_surv))],
                         FUN = function(category) {
                           estimate_rr_category(data, model, category)})
  metrics <- bind_rows(metrics_list)
  
  # Estimate confidence intervals based on non-parametric bootstrap:
  if (confint == TRUE) {
    
    # Draw bootstrap samples:
    sample_ids <- lapply(X = 1:samples, FUN = function(i) {
      sample <- sample(x = 1:nrow(data), size = nrow(data), replace = TRUE)
      return(sample)
    })
    sample_ids <- as.data.frame(bind_cols(sample_ids))
    
    bootstrap_metrics <- lapply(X = 1:samples, FUN = function(i) {
      print(i)
      
      # Estimate Cox model:
      data_boot <- data %>% slice(sample_ids[, i])
      
      if (type %in% c("main", "known_vaccinations")) {
        model_boot <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                              Geschlecht + Impfstatus_surv +
                              pspline(Alter_surv_num, df = 4) +
                              pspline(Erkrankungsdatum_num, df = 4),
                            x = TRUE, data = data_boot)
      }
      if (type == "80+") {
        model_boot <- coxph(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                              Geschlecht + Impfstatus_surv + Alter_surv_num +
                              pspline(Erkrankungsdatum_num, df = 4),
                            x = TRUE, data = data_boot)
      }
      
      # Compute metrics:
      metrics_list <- lapply(X = levels(data_boot$Impfstatus_surv)[2:length(levels(data_boot$Impfstatus_surv))],
                             FUN = function(category) {
                               estimate_rr_category(data, model_boot, category)})
      metrics <- bind_rows(metrics_list)
      return(metrics)
    })
    
    # Compute bootstrap quantiles:
    quantiles_0.025 <- data.frame("category" = bootstrap_metrics[[1]]$category)
    quantiles_0.975 <- data.frame("category" = bootstrap_metrics[[1]]$category)
    quantiles_0.025[, 1] <- bootstrap_metrics[[1]]$category
    quantiles_0.975[, 1] <- bootstrap_metrics[[1]]$category
    
    for(i in 1:nrow(bootstrap_metrics[[1]])) {
      for (j in 2:ncol(bootstrap_metrics[[1]])) {
        quantiles <- quantile(x = sapply(X = 1:samples,
                                         FUN = function(k) {
                                           bootstrap_metrics[[k]][i, j]}),
                              probs = c(0.025, 0.975))
        quantiles_0.025[i, j] <- quantiles[1]
        quantiles_0.975[i, j] <- quantiles[2]
      }
    }
    colnames(quantiles_0.025) <- c("category", "rrr", "arr", "ntt")
    colnames(quantiles_0.975) <- c("category", "rrr", "arr", "ntt")
  }
  
  # Preparation of output:
  results <- list("metrics" = metrics)
  if (confint == TRUE) {
    results$quantiles_0.025 <- quantiles_0.025
    results$quantiles_0.975 <- quantiles_0.975
  }
  return(results)
}

# Function to estimate risk reduction measures for a single category:
estimate_rr_category <- function(data, model, category) {
  # Compute ARR, RRR and NNT estimates:
  data_treat <- data %>% mutate(Impfstatus_surv = category)
  pred_treat <- 1 - predictSurvProb(object = model, newdata = data_treat,
                                         times = 60)
  
  data_control <- data %>% mutate(Impfstatus_surv = "ungeimpft")
  pred_control <- 1 - predictSurvProb(object = model,
                                           newdata = data_control, times = 60)
  
  rrr <- mean(pred_control - pred_treat) / mean(pred_control)
  arr <- mean(pred_control - pred_treat)
  ntt <- 1 / arr
  
  results <- data.frame("category" = category, "rrr" = rrr, "arr" = arr,
                        "ntt" = ntt)
  return(results)
}


######################
### Plot Functions ###
######################

## ggplot theme:
theme <- theme_bw() +
  theme(text = element_text(size = 16), axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16),
        legend.key.width = unit(2, "lines"),
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        strip.text = element_text(size = 18, face = "bold"),
        strip.text.y = element_text(size = 16), legend.text.align = 0,
        strip.placement = "outside", strip.background = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)))


## Kaplan Meier Plot Functions:
km_plot <- function(data, version = "english", conf_int = FALSE) {
  
  # Reorder levels of Impfstatus_surv:
  data$Impfstatus_surv <- factor(data$Impfstatus_surv,
                                 levels = c(levels(data$Impfstatus_surv)[c(6:1, 7)]))
  
  # Define parameters for naming:
  if (version == "english") {
    x_name <- "Days after the date of infection"
    y_name <- "Survival probability"
    legend_labels <- c(rev(c("unvaccinated",
                       "incomplete primary LIm",
                       "full primary LIm achieved more than six months before the date of infection", 
                       "full primary LIm achieved less than six months before the date of infection",
                       "boosted LIm achieved more than three months before the date of infection",
                       "boosted LIm achieved less than three months before the date of infection")),
                       "unknown")
    table_title <- "Number of cases at risk"
  }
  if (version == "german") {
    options(encoding = "UTF-8")
    x_name <- "Tage seit dem Zeitpunkt der Infektion"
    y_name <- "Überlebenswahrscheinlichkeit"
    legend_labels <- c(rev(c("nicht geimpft",
                       "unvollständiges primäres Immunitätsniveau",
                       "vollständiges primäres Immunitätsniveau erreicht mehr als sechs Monate vor dem Zeitpunkt der Infektion", 
                       "vollständiges primäres Immunitätsniveau erreicht weniger als sechs Monate vor dem Zeitpunkt der Infektion",
                       "geboostertes Immunitätsniveau erreicht mehr als drei Monate vor dem Zeitpunkt der Infektion",
                       "geboostertes Immunitätsniveau erreicht weniger als drei Monate vor dem Zeitpunkt der Infektion")),
                       "unbekannt")
    table_title <- "Anzahl der gefährdeten Fälle "
  }
  
  # Kaplan-Meier plot function stratified by vaccination status
    km_fit <- survfit(formula = Surv(time = time, event = VerstorbenStatus_surv) ~
                        Impfstatus_surv, type = "kaplan-meier",
                      data = data)
    
    gg_km <- ggsurvplot(fit = km_fit, data = data, conf.int = conf_int, risk.table = TRUE, 
                        xlab = x_name,
                        ylab = y_name, title = "",
                        ylim = c(0.97, 1), 
                        surv.scale = "percent",
                        palette = c(rev(c("#480091", "#002050" , "#31688e",
                                          "#A3D0D4", "#026645", "#90d743")), "grey"),
                        legend.labs = legend_labels,
                        legend.title = "",
                        risk.table.title = table_title,
                        legend = "top", fontsize = 6,
                        risk.table.height = 0.35,
                        risk.table.ytext = FALSE,
                        tables.y.text = FALSE, conf.int.alpha = 0.5, 
                        ggtheme = theme +
                          theme(legend.justification = "left"))
    
    gg_km$plot <- gg_km$plot + guides(colour = guide_legend(nrow = length(legend_labels)))
    gg_km$table <- gg_km$table + labs(x = NULL, y = NULL) +
      theme(plot.margin=unit(c(1,0,1.5,0),"cm"))
  
  return(gg_km)
}


## Coefficient plot of Cox-PH-model:
coef_plot <- function(model, type = "main", version = "english") {
  
  # Define parameters for naming:
  if (version == "english") {
    x_name <- "Level of immunity"
    y_name <- "Hazard ratio"
    sep_labels <- c("unvaccinated", "incomplete\n primary", "full primary\n > 6 months", 
                    "full primary\n \u2264 6 months", "boosted\n > 3 months", 
                    "boosted\n \u2264 3 months", "unknown")
  }
  if (version == "german") {
    options(encoding = "UTF-8")
    x_name <- "Immunitätsniveau"
    y_name <- "Hazard Ratio"
    sep_labels <- c("nicht geimpft", "unvollständig\n primär", "vollständig\n primär\n > 6 Monate", 
                    "vollständig\n primär\n \u2264 6 Monate", "geboostert\n > 3 Monate", 
                    "geboostert\n \u2264 3 Monate", "unbekannt")
  }
  
  # Prepare plot data:
  plot_dat <- data.frame(summary(model)$conf.int[,-2])
  plot_dat <- plot_dat %>% filter(grepl("Impfstatus", rownames(plot_dat)))
  plot_dat <- rbind(c(1, 0, 0), plot_dat)
  plot_dat <- cbind(param = rownames(plot_dat), plot_dat)
  rownames(plot_dat) <- NULL
  plot_dat <- plot_dat %>%
    dplyr::rename(coef = exp.coef., CI_lower = lower..95, CI_upper = upper..95)

  if (type == "known_vaccinations") {
    sep_labels[-length(sep_labels)]
  }
  
  # Graphical visualization:
  gg_coef <- ggplot(plot_dat, mapping = aes(x = param, y = coef)) +
    geom_hline(yintercept = 1, col = gray(0.3), lty = 2) +
    geom_pointrange(mapping = aes(ymin = CI_lower, ymax = CI_upper),
                    size = 1, fatten = 3) +
    geom_point() + theme +
    scale_x_discrete(limits=plot_dat$param, labels= sep_labels) +
    scale_y_continuous(trans = log_trans(), limits = c(0.05, 30),
                       breaks = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
                       labels = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16)) +
    labs(x = x_name, y = y_name)
  gg_coef
}

## Spline plot of Cox-PH-model:
spline_plot <- function(model, terms = c(4,3), type = "main", version = "english") {
  
  # Define parameters for naming:
  if (version == "english") {
    x_name_age <- "Age [in years]"
    y_name_age <- "Hazard ratio"
    x_name_time <- "Date of infection"
    y_name_time <- ""
  }
  if (version == "german") {
    x_name_age <- "Alter [in Jahren]"
    y_name_age <- "Hazard Ratio"
    x_name_time <- "Zeitpunkt der Infektion"
    y_name_time <- ""
  }
  
  # Prepare plot data:
  plt_data_1 <- termplot(model, term = terms[1], se=TRUE, col.term = 1, col.se = 1, plot = FALSE)
  plt_data_1 <- plt_data_1$Erkrankungsdatum_num
  plt_data_1 <- plt_data_1 %>% 
    mutate(y_exp = exp(y),
           lower_exp = exp(y - qnorm(0.975) * se),
           upper_exp = exp(y + qnorm(0.975) * se),
           dates = seq(min(data$Erkrankungsdatum_surv), by = "day", length.out = max(data$Erkrankungsdatum_num)+1)
    )
  
  # Graphical visualization:
  # Time effect:
  plt_1 <- ggplot(data=plt_data_1, aes(x=dates, y=y_exp), ggtheme = theme) + 
    geom_line() + theme +
    scale_x_date(breaks = seq(from = as.Date("2022-01-01"),
                              to = as.Date("2022-06-30"), by = "1 month"),
                 date_labels =  "%b",
                 limits = c(as.Date("2022-01-01"), as.Date("2022-06-30"))) +
    geom_hline(yintercept = 1, lty = "dashed") +
    scale_y_continuous(trans = log_trans(), limits = c(0.05, 30),
                       breaks = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
                       labels = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
                       position = "right") +
    labs(y= y_name_time, x= x_name_time)
  
  plt_1 <- plt_1 + geom_ribbon(aes(ymin=lower_exp, ymax=upper_exp), linetype=2, alpha=0.1)
  
  # Age effect:
    plt_data_2 <- termplot(model, term = terms[2], se=TRUE, col.term = 1, col.se = 1, plot = FALSE)
    plt_data_2 <- plt_data_2$Alter_surv
    plt_data_2 <- plt_data_2 %>% 
      mutate(y_exp = exp(y),
             lower_exp = exp(y - qnorm(0.975) * se),
             upper_exp = exp(y + qnorm(0.975) * se))
    
    plt_2 <- ggplot(data=plt_data_2, aes(x=x, y=y_exp, group = 1), ggtheme = theme) + 
      geom_line() + theme +
      geom_hline(yintercept = 1, lty = "dashed") +
      scale_x_continuous(breaks = seq(from = 60, to = 95, by = 5),
                         labels = seq(from = 60, to = 95, by = 5)) +
      scale_y_continuous(trans = log_trans(), limits = c(0.05, 30),
                         breaks = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16),
                         labels = c(0, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16)) +
      labs(y = y_name_age, x = x_name_age)
    
    plt_2 <- plt_2 + geom_ribbon(aes(ymin=lower_exp, ymax=upper_exp), linetype=2, alpha=0.1)
    
    # Common visualization:
    sp_gg <- grid.arrange(plt_2, plt_1, ncol=2)
  return(sp_gg)
}



