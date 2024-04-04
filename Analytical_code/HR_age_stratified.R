#Libraries

library(tidyverse)
library(survival)
library(broom)
library(patchwork)
library(survminer)
library(ggpubr)

#Import data

load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_main_analysis.RData")

#Absolute risk

imap_dfr(data_main_analysis,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      data_rheuma_and_psychiatric_dg %>%
                        mutate(age_group = case_when(VEK <= 40 ~ "40 or less",
                                                     VEK >= 41 & VEK <= 59 ~ "41 to 59",
                                                     VEK >= 60 ~ "60 or more")) %>%
                        group_by(age_group, exposure) %>%
                        summarise(abs_risk = paste(formatC(sum(outcome == 1), big.mark = " "),
                                                   paste0("(",
                                                          formatC(round(mean(outcome) * 100, 2), format = "f", digits = 2),
                                                          ")"))) %>%
                        ungroup() %>%
                        mutate(cohort = rheuma_cohort_names,
                               outcome = outcome_names) 
                      
                    })
         }) %>%
  pivot_wider(names_from = c("cohort", "exposure", "age_group"),
              values_from = "abs_risk") %>%
  mutate(order = case_when(outcome == "Organic disorders" ~ 1,
                           outcome == "Alzheimer’s disease" ~ 2,
                           outcome == "Substance use disorders" ~ 3,
                           outcome == "Alcohol use disorders" ~ 4,
                           outcome == "Drug use disorders" ~ 5,
                           outcome == "Psychotic disorders" ~ 6,
                           outcome == "Schizophrenia" ~ 7,
                           outcome == "Mood disorders" ~ 8,
                           outcome == "Bipolar disorder" ~ 9,
                           outcome == "Depression" ~ 10,
                           outcome == "Moderate or severe depression" ~ 11,
                           outcome == "Anxiety disorders" ~ 12,
                           outcome == "Other anxiety disorders" ~ 13,
                           outcome == "Reaction to severe stress" ~ 14,
                           outcome == "Somatoform disorders" ~ 15,
                           outcome == "Other neurotic disorders" ~ 16,
                           outcome == "Behavioural syndromes" ~ 17)) %>%
  arrange(order) %>%
  select(-order) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/Abs_risk_age_stratified.csv",
            row.names = FALSE)

#Fitting models
#Up to 40 years

mfull_up_to_40y <- imap_dfr(data_main_analysis,
                  function(data_rheuma_dg, rheuma_cohort_names) {
                    
                    imap_dfr(data_rheuma_dg,
                             function(data_rheuma_and_psychiatric_dg, outcome_names) {
                               
                               tidy(coxph(Surv(years_until_outcome_or_censoring, outcome) ~ exposure + VEK + POHL + month_discharge + year_discharge + strata(RODCIS2_exposed), 
                                          data = data_rheuma_and_psychiatric_dg,
                                          subset = VEK <= 40),
                                    conf.int = TRUE,
                                    exponentiate = TRUE) %>%
                                 mutate(cohort = rheuma_cohort_names,
                                        outcome = outcome_names) %>%
                                 filter(term == "exposureexposed")
                             })
                  })

#41 to 59 years

mfull_41_59y <- imap_dfr(data_main_analysis,
                            function(data_rheuma_dg, rheuma_cohort_names) {
                              
                              imap_dfr(data_rheuma_dg,
                                       function(data_rheuma_and_psychiatric_dg, outcome_names) {
                                         
                                         tidy(coxph(Surv(years_until_outcome_or_censoring, outcome) ~ exposure + VEK + POHL + month_discharge + year_discharge + strata(RODCIS2_exposed), 
                                                    data = data_rheuma_and_psychiatric_dg,
                                                    subset = VEK >= 41 & VEK <= 59),
                                              conf.int = TRUE,
                                              exponentiate = TRUE) %>%
                                           mutate(cohort = rheuma_cohort_names,
                                                  outcome = outcome_names) %>%
                                           filter(term == "exposureexposed")
                                       })
                            })

#60 years or more 

mfull_60y_or_more <- imap_dfr(data_main_analysis,
                         function(data_rheuma_dg, rheuma_cohort_names) {
                           
                           imap_dfr(data_rheuma_dg,
                                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                                      
                                      tidy(coxph(Surv(years_until_outcome_or_censoring, outcome) ~ exposure + VEK + POHL + month_discharge + year_discharge + strata(RODCIS2_exposed), 
                                                 data = data_rheuma_and_psychiatric_dg,
                                                 subset = VEK >= 60),
                                           conf.int = TRUE,
                                           exponentiate = TRUE) %>%
                                        mutate(cohort = rheuma_cohort_names,
                                               outcome = outcome_names) %>%
                                        filter(term == "exposureexposed")
                                    })
                         })
#Table

mfull_up_to_40y %>%
  mutate(age_group = "40 years or less") %>%
  bind_rows(mfull_41_59y %>%
              mutate(age_group = "41 to 59 years")) %>%
  bind_rows(mfull_60y_or_more %>%
              mutate(age_group = "60 years or more")) %>%
  mutate(age_group = factor(age_group,
                            levels = c("40 years or less",
                                       "41 to 59 years",
                                       "60 years or more"))) %>%
  mutate(cohort = factor(cohort,
                         levels = c("Rheumatoid arthritis or ankylosing spondylitis",
                                    "Rheumatoid arthritis",
                                    "Ankylosing spondylitis"))) %>%
  mutate(order = case_when(outcome == "Organic disorders" ~ 1,
                           outcome == "Alzheimer’s disease" ~ 2,
                           outcome == "Substance use disorders" ~ 3,
                           outcome == "Alcohol use disorders" ~ 4,
                           outcome == "Drug use disorders" ~ 5,
                           outcome == "Psychotic disorders" ~ 6,
                           outcome == "Schizophrenia" ~ 7,
                           outcome == "Mood disorders" ~ 8,
                           outcome == "Bipolar disorder" ~ 9,
                           outcome == "Depression" ~ 10,
                           outcome == "Moderate or severe depression" ~ 11,
                           outcome == "Anxiety disorders" ~ 12,
                           outcome == "Other anxiety disorders" ~ 13,
                           outcome == "Reaction to severe stress" ~ 14,
                           outcome == "Somatoform disorders" ~ 15,
                           outcome == "Other neurotic disorders" ~ 16,
                           outcome == "Behavioural syndromes" ~ 17)) %>%
  arrange(cohort, age_group, order) %>%
  transmute(cohort,
            age_group,
            outcome,
            order,
            estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                             paste0("(", 
                                    formatC(round(conf.low, 2), format = "f", digits = 2),
                                    "; ",
                                    formatC(round(conf.high, 2), format = "f", digits = 2),
                                    ")"))) %>%
  pivot_wider(names_from = c("cohort", "age_group"),
              values_from = "estimate") %>%
  select(-order) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/HR_age_stratified.csv",
            row.names = FALSE)


#Cumulative event plots
#Define custom plotting function

custom_theme <- function() {
  theme_survminer() %+replace%
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.title.y = element_text(face = "bold", size = 10, angle = 90),
          axis.text.x = element_text(face = "bold", size = 10),
          axis.text.y = element_text(face = "bold", size = 10),
          axis.ticks.x = element_blank(),    
          axis.ticks.y = element_blank(),    
          legend.text = element_text(face = "bold", size = 10),
          legend.title = element_blank())
}

#Programmatic ploting
#40 years or less

imap_dfr(data_main_analysis,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      p <- ggsurvplot(surv_fit(Surv(years_until_outcome_or_censoring, outcome) ~ exposure, 
                                               data = data_rheuma_and_psychiatric_dg %>% 
                                                 filter(VEK <= 40)), 
                                      fun = "event",
                                      conf.int = TRUE, 
                                      conf.int.alpha = 0.3,
                                      risk.table = TRUE,
                                      cumcensor = TRUE,
                                      cumevents = TRUE,
                                      xlim = c(0, 20),
                                      break.x.by = 5,
                                      legend.labs = c(paste0("People without ", tolower(rheuma_cohort_names)), paste0("People with ", tolower(rheuma_cohort_names))),
                                      title = paste0(outcome_names, " following ", tolower(rheuma_cohort_names), ", 40 years or less"),
                                      xlab = "Time (years) since the health condition",
                                      ylab = "Cumulative event (95% CI)",
                                      palette = c("blue", "red"),
                                      legend.title = "",
                                      ggtheme = custom_theme())
                      
                      p1 = p$plot
                      p2 = p$table
                      p3 = p$ncensor.plot
                      p4 <- p$cumevents
                      plots = cowplot::plot_grid(p1, p2, p3, p4, align = "v", ncol = 1, rel_heights = c(4, 1, 1, 1))
                      
                      ggsave(plot = plots,
                             filename = paste0("Cumulative_event_plot_", tolower(outcome_names), "_", tolower(rheuma_cohort_names), "_40_or_less_years", ".eps"),
                             path = "path/AZV_rheuma_risk_of_psych_hosp/Results/Cumulative_event_plots",
                             device = cairo_ps,
                             width = 15, 
                             height = 13, 
                             dpi = 300)
                    })
         })

#41-59 years

imap_dfr(data_main_analysis,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      p <- ggsurvplot(surv_fit(Surv(years_until_outcome_or_censoring, outcome) ~ exposure, 
                                               data = data_rheuma_and_psychiatric_dg %>% 
                                                 filter(VEK >= 41 & VEK <= 59)), 
                                      fun = "event",
                                      conf.int = TRUE, 
                                      conf.int.alpha = 0.3,
                                      risk.table = TRUE,
                                      cumcensor = TRUE,
                                      cumevents = TRUE,
                                      xlim = c(0, 20),
                                      break.x.by = 5,
                                      legend.labs = c(paste0("People without ", tolower(rheuma_cohort_names)), paste0("People with ", tolower(rheuma_cohort_names))),
                                      title = paste0(outcome_names, " following ", tolower(rheuma_cohort_names), ", 41 to 59 years"),
                                      xlab = "Time (years) since the health condition",
                                      ylab = "Cumulative event (95% CI)",
                                      palette = c("blue", "red"),
                                      legend.title = "",
                                      ggtheme = custom_theme())
                      
                      p1 = p$plot
                      p2 = p$table
                      p3 = p$ncensor.plot
                      p4 <- p$cumevents
                      plots = cowplot::plot_grid(p1, p2, p3, p4, align = "v", ncol = 1, rel_heights = c(4, 1, 1, 1))
                      
                      ggsave(plot = plots,
                             filename = paste0("Cumulative_event_plot_", tolower(outcome_names), "_", tolower(rheuma_cohort_names), "_41_to_59_years", ".eps"),
                             path = "path/AZV_rheuma_risk_of_psych_hosp/Results/Cumulative_event_plots",
                             device = cairo_ps,
                             width = 15, 
                             height = 13, 
                             dpi = 300)
                    })
         })

#60 or more years

imap_dfr(data_main_analysis,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      p <- ggsurvplot(surv_fit(Surv(years_until_outcome_or_censoring, outcome) ~ exposure, 
                                               data = data_rheuma_and_psychiatric_dg %>% 
                                                 filter(VEK >= 60)), 
                                      fun = "event",
                                      conf.int = TRUE, 
                                      conf.int.alpha = 0.3,
                                      risk.table = TRUE,
                                      cumcensor = TRUE,
                                      cumevents = TRUE,
                                      xlim = c(0, 20),
                                      break.x.by = 5,
                                      legend.labs = c(paste0("People without ", tolower(rheuma_cohort_names)), paste0("People with ", tolower(rheuma_cohort_names))),
                                      title = paste0(outcome_names, " following ", tolower(rheuma_cohort_names), ", 60 or more years"),
                                      xlab = "Time (years) since the health condition",
                                      ylab = "Cumulative event (95% CI)",
                                      palette = c("blue", "red"),
                                      legend.title = "",
                                      ggtheme = custom_theme())
                      
                      p1 = p$plot
                      p2 = p$table
                      p3 = p$ncensor.plot
                      p4 <- p$cumevents
                      plots = cowplot::plot_grid(p1, p2, p3, p4, align = "v", ncol = 1, rel_heights = c(4, 1, 1, 1))
                      
                      ggsave(plot = plots,
                             filename = paste0("Cumulative_event_plot_", tolower(outcome_names), "_", tolower(rheuma_cohort_names), "_60_or_more_years", ".eps"),
                             path = "path/AZV_rheuma_risk_of_psych_hosp/Results/Cumulative_event_plots",
                             device = cairo_ps,
                             width = 15, 
                             height = 13, 
                             dpi = 300)
                    })
         })

