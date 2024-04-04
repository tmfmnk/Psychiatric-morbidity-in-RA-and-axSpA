#Libraries

library(tidyverse)
library(survival)
library(broom)
library(patchwork)
library(extrafont)
library(patchwork)
library(survminer)
library(ggpubr)

#Import data

load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_washout.RData")

#Absolute risk

imap_dfr(data_washout,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      data_rheuma_and_psychiatric_dg %>%
                        group_by(exposure) %>%
                        summarise(abs_risk = paste(formatC(sum(outcome == 1), big.mark = " "),
                                                   paste0("(",
                                                          formatC(round(mean(outcome) * 100, 2), format = "f", digits = 2),
                                                          ")"))) %>%
                        ungroup() %>%
                        mutate(cohort = rheuma_cohort_names,
                               outcome = outcome_names) 
                      
                    })
         }) %>%
  pivot_wider(names_from = c("cohort", "exposure"),
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
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/Abs_risk_washout.csv",
            row.names = FALSE)

#Fitting models

mfull <- imap_dfr(data_washout,
                  function(data_rheuma_dg, rheuma_cohort_names) {
                    
                    imap_dfr(data_rheuma_dg,
                             function(data_rheuma_and_psychiatric_dg, outcome_names) {
                               
                               tidy(coxph(Surv(years_until_outcome_or_censoring, outcome) ~ exposure + VEK + POHL + month_discharge + year_discharge + strata(RODCIS2_exposed), 
                                          data = data_rheuma_and_psychiatric_dg),
                                    conf.int = TRUE,
                                    exponentiate = TRUE) %>%
                                 mutate(cohort = rheuma_cohort_names,
                                        outcome = outcome_names) %>%
                                 filter(term == "exposureexposed")
                             })
                  })

#Table

mfull %>%
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
  arrange(cohort, order) %>%
  transmute(cohort,
            outcome,
            order,
            estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                             paste0("(", 
                                    formatC(round(conf.low, 2), format = "f", digits = 2),
                                    "; ",
                                    formatC(round(conf.high, 2), format = "f", digits = 2),
                                    ")"))) %>%
  pivot_wider(names_from = "cohort",
              values_from = "estimate") %>%
  select(-order) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/HR_washout.csv",
            row.names = FALSE)

#Graph

p1_ra_and_as <- mfull %>%
  filter(cohort == "Rheumatoid arthritis or ankylosing spondylitis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  transmute(cohort,
            outcome,
            estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                             paste0("(", 
                                    formatC(round(conf.low, 2), format = "f", digits = 2),
                                    "; ",
                                    formatC(round(conf.high, 2), format = "f", digits = 2),
                                    ")"))) %>%
  pivot_longer(-c(cohort, outcome),
               values_transform = list(value = as.character),
               names_to = "variable",
               values_to = "values")  %>%
  mutate(variables = factor(variable, "estimate")) %>%
  ggplot(aes(x = variable, y = outcome, label = values)) +
  geom_text(size = 4,
            family = "Times New Roman") +
  scale_x_discrete(position = "top", 
                   labels = c("aHR (95% CI)")) +
  scale_colour_identity() +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  facet_wrap(~ cohort,
             ncol = 1,
             nrow = 3,
             scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", hjust = 0, size = 12),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.title.y = element_text(family = "Times New Roman", size = 13),
        axis.line = element_blank(),
        axis.ticks = element_blank())

p1_ra <- mfull %>%
  filter(cohort == "Rheumatoid arthritis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  transmute(cohort,
            outcome,
            estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                             paste0("(", 
                                    formatC(round(conf.low, 2), format = "f", digits = 2),
                                    "; ",
                                    formatC(round(conf.high, 2), format = "f", digits = 2),
                                    ")"))) %>%
  pivot_longer(-c(cohort, outcome),
               values_transform = list(value = as.character),
               names_to = "variable",
               values_to = "values")  %>%
  mutate(variables = factor(variable, "estimate")) %>%
  ggplot(aes(x = variable, y = outcome, label = values)) +
  geom_text(size = 4,
            family = "Times New Roman") +
  scale_x_discrete(position = "top", 
                   labels = c("aHR (95% CI)")) +
  scale_colour_identity() +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  facet_wrap(~ cohort,
             ncol = 1,
             nrow = 3,
             scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", hjust = 0, size = 12),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.title.y = element_text(family = "Times New Roman", size = 13),
        axis.line = element_blank(),
        axis.ticks = element_blank())

p1_as <- mfull %>%
  filter(cohort == "Ankylosing spondylitis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  transmute(cohort,
            outcome,
            estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                             paste0("(", 
                                    formatC(round(conf.low, 2), format = "f", digits = 2),
                                    "; ",
                                    formatC(round(conf.high, 2), format = "f", digits = 2),
                                    ")"))) %>%
  pivot_longer(-c(cohort, outcome),
               values_transform = list(value = as.character),
               names_to = "variable",
               values_to = "values")  %>%
  mutate(variables = factor(variable, "estimate")) %>%
  ggplot(aes(x = variable, y = outcome, label = values)) +
  geom_text(size = 4,
            family = "Times New Roman") +
  scale_x_discrete(position = "top", 
                   labels = c("aHR (95% CI)")) +
  scale_colour_identity() +
  labs(y = NULL, x = NULL) +
  theme_classic() +
  facet_wrap(~ cohort,
             ncol = 1,
             nrow = 3,
             scales = "free") +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(family = "Times New Roman", hjust = 0, size = 12),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.title.y = element_text(family = "Times New Roman", size = 13),
        axis.line = element_blank(),
        axis.ticks = element_blank())

p2_ra_and_as <- mfull %>%
  filter(cohort == "Rheumatoid arthritis or ankylosing spondylitis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  ggplot(aes(x = outcome, 
             y = estimate, 
             ymin = conf.low, 
             ymax = conf.high)) +
  geom_point(size = 1.5, 
             stroke = 0.5,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.7,
                cex = 0.5,
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 1, 
             linetype = 2) +
  scale_y_log10(breaks = c(0.25, 0.5, 0.7, 1, 1.5, 2, 3, 5),
                limits = c(0.25, 5)) +
  coord_flip() +
  labs(x = NULL,
       y = "aHR (95% CI)") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(family = "Times New Roman", size = 14),
        axis.text.y =  element_blank(),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.ticks.y = element_blank())

p2_ra <- mfull %>%
  filter(cohort == "Rheumatoid arthritis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  ggplot(aes(x = outcome, 
             y = estimate, 
             ymin = conf.low, 
             ymax = conf.high)) +
  geom_point(size = 1.5, 
             stroke = 0.5,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.7,
                cex = 0.5,
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 1, 
             linetype = 2) +
  scale_y_log10(breaks = c(0.25, 0.5, 0.7, 1, 1.5, 2, 3, 5),
                limits = c(0.25, 5)) +
  coord_flip() +
  labs(x = NULL,
       y = "aHR (95% CI)") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(family = "Times New Roman", size = 14),
        axis.text.y =  element_blank(),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.ticks.y = element_blank())

p2_as <- mfull %>%
  filter(cohort == "Ankylosing spondylitis") %>%
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
  arrange(cohort, order) %>%
  mutate(outcome = fct_rev(factor(outcome, levels = unique(outcome))),
         outcome = fct_relabel(outcome, 
                               ~ ifelse(.x %in% c("Organic disorders",
                                                  "Substance use disorders",
                                                  "Psychotic disorders",
                                                  "Mood disorders",
                                                  "Anxiety disorders",
                                                  "Behavioural syndromes"),
                                        .x,
                                        paste0("       ", .x)))) %>%
  ggplot(aes(x = outcome, 
             y = estimate, 
             ymin = conf.low, 
             ymax = conf.high)) +
  geom_point(size = 1.5, 
             stroke = 0.5,
             position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.7,
                cex = 0.5,
                position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 1, 
             linetype = 2) +
  scale_y_log10(breaks = c(0.25, 0.5, 0.7, 1, 1.5, 2, 3, 5),
                limits = c(0.25, 5)) +
  coord_flip() +
  labs(x = NULL,
       y = "aHR (95% CI)") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(family = "Times New Roman", size = 14),
        axis.text.y =  element_blank(),
        axis.text.x = element_text(family = "Times New Roman", size = 12),
        axis.title.x = element_text(family = "Times New Roman", size = 13),
        axis.ticks.y = element_blank())

p_comb_ra_and_as <- p1_ra_and_as + p2_ra_and_as + 
  plot_layout(widths = c(3, 10)) +
  plot_annotation(title = "Rheumatoid arthritis or axial spondyloarthritis") & 
  theme(plot.title = element_text(family = "Times New Roman", size = 15, hjust = 0.5))

p_comb_ra <- p1_ra + p2_ra + 
  plot_layout(widths = c(3, 10)) +
  plot_annotation(title = "Rheumatoid arthritis") & 
  theme(plot.title = element_text(family = "Times New Roman", size = 15, hjust = 0.5))

p_comb_as <- p1_as + p2_as + 
  plot_layout(widths = c(3, 10)) +
  plot_annotation(title = "Axial spondyloarthritis") & 
  theme(plot.title = element_text(family = "Times New Roman", size = 15, hjust = 0.5))

wrap_elements(p_comb_ra_and_as) / wrap_elements(p_comb_ra)/ wrap_elements(p_comb_as)

ggsave(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/HR_washout.eps",
       device = cairo_ps,
       width = 40,
       height = 55,
       units = "cm",
       dpi = 300)

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

imap_dfr(data_washout,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      p <- ggsurvplot(surv_fit(Surv(years_until_outcome_or_censoring, outcome) ~ exposure, 
                                               data = data_rheuma_and_psychiatric_dg), 
                                      fun = "event",
                                      conf.int = TRUE, 
                                      conf.int.alpha = 0.3,
                                      risk.table = TRUE,
                                      cumcensor = TRUE,
                                      cumevents = TRUE,
                                      xlim = c(0, 20),
                                      break.x.by = 5,
                                      legend.labs = c(paste0("People without ", tolower(rheuma_cohort_names)), paste0("People with ", tolower(rheuma_cohort_names))),
                                      title = paste0(outcome_names, " following ", tolower(rheuma_cohort_names), ", with wash-out period"),
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
                             filename = paste0("Cumulative_event_plot_", tolower(outcome_names), "_", tolower(rheuma_cohort_names), "_washout", ".eps"),
                             path = "path/AZV_rheuma_risk_of_psych_hosp/Results/Cumulative_event_plots",
                             device = cairo_ps,
                             width = 15, 
                             height = 13, 
                             dpi = 300)
                    })
         })
