#Libraries

library(tidyverse)

#Import data

load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_washout.RData")

#Create descriptive table

imap_dfr(data_washout,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           imap_dfr(data_rheuma_dg,
                    function(data_rheuma_and_psychiatric_dg, outcome_names) {
                      
                      data_rheuma_and_psychiatric_dg %>%
                        group_by(exposure) %>%
                        summarise(cohort = rheuma_cohort_names,
                                  outcome = outcome_names,
                                  overall_n = formatC(n(), big.mark = " "),
                                  age =  paste(formatC(round(mean(VEK), 2), format = "f", digits = 2),
                                               paste0("(",
                                                      formatC(round(sd(VEK), 2), format = "f", digits = 2),
                                                      ")")),
                                  females = paste(formatC(sum(POHL == 2), big.mark = " "),
                                                  paste0("(",
                                                         formatC(round(sum(POHL == 2)/n() * 100, 2), format = "f", digits = 2),
                                                         ")")),
                                  year_discharge = paste(median(year_discharge),
                                                         paste0("(",
                                                                paste0(quantile(year_discharge, 0.25, na.rm = TRUE),
                                                                       "-",
                                                                       quantile(year_discharge, 0.75, na.rm = TRUE)),
                                                                ")")),
                                  month_discharge = paste(median(month_discharge),
                                                          paste0("(",
                                                                 paste0(quantile(month_discharge, 0.25, na.rm = TRUE),
                                                                        "-",
                                                                        quantile(month_discharge, 0.75, na.rm = TRUE)),
                                                                 ")"))) %>%
                        ungroup()
                    })
         }) %>%
  pivot_wider(names_from = "exposure",
              values_from = c("overall_n", "age", "females", "year_discharge", "month_discharge")) %>%
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
  select(-order) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/Descriptive_statistics_washout.csv",
            row.names = FALSE)
