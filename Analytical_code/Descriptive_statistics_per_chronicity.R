#Libraries

library(tidyverse)

#Import data

load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_chronic_disease_comparison.RData")
load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_non_chronic_disease_comparison.RData")

#Create descriptive table

imap_dfr(data_chronic_disease_comparison,
         function(data_rheuma_dg, rheuma_cohort_names) {
           
           map_dfr(data_rheuma_dg,
                   function(data_rheuma_and_psychiatric_dg) {
                     
                     data_rheuma_and_psychiatric_dg %>%
                       group_by(exposure) %>%
                       summarise(cohort = rheuma_cohort_names,
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
  group_by(cohort, exposure) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(comparison = "Counterparts with chronic diseases") %>%
  bind_rows(imap_dfr(data_non_chronic_disease_comparison,
                     function(data_rheuma_dg, rheuma_cohort_names) {
                       
                       map_dfr(data_rheuma_dg,
                               function(data_rheuma_and_psychiatric_dg) {
                                 
                                 data_rheuma_and_psychiatric_dg %>%
                                   group_by(exposure) %>%
                                   summarise(cohort = rheuma_cohort_names,
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
              group_by(cohort, exposure) %>%
              slice(1) %>%
              ungroup() %>%
              mutate(comparison = "Counterparts without chronic diseases")) %>%
  pivot_longer(-c(exposure, cohort, comparison)) %>%
  pivot_wider(names_from = c("cohort", "comparison", "exposure"),
              values_from = "value") %>%
  arrange(match(name, c("overall_n", "age", "females", "year_discharge", "month_discharge"))) %>%
  select(name, 
         `Rheumatoid arthritis or ankylosing spondylitis_Counterparts with chronic diseases_unexposed`,
         `Rheumatoid arthritis or ankylosing spondylitis_Counterparts with chronic diseases_exposed`,
         `Rheumatoid arthritis_Counterparts with chronic diseases_unexposed`,
         `Rheumatoid arthritis_Counterparts with chronic diseases_exposed`,
         `Ankylosing spondylitis_Counterparts with chronic diseases_unexposed`,
         `Ankylosing spondylitis_Counterparts with chronic diseases_exposed`,
         `Rheumatoid arthritis or ankylosing spondylitis_Counterparts without chronic diseases_unexposed`,
         `Rheumatoid arthritis or ankylosing spondylitis_Counterparts without chronic diseases_exposed`,
         `Rheumatoid arthritis_Counterparts without chronic diseases_unexposed`,
         `Rheumatoid arthritis_Counterparts without chronic diseases_exposed`,
         `Ankylosing spondylitis_Counterparts without chronic diseases_unexposed`,
         `Ankylosing spondylitis_Counterparts without chronic diseases_exposed`) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/Descriptive_statistics_per_chronicity.csv",
            row.names = FALSE)
