#Libraries

library(tidyverse)

#Import data

load(file = "path/AZV_rheuma_risk_of_psych_hosp/Data/data_main_analysis.RData")

#Create descriptive table

imap_dfr(data_main_analysis,
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
  pivot_longer(-c(exposure, cohort)) %>%
  pivot_wider(names_from = c("cohort", "exposure"),
              values_from = "value") %>%
  arrange(match(name, c("overall_n", "age", "females", "year_discharge", "month_discharge"))) %>%
  select(name, 
         `Rheumatoid arthritis or ankylosing spondylitis_unexposed`,
         `Rheumatoid arthritis or ankylosing spondylitis_exposed`,
         `Rheumatoid arthritis_unexposed`,
         `Rheumatoid arthritis_exposed`,
         `Ankylosing spondylitis_unexposed`,
         `Ankylosing spondylitis_exposed`) %>%
  write.csv(file = "path/AZV_rheuma_risk_of_psych_hosp/Results/Descriptive_statistics_main_analysis.csv",
            row.names = FALSE)
