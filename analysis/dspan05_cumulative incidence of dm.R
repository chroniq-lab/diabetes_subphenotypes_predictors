rm(list = ls());gc();source(".Rprofile")

library(survival)
library(survminer)
library(ggplot2)

df <- cross_df %>% 
  mutate(cluster = factor(cluster,
                          levels = c("MOD", "SIDD", "MARD", "SIDD"),
                          labels = c("MOD", "SIDD", "MARD", "SIDD")))

cox_fit = coxph(Surv(time_to_event, event) ~ strata(cluster) + race + female + age + bmi + hba1c + homa2b + homa2ir 
                + ldlc + sbp + egfr_ckdepi_2021, 
                data = cross_df, weights = ipcw_cluster)

incidence_fig = survfit2(cox_fit) %>% 
  ggsurvfit(.,type = "risk") +
  xlab("Time to Diabetes (years)") +
  ylab("Event (proportion)") +
  add_confidence_interval() +
 # add_risktable(risktable_stats = c("n.risk")) +
  theme_bw()

# all 10 dfs
# add "study"