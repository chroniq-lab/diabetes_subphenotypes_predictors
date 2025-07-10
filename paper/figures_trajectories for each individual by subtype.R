rm(list = ls());gc();source(".Rprofile")

library(ggplot2)
library(purrr)
library(tidyr)
library(forcats)
library(patchwork)

# Step 1: Load and preprocess
analytic_df <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/processed/dspan01_analytic sample.RDS")) %>% 
  dplyr::filter(t <= 0 & t >= -15) %>% 
  arrange(joint_id, t) %>% 
  distinct(joint_id, t, .keep_all = TRUE) %>% 
  mutate(across(one_of(c("subtype", "race", "study")), as.factor)) %>%
  mutate(dpp_intervention = case_when(dpp_intervention == 1 ~ "intervention arm",
                                      TRUE ~ "others"))

# Step 2: Long-format data
biomarkers_long <- analytic_df %>%
  select(joint_id, t, subtype, hba1c, bmi, homa2b, homa2ir) %>%
  pivot_longer(cols = c(hba1c, bmi, homa2b, homa2ir),
               names_to = "biomarker", values_to = "value") %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(
    biomarker = factor(biomarker,
                       levels = c("hba1c", "bmi", "homa2b", "homa2ir"),
                       labels = c("HbA1c (%)", "BMI (kg/m²)", "HOMA2-%B", "HOMA2-IR")),
    subtype = factor(subtype, levels = c("NOT2D", "MOD", "SIRD", "SIDD", "MARD"))
  )

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(patchwork)

# Define custom y-axis limits per biomarker
y_limits <- list(
  "HbA1c (%)" = c(4, 14),
  "BMI (kg/m²)" = c(20, 65),
  "HOMA2-%B" = c(0, 325),
  "HOMA2-IR" = c(0, 12)
)

# Helper to create plot per biomarker for a given subtype
plot_biomarker <- function(data, biomarker_name) {
  ggplot(data %>% dplyr::filter(biomarker == biomarker_name), 
         aes(x = t, y = value, group = joint_id)) +
    geom_line(alpha = 0.15, color = "grey40") +
    geom_point(alpha = 0.2, size = 0.6) +
    scale_x_continuous(limits = c(-15, 0), breaks = seq(-15, 0, by = 5)) +
    scale_y_continuous(limits = y_limits[[biomarker_name]], oob = scales::squish) +
    labs(x = "Time (years)", y = biomarker_name, title = NULL) +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 13)
    )
}

# Plot per subtype
plots_by_subtype <- biomarkers_long %>%
  split(.$subtype) %>%
  map(function(df) {
    p1 <- plot_biomarker(df, "HbA1c (%)")
    p2 <- plot_biomarker(df, "BMI (kg/m²)")
    p3 <- plot_biomarker(df, "HOMA2-%B")
    p4 <- plot_biomarker(df, "HOMA2-IR")
    (p1 + p2) / (p3 + p4)  # 2x2 layout
  })

# Save
walk2(plots_by_subtype, names(plots_by_subtype), ~ggsave(
  filename = paste0(path_diabetes_subphenotypes_predictors_folder,"/figures/spaghetti_", .y, ".png"),
  plot = .x, width = 10, height = 8, dpi = 300
))

