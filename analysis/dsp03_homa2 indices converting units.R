rm(list = ls());gc();source(".Rprofile")

## calculate HOMA2 indices

# convert unit, exclude outliers
# glucose 3.0-25.0 mmol/L (mg/dL --> mmol/L)
# insulin 20-400 pmol/L (µU/mL --> pmol/L)

# unit conversion reference:
# [1] Knopp JL, Holder-Pearson L, Chase JG. Insulin Units and Conversion Factors: A Story of Truth, Boots, and Faster Half-Truths. J Diabetes Sci Technol. 
#     2019 May;13(3):597-600. doi: 10.1177/1932296818805074. Epub 2018 Oct 13. PMID: 30318910; PMCID: PMC6501531.
# [2] Riemsma R, Corro Ramos I, Birnie R, et al. Integrated sensor-augmented pump therapy systems [the MiniMed® Paradigm™ Veo system and the Vibe™ and G4® PLATINUM CGM (continuous glucose monitoring) system] for managing blood glucose levels in type 1 diabetes: 
#     a systematic review and economic evaluation. Southampton (UK): NIHR Journals Library; 2016 Feb. (Health Technology Assessment, No. 20.17.) Appendix 5, Conversion tables for glycated haemoglobin and glucose values. 
#     Available from: https://www.ncbi.nlm.nih.gov/books/NBK348987/

# ?????????????????????????????????????????
# glucosef (mg/dL), insulinf (µU/mL)
# glucosef2	(mg/dL), insulinf2 (mU/L)

merged_df <- readRDS(paste0(path_diabetes_subphenotypes_predictors_folder,"/working/cleaned/dsppre01_merged df.RDS"))

merged_df_homa2 <- merged_df %>% 
  dplyr::select(study_id,study,glucosef,insulinf,glucosef2,insulinf2) %>% 
  mutate(
    glucose = case_when(
      study == "mesa" ~ glucosef2,
      TRUE ~ glucosef
    ),
    insulin = case_when(
      study == "mesa" ~ insulinf2,
      TRUE ~ insulinf
    )
  ) %>%
  dplyr::filter(!is.na(glucose) & !is.na(insulin)) %>% 
  group_by(study_id) %>% 
  mutate(
    # Convert glucose from mg/dL to mmol/L and insulin from µU/mL to pmol/L
    glucosef_mmol_l = glucose / 18,
    insulinf_µU_ml = insulin * 6
  ) %>% 
  ungroup() %>%
  mutate(insulinf_µU_ml = case_when(
    insulinf_µU_ml < 20 ~ 20,
    insulinf_µU_ml > 400 ~ 400,
    TRUE ~ insulinf_µU_ml
  ),
  glucosef_mmol_l = case_when(
    glucosef_mmol_l < 3 ~ 3,
    glucosef_mmol_l > 25 ~ 25,
    TRUE ~ glucosef_mmol_l
  ))

write.xlsx(merged_df_homa2, file = "analysis/dsy03_homa2 indices converting units.xlsx")


#-------------------------------------------------------------------------------------------------------------------------------


Glucose <- merged_df_homa2$glucosef_mmol_l
Insulin <- merged_df_homa2$insulinf_µU_ml

homa2_wb <- loadWorkbook("C:/Users/JGUO258/Downloads/HOMA2 Calculation/HOMA2Calculator.xls")
