
library(here)

# ==============================
# Load custom dataset loader script
# ==============================
source(here("Scripts", "open_omop_dataset.R"))

# ==============================
# Define Tranexamic Acid drug_concept_ids
# ==============================
# 43199161 - Tranexamic Acid 500 MG Delayed Release Oral Tablet 
# 21078067 - 5 ML Tranexamic Acid 100 MG/ML Prefilled Syringe   
# 19106002 - Tranexamic Acid 50 MG/ML Mouthwash                  
# 21023815 - Tranexamic Acid 50 MG/ML Oral Suspension 

df_TXA_drug_exposure <- omop$drug_exposure %>% 
  filter(drug_concept_id %in% c(43199161, 21078067, 19106002, 21023815)) %>% 
  collect()

# Add drug and concept names for readability
df_TXA_drug_exposure <- df_TXA_drug_exposure %>% 
  omop_join_name_all()

# Extract distinct person_ids for patients exposed to TXA
df_TXA_person_id <- df_TXA_drug_exposure %>% 
  select(person_id) %>% 
  distinct()

# ==============================
# Extract surgical procedures
# ==============================
df_surgical_procedure_id <- omop$procedure_occurrence_links %>%
  filter(plugin_provenance == "surgical") %>% 
  select(procedure_occurrence_id) %>%          
  collect()

# Filter procedure_occurrence to include only surgical procedures 
# and only those patients who received TXA
df_procedure_occurrence <- omop$procedure_occurrence %>%
  filter(procedure_occurrence_id %in% df_surgical_procedure_id$procedure_occurrence_id) %>%
  filter(person_id %in% df_TXA_person_id$person_id) %>%
  collect() %>%
  omop_join_name_all()

# ==============================
# Link TXA exposure with surgical procedures
# ==============================
df_tx_procedure <- df_TXA_drug_exposure %>%
  left_join(
    df_procedure_occurrence,
    by = c("person_id", "drug_exposure_start_date" = "procedure_date")
  ) %>%
  select(
    person_id,
    drug_concept_name,
    drug_exposure_start_date,
    drug_exposure_start_datetime,
    procedure_datetime
  ) %>%
  mutate(
    # Compute time difference in minutes between TXA administration and surgery
    time_diff_min = as.numeric(
      difftime(drug_exposure_start_datetime, procedure_datetime, units = "mins")
    ),
    # TRUE if TXA given within ±60 minutes of surgery start
    within_60_min = abs(time_diff_min) <= 60
  )

# ==============================
# Visualization: Histogram of TXA timing relative to surgery
# ==============================
df_tx_procedure %>%
  filter(within_60_min == TRUE) %>%
  ggplot(aes(x = time_diff_min, fill = drug_concept_name)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white", boundary = 0) +
  labs(
    title = "TXA Timing Relative to Surgery",
    x = "Time Difference (minutes, TXA - Surgery start)",
    y = "Number of Patients"
  ) +
  theme_minimal()


df_tx_procedure %>%
  filter(within_60_min == TRUE) %>% group_by(drug_concept_name) %>% summarise(n()) %>% view()

df_tx_procedure %>% view()
