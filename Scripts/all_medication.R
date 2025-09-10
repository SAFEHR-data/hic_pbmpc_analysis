# --------------------------------------------
# Load required libraries and dataset
# --------------------------------------------
library(here)

# Load OMOP dataset (custom script)
source(here("Scripts", "open_omop_dataset.R"))

# --------------------------------------------
# Extract Drug Exposure table
# --------------------------------------------
df_drug_exposure <- omop$drug_exposure %>%
  collect()

# --------------------------------------------
# Join with concept names and select key columns
# --------------------------------------------
df_drug_exposure <- df_drug_exposure %>%
  omop_join_name_all() %>%
  select(
    drug_exposure_id,
    person_id,
    drug_concept_id,
    drug_concept_name,
    drug_type_concept_id,
    drug_type_concept_name,
    drug_exposure_start_date,
    drug_exposure_start_datetime,
    drug_exposure_end_date,
    drug_exposure_end_datetime,
    route_concept_name,
    route_source_value,
    dose_unit_source_value,
    quantity
  )


