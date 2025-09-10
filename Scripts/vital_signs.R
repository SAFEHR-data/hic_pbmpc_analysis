# --------------------------------------------
# Load required libraries and dataset
# --------------------------------------------
library(here)

# Load OMOP dataset (custom script)
source(here("Scripts", "open_omop_dataset.R"))

# --------------------------------------------
# Define a merged concept set for vital signs
# --------------------------------------------
vital_sign_concepts <- c(
  4301868,   # Pulse rate
  4313591,   # Respiratory rate
  4302666,   # Body temperature
  4152194,   # Systolic blood pressure
  4154790,   # Diastolic blood pressure
  4201235,   # Body surface area
  4099154,   # Body weight
  4239021,   # Mean blood pressure
  4296538,   # Glasgow Coma Scale
  44782824   # Mean inspiratory airway pressure
)

# --------------------------------------------
# Extract vital sign records from measurement table
# --------------------------------------------
df_vitals <- omop$measurement %>%
  filter(measurement_concept_id %in% vital_sign_concepts) %>%
  collect()

# Add concept names (join helper function)
df_vitals <- df_vitals %>%
  omop_join_name_all()

# Select key columns for analysis
df_vitals <- df_vitals %>%
  select(
    measurement_id,
    person_id,
    measurement_concept_id,
    measurement_concept_name,
    measurement_date,
    measurement_datetime,
    operator_concept_id,
    operator_concept_name,
    value_as_number,
    unit_source_value,
    measurement_source_value
  )
