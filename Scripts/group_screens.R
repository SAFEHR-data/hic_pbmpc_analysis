# Load necessary packages
library(here)

# Load custom dataset loader script
source(here("Scripts", "open_omop_dataset.R"))

# ------------------------------------------------------------------------------
# Extract Blood Group and RhD data from OMOP Measurement table
# ------------------------------------------------------------------------------
# CONCEPT_IDs:
# - 3002529: Blood Group
# - 3003310: RhD



# Filter blood group and RhD measurements
df_blood_group_rhd <- omop$measurement %>%
  filter(measurement_concept_id %in% c(3002529, 3003310)) %>%
  collect()

# Join measurement concept names to the dataset
df_blood_group_rhd <- df_blood_group_rhd %>%
  omop_join_name_all()

# ------------------------------------------------------------------------------
# Extract sample IDs linked to the selected measurements
# ------------------------------------------------------------------------------

df_measurment_link_sample_id <- omop$measurement_links %>%
  filter(measurement_id %in% df_blood_group_rhd$measurement_id) %>%
  collect()

# Join sample_id to the main blood group dataset
df_blood_group_rhd <- df_blood_group_rhd %>%
  left_join(
    df_measurment_link_sample_id %>%
      select(measurement_id, sample_id),
    by = "measurement_id"
  )

# Move sample_id column after measurement_id
df_blood_group_rhd <- df_blood_group_rhd %>%
  relocate(sample_id, .after = measurement_id)

# ------------------------------------------------------------------------------
# Select and arrange columns for output
# ------------------------------------------------------------------------------

df_blood_group_rhd <- df_blood_group_rhd %>%
  select(
    measurement_id,
    sample_id,
    person_id,
    measurement_concept_id,
    measurement_concept_name,
    measurement_date,
    measurement_datetime,
    value_as_concept_id,
    value_as_concept_name,
    measurement_source_value
  )

# ------------------------------------------------------------------------------
# Export the final dataset to CSV
# ------------------------------------------------------------------------------

df_blood_group_rhd %>%
  arrange(measurement_id, person_id) %>%
  write_csv("/data/mqummeru/Output_Report/group_screen.csv")
