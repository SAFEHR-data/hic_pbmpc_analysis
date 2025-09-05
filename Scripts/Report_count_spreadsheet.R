# DAR-594 Report count information from benchmarking spreadsheet

library(here)
source(here("Scripts", "open_omop_dataset.R"))

# -------------------------------------------------------------------------
# Load device_exposure_id from links
# -------------------------------------------------------------------------
df_device_exposure_id <- omop$device_exposure_links %>%
   select(device_exposure_id) %>%
   collect()

# -------------------------------------------------------------------------
# Define concept IDs for various blood products
# -------------------------------------------------------------------------
blood_product <- c(4054727, 4103787, 4054726, 4103788, 4106312, 4103613)

# -------------------------------------------------------------------------
# Filter device exposures: keep relevant IDs or known blood product concepts
# -------------------------------------------------------------------------
rm(df_device_exposure)

df_device_exposure_transfusion <- omop$device_exposure %>%
   filter(device_concept_id %in% blood_product) %>%
   collect() %>%
   omop_join_name_all() %>%
   collect()

# -------------------------------------------------------------------------
# Load person table and join concept names
# -------------------------------------------------------------------------
df_person <- omop$person %>%
   collect() %>%
   omop_join_name_all()

# -------------------------------------------------------------------------
# Add demographics and calculate age at transfusion
# -------------------------------------------------------------------------
df_device_exposure_transfusion <- df_device_exposure_transfusion %>%
   inner_join(df_person, by = "person_id") %>%
   mutate(
      age_at_transfusion = ceiling(
         as.numeric(difftime(device_exposure_start_date, birth_datetime, units = "days")) / 365
      )
   )

# -------------------------------------------------------------------------
# First exposure date and overall transfusion quantity
# -------------------------------------------------------------------------
df_first_exposure_date_total_quantity <- df_device_exposure_transfusion %>%
   group_by(person_id) %>%
   summarise(
      min_date = min(device_exposure_start_date, na.rm = TRUE),
      overall_quantity_transfused = sum(quantity),
      .groups = "drop"
   )

# -------------------------------------------------------------------------
# NOTE: The commented block below calculates total quantity by age.
# Keeping it here for reference, but currently not in use.
# -------------------------------------------------------------------------
# df_1 <- df_device_exposure_transfusion %>%
#   group_by(person_id, age_at_transfusion) %>%
#   summarise(
#     total_quantity_transfused_age = sum(quantity, na.rm = TRUE),
#     .groups = "drop"
#   ) %>%
#   left_join(
#     df_person %>% select(person_id, gender_concept_name),
#     by = "person_id"
#   ) %>%
#   select(person_id, gender_concept_name, age_at_transfusion, total_quantity_transfused_age) %>%
#   arrange(person_id, age_at_transfusion)

# -------------------------------------------------------------------------
# Merge transfusion data with first exposure/overall totals
# -------------------------------------------------------------------------
df <- df_device_exposure_transfusion %>%
   left_join(df_first_exposure_date_total_quantity, by = "person_id", "device_exposure_start_date=min_date")



# -------------------------------------------------------------------------
# Reporting metrics
# -------------------------------------------------------------------------

# Total number of patients receiving RBC transfusions
# Total number of RBC units transfused
df %>%
   summarise(
      Total_Person = n_distinct(person_id),
      total_RBC_Unit = sum(quantity)
   )

# Male and Female patient count
df %>%
   group_by(gender_concept_name) %>%
   summarise(cnt = n_distinct(person_id))

# -------------------------------------------------------------------------
# Age group analysis
# -------------------------------------------------------------------------

# First transfusion age and total transfusions per patient
df_age_window <- df %>%
   group_by(person_id, min_date) %>%
   summarise(
      ist_age = min(age_at_transfusion),
      total_transfuse = sum(quantity),
      .groups = "drop"
   )

# Distribution by age group
df_age_window %>%
   mutate(
      age_group = case_when(
         ist_age < 1 ~ "<1 year",
         ist_age >= 1 & ist_age <= 17 ~ "1–17 years",
         ist_age >= 18 & ist_age <= 64 ~ "18–64 years",
         ist_age >= 65 ~ "≥65 years"
      )
   ) %>%
   group_by(age_group) %>%
   summarise(
      num_patients = n(),
      total_transfuse = sum(total_transfuse)
   ) %>%
   arrange(age_group)
