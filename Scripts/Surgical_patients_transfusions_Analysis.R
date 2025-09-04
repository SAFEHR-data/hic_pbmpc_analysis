#DAR-593 Surgical patients and their transfusions



library(here)

# =========================================================
# Load dataset
# =========================================================
source(here("Scripts", "open_omop_dataset.R"))

# =========================================================
# Device exposures: identify blood product usage
# =========================================================

# Collect device exposure IDs from links table
df_device_exposure_id <- omop$device_exposure_links %>%
  select(device_exposure_id) %>%
  collect()

# Define concept IDs for known blood products
blood_product <- c(4054727, 4103787, 4054726, 4103788, 4103613)

# Filter device exposures: keep IDs linked or matching known blood products
df_device_exposure <- omop$device_exposure %>%
  filter(
    device_exposure_id %in% df_device_exposure_id$device_exposure_id |
      device_concept_id %in% blood_product
  ) %>%
  collect() %>%
  omop_join_name_all() %>%
  collect()

# Aggregate device exposures per patient per date
df_device_exposure_1 <- df_device_exposure %>%
  group_by(person_id, device_exposure_start_date) %>%
  summarise(quantity = sum(quantity), .groups = "drop")

# Extract unique patient IDs with device exposures
df_device_exposure_person_id <- df_device_exposure_1 %>%
  select(person_id) %>%
  distinct()

# Count distinct patients
df_device_exposure_person_id %>% count(person_id)
# Expected: 20,903

# =========================================================
# Surgical procedures
# =========================================================

# Get surgical procedure IDs from provenance
df_surgical_procedure_id <- omop$procedure_occurrence_links %>%
  filter(plugin_provenance == "surgical") %>%
  select(procedure_occurrence_id) %>%
  collect()

# Retrieve surgical procedures for patients with device exposures
rm(df_procedure_occurrence)
df_procedure_occurrence <- omop$procedure_occurrence %>%
  filter(procedure_occurrence_id %in% df_surgical_procedure_id$procedure_occurrence_id) %>%
  filter(person_id %in% df_device_exposure_person_id$person_id) %>%
  collect() %>%
  omop_join_name_all()

# Add 7-day observation window
df_procedure_occurrence <- df_procedure_occurrence %>%
  mutate(
    window_start = procedure_date,
    window_end   = procedure_date + days(7)
  )

# Keep distinct valid procedures
df_procedure_occurrence_1 <- df_procedure_occurrence %>%
  filter(procedure_concept_id > 0) %>%
  select(
    person_id,
    procedure_concept_id,
    procedure_concept_name,
    procedure_date,
    window_start,
    window_end
  ) %>%
  distinct()

# Count distinct patients with surgical procedures
df_procedure_occurrence %>%
  select(person_id) %>%
  distinct() %>%
  count(person_id)

# =========================================================
# Transfusions within 7 days of procedure
# =========================================================
rm(df_transfusion_7days)

df_transfusion_7days <- df_procedure_occurrence_1 %>%
  inner_join(
    df_device_exposure_1 %>%
      select(person_id, device_exposure_start_date, quantity),
    by = "person_id"
  ) %>%
  mutate(
    within_7_days = case_when(
      device_exposure_start_date >= window_start &
        device_exposure_start_date <= window_end ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(within_7_days == TRUE) %>%
  distinct() %>%
  arrange(person_id, procedure_date)

# =========================================================
# Summarise transfusion by procedure
# =========================================================
rm(summary_by_procedure)

summary_by_procedure <- df_transfusion_7days %>%
  filter(procedure_concept_id > 0) %>%
  group_by(procedure_concept_name, procedure_concept_id) %>%
  summarise(
    patients_transfused = n_distinct(person_id),
    units_received      = sum(quantity, na.rm = TRUE),
    .groups             = "drop"
  ) %>%
  arrange(desc(units_received))

# =========================================================
# Top N transfusion procedures: plot
# =========================================================
n_top <- 20

# Reshape for plotting
df_long <- summary_by_procedure %>%
  slice_head(n = n_top) %>%
  pivot_longer(
    cols = c(patients_transfused, units_received),
    names_to = "metric",
    values_to = "value"
  )

# Bar plot comparing transfused patients vs. units received
df_long %>%
  ggplot(aes(
    x = reorder(procedure_concept_name, value),
    y = value,
    fill = metric
  )) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    x        = "Procedure",
    y        = "Count",
    fill     = "Metric",
    title    = paste("Top", n_top, "Blood Transfusions by Procedure"),
    subtitle = "Comparing patients transfused vs. units received"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    axis.text.y     = element_text(size = 10)
  )
