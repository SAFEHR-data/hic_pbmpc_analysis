source("/data/mqummeru/hic_pbmpc_analysis/Script/open_omop_dataset.R")

# Define measurement_concept_ids for body weight and height
# 4099154 = Body weight (assumed to be in grams)
# 4177340 = Body height measure
height_weight <- c(4099154, 4177340)

# Filter only the height and weight measurements from the OMOP measurement table
df_height_weight_measurement <- custom_omop_ds$measurement %>%
  filter(measurement_concept_id %in% height_weight)

# Join concept names (assumes omop_join_name_all is a custom function that adds descriptive names)
df_height_weight_measurement <- df_height_weight_measurement %>% 
  omop_join_name_all()

# Convert weight from grams to kilograms and update units
df_height_weight_measurement <-df_height_weight_measurement %>%
  mutate(
    # Convert grams to kg for weight (concept_id = 4099154)
    value_as_number = if_else(
      measurement_concept_id == 4099154, 
      round(value_as_number / 1000, 2),  # convert and round to 2 decimal places
      value_as_number
    ),
    
    # Update unit name to "kg" for the same concept
    unit_concept_name = if_else(
      measurement_concept_id == 4099154, 
      "kg", 
      unit_concept_name
    )
  ) %>%
  select(
    measurement_id,
    person_id,
    measurement_concept_id,
    measurement_concept_name,
    measurement_date, 
    measurement_datetime,
    value_as_number,
    unit_concept_name
  ) %>% 
  arrange(person_id,measurement_id)



# -------------------------------------------
# Count NA values in each column of the measurement table
# -------------------------------------------
custom_omop_ds$measurement %>%
  as_tibble() %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))  # Count NAs per column
  ) %>%
  pivot_longer(
    cols = everything(), 
    names_to = "column", 
    values_to = "na_count"
  ) %>%
  arrange(desc(na_count)) %>%      # Sort by most missing values
  filter(na_count > 0) %>%         # Show only columns with missing values
  view()                          

# column                   na_count
# <chr>                       <int>
#   1 measurement_time         21425605
# 2 provider_id              21425605
# 3 visit_detail_id          21425605
# 4 value_source_value       21371732
# 5 measurement_source_value 20213110
# 6 range_low                 3515474
# 7 range_high                3515474
# 8 visit_occurrence_id       2723759
# 9 measurement_event_id      1168465
# 10 value_as_number            530042
# 11 unit_source_value          530042

