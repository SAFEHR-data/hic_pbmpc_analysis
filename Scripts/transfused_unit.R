# Load necessary library and source the script to open the OMOP dataset
library(here)
source(here("Scripts", "open_omop_dataset.R"))

# Define concept IDs for various blood products
blood_product <- c(4054727, 4103787, 4054726, 4103788, 4103613)

# Filter the device_exposure table for rows where the device_concept_id matches blood products
df_device_exposure <- omop$device_exposure %>%
  filter(device_concept_id %in% blood_product) %>%
  collect() %>%                
  as_tibble()                        

# Join with concept names (e.g., to get device_concept_name) using a helper function
df_device_exposure <- df_device_exposure %>%
  omop_join_name_all() %>%
  collect() %>%
  as_tibble()

# Select relevant columns for analysis or export
df_device_exposure_select <- df_device_exposure %>%
  select(
    device_exposure_id,
    person_id,
    device_concept_name,
    device_exposure_start_date,
    device_exposure_start_datetime,
    device_exposure_end_date,
    device_exposure_end_datetime
  )


df_device_exposure_select %>% arrange(person_id, device_exposure_id)%>% write_csv("/data/mqummeru/Output_Report/transfused_Unit.csv")


