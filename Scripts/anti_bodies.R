source(here("Scripts", "open_omop_dataset.R"))

# Step 1: Get measurement IDs linked to blood antibody plugin
df_antibodies_measurrment_link <- custom_omop_ds$measurement_links %>%
  filter(plugin_provenance == "blood_antibody_bm") %>%
  select(measurement_id) %>%
  collect()

# Step 2: Retrieve measurement records that match the antibody measurement IDs
df_antibodies_link_data <- custom_omop_ds$measurement %>%
  filter(measurement_id %in% df_antibodies_measurrment_link$measurement_id)

# Step 3: Join with concept names for readability (assumes omop_join_name_all enriches concept fields)
df_antibodies_link_data <- df_antibodies_link_data %>%
  omop_join_name_all()

# Step 4: Select and arrange relevant columns for final dataset
df_antibodies_link_data <- df_antibodies_link_data %>%
  select(
    measurement_id,
    person_id,
    measurement_concept_id,
    measurement_concept_name,
    measurement_source_value,
    measurement_date,
    measurement_datetime,
    value_as_concept_name
  ) %>%
  arrange(person_id, measurement_id)


df_antibodies_link_data %>% filter(measurement_concept_id!=3001079) %>%tail(10) %>% view() 

skimr::skim(df_antibodies_link_data)



# 
# # Step 1: Define a vector of concept IDs for various antibodies
# anti_bodies <- c(
#   3016598, 3022247, 3014694, 3022924, 40768029, 3038770, 3039113, 3020342,
#   3015654, 3008003, 3007705, 40761160, 3001981, 3015600, 3002402, 3002171,
#   3001819, 40768018, 3017125, 3037405, 40761162, 3017800, 3053246, 3016597,
#   3020920, 3010889, 3026722, 40768022, 3027304, 3005119, 3021816, 3005386,
#   40761169, 3034851, 3034176, 3035056, 3014586, 3005723, 3024811, 3014995,
#   3021215, 3026986, 40761173, 3003351, 3017860, 40758467, 3002134, 3001631,
#   40761182, 40761185, 40761175, 40768037, 3011656, 40761177, 3012260, 3012905,
#   3025123, 3965130, 40771894
# )
# 
# # Step 2: Filter the measurement table to get only antibody-related records
# df_antibodies <- custom_omop_ds$measurement %>%
#   filter(measurement_concept_id %in% anti_bodies) %>%   # keep only antibody measurements
#   collect()                                              # bring data into local memory
# 
# # Step 3: Join concept IDs to human-readable names for better interpretation
# df_antibodies <- df_antibodies %>%
#   omop_join_name_all()
# 
# # Step 4: View the first few records of the enriched antibody data
# df_antibodies %>%
#   head() %>%
#   view()


df_antibodies %>% count(measurement_concept_name) %>% view()




