source("/data/mqummeru/hic_pbmpc_analysis/Script/open_omop_dataset.R")

# Extract distinct procedure_occurrence_id values where the plugin provenance is "surgical"
# rm(df_surgical_procedure_id)
# df_surgical_procedure_id<- custom_omop_ds$procedure_occurrence_links %>% 
#   filter(plugin_provenance=="surgical") %>%  
#   select(procedure_occurrence_id ) %>% 
#   distinct() %>%
#   as_tibble()


# Filter procedures to only include records where the procedure_source_value is "UCLH AN VENT MODE"

rm(df_procedure_occurrence)
df_procedure_occurrence <- custom_omop_ds$procedure_occurrence %>% 
  filter(
    procedure_source_value == "UCLH AN VENT MODE" |
      procedure_source_value == "R UCLH SSKIN AREAS OBSERVED" |
      procedure_source_value == "LDA PERIPHERAL IV" |
      procedure_source_value == "R IP VENT MODE" |
      procedure_source_value == "LDA WOUND INCISION"
  ) %>%
  collect() %>%    # collect before converting to tibble
  as_tibble()

df_procedure_occurrence %>% count(procedure_source_value)




# procedure_source_value            n
# <chr>                         <int>
# 1 LDA PERIPHERAL IV            435676
# 2 LDA WOUND INCISION            92830
# 3 R IP VENT MODE               247163
# 4 R UCLH SSKIN AREAS OBSERVED 1048627
# 5 UCLH AN VENT MODE           5466702

# Add human-readable concept names to the dataset using OMOP concept mappings  
df_procedure_occurrence <- df_procedure_occurrence %>%omop_join_name_all()


df_procedure_occurrence_start_end_time <- df_procedure_occurrence %>%
  group_by(person_id, procedure_source_value,procedure_concept_name) %>%
  summarise(
    procedure_start = min(procedure_datetime),
    procedure_end   = max(procedure_datetime),
    .groups = "drop"
  ) %>% arrange(person_id,procedure_source_value,procedure_concept_name,procedure_start ) %>% collect() 

df_procedure_occurrence %>%count(procedure_source_value,procedure_concept_name) %>% write_csv("Procedure_Summary.csv")
# Count and display columns with missing (NA) values
df_procedure_occurrence %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "column", values_to = "na_count") %>%
  arrange(desc(na_count)) %>%
  filter(na_count > 0) %>%
  view()

# Visit detail 

df_visit_detail <- custom_omop_ds$visit_detail %>%
  filter(visit_detail_id %in% df_procedure_occurrence$visit_detail_id) %>% as_tibble() %>% collect()

# A tibble: 0 × 19 : Visit detail id is column is empty



# Load relevant visit_occurrence records from custom OMOP dataset
# Keep only those visits that are linked to surgical procedures
df_visit_occurrence <- custom_omop_ds$visit_occurrence %>%
  filter(visit_occurrence_id %in% df_procedure_occurrence$visit_occurrence_id) %>%  # Match visit IDs with those in surgical procedures
  as_tibble() %>%
  collect()

# ️ Join human-readable names to concept IDs (e.g., visit type name, visit source name)
df_visit_occurrence <- df_visit_occurrence %>%
  omop_join_name_all()


# Join procedure and visit data by person_id, visit_occurrence_id, and date
rm(df_procedure_visit_occurrence)
df_procedure_visit_occurrence <-df_procedure_occurrence %>%
  left_join(
    df_visit_occurrence,
    by = c(
      "person_id" = "person_id",
      "visit_occurrence_id" = "visit_occurrence_id",
      "procedure_date" = "visit_start_date"
    )
  ) %>% 
  select(
    person_id,
    procedure_occurrence_id,
    procedure_concept_id,
    procedure_concept_name,
    procedure_source_value,
    procedure_date,
    procedure_datetime,
    procedure_end_date,
    procedure_end_datetime, 
    visit_occurrence_id,
    visit_concept_name,
    visit_start_datetime,
    visit_end_date,
    visit_end_datetime,
    care_site_id
  ) %>%
  collect()


df_procedure_visit_occurrence %>% view()

# Plot top 50 procedure names by count

  df_procedure_occurrence %>%
  count(procedure_concept_name, sort = TRUE) %>%
  top_n(50, n) %>%
  ggplot(aes(x = reorder(procedure_concept_name, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 50 Procedure Concept Names",
    x = "Procedure Concept Name",
    y = "Count"
  ) +
  theme_minimal()

ggsave("top_50_procedures.png", plot_procedures, width = 10, height = 8, dpi = 300)







