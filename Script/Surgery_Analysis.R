library(arrow)
library(tools)
library(purrr)
library(tidyverse)


read_omop_dataset <- function(path) {
  batches <- fs::dir_ls(path, type = "directory")
  
  # Check if we found any batch folders
  if (length(batches) == 0) stop("No batch folders found.")
  
  # Get one example batch to extract table names
  first_batch <- batches[1]
  example_files <- fs::dir_ls(first_batch, glob = "*.parquet")
  
  # Check if any files found
  if (length(example_files) == 0) stop("No .parquet files found in first batch folder.")
  
  table_names <- tools::file_path_sans_ext(basename(example_files))
  
  omop_ds <- list()
  
  for (table in table_names) {
    files_for_table <- file.path(batches, paste0(table, ".parquet"))
    existing_files <- files_for_table[file.exists(files_for_table)]
    omop_ds[[tolower(table)]] <- open_dataset(existing_files)
  }
  
  return(omop_ds)
}

custom_path <- "/data/mqummeru/Extract_hic_PBPMC/"   # e.g., "/data/omop/custom/"

custom_omop_ds <- read_omop_dataset(custom_path)


custom_omop_ds$

# Extract distinct procedure_occurrence_id values where the plugin provenance is "surgical"
df_surgical_procedure_id<- custom_omop_ds$procedure_occurrence_links %>% 
  filter(plugin_provenance=="surgical") %>%  
  select(procedure_occurrence_id ) %>% 
  distinct() %>%
  as_tibble()


# load procedure_occuranre table 
# Extract surgical procedures from the OMOP dataset:
# - Select from the `procedure_occurrence` table
# - Join with the filtered list of surgical procedure IDs
# - Keep only rows where the `procedure_concept_id` is greater than 0 (valid concepts)
# - Convert to tibble and collect into memory (from Arrow/Disk backend)

df_procedure_occurance <- custom_omop_ds$procedure_occurrence %>% 
  inner_join(df_surgical_procedure_id, by = "procedure_occurrence_id") %>% 
  filter(procedure_concept_id > 0) %>%
  as_tibble() %>%
  collect()

# Add human-readable concept names to the dataset using OMOP concept mappings  
df_procedure_occurance <- df_procedure_occurance %>%omop_join_name_all()


df_visit_detail <- custom_omop_ds$visit_detail %>%
  filter(visit_detail_id %in% df_procedure_occurance$visit_detail_id) %>% as_tibble() %>% collect()

# A tibble: 0 × 19 : Visit detail id is column is empty


# Load relevant visit_occurrence records from custom OMOP dataset
# Keep only those visits that are linked to surgical procedures
df_visit_occurrence <- custom_omop_ds$visit_occurrence %>%
  filter(visit_occurrence_id %in% df_procedure_occurance$visit_occurrence_id) %>%  # Match visit IDs with those in surgical procedures
  as_tibble() %>%
  collect()

# ️ Join human-readable names to concept IDs (e.g., visit type name, visit source name)
df_visit_occurrence <- df_visit_occurrence %>%
  omop_join_name_all()

df_visit_occurrence %>% view()




