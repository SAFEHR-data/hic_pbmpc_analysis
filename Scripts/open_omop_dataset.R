library(arrow)
library(tools)
library(purrr)
library(tidyverse)
library(omopcept)
library(here)

read_omop_dataset <- function(path) {
  # List all subdirectories (each subdir = a table name like 'visit_occurrence')
  list.dirs(path, recursive = FALSE) |>
    # Use folder names as list names
    set_names(~ basename(.)) |>
    # For each folder, open it as an Arrow dataset
    map(arrow::open_dataset)
}
# read_omop_dataset <- function(path) {
#   batches <- fs::dir_ls(path, type = "directory")
#   
#   # Check if we found any batch folders
#   if (length(batches) == 0) stop("No batch folders found.")
#   
#   # Get one example batch to extract table names
#   first_batch <- batches[1]
#   example_files <- fs::dir_ls(first_batch, glob = "*.parquet")
#   
#   # Check if any files found
#   if (length(example_files) == 0) stop("No .parquet files found in first batch folder.")
#   
#   table_names <- tools::file_path_sans_ext(basename(example_files))
#   
#   omop_ds <- list()
#   
#   for (table in table_names) {
#     files_for_table <- file.path(batches, paste0(table, ".parquet"))
#     existing_files <- files_for_table[file.exists(files_for_table)]
#     omop_ds[[tolower(table)]] <- open_dataset(existing_files)
#   }
#   
#   return(omop_ds)
# }



custom_path <- "/data/mqummeru/Extract_hic_PBPMC/"   # e.g., "/data/omop/custom/"
custom_omop_ds <- read_omop_dataset(custom_path)