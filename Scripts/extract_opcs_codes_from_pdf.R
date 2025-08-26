# Load required libraries
library(tidyverse)
library(pdftools)


library(tidyverse)

# Load tab-separated text file (already done)
df_text <- read_tsv("/data/mqummeru/hic_pbmpc_analysis/output_report/OPCS410_Code.txt", col_names = FALSE)

# Rename columns to something meaningful
df_opcs_code <- df_text %>%
  rename(code = X1, description = X2) %>%
  mutate(
    code1 = str_replace_all(code, "\\.", ""),     # Remove dot if needed for joins
    description = str_trim(description)           # Clean up whitespace
  )

# View the cleaned data
glimpse(df_opcs_code)

# Read the PDF file
pdf_text <- pdf_text("/Users/qummer.arfeen/Documents/OPCS_codes_supplement_UKHSA.pdf")

df_text <- read_tsv("/data/mqummeru/hic_pbmpc_analysis/output_report/OPCS410_Code.txt", col_names = FALSE)

# Split lines and trim whitespace
lines <- unlist(strsplit(pdf_text, "\n"))
lines <- trimws(lines)

# Extract lines that start with OPCS code format (e.g. A12, A12.3)
df_opcs <- tibble(raw = lines) %>%
  filter(str_detect(raw, "^[A-Z]\\d{2}(\\.\\d)?\\s")) %>%
  separate(raw, into = c("code", "description"), sep = "\\s+", extra = "merge")

# Clean and format the code and description
df_opcs <- df_opcs %>%
  mutate(
    code1 = str_replace_all(code, "\\.", ""),
    description = str_replace_all(description, "\\s+", " ")
  ) %>%
  filter(!is.na(code) & !is.na(description))

# Save the cleaned data to a CSV file
write_csv(df_opcs, "OPCS_codes_supplement_UKHSA.csv")
