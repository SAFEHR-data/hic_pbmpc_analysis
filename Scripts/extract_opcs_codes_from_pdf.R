# Load required libraries
library(tidyverse)
library(pdftools)

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
