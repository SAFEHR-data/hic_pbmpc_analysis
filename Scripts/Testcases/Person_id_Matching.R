#person_id matches between episode & spell for same spell_id
library(testthat)
library(dplyr)
library(testthat)

test_that("person_id matches between episode & spell for same spell_id", {
  

  # Join on spell_id to compare
  joined <- df_inpatient_episode %>% select(person_id,spell_id) %>% 
    inner_join(df_inpatient_spell %>% select(person_id, spell_id), by = "spell_id", suffix = c("_a", "_b"))
  
  # Check for mismatches
  mismatches <- joined %>%
    filter(person_id_a != person_id_b)
  
  expect_true(
    nrow(mismatches) == 0,
    info = paste("Mismatched person_id(s) found for spell_id:",
                 paste(mismatches$spell_id, collapse = ", "))
  )
})



df_inpatient_episode %>%  filter(spell_id==18096702) %>% view()
df_inpatient_spell %>% filter(spell_id==18096702)
