# DAR-594 Report count information from benchmarking spreadsheet
library(here)
source(here("Scripts", "open_omop_dataset.R"))

df_device_exposure_id <- omop$device_exposure_links%>%
   select(device_exposure_id) %>%
   collect()

# Define concept IDs for various blood products
blood_product <- c(4054727, 4103787, 4054726, 4103788, 4103613)

# Filter device exposures: keep relevant IDs or known blood product concepts

rm(df_device_exposure)
df_device_exposure_transfusion <- omop$device_exposure %>%
   filter(
      device_exposure_id %in% df_device_exposure_id$device_exposure_id |
         device_concept_id %in% blood_product
   ) %>%
   collect() %>%
   omop_join_name_all() %>%
   collect()

df_person <- omop$person %>% collect() %>% omop_join_name_all()


df_device_exposure_transfusion <-df_device_exposure_transfusion %>%
   inner_join(df_person , by = "person_id") %>%
   mutate(age_at_transfusion = ceiling(as.numeric(difftime(device_exposure_start_date, birth_datetime, units = "days")) / 365))

df_first_exposure_date_total_qunatity <- 
   df_device_exposure_transfusion %>% group_by(person_id) %>% summarise(
      min_date=min(device_exposure_start_date, na.rm = TRUE),
      overall_quantity_transfused=sum(quantity), 
      .groups = "drop"
      )

 
df_total_qunatity_age <-
   df_device_exposure_transfusion %>% group_by(person_id,age_at_transfusion,device_exposure_start_date) %>% summarise(
      total_quantity_transfused_age=sum(quantity), 
      .groups = "drop"
   ) %>% select(person_id,device_exposure_start_date,age_at_transfusion,total_quantity_transfused_age) %>%
   arrange(person_id,device_exposure_start_date,age_at_transfusion)
 
   

df <-df_device_exposure_transfusion %>%
   left_join(df_first_exposure_date_total_qunatity, by="person_id","device_exposure_start_date=min_date") %>%
   left_join(df_total_qunatity_age, by="person_id","device_exposure_start_date=device_exposure_start_date", "age_at_transfusion") 
   
 
df %>% head %>% view()

#Total number of patients receiving RBC transfusions
#Total number of RBC units transfused
df %>% summarise(Total_Person= n_distinct(person_id), total_RBC_Unit=sum(quantity))


 

 # Male and Female Count

 df %>%  group_by(gender_concept_name) %>% summarise(cnt=n_distinct(person_id))

# Number of RBC transfused to patients <1 year of age
#  Number of RBC transfused to patients 1–17 year's of age
#  Number of RBC transfused to patients 18-64 year's of age
#  Number of RBC transfused to patients  ≥65 year's of age

 
 df_age_window <-df %>% group_by(person_id,min_date) %>%summarise(ist_age= min(age_at_transfusion))
 df_age_window %>%mutate(age_group = case_when(
    ist_age < 1 ~ "<1 year",
    ist_age >= 1 & ist_age <= 17 ~ "1–17 years",
    ist_age >= 18 & ist_age <= 64 ~ "18–64 years",
    ist_age >= 65 ~ "≥65 years"
 )) %>%
    group_by(age_group) %>%
    summarise(num_patients = n()) %>%
    arrange(age_group) 
 

                                  