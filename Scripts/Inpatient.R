library(here)
source(here("Scripts", "open_omop_dataset.R"))

rm(df_inpatient_procedure)
# InPatient Procedure
df_inpatient_procedure <- omop$inpatient_procedure %>% collect()
df_inpatient_procedure <- df_inpatient_procedure %>% left_join(df_opcs_code %>% 
  select(code1,description), by=c("procedure_code"="code1")) %>%
  relocate(description   ,  .after=procedure_code ) 

# InPatient Episode
df_inpatient_episode <- omop$inpatient_episode %>% collect() 

#InPatient Spell
df_inpatient_spell <- omop$inpatient_spell %>% 
mutate(admission_dt=date(admission_date),
discharge_dt=date(discharge_date)) %>%
  collect()
 
 rm(df_person_id_procedure_episode)
 df_person_id_procedure_episode <- df_inpatient_procedure %>% select(episode_id) %>% distinct() %>%
   left_join(df_inpatient_episode %>% select(person_id,episode_id,spell_id) , by="episode_id") %>% 
   select(person_id,spell_id) %>% distinct() 
 
 
 
df_spell_person_id <- df_inpatient_spell %>%
select(person_id,spell_id) %>% distinct()
 
df_person_id_procedure_episode %>% left_join(df_spell_person_id, by="spell_id") %>%
  mutate (flg_Same= case_when())

sd_check <-df_person_id_procedure_episode %>%
  left_join(df_spell_person_id, by = "spell_id", suffix = c(".a", ".b")) %>%
  mutate(flg_Same_Person_id = ifelse(person_id.a == person_id.b, 1, 0)) 


sd_check  %>% arrange(spell_id) %>% view()

df_person_id_procedure_episode %>% filter(person_id==4035)

df_spell_person_id  %>% filter(person_id==4035)
 
sd_check %>% filter(person_id.a==4035) %>% view()
                                                                     