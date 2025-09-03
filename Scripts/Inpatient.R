library(here)
source(here("Scripts", "open_omop_dataset.R"))

rm(df_inpatient_procedure)
# InPatient Procedure
df_inpatient_procedure <- omop$inpatient_procedure %>% collect()
df_inpatient_procedure <- df_inpatient_procedure %>% left_join(df_opcs_code %>% 
  select(code1,description), by=c("procedure_code"="code1")) %>%
  relocate(description    ,  .after=procedure_code ) 





# InPatient Episode
df_inpatient_episode <- omop$inpatient_episode %>% collect() 

df_inpatient_episode %>% filter(spell_id %in%c(18937)) %>% 
  select(episode_id ,person_id,spell_id) %>% arrange(person_id)

df_inpatient_episode %>% head() %>% view()

#InPatient Spell
df_inpatient_spell <- omop$inpatient_spell %>% 
mutate(admission_dt=date(admission_date),
discharge_dt=date(discharge_date)) %>%
  collect()

df_inpatient_spell %>% filter(spell_id %in%c(18937)) %>% 
  select(person_id,spell_id) %>% arrange(person_id) %>% view()


df_inpatient_spell %>%
  group_by(spell_id) %>%
  summarise(cnt_distinct = n_distinct(person_id)) %>% filter(cnt_distinct >1) %>% view()


df_inpatient_spell %>% head()
 
 rm(df_person_id_procedure_episode)
 df_person_id_procedure_episode <- df_inpatient_procedure %>% select(episode_id) %>% distinct() %>%
   left_join(df_inpatient_episode %>% select(person_id,episode_id,spell_id) , by="episode_id") %>% 
   select(person_id,spell_id) %>% distinct() 
 
 df_person_id_procedure_episode
 
 
 
df_spell_person_id <- df_inpatient_spell %>%
select(person_id,spell_id) %>% distinct()
 
df_inpatient_diagnosis <- omop$inpatient_diagnosis %>% collect()


sd_check <-df_inpatient_spell %>%
  inner_join(df_inpatient_episode, by = "spell_id", suffix = c(".a", ".b")) %>%
  mutate(flg_Same_Person_id = ifelse(person_id.a == person_id.b, 1, 0)) 


sd_check %>% filter(flg_Same_Person_id==0) %>% head(10)  %>% view()





sd_check  %>% arrange(spell_id) %>% head(1000) %>% view()

sd_check %>% count(flg_Same_Person_id)

df_person_id_procedure_episode %>% filter(person_id==4035)

df_spell_person_id  %>% filter(person_id==4035)
 
sd_check %>% filter(person_id.a==4035) %>% view()

sd_check %>% filter(person_id.a %in%c(4276,1139))



df_inpatient_episode %>% filter(spell_id==14384 ) %>%
  select(spell_id, episode_id, person_id) %>% view()

df_inpatient_spell %>% filter(spell_id==14384) %>%
  select(spell_id, person_id) 


df_inpatient_diagnosis <-omop$inpatient_diagnosis %>% collect()

                                                                     