library(tidyverse)
library(ggridges)
library(nomnoml)

bmcph_2022_df <- read_rds("data/bmcph_2022_df.RDS")
ijerph_2022_df <- read_rds("data/ijerph_2022_df.RDS")
jeph_2022_df <- read_rds("data/jeph_2022_df.RDS")
ijerph_2022_special <- read_rds("data/ijerph_2022_special.RDS")
jeph_2022_special <- read_rds("data/jeph_2022_special.RDS")



#slight issue with binding reference variables so just dropped 

analysis_df <- bind_rows("ijerph"=ijerph_2022_df, "bmcph"= bmcph_2022_df, .id="journal") %>%
  select(-references, -pmcrefcount)

analysis_df <- analysis_df %>%
  bind_rows(jeph_2022_df) %>%
  select(-starts_with("references"), -pmcrefcount) %>%
  mutate(journal=ifelse(fulljournalname=="Journal of environmental and public health",
                        "jeph", journal))

#merge in specials

analysis_df <- analysis_df %>%
  mutate(elocationid2=str_sub(elocationid, start=6)) %>%
  left_join(bind_rows(jeph_2022_special, ijerph_2022_special))
  
#is it a special issue

analysis_df <- analysis_df %>% 
  mutate(specialornot=case_when(journal=="ijerph" ~ str_detect(special, "Special Issue"),
                                journal=="jeph" ~   str_detect(special, "[^character(0)]"))) %>%
  mutate(journal2 = case_when(journal=="bmcph" ~ "BMC_PH",
                              journal=="ijerph" & specialornot==TRUE ~ "IJERPH - special",
                              journal=="ijerph" & specialornot==FALSE ~ "IJERPH - ordinary",
                              journal=="jeph" & specialornot==TRUE ~ "JEPH - special",
                              journal=="jeph" & specialornot==FALSE ~ "JEPH - ordinary")) 
         

#Remove a couple of nas in rec_to_acc

analysis_df <- analysis_df %>%
  filter(!is.na(rec_to_acc))

summary_by_journal <- analysis_df %>%
  group_by(journal2) %>%
  summarise(n(),
            mean(rec_to_acc),
            IQR(rec_to_acc),
            median(rec_to_acc),
            sd(rec_to_acc))

ggplot(analysis_df, aes(x = rec_to_acc, y=journal2)) +
  geom_density_ridges(
    jittered_points = TRUE, position = "raincloud",
    alpha = 0.7, scale = 0.9) +
  xlab("Received to acceptance in days") +
  ylab("")

ggsave("summary.png")



#focus on ijerph

analysis_df_ijerph <- analysis_df %>% 
  filter(journal=="ijerph") %>%
  mutate(specialname=str_extract(special, "(?<=Special Issue )[^\\)]*")) %>%
  mutate(rec_to_rev=as.period(received%--%revised, unit="day")) %>%
  mutate(rec_to_rev=time_length(rec_to_rev, unit="days")) %>%
  group_by(specialname) %>%
  mutate(specialno=ifelse(!is.na(specialname), cur_group_id(), NA)) %>%
  mutate(specialtotal=ifelse(!is.na(specialname), n(), NA)) %>%
  mutate(specialmedian=ifelse(!is.na(specialname) & specialtotal > 4, median(rec_to_rev), NA)) %>%
  ungroup()
  
ggplot(filter(analysis_df_ijerph, specialmedian < 24), aes(x = rec_to_acc, y=factor(specialno))) +
  geom_density_ridges(
    jittered_points = TRUE, position = "raincloud",
    alpha = 0.7, scale = 0.9) +
  xlab("Received to revision in days") +
  ylab("")

ijerph_2022_df_special <- analysis_df_ijerph %>%
  filter(specialornot==TRUE) 

saveRDS(ijerph_2022_df_special, file="data/ijerph_df_special.RDS")

#Takes time - just used save file

# ijerph_2022_eds <- ijerph_2022_df_special %>%
#  select(elocationid2, special_href) %>%
#  nest_by(special_href) %>%
#  mutate(special_editors=map(special_href, ~read_html(.x) %>%
#                               html_elements(".editor-div__content") %>%
#                               html_elements(".sciprofiles-link") %>%
#                               html_text2()))

#ijerph_2022_eds <- ijerph_2022_eds %>%
#  mutate(special_sciprofile=map(special_href, ~read_html(.x)  %>%
#  html_elements(".editor-div__content") %>%
#  html_elements(".sciprofiles-link__link") %>%
#  html_attr('href')))

#ijerph_2022_eds <- ijerph_2022_eds %>%
#  mutate(special_editors=list(as_tibble(special_editors)))

#saveRDS(ijerph_2022_eds, file="data/ijerph_2022_eds.RDS")

# editors as authors  

analysis_df_ijerph_2 <- analysis_df_ijerph %>%
  select(elocationid2, specialornot, special_sciprofile) %>%
  separate_longer_delim(special_sciprofile, delim = ",") %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\\)")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "c\\(")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\"")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\"")) %>%
  mutate(special_sciprofile=str_trim(special_sciprofile)) 
  


ijerph_2022_eds_2 <- read_rds("data/ijerph_2022_eds.RDS") %>%
  unnest(data) %>%
  ungroup() %>%
  select(-special_editors) %>%
  separate_longer_delim(special_sciprofile, delim = ",") %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\\)")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "c\\(")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\"")) %>%
  mutate(special_sciprofile=str_remove(special_sciprofile, "\"")) %>%
  mutate(special_sciprofile=str_trim(special_sciprofile)) 


analysis_df_ijerph_3 <- ijerph_2022_eds_2 %>%
  left_join(analysis_df_ijerph_2, by = join_by(elocationid2, special_sciprofile), keep=TRUE) %>%
  mutate(edasauth=ifelse(is.na(elocationid2.y), FALSE, TRUE)) %>%
  group_by(special_href, elocationid2.x) %>%
  summarise(edasauth2=any(edasauth==TRUE)) %>%
  ungroup()

analysis_df_ijerph_4 <-  analysis_df_ijerph_3 %>%
  group_by(special_href) %>%
  summarise(edasauth2=any(edasauth2==TRUE)) %>%
  ungroup()

analysis_df_ijerph_5 <-  analysis_df_ijerph_3 %>%
  group_by(special_href) %>%
  filter(n() > 2) %>%
  summarise(edasauth2=all(edasauth2==TRUE)) %>%
  ungroup()

test34 <- analysis_df_ijerph_2 %>%
  group_by(elocationid2) %>%
  mutate(special_sciprofile2=special_sciprofile) %>%
  expand(special_sciprofile, special_sciprofile2) %>%
  group_by(elocationid2, special_sciprofile2) %>%
  arrange(special_sciprofile, .by_group = TRUE) %>%
  mutate(n=row_number()) %>%
  group_by(elocationid2, special_sciprofile) %>%
  arrange(special_sciprofile2, .by_group = TRUE) %>%
  mutate(n2=row_number()) %>%
  filter(n < n2) %>%
  ungroup() %>%
  left_join(select(analysis_df_ijerph, elocationid2, specialornot, specialname), by="elocationid2") 


test35 <- test34 %>%
  #filter(specialname=="2nd Edition: Public Safety Personnel: Mental Health and Well-Being") %>%
  select(special_sciprofile, special_sciprofile2) %>%
  mutate(special_sciprofile=as.character(str_extract_all(special_sciprofile, "(?<=e/).+$")),
         special_sciprofile2=as.character(str_extract_all(special_sciprofile2, "(?<=e/).+$")))
  
library(igraph)

test36 <-  graph_from_edgelist(as.matrix(test35), directed = F)

test37 <- cliques(test36, min=50)
  
plot(test36)
  
ised <- test34 %>%
  filter(specialname=="2nd Edition: Public Safety Personnel: Mental Health and Well-Being") %>%
  pivot_longer(cols=starts_with("special_sci"),  values_to = "special_sciprofile") %>%
  distinct(special_sciprofile, .keep_all = TRUE) %>%
  left_join(ijerph_2022_eds_2, join_by(elocationid2, special_sciprofile)) %>%
  mutate(ised=ifelse(is.na(special_href), "Not", "Editor")) %>%
  select(-special_href)
  

