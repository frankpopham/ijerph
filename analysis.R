library(tidyverse)
library(ggridges)

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



#special issue name

ijerph_2022_df <- ijerph_2022_df %>% 
  mutate(specialname=str_extract(special, "(?<=Special Issue )[^\\)]*")) %>%
  group_by(specialname) %>%
  mutate(specialno=ifelse(!is.na(specialname), cur_group_id(), NA)) %>%
  mutate(specialtotal=ifelse(!is.na(specialname), n(), NA)) %>%
  mutate(specialmedian=ifelse(!is.na(specialname) & specialtotal > 3, median(rec_to_acc), NA)) %>%
  ungroup()
  