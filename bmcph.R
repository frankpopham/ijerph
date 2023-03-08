library(tidyverse)
library(glue)
library(rvest)
library(rentrez)



bmcph_2022 <- entrez_search(db="pubmed", term="BMC Public Health[JOUR] & 22[VOL]", retmax=2460)


bmcph_2022_ids <- 
  tibble(pmid=bmcph_2022$ids) %>%
  group_by(batch=row_number() %/% 200) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")

bmcph_2022_wh <- files %>%
  map(~read_rds(.x) %>%
        entrez_post(db="pubmed", id=.$pmid, use_history=TRUE, retmax=200), .progress=TRUE)

file.remove(files)

# get summary details
bmcph_2022_details <- bmcph_2022_wh %>%
  map(~entrez_summary(db="pubmed", web_history =.x,  always_return_list = TRUE), .progress=TRUE)

#make df

bmcph_2022_df_base <- bmcph_2022_details %>%
  list_flatten() %>%
  tibble(papers=.) %>%
  unnest_wider(papers)

#work on removing non papers

bmcph_2022_df <- bmcph_2022_df_base %>%
  select(uid:elocationid)  %>%
  unnest_wider(pubtype, names_sep="_")

bmcph_2022_pubtype <- bmcph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)

#Remove Editorial, Letter , Comment, Retraction of Publication, Erratum, or No publication type 

bmcph_2022_df <- bmcph_2022_df %>%
  filter(pubtype_1!="Editorial", pubtype_1!="Letter" , pubtype_1!="Comment", 
         pubtype_1!="Retraction of Publication", pubtype_1!="Published Erratum") %>% # 64 removed 2460 > 2396
  filter(pubtype_2!="Comment" | is.na(pubtype_2)) %>%  # 1 removed 2396 > 2395   
  filter(pubtype_3!="Comment" | is.na(pubtype_3)) # 1 removed 2395 >  2394

bmcph_2022_pubtype2 <- bmcph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)

bmcph_2022_df <- bmcph_2022_df %>%
  mutate(history2=map(history, ~pivot_wider(.x, names_from = pubstatus, values_from = date))) %>%
  unnest_wider(history2) 

bmcph_2022_df <- bmcph_2022_df %>%
  mutate(across(received:revised, ~ymd_hm(.x)))

#interval between received and accepted

bmcph_2022_df <- bmcph_2022_df %>%
  mutate(rec_to_acc=as.period(received%--%accepted, unit="day")) %>%
  mutate(rec_to_acc=time_length(rec_to_acc, unit="days"))

saveRDS(bmcph_2022_df, file="data/bmcph_2022_df.RDS")