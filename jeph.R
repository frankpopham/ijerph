jeph_2022  <- entrez_search(db="pubmed", term="J Environ Public Health[JOUR] & 2022[VOL]", retmax=10000)


jeph_2022_ids <- 
  tibble(pmid=jeph_2022$ids) %>%
  group_by(batch=row_number() %/% 200) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")

jeph_2022_wh <- files %>%
  map(~read_rds(.x) %>%
        entrez_post(db="pubmed", id=.$pmid, use_history=TRUE, retmax=400), .progress=TRUE)

file.remove(files)

# get summary details
jeph_2022_details <- jeph_2022_wh %>%
  map(~entrez_summary(db="pubmed", web_history =.x,  always_return_list = TRUE), .progress=TRUE)

#make df

jeph_2022_df_base <- jeph_2022_details %>%
  list_flatten() %>%
  tibble(papers=.) %>%
  unnest_wider(papers)

#work on removing non papers

jeph_2022_df <- jeph_2022_df_base %>%
  select(uid:elocationid)  %>%
  unnest_wider(pubtype, names_sep="_")

jeph_2022_pubtype <- jeph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)

jeph_2022_df <- jeph_2022_df %>%
  filter(pubtype_1!="Editorial", pubtype_1!="Letter" , pubtype_1!="Comment", 
         pubtype_1!="Retraction of Publication", pubtype_1!="Published Erratum") %>% # 1 removed 680 > 679
  filter(pubtype_2!="Retracted Publication" | is.na(pubtype_2))  # 5 removed 679 > 674
  
jeph_2022_pubtype2 <- jeph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)  
    
jeph_2022_df <- jeph_2022_df %>%
  mutate(history2=map(history, ~pivot_wider(.x, names_from = pubstatus, values_from = date))) %>%
  unnest_wider(history2) 

jeph_2022_df <- jeph_2022_df %>%
  mutate(across(received:medline, ~ymd_hm(.x)))

#interval between received and accepted

jeph_2022_df <- jeph_2022_df %>%
  mutate(rec_to_acc=as.period(received%--%accepted, unit="day")) %>%
  mutate(rec_to_acc=time_length(rec_to_acc, unit="days"))

saveRDS(jeph_2022_df, file="data/jeph_2022_df.RDS")

#all 2022
#scrape  html of articles using doi

jeph_2022_df <- jeph_2022_df %>%
  mutate(elocationid2=str_sub(elocationid, start=6)) 

#using batches to avoid issues with drop out

jeph_2022_df %>%
  select(elocationid2) %>%
  group_by(batch=row_number() %/% 100) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")


walk(files, ~read_rds(.x) %>%
       mutate(special=map(elocationid2, ~read_html(glue("https://doi.org/{.x}")) %>%
                            html_elements(".articleHeader__specialIssue_title") %>%
                            html_text2(), .progress=TRUE)) %>%
       saveRDS(., glue("{.x}")))

#make a df

jeph_2022_special <- files %>%
  map(~read_rds(.x)) %>%
  reduce(bind_rows) %>%
  mutate(special=as.character(special))

saveRDS(jeph_2022_special, file="data/jeph_2022_special.RDS")


