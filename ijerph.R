
library(tidyverse)
library(glue)
library(rvest)
library(rentrez)

#dir.create("data")

#Recorded number of papers per 2022 issue from website (split need to avoid limits on entrez search)

papers <- c(615, 406, 889, 580, 656, 600, 674, 508, 854, 570, 602, 597, 672, 662, 890, 687,
         684, 695, 1144, 731, 904, 803, 952, 728)

ijerph_2022 <- map2(1:24, papers, ~ entrez_search(db="pubmed", term=glue("Int J Environ Res Public Health[JOUR] & 19[VOL] & {.x}[ISS]"), 
                        retmax=.y)
)




# because of limits to searches and various complications split ids into 200ish batches and get web history from pubmed
ijerph_2022_ids <- map(ijerph_2022, ~.x$ids)  %>%
  unlist() %>%
  tibble(pmid=.) %>%
  group_by(batch=row_number() %/% 200) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))
  
files <- dir(pattern = "*.RDS")

ijerph_2022_wh <- files %>%
  map(~read_rds(.x) %>%
  entrez_post(db="pubmed", id=.$pmid, use_history=TRUE, retmax=200), .progress=TRUE)  

file.remove(files)

# get summary details
ijerph_2022_details <- ijerph_2022_wh %>%
 map(~entrez_summary(db="pubmed", web_history =.x,  always_return_list = TRUE), .progress=TRUE)

#make df

ijerph_2022_df_base <- ijerph_2022_details %>%
  list_flatten() %>%
  tibble(papers=.) %>%
  unnest_wider(papers)

#work on removing non papers

ijerph_2022_df <- ijerph_2022_df_base %>%
  select(uid:elocationid) %>%
  unnest_wider(pubtype, names_sep="_")
  

ijerph_2022_pubtype <- ijerph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)

#Remove Editorial, Letter , Comment, Retraction of Publication, Erratum, or No publication type 

ijerph_2022_df <- ijerph_2022_df %>%
  filter(!is.na(pubtype_1)) %>% # 113 removed 17102 > 16989 
  filter(pubtype_1!="Editorial", pubtype_1!="Letter" , pubtype_1!="Comment", 
         pubtype_1!="Retraction of Publication", pubtype_1!="Published Erratum") %>% # 164 removed 16989 > 16825
  filter(pubtype_2!="Comment" | is.na(pubtype_2)) %>%  # 18 removed 16825 > 16807   
  filter(pubtype_3!="Comment" | is.na(pubtype_3)) # 2 removed 16807 >  16805


ijerph_2022_pubtype2 <- ijerph_2022_df %>%
  count(pubtype_1, pubtype_2, pubtype_3)

# publication timing

ijerph_2022_df <- ijerph_2022_df %>%
  mutate(history2=map(history, ~pivot_wider(.x, names_from = pubstatus, values_from = date))) %>%
  unnest_wider(history2) 

ijerph_2022_df <- ijerph_2022_df %>%
 mutate(across(received:version, ~ymd_hm(.x)))

#interval between received and revised

ijerph_2022_df <- ijerph_2022_df %>%
  mutate(rec_to_acc=as.period(received%--%accepted, unit="day")) %>%
  mutate(rec_to_acc=time_length(rec_to_acc, unit="days"))

saveRDS(ijerph_2022_df, file="data/ijerph_2022_df.RDS")

ijerph_2022_df <- read_rds("data/ijerph_2022_df.RDS")


#all 2022
#scrape  html of articles using doi

ijerph_2022_df <- ijerph_2022_df %>%
  mutate(elocationid2=str_sub(elocationid, start=6)) 

#using batches to avoid issues with drop out, this takes 3 hoursish

ijerph_2022_df %>%
  select(elocationid2) %>%
  group_by(batch=row_number() %/% 500) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")


walk(files, ~read_rds(.x) %>%
  mutate(special=map(elocationid2, ~read_html(glue("https://doi.org/{.x}")) %>%
                                                html_elements(".belongsTo") %>%
                                                html_text2()))  %>%
  mutate(special_href=map(elocationid2, ~read_html(glue("https://doi.org/{.x}")) %>%
                              html_elements(".belongsTo") %>%
                              html_element("a") %>% 
                              html_attr('href'))) %>% 
  saveRDS(., glue("{.x}")), .progress = TRUE)


#get author sci profiles

walk(files, ~read_rds(.x) %>%
       mutate(special_sciprofile=map(elocationid2, ~read_html(glue("https://doi.org/{.x}")) %>%
                            html_elements(".hypothesis_container") %>%
                            html_elements(".sciprofiles-link__link") %>%
                            html_attr('href'))) %>%
       saveRDS(., glue("{.x}")), .progress = TRUE)


 #make a df

ijerph_2022_special <- files %>%
  map(~read_rds(.x)) %>%
  reduce(bind_rows) %>%
  mutate(special=as.character(special)) %>%
  mutate(special_href=trim(as.character(special_href))) %>%
  mutate(special_href=ifelse(special!="character(0)", glue({"https://www.mdpi.com{special_href}"}), NA)) 

saveRDS(ijerph_2022_special, file="data/ijerph_2022_special.RDS")


