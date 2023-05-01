library(tidyverse)
library(glue)
library(rvest)
library(rentrez)
library(RefManageR)
library(rcrossref)

#run 5th April closed in 2022 and up to March 2023


# ijerph_special_03_02 <- map(14:27,
# ~read_html(
# glue("https://www.mdpi.com/journal/ijerph/special_issues?page_count=100&search=&section_id=&sort=deadline&view=closed&page_no={.x}")) %>%
#                        html_elements(".title-link") %>%
#                        html_attr("href"), .progress = T)
# 
# 
# 
# ijerph_special_03_02_eds3 <- map(14:27,
#                                  ~read_html(
#                                    glue("https://www.mdpi.com/journal/ijerph/special_issues?page_count=100&search=&section_id=&sort=deadline&view=closed&page_no={.x}")) %>%
#                                    html_elements("strong") %>%
#                                    html_text2(), .progress = T)
# 
# saveRDS(ijerph_special_03_02, file = "data/ijerph_special_03_02.RDS")
# saveRDS(ijerph_special_03_02_eds3, file = "data/ijerph_special_03_02_eds3.RDS")

##############################################################

ijerph_special_03_02 <- read_rds("data/ijerph_special_03_02.RDS")

ijerph_special_03_02 <- ijerph_special_03_02 %>%
  tibble(special_page=.) %>%
  separate_longer_delim(special_page, delim=",") %>%
  mutate(special_page=str_remove(special_page, "\\)")) %>%
  mutate(special_page=str_remove(special_page, "c\\(")) %>%
  mutate(special_page=str_remove(special_page, "\"")) %>%
  mutate(special_page=str_remove(special_page, "\"")) %>%
  mutate(special_page=str_trim(special_page)) 


ijerph_special_03_02_eds3 <- read_rds("data/ijerph_special_03_02_eds3.RDS")

ijerph_special_03_02_eds3 <- ijerph_special_03_02_eds3 %>%
  map(~tibble(closed_date=.) %>%
        mutate(closed_date=dmy(closed_date)) %>%
        filter(!is.na(closed_date))) %>%
  list_rbind() 

ijerph_special_03_02 <- ijerph_special_03_02 %>%
  bind_cols(ijerph_special_03_02_eds3) %>%
  filter(closed_date!="2021-12-31") %>%
  filter(closed_date!="2023-04-01") %>%
  mutate(special_page=glue("https://www.mdpi.com{special_page}"))


ijerph_special_03_02 <- ijerph_special_03_02 %>%
mutate(special_editors=map(special_page, ~read_html(.x) %>%
                               html_elements(".editor-div__content") %>%
                               html_elements(".sciprofiles-link") %>%
                               html_text2(), .progress=T)) 

ijerph_special_03_02 <- ijerph_special_03_02 %>%
mutate(special_editors_sciprofile=map(special_page, ~read_html(.x)  %>%
 html_elements(".editor-div__content") %>%
 html_elements(".sciprofiles-link__link") %>%
 html_attr('href'), .progress=T))

ijerph_special_03_02 <- ijerph_special_03_02 %>%
  mutate(paper_titles=map(special_page, ~read_html(.x)  %>%
                                          html_elements(".title-link") %>%
                                          html_text2(), .progress=T))

ijerph_special_03_02 <- ijerph_special_03_02 %>%
  mutate(paper_href=map(special_page, ~read_html(.x)  %>%
                            html_elements(".title-link") %>%
                            html_attr('href'), .progress=T))


saveRDS(ijerph_special_03_02, file = "data/ijerph_special_03_02_detail.RDS")

#####

ijerph_special_03_02_detail <- read_rds(file = "data/ijerph_special_03_02_detail.RDS")

ijerph_special_03_02_detail <- ijerph_special_03_02_detail %>%
  mutate(across(special_editors:paper_href, ~str_remove(.x, "\\)$"))) %>%
  mutate(across(special_editors:paper_href, ~str_remove(.x, "c\\("))) %>%
  separate_longer_delim(special_editors, delim = ",") %>%
  filter(special_editors!= " Jr.\"") %>%
  nest(eds=c(special_editors)) %>%
  separate_longer_delim(special_editors_sciprofile, delim = ",") %>%
  nest(eds2=c(special_editors_sciprofile)) %>%
  separate_longer_delim(paper_titles, delim = "\",") %>%
  nest(titles=c(paper_titles)) %>%
  separate_longer_delim(paper_href, delim = ",") %>%
  nest(href=c(paper_href)) 
  
#some checks

check <- ijerph_special_03_02_detail %>%
  mutate(edsno=map(eds, ~sum(!is.na(.x$special_editors)))) %>%
  mutate(edsprofileno=map(eds2, ~sum(!is.na(.x$special_editors_sciprofile)))) %>%
  filter(as.numeric(edsno)!=as.numeric(edsprofileno))
# in https://www.mdpi.com/journal/ijerph/special_issues/SHWHW one editor doesn't have a sciprofile, left as missing
# in https://www.mdpi.com/journal/ijerph/special_issues/RCMI_2022 .jr is an editor, delete in above (DONE)
# https://www.mdpi.com/journal/ijerph/special_issues/D23QV6XMX3 one editor (same as 1st case) doesn't have a sciprofile, left as missing

check <- ijerph_special_03_02_detail %>%
  mutate(titlesno=map(titles, ~sum(!is.na(.x$paper_titles)))) %>%
  mutate(hrefno=map(href, ~sum(!is.na(.x$paper_href)))) %>%
  filter(as.numeric(titlesno)!=as.numeric(hrefno))

#this is fine

#add authors, 


# 
ijerph_special_03_02_detail <- ijerph_special_03_02_detail %>%
  mutate(titles = map2(titles, href, ~ bind_cols(.x, paper_href = .y))) %>%
  select(-href) %>%
  unnest(titles) %>%
  mutate(across(paper_titles:paper_href, ~str_trim(.x))) %>%
  mutate(across(paper_titles:paper_href, ~str_remove_all(.x, "\""))) %>%
  filter(paper_titles!="character(0") %>% #  11214 to 11147 as 67 special issues no papers
  mutate(paper_href=glue("https://www.mdpi.com{paper_href}")) 

#go to author papers

ijerph_special_03_02_detail %>%
  group_by(batch=row_number() %/% 1000) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")

files %>%
  walk(~read_rds(.x) %>%
  mutate(paper_authors=map(paper_href, ~read_html(.x)  %>%
                          html_elements(".hypothesis_container") %>%
                          html_elements(".sciprofiles-link__link") %>%  
                          html_attr('href'))) %>%
                          saveRDS(., glue("{.x}")), .progress=T)

ijerph_special_03_02_all <- files %>%
  map(~read_rds(.x)) %>%
  reduce(bind_rows) %>%
  separate_longer_delim(paper_authors, delim=",") %>%
  mutate(paper_authors=str_remove(paper_authors, "\\)$")) %>%
  mutate(paper_authors=str_remove(paper_authors, "c\\(")) %>%
  mutate(paper_authors=str_remove_all(paper_authors, "\"")) %>%
  mutate(paper_authors=str_trim(paper_authors))

editors <- ijerph_special_03_02_all %>% 
  select(eds2, special_page) %>%
  distinct(special_page, .keep_all=T) %>%
  unnest_longer(eds2) %>%
  unnest(c(eds2)) %>%
  mutate(editors=str_remove_all(special_editors_sciprofile, "\"")) %>%
  mutate(editors=str_trim(editors)) %>%
  select(-special_editors_sciprofile)         

ijerph_special_03_02_all <- ijerph_special_03_02_all %>%
  left_join(editors, by=join_by(special_page, paper_authors==editors), keep=TRUE) %>%
  rename(special_page=special_page.x) %>%
  select(-special_page.y) %>%
  mutate(editors=ifelse(is.na(editors), 0, 1))

saveRDS(ijerph_special_03_02_all, file = "data/ijerph_special_03_02_all.RDS")


ijerph_special_03_02_all <- read_rds("data/ijerph_special_03_02_all.RDS")

ijerph_special_03_02_all %>%
  distinct(paper_href, .keep_all = T) %>%
  group_by(batch=row_number() %/% 200) %>%
  group_walk(~ saveRDS(.x, glue("file_{.y}.RDS")))

files <- dir(pattern = "*.RDS")

files %>%
  walk(~read_rds(.x) %>%
         mutate(paper_doi=map(paper_href, ~read_html(.x)  %>%
                                html_element(".bib-identity") %>%
                                html_element("a") %>%
                                html_attr('href'))) %>% 
         saveRDS(., glue("{.x}")), .progress=T)
                                             


pmid <- files %>%
  map(~read_rds(.x) %>%
  mutate(paper_doi=str_sub(paper_doi, start=17)) %>%
  pull(paper_doi) %>%
  id_converter(.))

  pmid <- pmid %>% 
    map(~.x$records %>%
    mutate(pub_sum=entrez_summary("pubmed", id=pmid)))
  
  pmid <- pmid %>%
    reduce(bind_rows)
 
 pmid <- pmid %>%
   unnest_wider(pub_sum)

 pmid <- pmid %>%
   mutate(history = map(history, ~ .x %>%
                       pivot_wider(
                         names_from = pubstatus,
                         values_from = date))) %>%
   unnest(history)
 
 ijerph_special_03_02_papers <- files %>%
   map(~read_rds(.)) %>%
   reduce(bind_rows)  %>%
   select(-paper_authors, -editors)
 
 
 ijerph_special_03_02_papers2 <- ijerph_special_03_02_all %>%
   mutate(noeds= map(eds, ~nrow(.))) %>%
   mutate(noeds=as.numeric(noeds)) %>%
   group_by(paper_href) %>%
   summarise(noedsasauth=sum(editors),
             noeds=mean(noeds))

 ijerph_special_03_02_papers <- ijerph_special_03_02_papers %>%
   left_join(ijerph_special_03_02_papers2)

   
 ijerph_special_03_02_papers <- ijerph_special_03_02_papers %>%
   mutate(paper_doi=unlist(paper_doi)) %>%
   mutate(paper_doi=str_sub(paper_doi, start=17))  %>%
   left_join(pmid, by=c("paper_doi"="doi"))

saveRDS(ijerph_special_03_02_papers, file="data/ijerph_special_03_02_papers.RDS")

file.remove(files)
   
paper_results <- ijerph_special_03_02_all %>%
  group_by(paper_titles) %>%
  summarise(seds=sum(editors)) %>%
  group_by(seds) %>%
  summarise(n=n()) %>%
  mutate(N=sum(n)) %>%
  mutate(pc=(n/N)*100)
  
special_results <- ijerph_special_03_02_all %>%
  group_by(special_page) %>%
  summarise(seds=sum(editors)) %>%
  group_by(seds) %>%
  summarise(n=n()) %>%
  mutate(N=sum(n)) %>%
  mutate(pc=(n/N)*100)









