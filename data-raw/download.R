library(ref2014)
library(readxl)

# REF 2014 results
results_file <- download_REF('results')
results <- read_xlsx(results_file, skip = 7, guess_max = 1e5, na = '-')
usethis::use_data(results)

# REF 2014 'Research outputs' submissions
outputs_file <- download_REF('outputs')
outputs <- read_xlsx(outputs_file, skip = 4, guess_max = 1e5, na = 'n/a')
usethis::use_data(outputs)

# REF 2014 DOIs lookup table for selected Units of Assessment
dois <- subset(tidy_outputs(),
               output_type == 'D'
               & uoa_name %in% c('Economics and Econometrics',
                                 'Mathematical Sciences',
                                 'Biological Sciences',
                                 'Physics')
               & !is.na(doi))$doi
dois <- unique(tolower(dois))
already_downloaded <- readr::read_csv('article_metadata.csv')
dois <- dois[!dois %in% tolower(already_downloaded$doi)]

dois_list <- split(dois, ceiling(seq_along(dois) / 100))
article_metadata <- rcrossref::cr_works(dois_list[[1]], .progress = 'text')$data %>%
  select(container.title, doi, issn, subject, title, publisher)
readr::write_csv(article_metadata, 'article_metadata.csv')
num_left <- length(dois_list) - 1
for (chunk in tail(dois_list, 48)) {
  Sys.sleep(3)
  message(num_left, ' to go')
  article_metadata <- rcrossref::cr_works(chunk, .progress = 'text')$data %>%
    select(container.title, doi, issn, subject, title, publisher)
  readr::write_csv(article_metadata, 'article_metadata.csv', append = TRUE)
  num_left <- num_left - 1
}
article_metadata <- readr::read_csv('article_metadata.csv')
invalid_dois <- dois[!dois %in% tolower(article_metadata$doi)]

# Disambiguate titles such as 'Review of Economic Studies' vs 'The Review of Economic Studies'
library(dplyr)
article_metadata <- article_metadata %>%
  tidyr::gather('id_type', 'id_value', container.title, issn) %>%
  select(doi, id_value) %>%
  filter(!is.na(id_value)) %>%
  igraph::graph_from_data_frame() %>%
  igraph::clusters() %>% igraph::membership() %>% stack() %>%
  rename(journal_id = values, doi = ind) %>%
  mutate(doi = as.character(doi)) %>%
  right_join(article_metadata, by = 'doi') %>%
  left_join(group_by(., journal_id) %>%
              summarise(unique_journal_title = first(container.title)),
            by = 'journal_id')
usethis::use_data(article_metadata)
