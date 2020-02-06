require(tidyverse)
require(quanteda)
require(stm)

##newsgroup <- rio::import("20_newsgroup.csv") %>% as_tibble

## set.seed(42)
## newsgroup %>% select(text, title) %>% mutate(ntoken = ntoken(text)) %>% sample_frac(1) %>% filter(text != "" & ntoken > 150) -> newsgroup5
## saveRDS(newsgroup5, "newsgroup5.RDS")
newsgroup5 <- readRDS("newsgroup5.RDS")
corpus(newsgroup5)

dfm(newsgroup5$text, tolower = TRUE, stem = TRUE, remove = stopwords('english'), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE) %>% dfm_trim(min_docfreq = 5, max_docfreq = 1000) %>% dfm_select(min_nchar = 3, pattern = "^[a-zA-Z]+$", valuetype = "regex") -> newsgroup5_dfm

docvars(newsgroup5_dfm, "title") <- newsgroup5$title

newsgroup5_dfm %>% convert(to = "stm", omit_empty = FALSE) -> newsgroup5_stm

newsgroup5_model <- stm(newsgroup5_stm$documents, newsgroup5_stm$vocab, data =newsgroup5_stm$meta, K = 10, seed = 42)


saveRDS(newsgroup5_model, "newsgroup_stm.RDS")
labelTopics(newsgroup5_model)

require(text2vec)
lda_model <- LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)

theta <- lda_model$fit_transform(x = newsgroup5_dfm, convergence_tol = 0.001, n_check_convergence = 25)
saveRDS(lda_model, "newsgroup_wraplda.RDS")
