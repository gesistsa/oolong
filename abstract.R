require(tidyverse)
require(quanteda)
require(stm)
require(oolong)
devtools::load_all()

## abstracts <- readRDS("~/dev/jcmc/finalsample.RDS")
## set.seed(46709394)
## abstracts %>% sample_n(2500) %>% mutate(text = abstract) %>% select(text) -> abstracts

## usethis::use_data(abstracts, overwrite = TRUE)

dfm(abstracts$text, tolower = TRUE, stem = TRUE, remove = stopwords('english'), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE) %>% dfm_trim(min_docfreq = 3, max_docfreq = 500) %>% dfm_select(min_nchar = 3, pattern = "^[a-zA-Z]+$", valuetype = "regex") -> abstracts_dfm

usethis::use_data(abstracts_dfm, overwrite = TRUE)

abstracts_dfm %>% convert(to = "stm", omit_empty = FALSE) -> abstracts_stm_data

abstracts_stm <- stm(abstracts_stm_data$documents, abstracts_stm_data$vocab, data =abstracts_stm_data$meta, K = 20, seed = 46709394)

usethis::use_data(abstracts_stm, overwrite = TRUE)

require(text2vec)
lda_model <- LDA$new(n_topics = 20, doc_topic_prior = 0.1, topic_word_prior = 0.01)

theta <- lda_model$fit_transform(x = abstracts_dfm, convergence_tol = 0.001, n_check_convergence = 25)

abstracts_warplda <- lda_model

usethis::use_data(abstracts_warplda, overwrite = TRUE)

lsa_model <- LSA$new(n_topics = 20)

###
require(topicmodels)

abstracts_tm <- convert(abstracts_dfm, to = "topicmodels")

abstracts_topicmodels <- LDA(abstracts_tm, k = 20)

usethis::use_data(abstracts_topicmodels, overwrite = TRUE)

require(BTM)

### Use the quanteda method suggested by Benoit.
### https://github.com/quanteda/quanteda/issues/1404

## TODO: removal of dense terms. 
tokens(corpus(abstracts$text), remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE) %>%  tokens_tolower() %>% tokens_remove(stopwords("en")) %>% tokens_wordstem() -> toks_q

as.data.frame.tokens <- function(x) {
  data.frame(
    doc_id = rep(names(x), lengths(x)),
    tokens = unlist(x, use.names = FALSE)
  )
}



abstracts_btm <- BTM(as.data.frame.tokens(toks_q), k = 20, beta = 0.01, iter = 100, trace = 10)

predict(abstracts_btm, newdata = as.data.frame.tokens(toks_q))

abstracts_btm

usethis::use_data(abstracts_btm, overwrite = TRUE)
