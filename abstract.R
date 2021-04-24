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

###

require(keyATM)
require(quanteda)
## 0.2.2

abstracts_dfm

abstracts_dictionary <- list(jstudies = c("frame", "news", "journalist", "newspap", "agenda"),
     polcomm = c("polit", "elect", "campaign", "candid", "opinion", "civil", "partisan", "ideolog"),
     healthcomm = c("health", "patient", "medic", "literaci", "hiv", "attitud", "perceiv", "risk", "messag", "smoke", "campaign", "alcohol", "food", "obes"),
     scicomm = c("scienc", "scientif", "public", "climat"),
     game = c("game", "player", "video", "gamer", "mmorpg", "violenc", "violent"),
     cmc = c("internet", "onlin", "sns", "twitter", "facebook", "comput", "cmc", "virtual", "technolog", "share", "privaci", "web", "search", "network", "digit"),
     culture = c("racial", "white", "black", "african", "communiti", "cultur", "postcoloni"),
     survey_meth = c("measur", "valid", "variabl", "instrument", "reliabl", "psychometr", "survey", "respond", "interview", "test", "item", "questionnair"),
     mediapsy = c("emot", "experiment", "behavior", "affect", "percept", "exposur", "influenc", "experi", "cognit"),
     lang = c("discours", "languag", "interact", "talk", "construct", "convers", "action", "ident"))

abstracts_keyatm <- keyATM(keyATM_read(abstracts_dfm), no_keyword_topics = 0, keywords = abstracts_dictionary, model = "base", options = list(seed = 46709394))
top_words(abstracts_keyatm, show_keyword = FALSE, n = 30)

usethis::use_data(abstracts_keyatm, overwrite = TRUE)
usethis::use_data(abstracts_dictionary, overwrite = TRUE)

oolong <- create_oolong(abstracts_keyatm)

oolong$do_word_intrusion_test()

oolong <- create_oolong(abstracts_keyatm, abstracts$text, exact_n = 10)


### very small k

abstracts_dfm %>% convert(to = "stm", omit_empty = FALSE) -> abstracts_stm_data_small

abstracts_stm_small <- stm(abstracts_stm_data_small$documents, abstracts_stm_data_small$vocab, data =abstracts_stm_data_small$meta, K = 4, seed = 46709394)

saveRDS(abstracts_stm_small, "./tests/testdata/abstracts_stm_small.RDS")

abstracts_tm <- convert(abstracts_dfm, to = "topicmodels")
require(topicmodels)
abstracts_topicmodels_small <- LDA(abstracts_tm, k = 3)

saveRDS(abstracts_topicmodels_small, "./tests/testdata/abstracts_topicmodels_small.RDS")

## seededlda
require(seededlda)
abstracts_seededlda <- textmodel_seededlda(x = abstracts_dfm, dictionary = dictionary(abstracts_dictionary), seeds = 46709394, verbose = TRUE)
##saveRDS(abstracts_seededlda, "./tests/testdata/abstracts_seededlda.RDS")
usethis::use_data(abstracts_seededlda, overwrite = TRUE)

abstracts_unseededlda <- textmodel_lda(x = abstracts_dfm, k = 20, verbose = TRUE)
##saveRDS(abstracts_unseededlda, "./tests/testdata/abstracts_unseededlda.RDS")
usethis::use_data(abstracts_unseededlda, overwrite = TRUE)
