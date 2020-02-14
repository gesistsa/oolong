### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.

Oolong_test <- R6::R6Class(
    "oolong_generic",
    public = list(
        initialize = function(input_model, input_corpus = NULL) {
            private$test_content <- list()
        },
        lock = function(force = FALSE) {
            if (!.check_test_content_complete(private$test_content) & !force) {
                stop("Not all tests are completed. Do all the tests or use $lock(force = TRUE) to bypass this.")
            }
            private$finalized <- TRUE
        }
    ),
    private = list(
        hash = NULL,
        test_content = list(),
        finalized = FALSE,
        check_finalized = function() {
            .cstop(private$finalized, "You can no longer modify this finalized test.")
        }
    )
)


#' Generate a oolong test
#'
#' This function generates a oolong test object that can either be used for validating a topic model or for creating ground truth (gold standard) of a text corpus.
#'
#' @param input_model a STM, WarpLDA or topicmodels object
#' @param input_corpus if input_model is not null, it should be the corpus (character vector or quanteda::corpus object) to generate the model object, if not NULL, topic intrusion test cases are generated. If input_model is null, it generates gold standard test cases.
#' @param n_top_terms integer, number of top topic words to be included in the candidates of word intrusion test
#' @param bottm_terms_percentile double, a term is considered to be an word intruder when its theta less than the percentile of this theta, must be within the range of 0 to 1
#' @param exact_n integer, number of topic intrusion test cases to generate, ignore if frac is not NULL
#' @param frac double, fraction of test cases to be generated from the corpus
#' @param n_top_topic integer, number of most relevant topics to be shown alongside the intruder topic
#' @param n_topiclabel_words integer, number of topic words to be shown as the topic label
#' @param use_frex_words logical, for a STM object, use FREX words if TRUE, use PROB words if FALSE
#' @param difficulty double, to adjust the difficulty of the test. Higher value indicates higher difficulty, must be within the range of 0 to 1, no effect for STM if use_frex_words is FALSE. Ignore for topicmodels objects.
#' @param input_dfm a dfm object used for training the input_model, if input_model is a WarpLDA object
#' @export
create_oolong <- function(input_model = NULL, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, use_frex_words = FALSE, difficulty = 1, input_dfm = NULL, target_value = NULL, construct = "positive") {
    if (!is.null(input_model)) {
        return(Oolong_test_tm$new(input_model = input_model, input_corpus = input_corpus, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words, difficulty = difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm))
    } else {
        return(Oolong_test_gs$new(input_corpus = input_corpus, exact_n = exact_n, frac = frac, target_value = target_value, construct = construct))
    }
}


#' Newsgroup text dataset
#'
#' This is a subset of the famous "newsgroup 20" dataset.
"newsgroup5"

