### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.

#' oolong: create and administrate validation tests for typical automated content analysis tools
#'
#' The oolong package makes it very easy to create, adminstrate and evaluate typical automated analysis tools by providing a framework for creating human-in-the-loop validation tests. For topic models, oolong can generate tests such as word intrusion test and topic intrusion test (Chang et al.). For dictionary-based methods, oolong can generate standardized interface for making gold standard ('Ground truth') data. There are only two core functions of this package: \code{\link{create_oolong}} and \code{\link{summarize_oolong}}.
#' @docType package
#' @name oolong
NULL

Oolong_test <- R6::R6Class(
    "oolong_generic",
    public = list(
        initialize = function(input_model, input_corpus = NULL) {
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
#' @param input_model (gs/tm) a STM, WarpLDA or topicmodels object; if it is NULL, create_oolong assumes that you want to create gold standard.
#' @param input_corpus (gs/tm) if input_model is not null, it should be the corpus (character vector or quanteda::corpus object) to generate the model object. If input_model and input_corpus are not NULL, topic intrusion test cases are generated. If input_model is null, it generates gold standard test cases.
#' @param n_top_terms (tm) integer, number of top topic words to be included in the candidates of word intrusion test. 
#' @param bottom_terms_percentile (tm) double, a term is considered to be an word intruder when its theta less than the percentile of this theta, must be within the range of 0 to 1
#' @param exact_n (tm/gs) integer, number of topic intrusion test cases to generate, ignore if frac is not NULL
#' @param frac (tm/gs) double, fraction of test cases to be generated from the corpus
#' @param n_top_topics (tm) integer, number of most relevant topics to be shown alongside the intruder topic
#' @param n_topiclabel_words (tm) integer, number of topic words to be shown as the topic label
#' @param use_frex_words (tm) logical, for a STM object, use FREX words if TRUE, use PROB words if FALSE
#' @param difficulty (tm) double, adjust the difficulty of the test. Higher value indicates higher difficulty and must be within the range of 0 to 1, no effect for STM if use_frex_words is FALSE. Ignore for topicmodels objects.
#' @param input_dfm (tm) a dfm object used for training the input_model, if input_model is a WarpLDA object
#' @param construct (gs) string, an adjective to describe the construct you want your coders to code the the gold standard test cases.
#' @return An oolong test object.
#' @author Chung-hong Chan
#' @examples
#' ## Creation of oolong test with only word intrusion test
#' data(abstracts_stm)
#' data(abstracts)
#' oolong_test <- create_oolong(input_model = abstracts_stm)
#' ## Creation of oolong test with both word intrusion test and topic intrusion test
#' oolong_test <- create_oolong(input_model = abstracts_stm, input_corpus = abstracts$text)
#' ## Creation of gold standard
#' oolong_test <- create_oolong(input_corpus = trump2k)
#' @export
create_oolong <- function(input_model = NULL, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, use_frex_words = FALSE, difficulty = 1, input_dfm = NULL, construct = "positive") {
    if (is.null(input_model) & is.null(input_corpus)) {
        stop("input_model and input_corpus cannot be both NULL.")
    }
    if (!is.null(input_model)) {
        if (!.is_topic_model(input_model)) {
            stop("input_model is not a topic model. If you want to create gold standard with an input_corpus, use: create_oolong(input_corpus = input_corpus)")
        }
    }
    if (!is.null(input_model)) {
        return(Oolong_test_tm$new(input_model = input_model, input_corpus = input_corpus, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words, difficulty = difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm))
    } else {
        return(Oolong_test_gs$new(input_corpus = input_corpus, exact_n = exact_n, frac = frac, construct = construct))
    }
}

.is_topic_model <- function(x) {
    if (any(class(x) %in% c("WarpLDA", "STM"))) {
        return(TRUE)
    }
    if (is.null(attr(class(x), "package"))) {
        return(FALSE)
    }
    if ("topicmodels" == attr(class(x), "package")) {
        return(TRUE)
    }
    return(FALSE)
}

#' Clone a oolong object.
#'
#' Clone a new oolong object. The oolong must not be locked and ever coded.
#' @param oolong an oolong object.
#' @return an oolong object
#' @author Chung-hong Chan
#' @export
clone_oolong <- function(oolong) {
    if (oolong$.__enclos_env__$private$finalized) {
        stop("oolong is locked.")
    }
    if (!.check_new(oolong)) {
        stop("oolong is partially coded.")
    }
    oolong$clone(deep = FALSE)
}
