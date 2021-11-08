### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.

#' Oolong: create and administrate validation tests for typical automated content analysis tools
#'
#' The oolong package makes it very easy to create, administrate and evaluate typical automated content analysis tools by providing a framework for creating human-in-the-loop validation tests. For topic models, oolong can generate tests such as word intrusion test, topic intrusion test (Chang et al.) and word set intrusion test (Ying et al.) For dictionary-based methods, oolong can generate standardized interface for making gold standard ('Ground truth') data.
#' @docType package
#' @author Chung-hong Chan
#' @name oolong
#' @importFrom R6 R6Class
NULL

Oolong_test <- R6::R6Class(
    "oolong_generic",
    public = list(
        initialize = function(input_model, input_corpus = NULL) {
        },
        lock = function(force = FALSE) {
            .cstop(!.check_test_content_complete(private$test_content) & !force, "Not all tests are completed. Do all the tests or use $lock(force = TRUE) to bypass this.")
            private$finalized <- TRUE
        },
        userid = NA
    ),
    private = list(
        hash = NULL,
        hash_input_model = NULL,
        hash_input_corpus = NULL,
        test_content = list(),
        meta = list(),
        finalized = FALSE,
        check_finalized = function() {
            .cstop(private$finalized, "You can no longer modify this finalized test.")
        }
    )
)

#' Generate an oolong test
#'
#' \code{create_oolong} generates an oolong test object that can either be used for validating a topic model or for creating ground truth (gold standard) of a text corpus. \code{wi} (word intrusion test), \code{ti} (topic intrusion test), \code{witi} (word and topic intrusion tests), \code{wsi} (word set intrusion test) and \code{gs} are handy wrappers to \code{create_oolong}. It is recommended to use these wrappers instead of \code{create_oolong}.
#'
#' @section Usage:
#'
#' Use \code{wi}, \code{ti}, \code{witi}, \code{wsi} or \code{gs} to generate an oolong test of your choice. It is recommended to supply also \code{userid} (current coder).
#' The names of the tests (word intrusion test and topic intrusion test) follow Chang et al (2009). In Ying et al. (forthcoming), topic intrusion test is named "T8WSI" (Top 8 Word Set Intrusion). Word set intrusion test in this package is actually the "R4WSI" (Random 4 Word Set Intrusion) in Lu et al (forthcoming). The default settings of \code{wi}, \code{witi}, and \code{ti} follow Chang et al (2009), e.g. \code{n_top_terms} = 5; instead of \code{n_top_terms} = 4 as in Lu et al (forthcoming). The default setting of \code{wsi} follows Ying et al. (forthcoming), e.g. \code{n_topiclabel_words} = 4.
#' As suggested by Song et al. (2020), 1% of the articles from \code{input_corpus} are randomly selected as the test cases of both \code{ti} and \code{gs}, i.e. \code{frac} = 0.01. However, it is generally believed that this proportion is dependent of the size of \code{input_corpus}, e.g. it does not make sense to draw 1% of the articles from only 100 articles. Use \code{exact_n} in these cases.
#' @section About create_oolong:
#'
#' Because \code{create_oolong} is not intuitive to use, it is no longer recommended to use \code{create_oolong} to generate oolong test. \code{create_oolong} is retained only for backward compatibility purposes. This function generates an oolong test object based on \code{input_model} and \code{input_corpus}. If \code{input_model} is not NULL, it generates oolong test for a topic model (tm). If \code{input_model} is NULL but input_corpus is not NULL, it generates oolong test for generating gold standard (gs).
#'
#' @section Methods:
#' An oolong object, depends on its purpose, has the following methods:
#' \describe{
#'   \item{\code{$do_word_intrusion_test()}}{(tm) launch the shiny-based word intrusion test. The coder should find out the intruder word that is not related to other words.}
#'   \item{\code{$do_topic_intrusion_test()}}{(tm) launch the shiny-based topic intrusion test. The coder should find out the intruder topic that is least likely to be the topic of the document.}
#'   \item{\code{$do_word_set_intrusion_test()}}{(tm) launch the shiny-based word set intrusion test. The coder should find out the intruder word set that is not related to other word sets.}
#'   \item{\code{$do_gold_standard_test()}}{(gs) launch the shiny-based test for generating gold standard. The coder should determine the level of the predetermined constructs with a 5-point Likert scale.}
#'   \item{\code{$lock(force = FALSE)}}{(gs/tm) lock the object so that it cannot be changed anymore. It enables \code{\link{summarize_oolong}} and the following method.}
#'   \item{\code{$turn_gold()}}{(gs) convert the oolong object into a quanteda compatible corpus.}
#' }
#' For more details, please see the overview vignette: \code{vignette("overview", package = "oolong")}
#' @param input_model (wi, ti, witi, wsi) a STM, WarpLDA, topicmodels, KeyATM, seededlda, textmodel_nb, or BTM object; if it is NULL, create_oolong assumes that you want to create gold standard.
#' @param input_corpus (wi, ti, witi, wsi, gs) if input_model is not null, it should be the corpus (character vector or quanteda::corpus object) to generate the model object. If input_model and input_corpus are not NULL, topic intrusion test cases are generated. If input_model is a BTM object, this argument is ignored. If input_model is null, it generates gold standard test cases.
#' @param n_top_terms (wi, witi) integer, number of top topic words to be included in the candidates of word intrusion test. 
#' @param bottom_terms_percentile (wi, witi) double, a term is considered to be an word intruder when its theta less than the percentile of this theta, must be within the range of 0 to 1
#' @param exact_n (ti, witi, gs) integer, number of topic intrusion test cases to generate, ignore if frac is not NULL
#' @param frac (ti, witi, gs) double, fraction of test cases to be generated from the corpus
#' @param n_top_topics (wi, witi) integer, number of most relevant topics to be shown alongside the intruder topic
#' @param n_topiclabel_words (witi, ti, wsi) integer, number of topic words to be shown as the topic ("ti" and "witi") / word set ("wsi") label
#' @param use_frex_words (wi, witi, ti, wsi) logical, for a STM object, use FREX words if TRUE, use PROB words if FALSE
#' @param difficulty (wi, witi, ti, wsi) double, adjust the difficulty of the test. Higher value indicates higher difficulty and must be within the range of 0 to 1, no effect for STM if use_frex_words is FALSE. Ignore for topicmodels objects
#' @param type (create_oolong) a character string to denote what you want to create. "wi": word intrusion test; "ti": topic intrusion test; "witi": both word intrusion test and topic intrusion test; "gs": gold standard generation
#' @param input_dfm (wi, witi, ti, wsi) a dfm object used for training the input_model, if input_model is a WarpLDA object
#' @param construct (gs) string, an adjective to describe the construct you want your coders to code the the gold standard test cases
#' @param btm_dataframe (witi, ti) dataframe used for training the input_model, if input_model is a BTM object
#' @param userid a character string to denote the name of the coder. Default to NA (no userid); not recommended
#' @param n_correct_ws (wsi) number of word sets to be shown alongside the intruder word set
#' @param wsi_n_top_terms (wsi) number of top topic words from each topic to be randomized selected as the word set label
#' @return an oolong test object.
#' @examples
#' ## Creation of oolong test with only word intrusion test
#' data(abstracts_keyatm)
#' data(abstracts)
#' oolong_test <- wi(input_model = abstracts_keyatm, userid = "Hadley")
#' ## Creation of oolong test with both word intrusion test and topic intrusion test
#' oolong_test <- witi(input_model = abstracts_keyatm, input_corpus = abstracts$text, userid = "Julia")
#' ## Creation of oolong test with topic intrusion test
#' oolong_test <- ti(input_model = abstracts_keyatm, input_corpus = abstracts$text, userid = "Jenny")
#' ## Creation of oolong test with word set intrusion test
#' oolong_test <- wsi(input_model = abstracts_keyatm, userid = "Garrett")
#' ## Creation of gold standard
#' oolong_test <- gs(input_corpus = trump2k, userid = "Yihui")
#' ## Using create_oolong(); not recommended
#' oolong_test <- create_oolong(input_model = abstracts_keyatm,
#' input_corpus = abstracts$text, userid = "JJ")
#' oolong_test <- create_oolong(input_model = abstracts_keyatm,
#' input_corpus = abstracts$text, userid = "Mara", type = "ti")
#' oolong_test <- create_oolong(input_corpus = abstracts$text, userid = "Winston", type = "gs")
#' @author Chung-hong Chan, Marius Sältzer
#' @references
#'   Chang, J., Gerrish, S., Wang, C., Boyd-Graber, J. L., & Blei, D. M. (2009). Reading tea leaves: How humans interpret topic models. In Advances in neural information processing systems (pp. 288-296).
#'
#'   Song et al. (2020) In validations we trust? The impact of imperfect human annotations as a gold standard on the quality of validation of automated content analysis. Political Communication.
#'
#'   Ying, L., Montgomery, J. M., & Stewart, B. M. (Forthcoming). Inferring concepts from topics: Towards procedures for validating topics as measures. Political Analysis
#' 
#' @export
create_oolong <- function(input_model = NULL, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, use_frex_words = FALSE, difficulty = 1, input_dfm = NULL, construct = "positive", btm_dataframe = NULL, n_correct_ws = 3, wsi_n_top_terms = 20, userid = NA, type = "witi") {
    .cstop(!type %in% c("wi", "witi", "ti", "gs", "wsi"), "Unknown type, available types are 'wi', 'witi', 'ti', 'wsi' and 'gs'")
    .cstop(is.null(input_model) & is.null(input_corpus), "input_model and input_corpus cannot be both NULL.")
    if (!is.null(input_model)) {
        .cstop(!.is_topic_model(input_model), "input_model is not a topic model. If you want to create gold standard with an input_corpus, use: create_oolong(input_corpus = input_corpus) or gs(input_corpus)")
    }
    .cstop(length(userid) > 1, "userid must not be a vector with length > 1.")
    if (!is.null(input_model) & type %in% c("wi", "ti", "witi", "wsi")) {
        return(Oolong_test_tm$new(input_model = input_model, input_corpus = input_corpus, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words, difficulty = difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm, btm_dataframe = btm_dataframe, n_correct_ws = n_correct_ws, wsi_n_top_terms = wsi_n_top_terms, userid = userid, type = type))
    }
    if (is.null(input_model) | type == "gs") {
        return(Oolong_test_gs$new(input_corpus = input_corpus, exact_n = exact_n, frac = frac, construct = construct, userid = userid))
    }
}


#' Clone an oolong object
#'
#' Clone a new oolong object. The oolong must not be locked and ever coded.
#' @param oolong an oolong object.
#' @param userid a character string to denote the name of the coder
#' @return an oolong object
#' @author Chung-hong Chan
#' @export
clone_oolong <- function(oolong, userid = NA) {
    if (oolong$.__enclos_env__$private$finalized) {
        stop("oolong is locked.")
    }
    if (!.check_new(oolong)) {
        stop("oolong is partially coded.")
    }
    newoolongobj <- oolong$clone(deep = FALSE)
    newoolongobj$userid <- userid
    return(newoolongobj)
}

#' Obtain a locked oolong from a downloaded data file
#'
#' To generate a locked oolong object with the original oolong object and the RDS file. The RDS file should have been downloaded from a deployed Shiny app.
#' @param oolong an oolong object used for deployment
#' @param rds_file path to the downloaded RDS file
#' @return a locked oolong object based on the data in the downloaded RDS file
#' @author Chung-hong Chan
#' @export
revert_oolong <- function(oolong, rds_file) {
    res <- readRDS(rds_file)
    .cstop(res$test_items_hash != .safe_hash(res$test_items), "The RDS file seems to have been tampered. Please check with userid:", res$userid, ".")
    cloned_oolong <- clone_oolong(oolong)
    .cstop(res$hash != cloned_oolong$.__enclos_env__$private$hash, "The oolong test result does not match the original oolong object.", call. = FALSE)
    cloned_oolong$.__enclos_env__$private$test_content[[1]] <- res$test_items
    cloned_oolong$userid <- res$userid
    cloned_oolong$lock()
    return(cloned_oolong)
}
