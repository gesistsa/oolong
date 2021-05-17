#' @rdname create_oolong
#' @export
wi <- function(input_model = NULL, userid = NA, n_top_terms = 5, bottom_terms_percentile = 0.6, difficulty = 1, use_frex_words = FALSE) {
    create_oolong(input_model = input_model, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, difficulty = difficulty, use_frex_words = use_frex_words, userid = userid, type = "wi")
}

#' @rdname create_oolong
#' @export
witi <- function(input_model = NULL, input_corpus = NULL, userid = NA, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, use_frex_words = FALSE, difficulty = 1, input_dfm = NULL, btm_dataframe = NULL) {
    create_oolong(input_model = input_model, input_corpus = input_corpus, n_top_terms = n_top_terms, bottom_terms_percentile = bottom_terms_percentile, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words, use_frex_words = use_frex_words, difficulty = difficulty, input_dfm = input_dfm, btm_dataframe = btm_dataframe, userid = userid, type = "witi")
}

#' @rdname create_oolong
#' @export
ti <- function(input_model = NULL, input_corpus = NULL, userid = NA, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, use_frex_words = FALSE, difficulty = 1, input_dfm = NULL, btm_dataframe = NULL) {
    create_oolong(input_model = input_model, input_corpus = input_corpus, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words, use_frex_words = use_frex_words, difficulty = difficulty, input_dfm = input_dfm, btm_dataframe = btm_dataframe, userid = userid, type = "ti")
}

#' @rdname create_oolong
#' @export
wsi <- function(input_model = NULL, userid = NA, n_topiclabel_words = 4, n_correct_ws = 3, wsi_n_top_terms = 20, difficulty = 1, use_frex_words = FALSE) {
    create_oolong(input_model = input_model, difficulty = difficulty, use_frex_words = use_frex_words, n_topiclabel_words = n_topiclabel_words, n_correct_ws = n_correct_ws, wsi_n_top_terms = wsi_n_top_terms, userid = userid, type = "wsi")
}

#' @rdname create_oolong
#' @export
gs <- function(input_corpus = NULL, userid = NA, construct = "positive", exact_n = NULL, frac = 0.01) {
    create_oolong(input_corpus = input_corpus, exact_n = exact_n, frac = frac, construct = construct, userid = userid, type = "gs")
}
