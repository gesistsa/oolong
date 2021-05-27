.extract_ingredients.input_model_s3_warplda <- function(input_model_s3, n_top_terms = 5, difficulty = 1, need_topic = TRUE, n_topiclabel_words = 8, input_dfm = NULL, ...) {
    input_model <- input_model_s3$model
    K <- input_model$.__enclos_env__$private$n_topics
    V <- length(input_model$.__enclos_env__$private$vocabulary)
    terms <- t(input_model$get_top_words(n = V, lambda = difficulty))
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        .cstop(is.null(input_dfm), "input_dfm must not be NULL when input_model is a WarpLDA object.")
        model_terms <- t(input_model$get_top_words(n = n_topiclabel_words, lambda = difficulty))
        theta <- input_model$transform(input_dfm)
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
