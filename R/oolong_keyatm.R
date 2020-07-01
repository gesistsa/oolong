.extract_ingredients.input_model_s3_keyatm <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, n_topiclabel_words = 8,...) {
    input_model <- input_model_s3$model
    K <- ncol(input_model$theta)
    V <- input_model$V
    terms <- t(as.matrix(keyATM::top_words(input_model, n = V, show_keyword = FALSE)))
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        model_terms <- t(as.matrix(keyATM::top_words(input_model, n = n_topiclabel_words, show_keyword = FALSE)))
        dimnames(model_terms)[[1]] <- NULL
        theta <- input_model$theta
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
