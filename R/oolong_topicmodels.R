.extract_ingredients.input_model_s3_topicmodels <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, n_topiclabel_words = 8,...) {
    input_model <- input_model_s3$model
    K <- input_model@k
    V <- length(input_model@terms)
    terms <- t(topicmodels::terms(input_model, k = V))
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        model_terms <- t(topicmodels::terms(input_model, k = n_topiclabel_words))
        dimnames(model_terms)[[1]] <- NULL
        theta <- topicmodels::posterior(input_model)$topic
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
