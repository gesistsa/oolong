.extract_ingredients.input_model_s3_seededlda <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, n_topiclabel_words = 8,...) {
    input_model <- input_model_s3$model
    K <- input_model[["k"]]
    V <- ncol(input_model$phi)
    terms <- t(as.matrix(seededlda::terms(input_model, n = ncol(input_model$phi))))
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        model_terms <- t(seededlda::terms(input_model, n = n_topiclabel_words))
        rownames(model_terms) <- NULL
        theta <- input_model[["theta"]]
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
