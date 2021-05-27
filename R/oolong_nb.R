.extract_ingredients.input_model_s3_nb <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, n_topiclabel_words = 8,...) {
    if (need_topic) {
        .cstop(TRUE, "Generation of topic intrusion tests with textmodel_nb is not supported.")
    }
    input_model <- input_model_s3$model
    terms <- as.data.frame(input_model$param)
    K <- nrow(terms)
    V <- ncol(terms)
    scores <- matrix(NA, K, V)
    all_terms <- as.character(names(terms))
    for (i in 1:nrow(terms)){
        props <- as.numeric(terms[i, ])
        scores[i,] <- all_terms[order(props, decreasing = T)]
    }
    all_terms <- names(terms)
    terms <- scores
    model_terms <- NULL
    theta <- NULL
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
