.extract_ingredients.input_model_s3_stm <- function(input_model_s3, n_top_terms = 5, difficulty = 1, use_frex_words = FALSE, need_topic = FALSE, n_topiclabel_words = 8, ...) {
    input_model <- input_model_s3$model
    K <- input_model$settings$dim$K
    V <- input_model$settings$dim$V
    if (use_frex_words) {
        terms <- stm::labelTopics(input_model, n = input_model$settings$dim$V, frexweight = difficulty)$frex
    } else {
        terms <- stm::labelTopics(input_model, n = input_model$settings$dim$V)$prob
    }
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        if (use_frex_words) {
            model_terms <- stm::labelTopics(input_model, n = n_topiclabel_words, frexweight = difficulty)$frex
        } else {
            model_terms <- stm::labelTopics(input_model, n = n_topiclabel_words)$prob
        }
        theta <- input_model$theta
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
