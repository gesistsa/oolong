.extract_ingredients.input_model_s3_btm <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, ...) {
    input_model <- input_model_s3$model
    K <- input_model$K
    V <- input_model$W
    terms <- t(apply(input_model$phi, MARGIN = 2, FUN = function(x){
        x <- data.frame(token = names(x), probability = x)
        x <- x[order(x$probability, decreasing = TRUE), ]
        x <- x$token
        head(x, n = length(x))
    })) 
    all_terms <- unique(as.vector(terms[,seq_len(n_top_terms)]))
    if (need_topic) {
        ## NOT SUPPORT (YET)!
        warning("Generating topic intrusion test for BTM is not supported.")
    }
    model_terms <- NULL
    theta <- NULL
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
