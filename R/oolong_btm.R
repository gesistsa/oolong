
.generate_btm_theta <- function(input_model, btm_dataframe, input_corpus) {
    dirty_theta <- predict(input_model, newdata = btm_dataframe)
    K <- input_model$K
    better_theta <- dirty_theta[match(quanteda::docid(input_corpus), row.names(dirty_theta)),]
    ## replace NA value with ambiguous theta, i.e. 1/K
    better_theta[is.na(better_theta)] <- 1/K
    rownames(better_theta) <- quanteda::docid(input_corpus)
    return(better_theta)
}

.extract_ingredients.input_model_s3_btm <- function(input_model_s3, n_top_terms = 5, need_topic = FALSE, btm_dataframe = NULL, input_corpus = NULL, n_topiclabel_words = 8,...) {
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
        if (is.null(input_corpus) | is.null(btm_dataframe) | !"corpus" %in% class(input_corpus)) {
            stop("You need to provide input_corpus (in quanteda format) and btm_dataframe for generating topic intrusion tests.")
        }
        model_terms <- terms[, seq_len(n_topiclabel_words)]
        theta <- .generate_btm_theta(input_model, btm_dataframe, input_corpus)        
    } else {
        model_terms <- NULL
        theta <- NULL
    }
    return(list(K = K, V = V, terms = terms, all_terms = all_terms, model_terms = model_terms, theta = theta))
}
