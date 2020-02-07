require(oolong)
require(tidyverse)
require(stm)


.sample_corpus <- function(corpus, exact_n = 30, frac = NULL) {
    sample(seq_len(length(corpus)), exact_n)
}

.insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- sample(good_terms)
    return(tibble::tibble(position = position, candidates = list(res)))
}

.get_intruder_by_position <- function(candidates, position) {
    candidates[position]
}

.generate_topic_frame <- function(i, target_text, target_theta, model_terms, k = k, n_top_topics = 3, n_top_words = 8) {
    text <- target_text[i]
    theta_rank <- rank(target_theta[i,])
    theta_pos <- which(theta_rank > (k - n_top_topics))
    intruder_pos <- sample(setdiff(seq_len(k), theta_pos), 1)
    position <- sample(seq_len(n_top_topics + 1), 1)
    topic_frame <- .insert(theta_pos, intruder_pos, position)
    topic_frame$text <- text
    topic_frame$topic_labels <- list(apply(model_terms[topic_frame$candidates[[1]],], 1, paste0, collapse = ", "))
    topic_frame$thetas <- list(target_theta[i, topic_frame$candidates[[1]]])
    topic_frame$answer <- NA
    topic_frame$intruder <- map2_int(topic_frame$candidates, topic_frame$position, .get_intruder_by_position)
    return(topic_frame)
}

.generate_topic_instrusion_test <- function(model, corpus, exact_n = 30, frac = NULL, n_top_topics = 3, n_top_words = 8, difficulty = 0.8) {
    sample_vec <- .sample_corpus(corpus, exact_n)
    model_terms <- stm::labelTopics(model, n = n_top_words, frexweight = difficulty)$frex
    target_theta <- model$theta[sample_vec, ]
    k <- ncol(target_theta)
    target_text <- corpus[sample_vec]
    test_content <- purrr::map_dfr(seq_len(exact_n), .generate_topic_frame, target_text = target_text, target_theta = target_theta, model_terms = model_terms, k = k, n_top_topics = n_top_topics, n_top_words = n_top_words)
    return(test_content)
}

topic_test_content <- .generate_topic_instrusion_test(newsgroup_stm, newsgroup5$text)
