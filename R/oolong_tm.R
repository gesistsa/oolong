### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.

.insert <- function(good_terms, intruder, position) {
    length_test_items <- length(c(good_terms, intruder))
    res <- rep(NA, length_test_items)
    res[position] <- intruder
    res[setdiff(1:length_test_items, position)] <- .safe_sample(good_terms)
    return(tibble::tibble(position = position, candidates = list(res)))
}

.generate_candidates <- function(i, terms, n_top_terms = 5, bottom_terms_percentile = 0.6, all_terms) {
    good_terms <- head(terms[i,], n_top_terms)
    term_pos <- match(all_terms, terms[i,])
    candidates <- tibble::tibble(all_terms, term_pos)
    non_na_candidates <- candidates[!is.na(candidates$term_pos),]
    intruder <- .safe_sample(non_na_candidates$all_terms[non_na_candidates$term_pos > quantile(non_na_candidates$term_pos, bottom_terms_percentile, na.rm = TRUE)], 1)
    position <- .safe_sample(1:(n_top_terms + 1), 1)
    .insert(good_terms, intruder, position) -> res
    res$t <- i
    res$intruder <- res$candidates[[1]][res$position]
    res$answer <- NA
    return(res)
}

.cal_model_precision <- function(test_items) {
    if (all(is.na(test_items$answer))) {
        return(0)
    } else {
        base <- sum(!is.na(test_items$answer))
        n_correct <- sum(test_items$answer[!is.na(test_items$answer)] == test_items$intruder[!is.na(test_items$answer)])
        return(round((n_correct / base) * 100, 2))
    }
}

### S3 generic

.extract_ingredients <- function(input_model_s3, ...) {
    UseMethod(".extract_ingredients", input_model_s3)
}

## every format should create a s3 method '.extract_ingredients' (e.g. .extract_ingredients.input_model_s3_warplda) to extract K (# of topics), V (# of terms), terms (a term matrix, K x V) and all_terms, model_terms and theta.
## refer to oolong_stm.R for an example

.generate_word_intrusion_test <- function(ingredients, bottom_terms_percentile = 0.6, n_top_terms) {
    test_items <- purrr::map_dfr(seq_len(ingredients$K), .generate_candidates, terms = ingredients$terms, all_terms = ingredients$all_terms, bottom_terms_percentile = bottom_terms_percentile, n_top_terms = n_top_terms)
    return(test_items)
}

.sample_corpus <- function(input_corpus, exact_n = 30) {
    .safe_sample(seq_len(length(input_corpus)), exact_n)
}

.get_intruder_by_position <- function(candidates, position) {
    candidates[position]
}

.generate_topic_frame <- function(i, target_text, target_theta, model_terms, k = k, n_top_topics = 3) {
    text <- target_text[i]
    theta_rank <- rank(target_theta[i,], ties.method = "random")
    theta_pos <- which(theta_rank > (k - n_top_topics))
    intruder_pos <- .safe_sample(setdiff(seq_len(k), theta_pos), 1)
    position <- .safe_sample(seq_len(n_top_topics + 1), 1)
    topic_frame <- .insert(theta_pos, intruder_pos, position)
    topic_frame$text <- text
    topic_frame$topic_labels <- list(apply(model_terms[topic_frame$candidates[[1]],], 1, paste0, collapse = ", "))
    topic_frame$thetas <- list(target_theta[i, topic_frame$candidates[[1]]])
    topic_frame$answer <- NA
    topic_frame$intruder <- purrr::map2_int(topic_frame$candidates, topic_frame$position, .get_intruder_by_position)
    return(topic_frame)
}

.generate_topic_intrusion_test <- function(input_corpus, ingredients, exact_n = NULL, frac = NULL, n_top_topics = 3, n_topiclabel_words = 8) {
    if ("corpus" %in% class(input_corpus)) {
        input_corpus <- as.character(input_corpus)
    }
    .cstop(n_top_topics <= 1, "n_top_topics must be larger than 1")
    .cstop(n_top_topics + 1 > ingredients$K, "n_top_topics + 1 must be smaller than K.")
    if (!is.null(frac) & is.null(exact_n)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(input_corpus) * frac)
    }
    .cstop(exact_n <=1, "exact_n or frac are too small for your sample size. Please increase either the exact_n or frac.")
    if (exact_n > length(input_corpus)) {
        warning("exact_n is larger than the size of input_corpus. Switch to frac = 1")
        exact_n <- length(input_corpus)
    }
    sample_vec <- .sample_corpus(input_corpus, exact_n)
    target_theta <- ingredients$theta[sample_vec,]
    k <- ncol(target_theta)
    target_text <- input_corpus[sample_vec]
    test_items <- purrr::map_dfr(seq_len(exact_n), .generate_topic_frame, target_text = target_text, target_theta = target_theta, model_terms = ingredients$model_terms, k = k, n_top_topics = n_top_topics)
    return(test_items)
}

.slice_sample <- function(x, n_topiclabel_words, n_correct_ws) {
    res <- split(.safe_sample(x), ceiling(seq_along(x) / n_topiclabel_words))
    names(res) <- NULL
    ## Filter out things that are not of the length n_topiclabel_words to prevent .safe_sample below select the last one
    res <- res[purrr::map_lgl(res, ~length(.) == n_topiclabel_words)]
    labels <- purrr::map_chr(res, ~(paste(., collapse = ", ")))
    .safe_sample(labels, n_correct_ws)
}

.generate_candidates_wsi <- function(i, terms, n_correct_ws = 3, n_topiclabel_words = 4, wsi_n_top_terms = 20) {
    .cstop(n_correct_ws * n_topiclabel_words > wsi_n_top_terms, "wsi_n_top_terms too small: make it larger than n_correct_ws * n_topiclabel_words")
    good_terms <- .slice_sample(head(terms[i,], wsi_n_top_terms), n_topiclabel_words, n_correct_ws)
    intruder_topic <- .safe_sample(setdiff(seq_len(nrow(terms)), i), 1)
    intruder <- .slice_sample(head(terms[intruder_topic,], wsi_n_top_terms), n_topiclabel_words, 1)
    position <- .safe_sample(1:(n_correct_ws + 1), 1)
    .insert(good_terms, intruder, position) -> res
    res$t <- i
    res$intruder <- res$candidates[[1]][res$position]
    res$answer <- NA
    return(res)
}

.generate_wsi <- function(ingredients, n_correct_ws = 3, n_topiclabel_words = 4, wsi_n_top_terms = 20) {
    test_content <- purrr::map_dfr(seq_len(ingredients$K), .generate_candidates_wsi, terms = ingredients$terms, n_correct_ws = n_correct_ws, n_topiclabel_words = n_topiclabel_words, wsi_n_top_terms = wsi_n_top_terms)
    return(test_content)
}

.generate_test_content <- function(input_model, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = NULL, frac = 0.01, n_top_topics = 3, n_topiclabel_words = 8, difficulty = 1, use_frex_words = FALSE, input_dfm = NULL, btm_dataframe = NULL, type = "witi", n_correct_ws = 3, wsi_n_top_terms = 20) {
    ingredients <- .extract_ingredients(.convert_input_model_s3(input_model), n_top_terms = n_top_terms, difficulty = difficulty, need_topic = !is.null(input_corpus), n_topiclabel_words = n_topiclabel_words, input_dfm = input_dfm, use_frex_words = use_frex_words, input_corpus = input_corpus, btm_dataframe = btm_dataframe)
    .cstop(type %in% c("ti") & is.null(ingredients$theta), "input_corpus can't be NULL for generating oolong test object with only topic intrusion test.")
    test_content <- list()
    if (type %in% c("wi", "witi")) {
        test_content$wi <- .generate_word_intrusion_test(ingredients, bottom_terms_percentile = bottom_terms_percentile, n_top_terms = n_top_terms)
    }
    if (is.null(ingredients$theta)) {
        test_content$ti <- NULL
    } else if (type %in% c("witi", "ti")) {
        test_content$ti <- .generate_topic_intrusion_test(input_corpus = input_corpus, ingredients = ingredients, exact_n = exact_n, frac = frac, n_top_topics = n_top_topics, n_topiclabel_words = n_topiclabel_words)
    } else {
        test_content$ti <- NULL        
    }
    if (type %in% c("wsi")) {
        test_content$wsi <- .generate_wsi(ingredients, n_correct_ws = n_correct_ws, n_topiclabel_words = n_topiclabel_words, wsi_n_top_terms = wsi_n_top_terms)
    } else {
        test_content$wsi <- NULL
    }
    return(test_content)
}

.check_test_content_complete <- function(test_content) {
    all(purrr::map_lgl(test_content, ~all(!is.na(.$answer))))
}

.cal_lr_diff <- function(i, test_items) {
    intruder_idx <- which(test_items$candidates[[i]] == test_items$intruder[i])
    answer_idx <- which(test_items$candidates[[i]] == test_items$answer[i])
    log(test_items$thetas[[i]][intruder_idx]) - log(test_items$thetas[[i]][answer_idx])
}

.cal_tlo <- function(test_items, mean_value = TRUE) {
    res <- purrr::map_dbl(seq_len(nrow(test_items[!is.na(test_items$answer),])), .cal_lr_diff, test_items = test_items[!is.na(test_items$answer),])
    if (mean_value) {
        return(mean(res))
    }
    return(res)
}

.print_oolong_test_tm <- function(private, userid) {
    .check_version(private)
    bool_word <- !is.null(private$test_content$wi)
    bool_topic <- !is.null(private$test_content$ti)
    bool_wsi <- !is.null(private$test_content$wsi)
    bool_finalized <- private$finalized
    cli::cli_h1("oolong (topic model)")
    cli::cli_text("{.sym_flip(bool_word)} {.strong WI} {.sym_flip(bool_topic)} {.strong TI} {.sym_flip(bool_wsi)} {.strong WSI}")
    if (!is.na(userid)) {
        cli::cli_text(cli::symbol$smiley, " ", userid)
    }
    if (bool_word) {
        cli::cli_alert_info("{.strong WI:} k = {nrow(private$test_content$wi)}, {sum(!is.na(private$test_content$wi$answer))} coded.")
    }
    if (bool_topic) {
        cli::cli_alert_info("{.strong TI:} n = {nrow(private$test_content$ti)}, {sum(!is.na(private$test_content$ti$answer))} coded.")
    }
    if (bool_wsi) {
        cli::cli_alert_info("{.strong WSI:} n = {nrow(private$test_content$wsi)}, {sum(!is.na(private$test_content$wsi$answer))} coded.")
    }
    if (bool_finalized) {
        cli::cli_h2("Results:")
        .cp(bool_word, round(.cal_model_precision(private$test_content$wi), 3),"%  precision")
        .cp(bool_wsi, round(.cal_model_precision(private$test_content$wsi), 3),"%  precision (WSI)")
        .cp(bool_topic, "TLO: ", round(.cal_tlo(private$test_content$ti, mean_value = TRUE), 3))
    } else {
        cli::cli_h2("Methods")
        cli::cli_ul()
        if (bool_word) {
            cli::cli_li("{.cls $do_word_intrusion_test()}: do word intrusion test")
        }
        if (bool_topic) {
            cli::cli_li("{.cls $do_topic_intrusion_test()}: do topic intrusion test")
        }
        if (bool_wsi) {
            cli::cli_li("{.cls $do_word_set_intrusion_test()}: do word set intrusion test")
        }
        cli::cli_li("{.cls $lock()}: finalize and see the results")
        cli::cli_end()
    }
}


Oolong_test_tm <-
    R6::R6Class(
        "oolong_test_tm",
        inherit = Oolong_test,
        public = list(
            initialize = function(input_model = NULL, input_corpus = NULL, n_top_terms = 5, bottom_terms_percentile = 0.6, exact_n = 15, frac = NULL, n_top_topics = 3, n_topiclabel_words = 8, difficulty = 1, use_frex_words = FALSE, input_dfm = NULL, btm_dataframe = NULL, userid = NA, n_correct_ws = 3, wsi_n_top_terms = 20, type = "witi") {
                private$test_content <- .generate_test_content(input_model, input_corpus, n_top_terms, bottom_terms_percentile, exact_n, frac, n_top_topics, n_topiclabel_words, difficulty, use_frex_words = use_frex_words, input_dfm = input_dfm, btm_dataframe = btm_dataframe, type = type, n_correct_ws = n_correct_ws, wsi_n_top_terms = wsi_n_top_terms)
                self$userid <- userid
                private$hash <- .safe_hash(private$test_content)
                private$hash_input_model <- .safe_hash(input_model)
                private$hash_input_corpus <- .safe_hash(input_corpus)
                private$meta <- .generate_meta()
            },
            print = function() {
                .print_oolong_test_tm(private, self$userid)
            },
            do_word_intrusion_test = function() {
                private$check_finalized()
                .cstop(is.null(private$test_content$wi), "No word intrusion test cases. Create the oolong test with type being 'wi' or 'witi' to generate word intrusion test cases.")
                private$test_content$wi <- .do_oolong_test(private$test_content$wi)
            },
            do_topic_intrusion_test = function() {
                private$check_finalized()
                .cstop(is.null(private$test_content$ti), "No topic intrusion test cases. Create the oolong test with the corpus to generate topic intrusion test cases.")
                private$test_content$ti <- .do_oolong_test(private$test_content$ti, ui = .UI_TOPIC_INTRUSION_TEST, .ren = .ren_topic_intrusion_test)
            },
            do_word_set_intrusion_test = function() {
                private$check_finalized()
                .cstop(is.null(private$test_content$wsi), "No word set intrusion test cases. Create the oolong test with the corpus to generate topic intrusion test cases.")
                private$test_content$wsi <- .do_oolong_test(private$test_content$wsi, .ren = .ren_word_set_intrusion_test)
            }
        ),
        private = list(
            hash = NULL,
            test_content = list(),
            finalized = FALSE
        )
        )


