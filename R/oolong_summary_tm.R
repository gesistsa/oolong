.check_hash_dot <- function(...) {
    length(unique(purrr::map_chr(list(...), ~ .$.__enclos_env__$private$hash))) == 1
}

.check_lock_dot <- function(...) {
    all(purrr::map_lgl(list(...), ~ .$.__enclos_env__$private$finalized))
}


.check_finished <- function(oolong) {
    if (!is.null(oolong$.__enclos_env__$private$test_content$wi) & any(is.na(oolong$.__enclos_env__$private$test_content$wi$answer))) {
        return(FALSE)
    }
    if (!is.null(oolong$.__enclos_env__$private$test_content$ti) & any(is.na(oolong$.__enclos_env__$private$test_content$ti$answer))) {
        return(FALSE)
    }
    return(TRUE)
}

.check_finished_dot <- function(...) {
    all(purrr::map_lgl(list(...), ~ .check_finished(.)))
}

### check whether the oolong object is in a new state. (i.e. Not yet coded in either word, topic and wsi tests.)
.check_new <- function(oolong) {
    all(purrr::map_lgl(oolong$.__enclos_env__$private$test_content, ~is.null(.) | (!is.null(.) & all(is.na(.$answer)))))
}


.clone_obj_list <- function(obj_list) {
    purrr::map(obj_list, ~ .$clone(deep = FALSE))
}

.cal_wi <- function(obj_list, res) {
    all_word_test_content <- purrr::map(obj_list, ~ .$.__enclos_env__$private$test_content$wi)
    if (any(purrr::map_lgl(all_word_test_content, is.null))) {
        res$rater_precision <- NA
        res$k_precision <- NA
        res$kripp_alpha <- NA
        res$multiple_test <- NA
        res$rater_precision_p_value <- NA
    } else {
        n_choices <- length(obj_list[[1]]$.__enclos_env__$private$test_content$wi$candidates[[1]])
        all_word_answers <- suppressMessages(purrr::map_dfc(obj_list, ~ .$.__enclos_env__$private$test_content$wi$answer))
        word_intruder <- obj_list[[1]]$.__enclos_env__$private$test_content$wi$intruder
        correction_matrix <- all_word_answers == word_intruder
        res$k_precision <- apply(correction_matrix, 1, sum) / ncol(correction_matrix)
        n_correct <- apply(correction_matrix, 2, sum)
        res$rater_precision <- as.vector(n_correct / nrow(correction_matrix))
        if (length(obj_list) == 1) {
            res$kripp_alpha <- NA
            res$multiple_test <- NA
            res$rater_precision_p_value <- NA
        } else {
            res$kripp_alpha <- irr::kripp.alpha(t(ifelse(correction_matrix, 2, 1)))$value
            res$multiple_test <- purrr::map(n_correct, ~binom.test(., n = nrow(correction_matrix), p = 1/n_choices, alternative = "greater"))
            res$rater_precision_p_value <- .combine_p_fisher(purrr::map_dbl(res$multiple_test, "p.value"))
        }
    }
    return(res)
}

.cal_ti <- function(obj_list, res, n_iter = 1500) {
    all_topic_test_content <- purrr::map(obj_list, ~ .$.__enclos_env__$private$test_content$ti)
    if (any(purrr::map_lgl(all_topic_test_content, is.null))) {
        res$tlo <- NA
        res$tlo_p_value <- NA
    } else {
        res$tlo <- .cal_tlo(purrr::map_dfr(all_topic_test_content, ~.), mean_value = FALSE) ### it should not be just the mean.
        monkey_median <- unlist(replicate(n_iter, .monkey_median(.clone_obj_list(obj_list))))
        res$tlo_p_value <- sum(monkey_median > median(res$tlo)) / n_iter
    }
    return(res)
}

.cal_wsi <- function(obj_list, res) {
    all_wsi_test_content <- purrr::map(obj_list, ~ .$.__enclos_env__$private$test_content$wsi)
    if (any(purrr::map_lgl(all_wsi_test_content, is.null))) {
        res$rater_precision_wsi <- NA
        res$k_precision_wsi <- NA
        res$kripp_alpha_wsi <- NA
    } else {
        n_choices <- length(obj_list[[1]]$.__enclos_env__$private$test_content$wsi$candidates[[1]])
        all_wsi_answers <- suppressMessages(purrr::map_dfc(obj_list, ~ .$.__enclos_env__$private$test_content$wsi$answer))
        wsi_intruder <- obj_list[[1]]$.__enclos_env__$private$test_content$wsi$intruder
        correction_matrix <- all_wsi_answers == wsi_intruder
        res$k_precision_wsi <- apply(correction_matrix, 1, sum) / ncol(correction_matrix)
        n_correct <- apply(correction_matrix, 2, sum)
        res$rater_precision_wsi <- as.vector(n_correct / nrow(correction_matrix))
        if (length(obj_list) == 1) {
            res$kripp_alpha_wsi <- NA                        
        } else {
            res$kripp_alpha_wsi <- irr::kripp.alpha(t(ifelse(correction_matrix, 2, 1)))$value
        }
    }
    return(res)
}

.summarize_oolong_tm <- function(..., n_iter = 1500) {
    .cstop(!.check_hash_dot(...), "Not all oolong object(s) are created with the same conditions.")
    .cstop(!.check_lock_dot(...), "Not all oolong objects(s) are locked.")
    if(!.check_finished_dot(...)) {
        warning("Some input objects were locked forcibly. Summary results might not make sense.")
    }
    obj_list <- list(...)
    res <- list()
    res$n_models <- length(obj_list)
    res <- .cal_wi(obj_list, res)
    res <- .cal_ti(obj_list, res, n_iter = n_iter)
    res <- .cal_wsi(obj_list, res)
    res$obj_list <- .clone_obj_list(obj_list)
    res$type <- "tm"
    class(res) <- append(class(res), "oolong_summary")
    return(res)
}

.combine_p_fisher <- function(p_values) {
    chisq <- (-2) * sum(log(p_values))
    df <- 2 * length(p_values)
    p <- pchisq(chisq, df, lower.tail = FALSE)
    return(p)
}

.monkey_test <- function(oolong, intelligent = 0) {
    if (!is.null(oolong$.__enclos_env__$private$test_content$wi)) {
        oolong$.__enclos_env__$private$test_content$wi$answer <- purrr::map_chr(oolong$.__enclos_env__$private$test_content$wi$candidates, ~ .safe_sample(., 1))
        correct <- rep(FALSE, nrow(oolong$.__enclos_env__$private$test_content$wi))
        correct[.safe_sample(seq_along(correct), size = floor(length(correct) * intelligent))] <- TRUE
        oolong$.__enclos_env__$private$test_content$wi$answer[correct] <- oolong$.__enclos_env__$private$test_content$wi$intruder[correct]
    }
    if (!is.null(oolong$.__enclos_env__$private$test_content$ti)) {
        oolong$.__enclos_env__$private$test_content$ti$answer <- purrr::map_int(oolong$.__enclos_env__$private$test_content$ti$candidates, ~ .safe_sample(., 1))
        correct <- rep(FALSE, nrow(oolong$.__enclos_env__$private$test_content$ti))
        correct[.safe_sample(seq_along(correct), size = floor(length(correct) * intelligent))] <- TRUE
        oolong$.__enclos_env__$private$test_content$ti$answer[correct] <- oolong$.__enclos_env__$private$test_content$ti$intruder[correct]
    }
    return(oolong)
}

.monkey_median <- function(obj_list) {
    monkeyed_obj_list <- purrr::map(obj_list, .monkey_test)
    all_topic_test_content <- purrr::map(monkeyed_obj_list, ~ .$.__enclos_env__$private$test_content$ti)
    median(.cal_tlo(purrr::map_dfr(all_topic_test_content, ~.), mean_value = FALSE))
}
