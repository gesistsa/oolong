.check_hash_dot <- function(...) {
    length(unique(purrr::map_chr(list(...), ~ .$.__enclos_env__$private$hash))) == 1
}

.check_lock_dot <- function(...) {
    all(purrr::map_lgl(list(...), ~ .$.__enclos_env__$private$finalized))
}


.check_finished <- function(oolong) {
    if (!is.null(oolong$.__enclos_env__$private$test_content$word) & any(is.na(oolong$.__enclos_env__$private$test_content$word$answer))) {
        return(FALSE)
    }
    if (!is.null(oolong$.__enclos_env__$private$test_content$topic) & any(is.na(oolong$.__enclos_env__$private$test_content$topic$answer))) {
        return(FALSE)
    }
    return(TRUE)
}

.check_finished_dot <- function(...) {
    all(purrr::map_lgl(list(...), ~ .check_finished(.)))
}

### check whether the oolong object is in a new state. (i.e. Not yet coded in either word and topic tests.)
.check_new <- function(oolong) {
    word_clean <- is.null(oolong$.__enclos_env__$private$test_content$word) | (!is.null(oolong$.__enclos_env__$private$test_content$word) & all(is.na(oolong$.__enclos_env__$private$test_content$word$answer)))
    topic_clean <- is.null(oolong$.__enclos_env__$private$test_content$topic) | (!is.null(oolong$.__enclos_env__$private$test_content$topic) & all(is.na(oolong$.__enclos_env__$private$test_content$topic$answer)))
    return(word_clean & topic_clean)
}

#' Clone a oolong object.
#'
#' Clone a new oolong object. The oolong must not be locked and ever coded.
#' @param oolong an oolong object.
#' @export
clone_oolong <- function(oolong) {
    if (oolong$.__enclos_env__$private$finalized) {
        stop("oolong is locked.")
    }
    if (!.check_new(oolong)) {
        stop("oolong is partially coded.")
    }
    oolong$clone(deep = FALSE)
}

#' Summarize multiple oolong objects.
#'
#' Summarise multiple oolong objects.
#' 
#' @export
summarize_oolong <- function(...) {
    if(!.check_hash_dot(...)) {
        stop("Not all oolong object(s) are created with the same conditions.")
    }
    if(!.check_lock_dot(...)) {
        stop("Not all oolong objects(s) are locked.")
    }
    if(!.check_finished_dot(...)) {
        warning("Some input objects were locked forcibly. Summary results might not make sense.")
    }
    obj_list <- list(...)
    n_choices <- length(obj_list[[1]]$.__enclos_env__$private$test_content$word$candidates[[1]])
    res <- list()
    all_word_answers <- purrr::map_dfc(obj_list, ~ .$.__enclos_env__$private$test_content$word$answer)
    word_intruder <- obj_list[[1]]$.__enclos_env__$private$test_content$word$intruder
    correction_matrix <- all_word_answers == word_intruder
    if (length(obj_list) == 1) {
        res$kripp_alpha <- NA
    } else {
        res$kripp_alpha <- irr::kripp.alpha(t(ifelse(correction_matrix, 2, 1)))$value
    }
    res$k_precision <- apply(correction_matrix, 1, sum) / ncol(correction_matrix)
    n_correct <- apply(correction_matrix, 2, sum)
    res$rater_precision <- as.vector(n_correct / nrow(correction_matrix))
    if (length(obj_list) == 1) {
        res$kripp_alpha <- NA
        res$multiple_test <- NA
    } else {
        res$kripp_alpha <- irr::kripp.alpha(t(ifelse(correction_matrix, 2, 1)))$value
        res$multiple_test <- purrr::map(n_correct, ~binom.test(., n = nrow(correction_matrix), p = 1/n_choices, alternative = "greater"))
    }    
    res$n_models <- length(obj_list)
    all_topic_test_content <- purrr::map(obj_list, ~ .$.__enclos_env__$private$test_content$topic)
    if (any(purrr::map_lgl(all_topic_test_content, is.null))) {
        res$tlo <- NA
    } else {
        res$tlo <- .cal_tlo(purrr::map_dfr(all_topic_test_content, ~.), mean_value = FALSE) ### it should not be just the mean.
    }
    res$obj_list <- obj_list
    class(res) <- append(class(res), "oolong_summary")
    return(res)
}

#' @export
summarise_oolong <- function(...) {
    summarize_oolong(...)
}

#' @export
print.oolong_summary <- function(oolong_summary) {
    .cp(TRUE, "Mean model precision: ", mean(oolong_summary$rater_precision))
    .cp(oolong_summary$n_models > 1, "Quantiles of model precision: ", paste(quantile(oolong_summary$rater_precision), collapse = ", "))
    .cp(oolong_summary$n_models > 1, "P-value of model precision (H0: Model precision is not better than random guess): ", .combine_p_fisher(purrr::map_dbl(oolong_summary$multiple_test, "p.value")))
    .cp(oolong_summary$n_models > 1, "Krippendorff's alpha: ", oolong_summary$kripp_alpha)
    .cp(TRUE, "K Precision: ", paste(round(oolong_summary$k_precision, 1), collapse = ", "))
    .cp(!is.na(oolong_summary$tlo[1]), "Mean TLO: ", round(mean(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Median TLO: ", round(median(oolong_summary$tlo), 2))
    .cp(!is.na(oolong_summary$tlo[1]), "Quantiles of TLO: ", paste(quantile(oolong_summary$tlo), collapse = ", "))
    if (!is.na(oolong_summary$tlo[1])) {
        monkey_median <- unlist(replicate(1000, .monkey_median(oolong_summary$obj_list)))
        .cp(TRUE, "P-Value of the median TLO (H0: Median TLO is not better than random guess): ", sum(monkey_median > median(oolong_summary$tlo)) / 1000)
    }
}

.combine_p_fisher <- function(p_values) {
    chisq <- (-2) * sum(log(p_values))
    df <- 2 * length(p_values)
    p <- pchisq(chisq, df, lower.tail = FALSE)
    return(p)
}

.monkey_test <- function(oolong, intelligent = 0) {
    oolong$.__enclos_env__$private$test_content$word$answer <- purrr::map_chr(oolong$.__enclos_env__$private$test_content$word$candidates, ~ sample(., 1))
    correct <- rep(FALSE, nrow(oolong$.__enclos_env__$private$test_content$word))
    correct[sample(seq_along(correct), size = floor(length(correct) * intelligent))] <- TRUE
    oolong$.__enclos_env__$private$test_content$word$answer[correct] <- oolong$.__enclos_env__$private$test_content$word$intruder[correct]
    if (!is.null(oolong$.__enclos_env__$private$test_content$topic)) {
        oolong$.__enclos_env__$private$test_content$topic$answer <- purrr::map_int(oolong$.__enclos_env__$private$test_content$topic$candidates, ~ sample(., 1))
        correct <- rep(FALSE, nrow(oolong$.__enclos_env__$private$test_content$topic))
        correct[sample(seq_along(correct), size = floor(length(correct) * intelligent))] <- TRUE
        oolong$.__enclos_env__$private$test_content$topic$answer[correct] <- oolong$.__enclos_env__$private$test_content$topic$intruder[correct]
    }
    return(oolong)
}

.monkey_median <- function(obj_list) {
    monkeyed_obj_list <- purrr::map(obj_list, .monkey_test)
    all_topic_test_content <- purrr::map(monkeyed_obj_list, ~ .$.__enclos_env__$private$test_content$topic)
    median(.cal_tlo(purrr::map_dfr(all_topic_test_content, ~.), mean_value = FALSE))
}
