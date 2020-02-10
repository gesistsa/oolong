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
    res <- list()
    all_word_answers <- purrr::map_dfc(obj_list, ~ .$.__enclos_env__$private$test_content$word$answer)
    word_intruder <- obj_list[[1]]$.__enclos_env__$private$test_content$word$intruder
    correction_matrix <- all_word_answers == word_intruder
    res$kripp_alpha <- irr::kripp.alpha(t(ifelse(correction_matrix, 2, 1)))$value
    res$k_precision <- apply(correction_matrix, 1, sum) / ncol(correction_matrix)
    res$rater_precision <- as.vector(apply(correction_matrix, 2, sum) / nrow(correction_matrix))
    class(res) <- append(class(res), "oolong_summary")
    return(res)
}

#' @export
print.oolong_summary <- function(oolong_summary) {
    .cp(TRUE, "Mean model precision: ", mean(oolong_summary$rater_precision))
    .cp(TRUE, "Quantiles of Model precision: ", paste(quantile(oolong_summary$rater_precision), collapse = ", "))
    .cp(TRUE, "Krippendorf's alpha: ", oolong_summary$kripp_alpha)
    .cp(TRUE, "K Precision: ", paste(round(oolong_summary$k_precision, 1), collapse = ", "))
}
