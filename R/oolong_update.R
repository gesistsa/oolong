## For future update, to notify users about old version and run update_oolong.
.check_version <- function(private) {
    if (.check_oolong(private)) {
        cli::cli_alert_warning("This object was created using an old version of oolong. Please update this object using update_oolong().")
        stop()
    }
}

.check_oolong <- function(private) {
    if (is.null(private$meta)) {
        return(TRUE)
    }
    if (any(purrr::map_lgl(c("topic", "word", "gold_standard"), ~. %in% names(private$test_content)))) {
        return(TRUE)
    }
    return(FALSE)
}

#' Check whether the oolong needs to be updated
#'
#' This function raises an error when the input oolong object needs to be updated. Oolong objects generated with an old version of oolong need to be updated to use the functionalities from the recent versions of oolong.
#' @param oolong an oolong object to be checked
#' @param verbose, logical, display messages
#' @return Nothing
#' @author Chung-hong Chan
#' @export
check_oolong <- function(oolong, verbose = FALSE) {
    .cstop(.check_oolong(oolong$.__enclos_env__$private), "Please update this oolong object with update_oolong()")
    .cp(verbose, "This oolong might probably be fine!")
}

#' Update an oolong object to the latest version
#'
#' This function update an old oolong object to the latest version.
#'
#' @param oolong an oolong object to be updated
#' @param verbose, logical, display messages
#' @return an updated oolong object
#' @author Chung-hong Chan
#' @export
update_oolong <- function(oolong, verbose = TRUE) {
    .cstop(!.check_oolong(oolong$.__enclos_env__$private), "This oolong object does not need to be updated.")
    if ("oolong_test_tm" %in% class(oolong)) {
        ## generate a dummy oolong object
        new_oolong <- create_oolong(abstracts_seededlda)
        new_oolong$.__enclos_env__$private$finalized <- oolong$.__enclos_env__$private$finalized
        new_oolong$.__enclos_env__$private$test_content <- oolong$.__enclos_env__$private$test_content
        ## renaming test_content
        old_names <- names(new_oolong$.__enclos_env__$private$test_content)
        names(new_oolong$.__enclos_env__$private$test_content)[old_names == "word"] <- "wi"
        names(new_oolong$.__enclos_env__$private$test_content)[old_names == "topic"] <- "ti"
        ## move hash_input_corpus and hash_input_corpus
        new_oolong$.__enclos_env__$private$hash_input_model <- oolong$.__enclos_env__$private$hash_input_model
        new_oolong$.__enclos_env__$private$hash_input_corpus <- oolong$.__enclos_env__$private$hash_input_corpus
        new_oolong$userid <- oolong$userid
        new_oolong$.__enclos_env__$private$meta <- .generate_meta()
        new_oolong$.__enclos_env__$private$hash <- .safe_hash(new_oolong$.__enclos_env__$private$test_content)
    }
    if ("oolong_test_gs" %in% class(oolong)) {
        new_oolong <- gs(abstracts$text, exact_n = 1)
        new_oolong$.__enclos_env__$private$finalized <- oolong$.__enclos_env__$private$finalized
        new_oolong$.__enclos_env__$private$test_content <- oolong$.__enclos_env__$private$test_content
        names(new_oolong$.__enclos_env__$private$test_content) <- "gs"
        new_oolong$userid <- oolong$userid
        new_oolong$.__enclos_env__$private$construct <- oolong$.__enclos_env__$private$construct
        new_oolong$.__enclos_env__$private$hash <- .safe_hash(new_oolong$.__enclos_env__$private$test_content)
        new_oolong$.__enclos_env__$private$hash_input_corpus <- oolong$.__enclos_env__$private$hash_input_corpus
        new_oolong$.__enclos_env__$private$meta <- .generate_meta()
    }
    if (is.null(new_oolong$userid)) {
        new_oolong$userid <- NA
        if (verbose) {
            warning("Please consider setting the userid by assigning the userid to the slot $userid, e.g. oolong$userid <- \"myname\"")
        }
    }
    if (is.null(new_oolong$.__enclos_env__$private$hash_input_model) & is.null(new_oolong$.__enclos_env__$private$hash_input_corpus) & verbose) {
        warning("The oolong object is too old. Some security features might not be available in the updated oolong object.")
    }
    return(new_oolong)
}
