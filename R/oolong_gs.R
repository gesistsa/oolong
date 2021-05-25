
.generate_gold_standard <- function(input_corpus, exact_n = NULL, frac = 0.01) {
    if ("corpus" %in% class(input_corpus)) {
        input_corpus <- as.character(input_corpus)
    }
    if (!is.null(frac) & is.null(exact_n)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(input_corpus) * frac)
    }
    sample_vec <- .sample_corpus(input_corpus, exact_n)
    target_text <- input_corpus[sample_vec]
    test_items <- tibble::tibble(case = seq_len(exact_n), text = target_text, answer = NA)
    return(test_items)
}


.turn_gold <- function(test_content) {
    res <- quanteda::corpus(test_content$gs$text)
    quanteda::docvars(res, "answer") <- test_content$gs$answer
    class(res) <- append("oolong_gold_standard", class(res))
    return(res)
}

#' Print oolong gold standard object
#'
#' This function prints a summary of the oolong gold standard object. An oolong gold standard object is a result of $turn_gold() method. It is a quanteda::corpus compatible object.
#' @param x an oolong gold standard object
#' @param ... other parameters
#' @method print oolong_gold_standard
#' @author Chung-hong Chan
#' @return None, a summary of the quanteda::corpus and what you should do are displayed
#' @export
print.oolong_gold_standard <- function(x, ...) {
    ##quanteda:::print.corpus(x)
    ## I just don't want to use the triple-colon operator, why makes it so hard?
    y <- x
    class(y) <- class(y)[class(y) != "oolong_gold_standard"]
    print(y)
    .cp(TRUE, "Access the answer from the coding with quanteda::docvars(obj, 'answer')")
}

.print_oolong_test_gs <- function(private, userid) {
    bool_finalized <- private$finalized
    cli::cli_h1("oolong (gold standard generation)")
    .check_version(private)
    if (!is.na(userid)) {
        cli::cli_text(cli::symbol$smiley, " ", userid)
    }
    cli::cli_alert_info("{.strong GS:} n = {nrow(private$test_content$gs)}, {sum(!is.na(private$test_content$gs$answer))} coded.")
    cli::cli_alert_info("{.strong Construct:}  {private$construct}.")
    cli::cli_h2("Methods")
    cli::cli_ul()
    if (bool_finalized) {
        cli::cli_li("{.cls $turn_gold()}: convert the test results into a quanteda corpus")
    } else {
        cli::cli_li("{.cls $do_gold_standard_test()}: generate gold standard")
        cli::cli_li("{.cls $lock()}: finalize this object and see the results")        
    }
    cli::cli_end()
}

Oolong_test_gs <-
    R6::R6Class(
        "oolong_test_gs",
        inherit = Oolong_test,
        public = list(
            initialize = function(input_corpus, exact_n = NULL, frac = 0.01, construct = "positive", userid = NA) {
                private$test_content$gs <- .generate_gold_standard(input_corpus, exact_n, frac)
                self$userid <- userid
                private$construct <- construct
                private$hash <- .safe_hash(private$test_content)
                private$hash_input_corpus <- .safe_hash(input_corpus)
                private$meta <- .generate_meta()
            },
            print = function() {
                .print_oolong_test_gs(private, self$userid)
            },
            do_gold_standard_test = function() {
                private$check_finalized()
                .ren <- function(output, test_content, res, hash = NULL) {
                    return(.ren_gold_standard_test(output, test_content, res, construct = private$construct, hash = NULL))
                }
                private$test_content$gs <- .do_oolong_test(private$test_content$gs, ui = .UI_GOLD_STANDARD_TEST, .ren = .ren)
            },
            turn_gold = function() {
                .cstop(!private$finalized, "You must first lock this object to use this method.")
                .turn_gold(private$test_content)
            }
        ),
        private = list(
            hash = NULL,
            test_content = list(),
            finalized = FALSE,
            construct = NULL
        )
    )
