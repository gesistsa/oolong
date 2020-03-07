### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.


.generate_gold_standard <- function(input_corpus, exact_n = NULL, frac = 0.01) {
    if ("corpus" %in% class(input_corpus)) {
        input_corpus <- quanteda::texts(input_corpus)
    }
    if (!is.null(frac) & is.null(exact_n)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(input_corpus) * frac)
    }
    sample_vec <- .sample_corpus(input_corpus, exact_n)
    target_text <- input_corpus[sample_vec]
    test_content <- tibble::tibble(case = seq_len(exact_n), text = target_text, answer = NA)
    return(test_content)
}

.UI_GOLD_STANDARD_TEST <- miniUI::miniPage(
                                        miniUI::gadgetTitleBar("oolong"),
                                        miniUI::miniContentPanel(
                                                    shiny::uiOutput("current_topic"),
                                                    shiny::uiOutput("text_content"),
                                                    shiny::uiOutput("score_slider"),
                                                    shiny::actionButton("confirm", "confirm"),
                                                    shiny::actionButton("nextq", "skip")
                                                )
                                    )

.ren_gold_standard_test <- function(output, test_content, res, construct = "positive") {
    .ren_choices <- function(test_content, res, construct) {        
        shiny::renderUI({
            shiny::sliderInput("intruder", label = paste("How ", construct, "is this text? (1 = Very not ", construct, "; 5 = Very ", construct, ")"), min = 1, max = 5, value = ifelse(is.na(res$intruder[res$current_row]), 3, res$intruder[res$current_row]), ticks = FALSE)
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderUI({
            shiny::strong(paste("Case ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]")))
        })
    }
    .ren_text_content <- function(test_content, res) {
        shiny::renderUI({
            shiny::tagList(
                shiny::hr(),
                shiny::p(test_content$text[res$current_row]),
                shiny::hr()
            )
        })
    }
    output$score_slider <- .ren_choices(test_content, res, construct)
    output$current_topic <- .ren_topic_bar(test_content, res)
    output$text_content <- .ren_text_content(test_content, res)
    return(output)
}

.turn_gold <- function(test_content) {
    res <- quanteda::corpus(test_content$gold_standard$text)
    quanteda::docvars(res, "answer") <- test_content$gold_standard$answer
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

Oolong_test_gs <-
    R6::R6Class(
            "oolong_test_gs",
            inherit = Oolong_test,
            public = list(
                initialize = function(input_corpus, exact_n = NULL, frac = 0.01, construct = "positive") {
                    private$test_content$gold_standard <- .generate_gold_standard(input_corpus, exact_n, frac)
                    private$hash <- digest::digest(private$test_content, algo = "sha1")
                    private$construct <- construct
                },
                print = function() {
                    .cp(TRUE, "An oolong test object (gold standard generation) with ", nrow(private$test_content$gold_standard), " cases, ", sum(!is.na(private$test_content$gold_standard$answer)), " coded.")
                    .cp(!private$finalized, "Use the method $do_gold_standard_test() to generate gold standard.")
                    .cp(private$finalized, "Use the method $turn_gold() to convert the test results into a quanteda corpus.")
                    .cp(!private$finalized, "Use the method $lock() to finalize this object and see the results.")
                },
                do_gold_standard_test = function() {
                    private$check_finalized()
                    .ren <- function(output, test_content, res) {
                        return(.ren_gold_standard_test(output, test_content, res, construct = private$construct))
                    }
                    private$test_content$gold_standard <- .do_oolong_test(private$test_content$gold_standard, ui = .UI_GOLD_STANDARD_TEST, .ren = .ren)
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

