### CODING STYLE
### function names: use full name, except ren for render
### data structures: use singular, except list-column.


.generate_gold_standard <- function(input_corpus, exact_n = NULL, frac = 0.01, test_value  = NULL) {
    if ("corpus" %in% class(input_corpus)) {
        input_corpus <- quanteda::texts(input_corpus)
    }
    if (!is.null(frac) & is.null(exact_n)) {
        stopifnot(frac >= 0 & frac <= 1)
        exact_n <- floor(length(input_corpus) * frac)
    }
    sample_vec <- .sample_corpus(input_corpus, exact_n)
    target_text <- input_corpus[sample_vec]
    if (!is.null(test_value)) {
        warning("Specifying test_value before coding is not recommended.")
        target_value <- test_value[sample_vec]
    } else {
        target_value <- NA
    }
    test_content <- tibble::tibble(case = seq_len(exact_n), text = target_text, answer = NA, target_value = target_value)
    return(test_content)
}

.UI_GOLD_STANDARD_TEST <- miniUI::miniPage(
                                        miniUI::gadgetTitleBar("oolong"),
                                        miniUI::miniContentPanel(
                                                    shiny::textOutput("current_topic"),
                                                    shiny::uiOutput("text_content"),
                                                    shiny::uiOutput("score_slider"),
                                                    shiny::actionButton("confirm", "confirm"),
                                                    shiny::actionButton("nextq", "skip")
                                                )
                                    )

.ren_gold_standard_test <- function(output, test_content, res, construct = "positive") {
    .ren_choices <- function(test_content, res, construct, max_score) {        
        shiny::renderUI({
            shiny::sliderInput("intruder", label = paste("How ", construct, "is this text?"), min = 1, max = 5, value = ifelse(is.na(res$intruder[res$current_row]), 3, res$intruder[res$current_row]), ticks = FALSE)
        })
    }
    .ren_topic_bar <- function(test_content, res) {
        shiny::renderText({
            paste("Case ", res$current_row, "of", nrow(test_content), ifelse(is.na(res$intruder[res$current_row]), "", " [coded]"))
        })
    }
    .ren_text_content <- function(test_content, res) {
        shiny::renderUI({
            shiny::pre(test_content$text[res$current_row])
        })
    }
    output$score_slider <- .ren_choices(test_content, res, construct)
    output$current_topic <- .ren_topic_bar(test_content, res)
    output$text_content <- .ren_text_content(test_content, res)
    return(output)
}
